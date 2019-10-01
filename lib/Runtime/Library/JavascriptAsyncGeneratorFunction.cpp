//-------------------------------------------------------------------------------------------------------
// Copyright (C) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE.txt file in the project root for full license information.
//-------------------------------------------------------------------------------------------------------
#include "RuntimeLibraryPch.h"

using namespace Js;

FunctionInfo JavascriptAsyncGeneratorFunction::functionInfo(
    FORCE_NO_WRITE_BARRIER_TAG(
        JavascriptAsyncGeneratorFunction::EntryAsyncGeneratorFunctionImplementation),
    (FunctionInfo::Attributes)(FunctionInfo::DoNotProfile | FunctionInfo::ErrorOnNew));

JavascriptAsyncGeneratorFunction::JavascriptAsyncGeneratorFunction(
    DynamicType* type,
    GeneratorVirtualScriptFunction* scriptFunction) :
        JavascriptGeneratorFunction(type, &functionInfo, scriptFunction)
{
    DebugOnly(VerifyEntryPoint());
}

JavascriptAsyncGeneratorFunction* JavascriptAsyncGeneratorFunction::New(
    ScriptContext* scriptContext,
    GeneratorVirtualScriptFunction* scriptFunction)
{
    return scriptContext->GetLibrary()->CreateAsyncGeneratorFunction(
        functionInfo.GetOriginalEntryPoint(),
        scriptFunction);
}

template<>
bool Js::VarIsImpl<JavascriptAsyncGeneratorFunction>(RecyclableObject* obj)
{
    return VarIs<JavascriptFunction>(obj) && (
        VirtualTableInfo<JavascriptAsyncGeneratorFunction>::HasVirtualTable(obj) ||
        VirtualTableInfo<CrossSiteObject<JavascriptAsyncGeneratorFunction>>::HasVirtualTable(obj)
    );
}

Var JavascriptAsyncGeneratorFunction::EntryAsyncGeneratorFunctionImplementation(
    RecyclableObject* function,
    CallInfo callInfo, ...)
{
    auto* scriptContext = function->GetScriptContext();
    PROBE_STACK(scriptContext, Js::Constants::MinStackDefault);
    ARGUMENTS(args, callInfo);

    Assert(!(callInfo.Flags & CallFlags_New));

    auto* asyncGeneratorFn = VarTo<JavascriptAsyncGeneratorFunction>(function);
    auto* library = scriptContext->GetLibrary();
    auto* prototype = library->CreateAsyncGeneratorConstructorPrototypeObject();
    auto* scriptFn = asyncGeneratorFn->GetGeneratorVirtualScriptFunction();
    auto* generator = library->CreateAsyncGenerator(args, scriptFn, prototype);

    // Set the prototype from constructor
    JavascriptOperators::OrdinaryCreateFromConstructor(
        function,
        generator,
        prototype,
        scriptContext);

    // Call next on the generator to execute until the beginning of the body
    BEGIN_SAFE_REENTRANT_CALL(scriptContext->GetThreadContext())
    {
        CALL_ENTRYPOINT(
            scriptContext->GetThreadContext(),
            generator->EntryNext,
            asyncGeneratorFn,
            CallInfo(CallFlags_Value, 1),
            generator);
    }
    END_SAFE_REENTRANT_CALL

    return generator;
}
