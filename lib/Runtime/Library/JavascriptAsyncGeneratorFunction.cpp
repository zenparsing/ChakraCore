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
    ARGUMENTS(stackArgs, callInfo);

    Assert(!(callInfo.Flags & CallFlags_New));

    auto* asyncGeneratorFn = VarTo<JavascriptAsyncGeneratorFunction>(function);

    // InterpreterStackFrame takes a pointer to the args, so copy them to the recycler heap
    // and use that buffer for this InterpreterStackFrame.
    Field(Var)* argsHeapCopy = RecyclerNewArray(
        scriptContext->GetRecycler(),
        Field(Var),
        stackArgs.Info.Count);

    CopyArray(argsHeapCopy, stackArgs.Info.Count, stackArgs.Values, stackArgs.Info.Count);
    Arguments heapArgs(callInfo, unsafe_write_barrier_cast<Var*>(argsHeapCopy));

    auto* library = scriptContext->GetLibrary();
    auto* prototype = library->CreateAsyncGeneratorConstructorPrototypeObject();
    auto* scriptFn = asyncGeneratorFn->GetGeneratorVirtualScriptFunction();
    auto* generator = library->CreateGenerator(heapArgs, scriptFn, prototype);

    generator->SetIsAsync();
    generator->InitialiseAsyncGenerator(scriptContext);

    // Set the prototype from constructor
    JavascriptOperators::OrdinaryCreateFromConstructor(
        function,
        generator,
        prototype,
        scriptContext);

    return generator;
}
