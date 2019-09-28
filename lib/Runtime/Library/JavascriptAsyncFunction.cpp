//-------------------------------------------------------------------------------------------------------
// Copyright (C) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE.txt file in the project root for full license information.
//-------------------------------------------------------------------------------------------------------
#include "RuntimeLibraryPch.h"

using namespace Js;

FunctionInfo JavascriptAsyncFunction::functionInfo(
    FORCE_NO_WRITE_BARRIER_TAG(JavascriptAsyncFunction::EntryAsyncFunctionImplementation),
    (FunctionInfo::Attributes)(FunctionInfo::DoNotProfile | FunctionInfo::ErrorOnNew));

JavascriptAsyncFunction::JavascriptAsyncFunction(
    DynamicType* type,
    GeneratorVirtualScriptFunction* scriptFunction) :
        JavascriptGeneratorFunction(type, &functionInfo, scriptFunction)
{
    DebugOnly(VerifyEntryPoint());
}

JavascriptAsyncFunction* JavascriptAsyncFunction::New(
    ScriptContext* scriptContext,
    GeneratorVirtualScriptFunction* scriptFunction)
{
    return scriptContext->GetLibrary()->CreateAsyncFunction(
        functionInfo.GetOriginalEntryPoint(),
        scriptFunction);
}

template<>
bool Js::VarIsImpl<JavascriptAsyncFunction>(RecyclableObject* obj)
{
    return VarIs<JavascriptFunction>(obj) && (
        VirtualTableInfo<JavascriptAsyncFunction>::HasVirtualTable(obj) ||
        VirtualTableInfo<CrossSiteObject<JavascriptAsyncFunction>>::HasVirtualTable(obj)
    );
}

Var JavascriptAsyncFunction::EntryAsyncFunctionImplementation(
    RecyclableObject* function,
    CallInfo callInfo, ...)
{
    auto* scriptContext = function->GetScriptContext();
    PROBE_STACK(scriptContext, Js::Constants::MinStackDefault);
    ARGUMENTS(stackArgs, callInfo);

    auto* library = scriptContext->GetLibrary();
    auto* prototype = scriptContext->GetLibrary()->GetNull();

    // InterpreterStackFrame takes a pointer to the args, so copy them to the recycler
    // heap and use that buffer for this InterpreterStackFrame
    Field(Var)* argsHeapCopy = RecyclerNewArray(
        scriptContext->GetRecycler(),
        Field(Var), 
        stackArgs.Info.Count);

    CopyArray(argsHeapCopy, stackArgs.Info.Count, stackArgs.Values, stackArgs.Info.Count);
    Arguments heapArgs(callInfo, unsafe_write_barrier_cast<Var*>(argsHeapCopy));

    auto* generator = scriptContext->GetLibrary()->CreateGenerator(
        heapArgs,
        VarTo<JavascriptAsyncFunction>(function)->GetGeneratorVirtualScriptFunction(),
        prototype);

    auto* executor = library->CreateAsyncSpawnExecutorFunction(generator, stackArgs[0]);
    JavascriptExceptionObject* exception = nullptr;
    auto* promise = library->CreatePromise();

    JavascriptPromiseResolveOrRejectFunction* resolve;
    JavascriptPromiseResolveOrRejectFunction* reject;
    JavascriptPromise::InitializePromise(promise, &resolve, &reject, scriptContext);

    try
    {
        BEGIN_SAFE_REENTRANT_CALL(scriptContext->GetThreadContext())
        {
            CALL_FUNCTION(
                scriptContext->GetThreadContext(),
                executor,
                CallInfo(CallFlags_Value, 3),
                library->GetUndefined(),
                resolve,
                reject);
        }
        END_SAFE_REENTRANT_CALL
    }
    catch (const JavascriptException& err)
    {
        exception = err.GetAndClear();
    }

    if (exception != nullptr)
        JavascriptPromise::TryRejectWithExceptionObject(exception, reject, scriptContext);

    return promise;
}

Var JavascriptAsyncFunction::EntryAsyncSpawnExecutorFunction(
    RecyclableObject* function,
    CallInfo callInfo, ...)
{
    PROBE_STACK(function->GetScriptContext(), Js::Constants::MinStackDefault);
    ARGUMENTS(args, callInfo);

    ScriptContext* scriptContext = function->GetScriptContext();
    JavascriptLibrary* library = scriptContext->GetLibrary();
    Var undefinedVar = library->GetUndefined();
    Var resolve = undefinedVar;
    Var reject = undefinedVar;

    Assert(args.Info.Count == 3);

    resolve = args[1];
    reject = args[2];

    Assert(JavascriptConversion::IsCallable(resolve) && JavascriptConversion::IsCallable(reject));

    auto* executor = VarTo<JavascriptAsyncSpawnExecutorFunction>(function);
    Var varCallArgs[] = { undefinedVar, executor->target };

    auto* stepFn = library->CreateAsyncSpawnStepFunction(
        EntryAsyncSpawnStepNextFunction,
        executor->generator,
        varCallArgs);

    AsyncSpawnStep(stepFn, executor->generator, resolve, reject);
    return undefinedVar;
}

Var JavascriptAsyncFunction::EntryAsyncSpawnStepNextFunction(
    RecyclableObject* function,
    CallInfo callInfo, ...)
{
    auto* scriptContext = function->GetScriptContext();
    PROBE_STACK(scriptContext, Js::Constants::MinStackDefault);

    auto* stepFn = VarTo<JavascriptAsyncSpawnStepFunction>(function);
    auto* nextFn = scriptContext->GetLibrary()->EnsureGeneratorNextFunction();

    BEGIN_SAFE_REENTRANT_CALL(scriptContext->GetThreadContext())
    {
        return CALL_FUNCTION(
            scriptContext->GetThreadContext(),
            nextFn,
            CallInfo(CallFlags_Value, 2),
            stepFn->generator,
            stepFn->argument);
    }
    END_SAFE_REENTRANT_CALL
}

Var JavascriptAsyncFunction::EntryAsyncSpawnStepThrowFunction(
    RecyclableObject* function,
    CallInfo callInfo, ...)
{
    auto* scriptContext = function->GetScriptContext();
    PROBE_STACK(scriptContext, Js::Constants::MinStackDefault);

    auto* stepFn = VarTo<JavascriptAsyncSpawnStepFunction>(function);
    auto* throwFn = scriptContext->GetLibrary()->EnsureGeneratorThrowFunction();

    BEGIN_SAFE_REENTRANT_CALL(scriptContext->GetThreadContext())
    {
        return CALL_FUNCTION(
            scriptContext->GetThreadContext(),
            throwFn,
            CallInfo(CallFlags_Value, 2),
            stepFn->generator,
            stepFn->argument);
    }
    END_SAFE_REENTRANT_CALL
}

Var JavascriptAsyncFunction::EntryAsyncSpawnCallStepFunction(
    RecyclableObject* function,
    CallInfo callInfo, ...)
{
    auto* scriptContext = function->GetScriptContext();
    PROBE_STACK(scriptContext, Js::Constants::MinStackDefault);
    ARGUMENTS(args, callInfo);

    auto* library = scriptContext->GetLibrary();
    Var undefinedVar = library->GetUndefined();
    Var resolvedValue = args.Info.Count > 1 ? args[1] : undefinedVar;

    auto* stepFn = VarTo<JavascriptAsyncSpawnStepFunction>(function);
    auto* generator = stepFn->generator;

    JavascriptMethod method = stepFn->isReject
        ? EntryAsyncSpawnStepThrowFunction
        : EntryAsyncSpawnStepNextFunction;
    
    auto* nextStepFn = library->CreateAsyncSpawnStepFunction(
        method,
        generator,
        resolvedValue);

    AsyncSpawnStep(nextStepFn, generator, stepFn->resolve, stepFn->reject);
    return undefinedVar;
}

void JavascriptAsyncFunction::AsyncSpawnStep(
    JavascriptAsyncSpawnStepFunction* stepFunction,
    JavascriptGenerator* generator,
    Var resolve,
    Var reject)
{
    ScriptContext* scriptContext = generator->GetScriptContext();
    BEGIN_SAFE_REENTRANT_REGION(scriptContext->GetThreadContext())

    JavascriptLibrary* library = scriptContext->GetLibrary();
    Var undefinedVar = library->GetUndefined();

    JavascriptExceptionObject* exception = nullptr;
    RecyclableObject* result = nullptr;

    try
    {
        Var resultVar = CALL_FUNCTION(
            scriptContext->GetThreadContext(),
            stepFunction,
            CallInfo(CallFlags_Value, 1),
            undefinedVar);

        result = VarTo<RecyclableObject>(resultVar);
    }
    catch (const JavascriptException& err)
    {
        exception = err.GetAndClear();
    }

    if (exception != nullptr)
    {
        // If the generator threw an exception, reject the promise
        JavascriptPromise::TryRejectWithExceptionObject(exception, reject, scriptContext);
        return;
    }

    Assert(result != nullptr);

    Var done = JavascriptOperators::GetProperty(result, PropertyIds::done, scriptContext);

    if (JavascriptConversion::ToBool(done, scriptContext))
    {
        // If the generator is done, resolve the promise
        Var value = JavascriptOperators::GetProperty(result, PropertyIds::value, scriptContext);

        if (!JavascriptConversion::IsCallable(resolve))
            JavascriptError::ThrowTypeError(scriptContext, JSERR_NeedFunction);

        CALL_FUNCTION(
            scriptContext->GetThreadContext(),
            VarTo<RecyclableObject>(resolve),
            CallInfo(CallFlags_Value, 2),
            undefinedVar,
            value);

        return;
    }

    // Chain off the yielded promise and step again
    auto* successFunction = library->CreateAsyncSpawnStepFunction(
        EntryAsyncSpawnCallStepFunction,
        generator,
        undefinedVar,
        resolve,
        reject);

    auto* failFunction = library->CreateAsyncSpawnStepFunction(
        EntryAsyncSpawnCallStepFunction,
        generator,
        undefinedVar,
        resolve,
        reject,
        true);

    Var value = JavascriptOperators::GetProperty(result, PropertyIds::value, scriptContext);
    auto* promise = JavascriptPromise::InternalPromiseResolve(value, scriptContext);
    auto* unused = JavascriptPromise::UnusedPromiseCapability(scriptContext);
    
    JavascriptPromise::PerformPromiseThen(
        promise,
        unused,
        successFunction,
        failFunction,
        scriptContext);

    END_SAFE_REENTRANT_REGION
}

template<>
bool Js::VarIsImpl<JavascriptAsyncSpawnExecutorFunction>(RecyclableObject* obj)
{
    return VarIs<JavascriptFunction>(obj) && (
        VirtualTableInfo<JavascriptAsyncSpawnExecutorFunction>::HasVirtualTable(obj) ||
        VirtualTableInfo<CrossSiteObject<JavascriptAsyncSpawnExecutorFunction>>::HasVirtualTable(obj)
    );
}

template<>
bool Js::VarIsImpl<JavascriptAsyncSpawnStepFunction>(RecyclableObject* obj)
{
    return VarIs<JavascriptFunction>(obj) && (
        VirtualTableInfo<JavascriptAsyncSpawnStepFunction>::HasVirtualTable(obj) ||
        VirtualTableInfo<CrossSiteObject<JavascriptAsyncSpawnStepFunction>>::HasVirtualTable(obj)
    );
}

#if ENABLE_TTD

TTD::NSSnapObjects::SnapObjectType JavascriptAsyncFunction::GetSnapTag_TTD() const
{
    return TTD::NSSnapObjects::SnapObjectType::SnapAsyncFunction;
}

void JavascriptAsyncFunction::ExtractSnapObjectDataInto(TTD::NSSnapObjects::SnapObject* objData, TTD::SlabAllocator& alloc)
{
    TTD::NSSnapObjects::SnapGeneratorFunctionInfo* fi = nullptr;
    uint32 depCount = 0;
    TTD_PTR_ID* depArray = nullptr;

    this->CreateSnapObjectInfo(alloc, &fi, &depArray, &depCount);

    if (depCount == 0)
    {
        TTD::NSSnapObjects::StdExtractSetKindSpecificInfo<TTD::NSSnapObjects::SnapGeneratorFunctionInfo*, TTD::NSSnapObjects::SnapObjectType::SnapAsyncFunction>(objData, fi);
    }
    else
    {
        TTDAssert(depArray != nullptr, "depArray should be non-null if depCount is > 0");
        TTD::NSSnapObjects::StdExtractSetKindSpecificInfo<TTD::NSSnapObjects::SnapGeneratorFunctionInfo*, TTD::NSSnapObjects::SnapObjectType::SnapAsyncFunction>(objData, fi, alloc, depCount, depArray);
    }
}

void JavascriptAsyncSpawnExecutorFunction::MarkVisitKindSpecificPtrs(TTD::SnapshotExtractor* extractor)
{
    if (this->generator != nullptr)
    {
        extractor->MarkVisitVar(this->generator);
    }

    if (this->target != nullptr)
    {
        extractor->MarkVisitVar(this->target);
    }
}

TTD::NSSnapObjects::SnapObjectType JavascriptAsyncSpawnExecutorFunction::GetSnapTag_TTD() const
{
    return TTD::NSSnapObjects::SnapObjectType::JavascriptAsyncSpawnExecutorFunction;
}

void JavascriptAsyncSpawnExecutorFunction::ExtractSnapObjectDataInto(TTD::NSSnapObjects::SnapObject* objData, TTD::SlabAllocator& alloc)
{
    TTD::NSSnapObjects::SnapJavascriptAsyncSpawnExecutorFunctionInfo* info = alloc.SlabAllocateStruct<TTD::NSSnapObjects::SnapJavascriptAsyncSpawnExecutorFunctionInfo>();
    info->generator= TTD_CONVERT_VAR_TO_PTR_ID(this->generator);
    info->target = TTD_CONVERT_JSVAR_TO_TTDVAR(this->target);
    TTD::NSSnapObjects::StdExtractSetKindSpecificInfo<TTD::NSSnapObjects::SnapJavascriptAsyncSpawnExecutorFunctionInfo*, TTD::NSSnapObjects::SnapObjectType::JavascriptAsyncSpawnExecutorFunction>(objData, info);
}

void JavascriptAsyncSpawnStepFunction::MarkVisitKindSpecificPtrs(TTD::SnapshotExtractor* extractor)
{
    if (this->generator != nullptr)
    {
        extractor->MarkVisitVar(this->generator);
    }

    if (this->reject != nullptr)
    {
        extractor->MarkVisitVar(this->reject);
    }

    if (this->resolve != nullptr)
    {
        extractor->MarkVisitVar(this->resolve);
    }

    if (this->argument != nullptr)
    {
        extractor->MarkVisitVar(this->argument);
    }
}

TTD::NSSnapObjects::SnapObjectType JavascriptAsyncSpawnStepFunction::GetSnapTag_TTD() const
{
    return TTD::NSSnapObjects::SnapObjectType::JavascriptAsyncSpawnStepFunction;
}

void JavascriptAsyncSpawnStepFunction::ExtractSnapObjectDataInto(TTD::NSSnapObjects::SnapObject* objData, TTD::SlabAllocator& alloc)
{
    TTD::NSSnapObjects::SnapJavascriptAsyncSpawnStepFunctionInfo* info = alloc.SlabAllocateStruct<TTD::NSSnapObjects::SnapJavascriptAsyncSpawnStepFunctionInfo>();
    info->generator = TTD_CONVERT_VAR_TO_PTR_ID(this->generator);
    info->reject = this->reject;
    info->resolve = this->resolve;
    info->argument = this->argument;
    info->isReject = this->isReject;

    info->entryPoint = 0;
    JavascriptMethod entryPoint = this->GetFunctionInfo()->GetOriginalEntryPoint();
    if (entryPoint == JavascriptAsyncFunction::EntryAsyncSpawnStepNextFunction)
    {
        info->entryPoint = 1;
    }
    else if (entryPoint == JavascriptAsyncFunction::EntryAsyncSpawnStepThrowFunction)
    {
        info->entryPoint = 2;
    }
    else if (entryPoint == JavascriptAsyncFunction::EntryAsyncSpawnCallStepFunction)
    {
        info->entryPoint = 3;
    }
    else
    {
        TTDAssert(false, "Unexpected entrypoint found JavascriptAsyncSpawnStepArgumentExecutorFunction");
    }

    const uint32 maxDeps = 4;
    uint32 depCount = 0;
    TTD_PTR_ID* depArray = alloc.SlabReserveArraySpace<TTD_PTR_ID>(maxDeps);
    if (this->reject != nullptr &&  TTD::JsSupport::IsVarComplexKind(this->reject))
    {
        depArray[depCount] = TTD_CONVERT_VAR_TO_PTR_ID(this->reject);
        depCount++;
    }

    if (this->resolve != nullptr &&  TTD::JsSupport::IsVarComplexKind(this->resolve))
    {
        depArray[depCount] = TTD_CONVERT_VAR_TO_PTR_ID(this->resolve);
        depCount++;
    }

    if (this->argument != nullptr &&  TTD::JsSupport::IsVarComplexKind(this->argument))
    {
        depArray[depCount] = TTD_CONVERT_VAR_TO_PTR_ID(this->argument);
        depCount++;
    }

    if (this->generator != nullptr)
    {
        depArray[depCount] = TTD_CONVERT_VAR_TO_PTR_ID(this->generator);
        depCount++;
    }

    if (depCount > 0)
    {
        alloc.SlabCommitArraySpace<TTD_PTR_ID>(depCount, maxDeps);
    }
    else
    {
        alloc.SlabAbortArraySpace<TTD_PTR_ID>(maxDeps);
    }

    if (depCount == 0)
    {
        TTD::NSSnapObjects::StdExtractSetKindSpecificInfo<TTD::NSSnapObjects::SnapJavascriptAsyncSpawnStepFunctionInfo*, TTD::NSSnapObjects::SnapObjectType::JavascriptAsyncSpawnStepFunction>(objData, info);
    }
    else
    {
        TTDAssert(depArray != nullptr, "depArray should be non-null if depCount is > 0");
        TTD::NSSnapObjects::StdExtractSetKindSpecificInfo<TTD::NSSnapObjects::SnapJavascriptAsyncSpawnStepFunctionInfo*, TTD::NSSnapObjects::SnapObjectType::JavascriptAsyncSpawnStepFunction>(objData, info, alloc, depCount, depArray);
    }
}

#endif
