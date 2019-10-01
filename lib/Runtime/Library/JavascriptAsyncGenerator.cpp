//-------------------------------------------------------------------------------------------------------
// Copyright (C) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE.txt file in the project root for full license information.
//-------------------------------------------------------------------------------------------------------
#include "RuntimeLibraryPch.h"
#include "Language/InterpreterStackFrame.h"

using namespace Js;

JavascriptAsyncGenerator* JavascriptAsyncGenerator::New(
    Recycler* recycler,
    DynamicType* generatorType,
    Arguments& args,
    ScriptFunction* scriptFunction)
{
    auto* requestQueue = RecyclerNew(recycler, JavascriptAsyncGenerator::RequestQueue, recycler);

    return RecyclerNew(
        recycler,
        JavascriptAsyncGenerator,
        generatorType,
        args,
        scriptFunction,
        requestQueue);
}

Var JavascriptAsyncGenerator::EntryNext(RecyclableObject* function, CallInfo callInfo, ...)
{
    PROBE_STACK(function->GetScriptContext(), Js::Constants::MinStackDefault);

    ARGUMENTS(args, callInfo);
    auto* scriptContext = function->GetScriptContext();
    auto* library = scriptContext->GetLibrary();

    AUTO_TAG_NATIVE_LIBRARY_ENTRY(function, callInfo, _u("AsyncGenerator.prototype.next"));

    Var thisValue = args[0];
    Var input = args.Info.Count > 1 ? args[1] : library->GetUndefined();

    return EnqueueRequest(
        thisValue,
        scriptContext,
        input,
        nullptr,
        _u("AsyncGenerator.prototype.next"));
}

Var JavascriptAsyncGenerator::EntryReturn(RecyclableObject* function, CallInfo callInfo, ...)
{
    PROBE_STACK(function->GetScriptContext(), Js::Constants::MinStackDefault);

    ARGUMENTS(args, callInfo);
    auto* scriptContext = function->GetScriptContext();
    auto* library = scriptContext->GetLibrary();

    AUTO_TAG_NATIVE_LIBRARY_ENTRY(function, callInfo, _u("AsyncGenerator.prototype.return"));

    Var thisValue = args[0];
    Var input = args.Info.Count > 1 ? args[1] : library->GetUndefined();

    auto* returnException = RecyclerNew(
        scriptContext->GetRecycler(),
        GeneratorReturnExceptionObject,
        input,
        scriptContext);

    return EnqueueRequest(
        thisValue,
        scriptContext,
        input,
        returnException,
        _u("AsyncGenerator.prototype.return"));
}

Var JavascriptAsyncGenerator::EntryThrow(RecyclableObject* function, CallInfo callInfo, ...)
{
    PROBE_STACK(function->GetScriptContext(), Js::Constants::MinStackDefault);

    ARGUMENTS(args, callInfo);
    auto* scriptContext = function->GetScriptContext();
    auto* library = scriptContext->GetLibrary();

    AUTO_TAG_NATIVE_LIBRARY_ENTRY(function, callInfo, _u("AsyncGenerator.prototype.throw"));

    Var thisValue = args[0];
    Var input = args.Info.Count > 1 ? args[1] : library->GetUndefined();

    auto* exception = RecyclerNew(
        scriptContext->GetRecycler(),
        JavascriptExceptionObject,
        input,
        scriptContext,
        nullptr);

    return EnqueueRequest(
        thisValue,
        scriptContext,
        input,
        exception,
        _u("AsyncGenerator.prototype.throw"));
}

Var JavascriptAsyncGenerator::EntryAwaitFulfilledCallback(
    RecyclableObject* function,
    CallInfo callInfo, ...)
{
    auto* scriptContext = function->GetScriptContext();

    PROBE_STACK(scriptContext, Js::Constants::MinStackDefault);
    ARGUMENTS(args, callInfo);

    AssertOrFailFast(args.Info.Count > 1);

    Var value = args[1];
    auto* generator = VarTo<AsyncGeneratorCallbackFunction>(function)->generator;

    PendingState state = generator->pendingState;
    generator->pendingState = PendingState::None;

    switch (state)
    {
        case PendingState::Await:
            generator->ResumeCoroutine(value, nullptr);
            break;
        case PendingState::AwaitReturn:
            generator->ResumeCoroutine(nullptr, RecyclerNew(
                scriptContext->GetRecycler(),
                GeneratorReturnExceptionObject,
                value,
                scriptContext));
            break;
        case PendingState::Yield:
            generator->ResolveNext(value);
            break;
        default:
            AssertMsg(false, "Expected an async generator pending state");
            break;
    }

    return scriptContext->GetLibrary()->GetUndefined();
}

Var JavascriptAsyncGenerator::EntryAwaitRejectedCallback(
    RecyclableObject* function,
    CallInfo callInfo, ...)
{
    auto* scriptContext = function->GetScriptContext();

    PROBE_STACK(scriptContext, Js::Constants::MinStackDefault);
    ARGUMENTS(args, callInfo);

    AssertOrFailFast(args.Info.Count > 1);

    Var value = args[1];
    auto* generator = VarTo<AsyncGeneratorCallbackFunction>(function)->generator;

    PendingState state = generator->pendingState;
    generator->pendingState = PendingState::None;

    switch (state)
    {
        case PendingState::Await:
        case PendingState::AwaitReturn:
            generator->ResumeCoroutine(nullptr, RecyclerNew(
                scriptContext->GetRecycler(),
                JavascriptExceptionObject,
                value,
                scriptContext,
                nullptr));
            break;
        case PendingState::Yield:
            generator->RejectNext(value);
            break;
        default:
            AssertMsg(false, "Expected an async generator pending state");
            break;
    }

    return scriptContext->GetLibrary()->GetUndefined();
}

Var JavascriptAsyncGenerator::EnqueueRequest(
    Var thisValue,
    ScriptContext* scriptContext,
    Var input,
    JavascriptExceptionObject* exceptionObj,
    const char16* apiNameForErrorMessage)
{
    auto* promise = JavascriptPromise::CreateEnginePromise(scriptContext);

    if (!VarIs<JavascriptAsyncGenerator>(thisValue))
    {
        auto* library = scriptContext->GetLibrary();
        auto* error = library->CreateTypeError();

        JavascriptError::SetErrorMessage(
            error,
            JSERR_NeedObjectOfType,
            apiNameForErrorMessage,
            scriptContext);

        promise->Reject(error, scriptContext);
    }
    else
    {
        auto* request = RecyclerNew(
            scriptContext->GetRecycler(),
            AsyncGeneratorRequest,
            input,
            exceptionObj,
            promise);

        auto* generator = UnsafeVarTo<JavascriptAsyncGenerator>(thisValue);
        generator->PushRequest(request);
        generator->ResumeNext();
    }

    return promise;
}

void JavascriptAsyncGenerator::ResumeNext()
{
    if (IsExecuting() || this->pendingState != PendingState::None || !HasRequest())
        return;

    auto* scriptContext = GetScriptContext();
    auto* library = scriptContext->GetLibrary();

    AsyncGeneratorRequest* next = PeekRequest();

    if (next->exceptionObj != nullptr)
    {
        if (IsSuspendedStart())
            SetState(GeneratorState::Completed);

        if (next->exceptionObj->IsGeneratorReturnException())
        {
            if (IsCompleted()) UnwrapAndResolveNext(next->data);
            else UnwrapReturnAndResumeCoroutine(next->data);
        }
        else
        {
            if (IsCompleted()) RejectNext(next->data);
            else ResumeCoroutine(nullptr, next->exceptionObj);
        }
    }
    else
    {
        if (IsCompleted()) ResolveNext(library->GetUndefined());
        else ResumeCoroutine(next->data, nullptr);
    }
}

void JavascriptAsyncGenerator::ResumeCoroutine(Var value, JavascriptExceptionObject* exception)
{
    Assert(this->pendingState == PendingState::None);

    RecyclableObject* result = nullptr;

    try
    {
        // Call the internal (sync) generator entry point
        ResumeYieldData yieldData(value, exception);
        Var resultVar = CallGenerator(&yieldData);
        result = VarTo<RecyclableObject>(resultVar);
    }
    catch (const JavascriptException& err)
    {
        RejectNext(err.GetAndClear()->GetThrownObject(nullptr));
        return;
    }

    Var resultValue = JavascriptOperators::GetProperty(
        result,
        PropertyIds::value,
        GetScriptContext());

    Assert(resultValue);

    if (result->HasOwnProperty(PropertyIds::_internalSymbolIsAwait))
    {
        // If the result object has an _internalSymbolIsAwait property, then
        // we are processing an await expression
        UnwrapAndResumeCoroutine(resultValue);
    }
    else
    {
        UnwrapAndResolveNext(resultValue);
    }
}

void JavascriptAsyncGenerator::ResolveNext(Var value)
{
    auto* scriptContext = GetScriptContext();
    auto* library = scriptContext->GetLibrary();
    Var result = library->CreateIteratorResultObject(value, IsCompleted());
    ShiftRequest()->promise->Resolve(result, scriptContext);
    ResumeNext();
}

void JavascriptAsyncGenerator::RejectNext(Var reason)
{
    ShiftRequest()->promise->Reject(reason, GetScriptContext());
    ResumeNext();
}

void JavascriptAsyncGenerator::RegisterAwaitCallbacks(
    Var value,
    JavascriptMethod fulfilledEntryPoint,
    JavascriptMethod rejectedEntryPoint)
{
    auto* scriptContext = GetScriptContext();
    auto* library = scriptContext->GetLibrary();
    auto* promise = JavascriptPromise::InternalPromiseResolve(value, scriptContext);
    auto* unused = JavascriptPromise::UnusedPromiseCapability(scriptContext);

    auto* onFulfilled = library->CreateAsyncGeneratorCallbackFunction(fulfilledEntryPoint, this);
    auto* onRejected = library->CreateAsyncGeneratorCallbackFunction(rejectedEntryPoint, this);

    JavascriptPromise::PerformPromiseThen(promise, unused, onFulfilled, onRejected, scriptContext);
}

void JavascriptAsyncGenerator::UnwrapAndResolveNext(Var value)
{
    this->pendingState = PendingState::Yield;
    RegisterAwaitCallbacks(value, EntryAwaitFulfilledCallback, EntryAwaitRejectedCallback);
}

void JavascriptAsyncGenerator::UnwrapAndResumeCoroutine(Var value)
{
    this->pendingState = PendingState::Await;
    RegisterAwaitCallbacks(value, EntryAwaitFulfilledCallback, EntryAwaitRejectedCallback);
}

void JavascriptAsyncGenerator::UnwrapReturnAndResumeCoroutine(Var value)
{
    this->pendingState = PendingState::AwaitReturn;
    RegisterAwaitCallbacks(value, EntryAwaitFulfilledCallback, EntryAwaitRejectedCallback);
}

template<>
bool Js::VarIsImpl<JavascriptAsyncGenerator>(RecyclableObject* obj)
{
    return JavascriptOperators::GetTypeId(obj) == TypeIds_AsyncGenerator;
}

template<>
bool Js::VarIsImpl<AsyncGeneratorCallbackFunction>(RecyclableObject* obj)
{
    return VarIs<JavascriptFunction>(obj) && (
        VirtualTableInfo<AsyncGeneratorCallbackFunction>::HasVirtualTable(obj) ||
        VirtualTableInfo<CrossSiteObject<AsyncGeneratorCallbackFunction>>::HasVirtualTable(obj)
    );
}
