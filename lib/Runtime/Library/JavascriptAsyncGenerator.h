//-------------------------------------------------------------------------------------------------------
// Copyright (C) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE.txt file in the project root for full license information.
//-------------------------------------------------------------------------------------------------------
#pragma once

namespace Js
{

class JavascriptAsyncGenerator : public JavascriptGenerator
{
    struct AsyncGeneratorRequest
    {
        Field(Var) data;
        Field(JavascriptExceptionObject*) exceptionObj;
        Field(JavascriptPromise*) promise;

        AsyncGeneratorRequest(
            Var data,
            JavascriptExceptionObject* exceptionObj,
            JavascriptPromise* promise) :
                data(data),
                exceptionObj(exceptionObj),
                promise(promise) {}
    };

    using RequestQueue = DList<AsyncGeneratorRequest*, Recycler>;

    enum class PendingState
    {
        None,
        Await,
        AwaitReturn,
        Yield
    };

private:
    DEFINE_VTABLE_CTOR(JavascriptAsyncGenerator, JavascriptGenerator);
    DEFINE_MARSHAL_OBJECT_TO_SCRIPT_CONTEXT(JavascriptAsyncGenerator);

    Field(RequestQueue*) requestQueue;
    Field(PendingState) pendingState = PendingState::None;

    JavascriptAsyncGenerator(
        DynamicType* type,
        Arguments& args,
        ScriptFunction* scriptFunction,
        RequestQueue* requestQueue) :
            JavascriptGenerator(type, args, scriptFunction),
            requestQueue(requestQueue) {}

public:
    static JavascriptAsyncGenerator* New(
        Recycler* recycler,
        DynamicType* generatorType,
        Arguments& args,
        ScriptFunction* scriptFunction);

    void PushRequest(AsyncGeneratorRequest* request)
    {
        requestQueue->Append(request);
    }

    AsyncGeneratorRequest* ShiftRequest()
    {
        Assert(!requestQueue->Empty());
        AsyncGeneratorRequest* request = requestQueue->Head();
        requestQueue->RemoveHead();
        return request;
    }

    AsyncGeneratorRequest* PeekRequest()
    {
        Assert(!requestQueue->Empty());
        return requestQueue->Head();
    }

    bool HasRequest()
    {
        return !requestQueue->Empty();
    }

    void ResumeNext();
    void ResumeCoroutine(Var value, JavascriptExceptionObject* exception);
    void ResolveNext(Var value);
    void RejectNext(Var reason);
    void UnwrapAndResolveNext(Var value);
    void UnwrapAndResumeCoroutine(Var value);
    void UnwrapReturnAndResumeCoroutine(Var value);

    void RegisterAwaitCallbacks(
        Var value,
        JavascriptMethod onFulfilled,
        JavascriptMethod onRejected);

    class EntryInfo
    {
    public:
        static FunctionInfo Next;
        static FunctionInfo Return;
        static FunctionInfo Throw;
    };

    static Var EntryNext(RecyclableObject* function, CallInfo callInfo, ...);
    static Var EntryReturn(RecyclableObject* function, CallInfo callInfo, ...);
    static Var EntryThrow(RecyclableObject* function, CallInfo callInfo, ...);

    static Var EntryAwaitFulfilledCallback(RecyclableObject* function, CallInfo callInfo, ...);
    static Var EntryAwaitRejectedCallback(RecyclableObject* function, CallInfo callInfo, ...);

    static Var EnqueueRequest(
        Var thisValue,
        ScriptContext* scriptContext,
        Var input,
        JavascriptExceptionObject* exceptionObj,
        const char16* apiNameForErrorMessage);
};

template<>
bool VarIsImpl<JavascriptAsyncGenerator>(RecyclableObject* obj);

class AsyncGeneratorCallbackFunction : public RuntimeFunction
{
protected:
    DEFINE_VTABLE_CTOR(AsyncGeneratorCallbackFunction, RuntimeFunction);
    DEFINE_MARSHAL_OBJECT_TO_SCRIPT_CONTEXT(AsyncGeneratorCallbackFunction);

public:
    AsyncGeneratorCallbackFunction(
        DynamicType* type,
        FunctionInfo* functionInfo,
        JavascriptAsyncGenerator* generator) :
            RuntimeFunction(type, functionInfo),
            generator(generator) {}

    Field(JavascriptAsyncGenerator*) generator;
};

template<>
bool VarIsImpl<AsyncGeneratorCallbackFunction>(RecyclableObject* obj);

}
