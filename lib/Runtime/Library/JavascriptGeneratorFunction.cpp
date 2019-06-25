//-------------------------------------------------------------------------------------------------------
// Copyright (C) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE.txt file in the project root for full license information.
//-------------------------------------------------------------------------------------------------------
#include "RuntimeLibraryPch.h"

using namespace Js;

    namespace {

        Arguments CopyArgumentsToHeap(ArgumentReader& stackArgs, CallInfo callInfo, ScriptContext* scriptContext)
        {
            // InterpreterStackFrame takes a pointer to the args, so copy them to the recycler heap
            // and use that buffer for this InterpreterStackFrame.
            Field(Var)* argsHeapCopy = RecyclerNewArray(scriptContext->GetRecycler(), Field(Var), stackArgs.Info.Count);
            CopyArray(argsHeapCopy, stackArgs.Info.Count, stackArgs.Values, stackArgs.Info.Count);
            return Arguments(callInfo, unsafe_write_barrier_cast<Var*>(argsHeapCopy));
        }

    }

    FunctionInfo JavascriptGeneratorFunction::functionInfo(
        FORCE_NO_WRITE_BARRIER_TAG(JavascriptGeneratorFunction::EntryGeneratorFunctionImplementation),
        (FunctionInfo::Attributes)(FunctionInfo::DoNotProfile | FunctionInfo::ErrorOnNew));
    FunctionInfo JavascriptAsyncFunction::functionInfo(
        FORCE_NO_WRITE_BARRIER_TAG(JavascriptGeneratorFunction::EntryAsyncFunctionImplementation),
        (FunctionInfo::Attributes)(FunctionInfo::DoNotProfile | FunctionInfo::ErrorOnNew));
    FunctionInfo JavascriptAsyncGeneratorFunction::functionInfo(
        FORCE_NO_WRITE_BARRIER_TAG(JavascriptGeneratorFunction::EntryAsyncGeneratorFunctionImplementation),
        (FunctionInfo::Attributes)(FunctionInfo::DoNotProfile | FunctionInfo::ErrorOnNew));

    JavascriptGeneratorFunction::JavascriptGeneratorFunction(DynamicType* type)
        : ScriptFunctionBase(type, &functionInfo),
        scriptFunction(nullptr)
    {
        // Constructor used during copy on write.
        DebugOnly(VerifyEntryPoint());
    }

    JavascriptGeneratorFunction::JavascriptGeneratorFunction(DynamicType* type, GeneratorVirtualScriptFunction* scriptFunction)
        : ScriptFunctionBase(type, &functionInfo),
        scriptFunction(scriptFunction)
    {
        DebugOnly(VerifyEntryPoint());
    }

    JavascriptGeneratorFunction::JavascriptGeneratorFunction(DynamicType* type, FunctionInfo* functionInfo, GeneratorVirtualScriptFunction* scriptFunction)
        : ScriptFunctionBase(type, functionInfo),
        scriptFunction(scriptFunction)
    {
        DebugOnly(VerifyEntryPoint());
    }

    JavascriptAsyncGeneratorFunction::JavascriptAsyncGeneratorFunction(DynamicType* type, GeneratorVirtualScriptFunction* scriptFunction)
        : JavascriptGeneratorFunction(type, &functionInfo, scriptFunction)
    {
        DebugOnly(VerifyEntryPoint());
    }

    JavascriptAsyncFunction::JavascriptAsyncFunction(DynamicType* type, GeneratorVirtualScriptFunction* scriptFunction)
        : JavascriptGeneratorFunction(type, &functionInfo, scriptFunction)
    {
        DebugOnly(VerifyEntryPoint());
    }

    JavascriptAsyncFunction* JavascriptAsyncFunction::New(ScriptContext* scriptContext, GeneratorVirtualScriptFunction* scriptFunction)
    {
        return scriptContext->GetLibrary()->CreateAsyncFunction(functionInfo.GetOriginalEntryPoint(), scriptFunction);
    }

    JavascriptGeneratorFunction* JavascriptGeneratorFunction::New(ScriptContext* scriptContext, GeneratorVirtualScriptFunction* scriptFunction)
    {
        return scriptContext->GetLibrary()->CreateGeneratorFunction(functionInfo.GetOriginalEntryPoint(), scriptFunction);
    }

    JavascriptAsyncGeneratorFunction* JavascriptAsyncGeneratorFunction::New(ScriptContext* scriptContext, GeneratorVirtualScriptFunction* scriptFunction)
    {
        return scriptContext->GetLibrary()->CreateAsyncGeneratorFunction(functionInfo.GetOriginalEntryPoint(), scriptFunction);
    }

    bool JavascriptGeneratorFunction::IsBaseGeneratorFunction(RecyclableObject* obj)
    {
        if (VarIs<JavascriptFunction>(obj))
        {
            return VirtualTableInfo<JavascriptGeneratorFunction>::HasVirtualTable(obj)
                || VirtualTableInfo<CrossSiteObject<JavascriptGeneratorFunction>>::HasVirtualTable(obj);
        }

        return false;
    }

    template <> bool Js::VarIsImpl<JavascriptGeneratorFunction>(RecyclableObject* obj)
    {
        return JavascriptGeneratorFunction::IsBaseGeneratorFunction(obj) || VarIs<JavascriptAsyncFunction>(obj) || VarIs<JavascriptAsyncGeneratorFunction>(obj);
    }

    template <> bool Js::VarIsImpl<JavascriptAsyncFunction>(RecyclableObject* obj)
    {
        if (VarIs<JavascriptFunction>(obj))
        {
            return VirtualTableInfo<JavascriptAsyncFunction>::HasVirtualTable(obj)
                || VirtualTableInfo<CrossSiteObject<JavascriptAsyncFunction>>::HasVirtualTable(obj);
        }

        return false;
    }

    template <> bool Js::VarIsImpl<JavascriptAsyncGeneratorFunction>(RecyclableObject* obj)
    {
        if (VarIs<JavascriptFunction>(obj))
        {
            return VirtualTableInfo<JavascriptAsyncGeneratorFunction>::HasVirtualTable(obj)
                || VirtualTableInfo<CrossSiteObject<JavascriptAsyncGeneratorFunction>>::HasVirtualTable(obj);
        }

        return false;
    }

    JavascriptGeneratorFunction* JavascriptGeneratorFunction::OP_NewScGenFunc(FrameDisplay *environment, FunctionInfoPtrPtr infoRef)
    {
        FunctionProxy* functionProxy = (*infoRef)->GetFunctionProxy();
        ScriptContext* scriptContext = functionProxy->GetScriptContext();
        JIT_HELPER_NOT_REENTRANT_HEADER(ScrFunc_OP_NewScGenFunc, reentrancylock, scriptContext->GetThreadContext());

        GeneratorVirtualScriptFunction* scriptFunction = scriptContext->GetLibrary()->CreateGeneratorVirtualScriptFunction(functionProxy);
        scriptFunction->SetEnvironment(environment);

        JS_ETW(EventWriteJSCRIPT_RECYCLER_ALLOCATE_FUNCTION(scriptFunction, EtwTrace::GetFunctionId(functionProxy)));

        JavascriptGeneratorFunction* genFunc = nullptr;
        if (functionProxy->IsAsync())
        {
            if (functionProxy->IsGenerator())
            {
                genFunc = JavascriptAsyncGeneratorFunction::New(scriptContext, scriptFunction);
            }
            else
            {
                genFunc = JavascriptAsyncFunction::New(scriptContext, scriptFunction);
            }
        }
        else
        {
            genFunc = JavascriptGeneratorFunction::New(scriptContext, scriptFunction);
        }

        scriptFunction->SetRealGeneratorFunction(genFunc);

        return genFunc;
        JIT_HELPER_END(ScrFunc_OP_NewScGenFunc);
    }

    JavascriptGeneratorFunction * JavascriptGeneratorFunction::OP_NewScGenFuncHomeObj(FrameDisplay *environment, FunctionInfoPtrPtr infoRef, Var homeObj)
    {
        Assert(homeObj != nullptr);
        JIT_HELPER_NOT_REENTRANT_HEADER(ScrFunc_OP_NewScGenFuncHomeObj, reentrancylock, (*infoRef)->GetFunctionProxy()->GetScriptContext()->GetThreadContext());

        JavascriptGeneratorFunction* genFunc = JavascriptGeneratorFunction::OP_NewScGenFunc(environment, infoRef);

        genFunc->SetHomeObj(homeObj);

        return genFunc;
        JIT_HELPER_END(ScrFunc_OP_NewScGenFuncHomeObj);
    }

    Var JavascriptGeneratorFunction::EntryGeneratorFunctionImplementation(
        RecyclableObject* function,
        CallInfo callInfo,
        ...)
    {
        PROBE_STACK(function->GetScriptContext(), Js::Constants::MinStackDefault);
        ARGUMENTS(stackArgs, callInfo);

        Assert(!(callInfo.Flags & CallFlags_New));

        ScriptContext* scriptContext = function->GetScriptContext();
        JavascriptLibrary* library = scriptContext->GetLibrary();
        Arguments heapArgs = CopyArgumentsToHeap(stackArgs, callInfo, scriptContext);
        DynamicObject* prototype = library->CreateGeneratorConstructorPrototypeObject();

        JavascriptGenerator* generator = library->CreateGenerator(
            heapArgs,
            VarTo<JavascriptGeneratorFunction>(function)->scriptFunction,
            prototype);

        // Set the prototype from constructor
        JavascriptOperators::OrdinaryCreateFromConstructor(function, generator, prototype, scriptContext);

        // Call next on the generator to execute until the beginning of the body
        BEGIN_SAFE_REENTRANT_CALL(scriptContext->GetThreadContext())
        {
            CALL_ENTRYPOINT(
                scriptContext->GetThreadContext(),
                generator->EntryNext,
                function,
                CallInfo(CallFlags_Value, 1),
                generator);
        }
        END_SAFE_REENTRANT_CALL

        return generator;
    }

    Var JavascriptGeneratorFunction::EntryAsyncGeneratorFunctionImplementation(
        RecyclableObject* function,
        CallInfo callInfo,
        ...)
    {
        PROBE_STACK(function->GetScriptContext(), Js::Constants::MinStackDefault);
        ARGUMENTS(stackArgs, callInfo);

        Assert(!(callInfo.Flags & CallFlags_New));

        ScriptContext* scriptContext = function->GetScriptContext();
        JavascriptLibrary* library = scriptContext->GetLibrary();
        Arguments heapArgs = CopyArgumentsToHeap(stackArgs, callInfo, scriptContext);
        DynamicObject* prototype = library->CreateAsyncGeneratorConstructorPrototypeObject();

        JavascriptGenerator* generator = library->CreateGenerator(
            heapArgs,
            VarTo<JavascriptGeneratorFunction>(function)->scriptFunction,
            prototype);

        generator->SetIsAsync();
        generator->InitialiseAsyncGenerator(scriptContext);

        // Set the prototype from constructor
        JavascriptOperators::OrdinaryCreateFromConstructor(function, generator, prototype, scriptContext);

        /*
        Would prefer to keep things simple like this:
        // Call next on the generator to execute until the beginning of the body
        BEGIN_SAFE_REENTRANT_CALL(scriptContext->GetThreadContext())
        {
            CALL_ENTRYPOINT(
                scriptContext->GetThreadContext(),
                generator->EntryNext,
                function,
                CallInfo(CallFlags_Value, 1),
                generator);
        }
        END_SAFE_REENTRANT_CALL
        */

        return generator;
    }

    Var JavascriptGeneratorFunction::EntryAsyncFunctionImplementation(
        RecyclableObject* function,
        CallInfo callInfo,
        ...)
    {
        PROBE_STACK(function->GetScriptContext(), Js::Constants::MinStackDefault);
        ARGUMENTS(stackArgs, callInfo);

        ScriptContext* scriptContext = function->GetScriptContext();
        JavascriptLibrary* library = scriptContext->GetLibrary();
        Arguments heapArgs = CopyArgumentsToHeap(stackArgs, callInfo, scriptContext);

        JavascriptGenerator* generator = library->CreateGenerator(
            heapArgs,
            VarTo<JavascriptGeneratorFunction>(function)->scriptFunction,
            scriptContext->GetLibrary()->GetNull());

        auto* executor = library->CreatePromiseAsyncSpawnExecutorFunction(generator, stackArgs[0]);
        auto* promise = library->CreatePromise();

        JavascriptPromiseResolveOrRejectFunction* resolve;
        JavascriptPromiseResolveOrRejectFunction* reject;
        JavascriptPromise::InitializePromise(promise, &resolve, &reject, scriptContext);

        JavascriptExceptionObject* e = nullptr;
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
            e = err.GetAndClear();
        }

        if (e != nullptr)
        {
            JavascriptPromise::TryRejectWithExceptionObject(e, reject, scriptContext);
        }

        return promise;
    }

    Var JavascriptGeneratorFunction::NewInstance(RecyclableObject* function, CallInfo callInfo, ...)
    {
        // Get called when creating a new generator function through the constructor (e.g. gf.__proto__.constructor) and sets EntryGeneratorFunctionImplementation as the entrypoint
        PROBE_STACK(function->GetScriptContext(), Js::Constants::MinStackDefault);

        ARGUMENTS(args, callInfo);

        return JavascriptFunction::NewInstanceHelper(function->GetScriptContext(), function, callInfo, args, FunctionKind::Generator);
    }

    Var JavascriptGeneratorFunction::NewInstanceRestrictedMode(RecyclableObject* function, CallInfo callInfo, ...)
    {
        ScriptContext* scriptContext = function->GetScriptContext();

        scriptContext->CheckEvalRestriction();

        PROBE_STACK(scriptContext, Js::Constants::MinStackDefault);

        ARGUMENTS(args, callInfo);

        return JavascriptFunction::NewInstanceHelper(scriptContext, function, callInfo, args, FunctionKind::Generator);
    }

    JavascriptString* JavascriptGeneratorFunction::GetDisplayNameImpl() const
    {
        return scriptFunction->GetDisplayNameImpl();
    }

    Var JavascriptGeneratorFunction::GetHomeObj() const
    {
        return scriptFunction->GetHomeObj();
    }

    void JavascriptGeneratorFunction::SetHomeObj(Var homeObj)
    {
        scriptFunction->SetHomeObj(homeObj);
    }

    void JavascriptGeneratorFunction::SetComputedNameVar(Var computedNameVar)
    {
        scriptFunction->SetComputedNameVar(computedNameVar);
    }

    Var JavascriptGeneratorFunction::GetComputedNameVar() const
    {
        return scriptFunction->GetComputedNameVar();
    }

    bool JavascriptGeneratorFunction::IsAnonymousFunction() const
    {
        return scriptFunction->IsAnonymousFunction();
    }

    Var JavascriptGeneratorFunction::GetSourceString() const
    {
        return scriptFunction->GetSourceString();
    }

    JavascriptString * JavascriptGeneratorFunction::EnsureSourceString()
    {
        return scriptFunction->EnsureSourceString();
    }

    PropertyQueryFlags JavascriptGeneratorFunction::HasPropertyQuery(PropertyId propertyId, _Inout_opt_ PropertyValueInfo* info)
    {
        if (propertyId == PropertyIds::caller || propertyId == PropertyIds::arguments)
        {
            // JavascriptFunction has special case for caller and arguments; call DynamicObject:: virtual directly to skip that.
            return DynamicObject::HasPropertyQuery(propertyId, info);
        }

        return JavascriptFunction::HasPropertyQuery(propertyId, info);
    }

    PropertyQueryFlags JavascriptGeneratorFunction::GetPropertyQuery(Var originalInstance, PropertyId propertyId, Var* value, PropertyValueInfo* info, ScriptContext* requestContext)
    {
        if (propertyId == PropertyIds::caller || propertyId == PropertyIds::arguments)
        {
            // JavascriptFunction has special case for caller and arguments; call DynamicObject:: virtual directly to skip that.
            return DynamicObject::GetPropertyQuery(originalInstance, propertyId, value, info, requestContext);
        }

        return JavascriptFunction::GetPropertyQuery(originalInstance, propertyId, value, info, requestContext);
    }

    PropertyQueryFlags JavascriptGeneratorFunction::GetPropertyQuery(Var originalInstance, JavascriptString* propertyNameString, Var* value, PropertyValueInfo* info, ScriptContext* requestContext)
    {
        PropertyRecord const* propertyRecord;
        this->GetScriptContext()->FindPropertyRecord(propertyNameString, &propertyRecord);

        if (propertyRecord != nullptr)
        {
            if (propertyRecord->GetPropertyId() == PropertyIds::caller || propertyRecord->GetPropertyId() == PropertyIds::arguments)
            {
                // JavascriptFunction has special case for caller and arguments; call DynamicObject:: virtual directly to skip that.
                return DynamicObject::GetPropertyQuery(originalInstance, propertyNameString, value, info, requestContext);
            }
        }

        return JavascriptFunction::GetPropertyQuery(originalInstance, propertyNameString, value, info, requestContext);
    }

    PropertyQueryFlags JavascriptGeneratorFunction::GetPropertyReferenceQuery(Var originalInstance, PropertyId propertyId, Var* value, PropertyValueInfo* info, ScriptContext* requestContext)
    {
        return JavascriptGeneratorFunction::GetPropertyQuery(originalInstance, propertyId, value, info, requestContext);
    }

    BOOL JavascriptGeneratorFunction::SetProperty(PropertyId propertyId, Var value, PropertyOperationFlags flags, PropertyValueInfo* info)
    {
        if (propertyId == PropertyIds::caller || propertyId == PropertyIds::arguments)
        {
            // JavascriptFunction has special case for caller and arguments; call DynamicObject:: virtual directly to skip that.
            return DynamicObject::SetProperty(propertyId, value, flags, info);
        }

        return JavascriptFunction::SetProperty(propertyId, value, flags, info);
    }

    BOOL JavascriptGeneratorFunction::SetProperty(JavascriptString* propertyNameString, Var value, PropertyOperationFlags flags, PropertyValueInfo* info)
    {
        PropertyRecord const* propertyRecord;
        this->GetScriptContext()->FindPropertyRecord(propertyNameString, &propertyRecord);

        if (propertyRecord != nullptr)
        {
            if (propertyRecord->GetPropertyId() == PropertyIds::caller || propertyRecord->GetPropertyId() == PropertyIds::arguments)
            {
                // JavascriptFunction has special case for caller and arguments; call DynamicObject:: virtual directly to skip that.
                return DynamicObject::SetProperty(propertyNameString, value, flags, info);
            }
        }

        return JavascriptFunction::SetProperty(propertyNameString, value, flags, info);
    }

    _Check_return_ _Success_(return) BOOL JavascriptGeneratorFunction::GetAccessors(PropertyId propertyId, _Outptr_result_maybenull_ Var* getter, _Outptr_result_maybenull_ Var* setter, ScriptContext* requestContext)
    {
        if (propertyId == PropertyIds::caller || propertyId == PropertyIds::arguments)
        {
            // JavascriptFunction has special case for caller and arguments; call DynamicObject:: virtual directly to skip that.
            return DynamicObject::GetAccessors(propertyId, getter, setter, requestContext);
        }

        return JavascriptFunction::GetAccessors(propertyId, getter, setter, requestContext);
    }

    DescriptorFlags JavascriptGeneratorFunction::GetSetter(PropertyId propertyId, Var *setterValue, PropertyValueInfo* info, ScriptContext* requestContext)
    {
        if (propertyId == PropertyIds::caller || propertyId == PropertyIds::arguments)
        {
            // JavascriptFunction has special case for caller and arguments; call DynamicObject:: virtual directly to skip that.
            return DynamicObject::GetSetter(propertyId, setterValue, info, requestContext);
        }

        return JavascriptFunction::GetSetter(propertyId, setterValue, info, requestContext);
    }

    DescriptorFlags JavascriptGeneratorFunction::GetSetter(JavascriptString* propertyNameString, Var *setterValue, PropertyValueInfo* info, ScriptContext* requestContext)
    {
        PropertyRecord const* propertyRecord;
        this->GetScriptContext()->FindPropertyRecord(propertyNameString, &propertyRecord);

        if (propertyRecord != nullptr)
        {
            if ((propertyRecord->GetPropertyId() == PropertyIds::caller || propertyRecord->GetPropertyId() == PropertyIds::arguments))
            {
                // JavascriptFunction has special case for caller and arguments; call DynamicObject:: virtual directly to skip that.
                return DynamicObject::GetSetter(propertyNameString, setterValue, info, requestContext);
            }
        }

        return JavascriptFunction::GetSetter(propertyNameString, setterValue, info, requestContext);
    }

    BOOL JavascriptGeneratorFunction::InitProperty(PropertyId propertyId, Var value, PropertyOperationFlags flags, PropertyValueInfo* info)
    {
        return SetProperty(propertyId, value, PropertyOperation_None, info);
    }

    BOOL JavascriptGeneratorFunction::DeleteProperty(PropertyId propertyId, PropertyOperationFlags flags)
    {
        if (propertyId == PropertyIds::caller || propertyId == PropertyIds::arguments)
        {
            // JavascriptFunction has special case for caller and arguments; call DynamicObject:: virtual directly to skip that.
            return DynamicObject::DeleteProperty(propertyId, flags);
        }

        return JavascriptFunction::DeleteProperty(propertyId, flags);
    }

    BOOL JavascriptGeneratorFunction::DeleteProperty(JavascriptString *propertyNameString, PropertyOperationFlags flags)
    {
        if (BuiltInPropertyRecords::caller.Equals(propertyNameString) || BuiltInPropertyRecords::arguments.Equals(propertyNameString))
        {
            // JavascriptFunction has special case for caller and arguments; call DynamicObject:: virtual directly to skip that.
            return DynamicObject::DeleteProperty(propertyNameString, flags);
        }

        return JavascriptFunction::DeleteProperty(propertyNameString, flags);
    }

    BOOL JavascriptGeneratorFunction::IsWritable(PropertyId propertyId)
    {
        if (propertyId == PropertyIds::caller || propertyId == PropertyIds::arguments)
        {
            // JavascriptFunction has special case for caller and arguments; call DynamicObject:: virtual directly to skip that.
            return DynamicObject::IsWritable(propertyId);
        }

        return JavascriptFunction::IsWritable(propertyId);
    }

    BOOL JavascriptGeneratorFunction::IsEnumerable(PropertyId propertyId)
    {
        if (propertyId == PropertyIds::caller || propertyId == PropertyIds::arguments)
        {
            // JavascriptFunction has special case for caller and arguments; call DynamicObject:: virtual directly to skip that.
            return DynamicObject::IsEnumerable(propertyId);
        }

        return JavascriptFunction::IsEnumerable(propertyId);
    }

#if ENABLE_TTD

    void JavascriptGeneratorFunction::MarkVisitKindSpecificPtrs(TTD::SnapshotExtractor* extractor)
    {
        if (this->scriptFunction != nullptr)
        {
            extractor->MarkVisitVar(this->scriptFunction);
        }
    }

    TTD::NSSnapObjects::SnapObjectType JavascriptGeneratorFunction::GetSnapTag_TTD() const
    {
        return TTD::NSSnapObjects::SnapObjectType::SnapGeneratorFunction;
    }

    void JavascriptGeneratorFunction::ExtractSnapObjectDataInto(TTD::NSSnapObjects::SnapObject* objData, TTD::SlabAllocator& alloc)
    {
        TTD::NSSnapObjects::SnapGeneratorFunctionInfo* fi = nullptr;
        uint32 depCount = 0;
        TTD_PTR_ID* depArray = nullptr;

        this->CreateSnapObjectInfo(alloc, &fi, &depArray, &depCount);

        if (depCount == 0)
        {
            TTD::NSSnapObjects::StdExtractSetKindSpecificInfo<TTD::NSSnapObjects::SnapGeneratorFunctionInfo*, TTD::NSSnapObjects::SnapObjectType::SnapGeneratorFunction>(objData, fi);
        }
        else
        {
            TTDAssert(depArray != nullptr, "depArray should be non-null if depCount is > 0");
            TTD::NSSnapObjects::StdExtractSetKindSpecificInfo<TTD::NSSnapObjects::SnapGeneratorFunctionInfo*, TTD::NSSnapObjects::SnapObjectType::SnapGeneratorFunction>(objData, fi, alloc, depCount, depArray);
        }
    }

    void JavascriptGeneratorFunction::CreateSnapObjectInfo(TTD::SlabAllocator& alloc, _Out_ TTD::NSSnapObjects::SnapGeneratorFunctionInfo** info, _Out_ TTD_PTR_ID** depArray, _Out_ uint32* depCount)
    {
        *info = alloc.SlabAllocateStruct<TTD::NSSnapObjects::SnapGeneratorFunctionInfo>();
        (*info)->scriptFunction = TTD_CONVERT_VAR_TO_PTR_ID(this->scriptFunction);
        (*info)->isAnonymousFunction = this->scriptFunction->IsAnonymousFunction();

        *depCount = 0;
        *depArray = nullptr;
        if (this->scriptFunction != nullptr &&  TTD::JsSupport::IsVarComplexKind(this->scriptFunction))
        {
            *depArray = alloc.SlabReserveArraySpace<TTD_PTR_ID>(1);
            (*depArray)[*depCount] = TTD_CONVERT_VAR_TO_PTR_ID(this->scriptFunction);
            *depCount = 1;
            alloc.SlabCommitArraySpace<TTD_PTR_ID>(*depCount, 1);
        }
    }

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

    void GeneratorVirtualScriptFunction::MarkVisitKindSpecificPtrs(TTD::SnapshotExtractor* extractor)
    {
        this->ScriptFunction::MarkVisitKindSpecificPtrs(extractor);

        extractor->MarkVisitVar(this->realFunction);
    }

    TTD::NSSnapObjects::SnapObjectType GeneratorVirtualScriptFunction::GetSnapTag_TTD() const
    {
        return TTD::NSSnapObjects::SnapObjectType::SnapGeneratorVirtualScriptFunction;
    }

    void GeneratorVirtualScriptFunction::ExtractSnapObjectDataInto(TTD::NSSnapObjects::SnapObject* objData, TTD::SlabAllocator& alloc)
    {
        TTD::NSSnapObjects::SnapGeneratorVirtualScriptFunctionInfo* fi = alloc.SlabAllocateStruct<TTD::NSSnapObjects::SnapGeneratorVirtualScriptFunctionInfo>();
        ScriptFunction::ExtractSnapObjectDataIntoSnapScriptFunctionInfo(fi, alloc);
        fi->realFunction = TTD_CONVERT_VAR_TO_PTR_ID(this->realFunction);
        TTD::NSSnapObjects::StdExtractSetKindSpecificInfo<TTD::NSSnapObjects::SnapGeneratorVirtualScriptFunctionInfo*, TTD::NSSnapObjects::SnapObjectType::SnapGeneratorVirtualScriptFunction>(objData, fi);
    }
#endif
