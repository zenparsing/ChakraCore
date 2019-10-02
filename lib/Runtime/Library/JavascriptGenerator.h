//-------------------------------------------------------------------------------------------------------
// Copyright (C) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE.txt file in the project root for full license information.
//-------------------------------------------------------------------------------------------------------
#pragma once

namespace Js
{

enum class ResumeYieldKind
{
    Normal,
    Throw,
    Return
};

// Helper struct used to communicate to a yield point whether it was resumed via next(),
// return(), or throw() and provide the data necessary for the corresponding action taken
// (see OP_ResumeYield) `data` stores the value that was passed in as parameter to .next()
struct ResumeYieldData
{
    ScriptContext* scriptContext;
    Var data;
    ResumeYieldKind kind;

    ResumeYieldData(ScriptContext* scriptContext, Var data, ResumeYieldKind kind) :
        scriptContext(scriptContext),
        data(data),
        kind(kind) {}
};

class JavascriptGenerator : public DynamicObject
{
public:
    enum class GeneratorState
    {
        SuspendedStart,
        Suspended,
        Executing,
        Completed
    };

    static uint32 GetFrameOffset()
    {
        return offsetof(JavascriptGenerator, frame);
    }

    static uint32 GetCallInfoOffset()
    {
        return offsetof(JavascriptGenerator, args) + Arguments::GetCallInfoOffset();
    }

    static uint32 GetArgsPtrOffset()
    {
        return offsetof(JavascriptGenerator, args) + Arguments::GetValuesOffset();
    }

    void SetState(GeneratorState state)
    {
        this->state = state;
        if (state == GeneratorState::Completed)
        {
            frame = nullptr;
            args.Values = nullptr;
            scriptFunction = nullptr;
        }
    }

    void ThrowIfExecuting(const char16* apiName);
    Var CallGenerator(Var data, ResumeYieldKind resumeKind);

private:
    Field(InterpreterStackFrame*) frame;
    Field(GeneratorState) state;
    Field(Arguments) args;
    Field(ScriptFunction*) scriptFunction;

protected:
    DEFINE_VTABLE_CTOR_MEMBER_INIT(JavascriptGenerator, DynamicObject, args);
    DEFINE_MARSHAL_OBJECT_TO_SCRIPT_CONTEXT(JavascriptGenerator);

    JavascriptGenerator(DynamicType* type, Arguments& args, ScriptFunction* scriptFunction);

public:

    static JavascriptGenerator* New(
        Recycler* recycler,
        DynamicType* generatorType,
        Arguments& args,
        ScriptFunction* scriptFunction);

    static JavascriptGenerator* New(
        Recycler* recycler,
        DynamicType* generatorType,
        Arguments &args,
        Js::JavascriptGenerator::GeneratorState generatorState);

    bool IsExecuting() const { return this->state == GeneratorState::Executing; }
    bool IsSuspended() const { return this->state == GeneratorState::Suspended; }
    bool IsCompleted() const { return this->state == GeneratorState::Completed; }

    bool IsSuspendedStart() const
    {
        return this->state == GeneratorState::SuspendedStart
            || (this->state == GeneratorState::Suspended && this->frame == nullptr);
    }

    InterpreterStackFrame* GetFrame() const { return frame; }
    const Arguments& GetArguments() const { return args; }

    void SetScriptFunction(ScriptFunction* sf) { this->scriptFunction = sf; }
    void SetFrame(InterpreterStackFrame* frame, size_t bytes);
    void SetFrameSlots(uint slotCount, Field(Var)* frameSlotArray);

#if GLOBAL_ENABLE_WRITE_BARRIER
    virtual void Finalize(bool isShutdown) override;
#endif

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

#if ENABLE_TTD
    virtual void MarkVisitKindSpecificPtrs(TTD::SnapshotExtractor* extractor) override;
    virtual TTD::NSSnapObjects::SnapObjectType GetSnapTag_TTD() const override;
    virtual void ExtractSnapObjectDataInto(TTD::NSSnapObjects::SnapObject* objData, TTD::SlabAllocator& alloc) override;
    // virtual void ProcessCorePaths() override;
#endif

#ifdef ENABLE_DEBUG_CONFIG_OPTIONS
public:
    struct BailInSymbol {
        uint32 id;
        Var value;
        static uint32 GetBailInSymbolIdOffset() { return offsetof(BailInSymbol, id); }
        static uint32 GetBailInSymbolValueOffset() { return offsetof(BailInSymbol, value); }
    };

    Field(BailInSymbol*) bailInSymbolsTraceArray = nullptr;
    Field(int) bailInSymbolsTraceArrayCount = 0;

    static uint32 GetBailInSymbolsTraceArrayOffset()
    {
        return offsetof(JavascriptGenerator, bailInSymbolsTraceArray);
    }

    static uint32 GetBailInSymbolsTraceArrayCountOffset()
    {
        return offsetof(JavascriptGenerator, bailInSymbolsTraceArrayCount);
    }

    static void OutputBailInTrace(JavascriptGenerator* generator);
#endif

};

template<>
bool VarIsImpl<JavascriptGenerator>(RecyclableObject* obj);

}
