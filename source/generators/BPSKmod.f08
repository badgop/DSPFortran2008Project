MODULE BPSKmod
    USE PSNSimpleMod
    USE impulseGeneratorModule
    USE complexSignalModule
    USE DiffDataModulatorMod
    USE analyticSignalModule
    USE DDSModule
    USE MathConstModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: BPSKmodulator_t
        PRIVATE
        INTEGER(8)                   :: baudRateInSamples
        INTEGER(8)                   :: SampleRate
        INTEGER(8)                   :: chipRateInSamples
        INTEGER(8)                   :: centralFrequency
        INTEGER(1)                   :: outPutSampleCapacity
        INTEGER(1)                   :: outputShift
        TYPE(analyticSignal_t)       :: impluseResponse
        TYPE(DiffCodeGenerator_t)    :: coder
        TYPE(PSNSimple_t)            :: psnGnerator
        TYPE(DDS_t)                  :: mixer
    CONTAINS
        PROCEDURE :: Constructor
        PROCEDURE :: Generate
        PROCEDURE :: GenerateDiffData
        FINAL     :: destructor
    END TYPE BPSKmodulator_t

CONTAINS

    SUBROUTINE Constructor(this,baudRate,SampleRate,centralFrequency,outPutSampleCapacity,psn,chipRateInSamples&
                          , impulseResponseArray,outputShift)
        class(BPSKmodulator_t), intent(inout) :: this
        INTEGER(8)  , intent(in)           :: baudRate
        INTEGER(8)  , intent(in)           :: SampleRate
        INTEGER(8)  , intent(in)           :: centralFrequency
        INTEGER(1)  , intent(in)           :: outPutSampleCapacity
        INTEGER(8)  , intent(in)           :: psn(:)
        INTEGER(8)  , intent(in)           :: chipRateInSamples
        INTEGER(8)  , intent(in)           :: impulseResponseArray(:)
        INTEGER(1)  , intent(in)           :: outputShift
        this%baudRateInSamples              = baudRate
        this%centralFrequency               = centralFrequency
        this%SampleRate                     = SampleRate
        this%outPutSampleCapacity           = outPutSampleCapacity
        this%outputShift                    = outputShift
        ! начальная фаза сигнала!!!!
        CALL this%coder%Constructor(int(1,8))
        CALL this%psnGnerator%Constructor (psn, chipRateInSamples)
        CALL this%impluseResponse%Constructor(impulseResponseArray)
    END SUBROUTINE
    

    FUNCTION GenerateDiffData (this, data)
        CLASS(BPSKmodulator_t), intent(inout) :: this
        INTEGER(1)  , intent(in)              :: data(:)
        INTEGER(1)  , allocatable             :: GenerateDiffData(:)

        GenerateDiffData = this%coder%GenerateDBPSKData(data)

    END FUNCTION GenerateDiffData

    FUNCTION Generate (this, data)
        CLASS(BPSKmodulator_t), intent(inout) :: this
        INTEGER(1)  , intent(in)              :: data(:)
        INTEGER(8)  , allocatable             :: Diffdata(:)
        CLASS(analyticSignal_t),  allocatable :: OutPutPsn
        CLASS(analyticSignal_t) , allocatable :: OutPutModulationSig
        INTEGER(8)              , allocatable :: outputDataSig(:)
        INTEGER(8)                            :: stat
        CLASS(analyticSignal_t), allocatable  :: Generate
        CLASS(analyticSignal_t), allocatable  :: heterodyneSignal


        ALLOCATE(Generate)
        ALLOCATE(OutPutPsn)
        ALLOCATE(OutPutModulationSig)
        ALLOCATE(heterodyneSignal)
        diffData = this%GenerateDiffData(data)
        outputDataSig = GenerateImpluseSequence(this%baudRateInSamples,diffData)
        CALL OutPutModulationSig%Constructor(outputDataSig)
        OutPutPsn = this%psnGnerator%OutPutPsnAs (int(size(diffData),8))
        Generate =  OutPutPsn * OutPutModulationSig
        CALL Generate%ZeroesStuffing(this%baudRateInSamples,this%baudRateInSamples)
        Generate=Generate.CONV.this%impluseResponse
        stat = this%mixer%Constructor (romLengthInBits = int(32,1)&
                                     , romLengthTruncedInBits =int(16,1) &
                                     , samplingFrequency =int(this%SampleRate,4) &
                                     , outputSignalSampleCapacity=int(16,1))

        CALL this%mixer%SetPhase(PI/2)
        CALL this%mixer%ComputeOutput(int(this%centralFrequency,8),(Generate%GetSignalSize()),heterodyneSignal)
        Generate = Generate*heterodyneSignal
        CALL Generate%RShift(this%outputShift)
        DEALLOCATE(OutPutPsn)
        DeALLOCATE(OutPutModulationSig)
        DEALLOCATE(heterodyneSignal)

    END FUNCTION Generate



    SUBROUTINE destructor(this)
        type(BPSKmodulator_t), intent(in) :: this
    END SUBROUTINE

END MODULE BPSKmod
