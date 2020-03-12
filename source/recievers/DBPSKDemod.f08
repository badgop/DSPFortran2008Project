MODULE DBPSKDemod
    USE PSNSimpleMod
    USE impulseGeneratorModule
    USE complexSignalModule
    USE DiffDataModulatorMod
    USE analyticSignalModule
    USE DDSModule
    USE MathConstModule
    USE PhaseDetectorModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: BPSKDemodulator_t
        PRIVATE
        INTEGER(8)                   :: baudRateInSamples
        INTEGER(8)                   :: SampleRate
        INTEGER(8)                   :: chipRateInSamples
        INTEGER(8)                   :: centralFrequency
        INTEGER(1)                   :: outPutSampleCapacity
        INTEGER(1)                   :: outputShift
        TYPE(analyticSignal_t)       :: impluseResponse

        TYPE(PSNSimple_t)            :: psnGnerator

    CONTAINS
        PROCEDURE :: Constructor
        PROCEDURE :: Demodulate

        FINAL     :: destructor
    END TYPE BPSKDemodulator_t

CONTAINS

    SUBROUTINE Constructor(this,baudRate,SampleRate,centralFrequency,outPutSampleCapacity,psn,chipRateInSamples&
                          , impulseResponseArray,outputShift)
        class(BPSKDemodulator_t), intent(inout) :: this
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

        CALL this%psnGnerator%Constructor (psn, chipRateInSamples)
        CALL this%impluseResponse%Constructor(impulseResponseArray)
    END SUBROUTINE
    
    FUNCTION Demodulate (this, data)
        CLASS(BPSKDemodulator_t), intent(inout) :: this
        INTEGER(8)  , intent(in)              :: data(:)
        INTEGER(8)  , allocatable             :: Diffdata(:)
        CLASS(analyticSignal_t),  allocatable              :: OutPutPsn
        CLASS(analyticSignal_t) , allocatable              :: OutPutModulationSig
        INTEGER(8)              , allocatable              :: outputDataSig(:)
        INTEGER(8)                            :: stat
        CLASS(analyticSignal_t), allocatable  :: Demodulate

        ALLOCATE (Demodulate)



    END FUNCTION Demodulate



    SUBROUTINE destructor(this)
        type(BPSKDemodulator_t), intent(in) :: this
    END SUBROUTINE

END MODULE DBPSKDemod
