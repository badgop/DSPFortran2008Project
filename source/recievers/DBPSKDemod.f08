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
        REAL(8)                      :: initialPhase
        INTEGER(1)                   :: outPutSampleCapacity
        INTEGER(8)                   :: outputShift
        TYPE(PSNSimple_t)            :: psnGnerator
        TYPE(PhaseDetector_t)        :: phaseDemodulator
    CONTAINS
        PROCEDURE :: Constructor
        PROCEDURE :: Demodulate
        FINAL     :: destructor
    END TYPE BPSKDemodulator_t

CONTAINS

    SUBROUTINE Constructor(this,baudRate,sampleRate,centralFrequency,initialPhase&
                               ,outPutSampleCapacity,psn,chipRateInSamples&
                               ,impulseResponseArray,outputShift)
        class(BPSKDemodulator_t), intent(inout) :: this
        INTEGER(8)  , intent(in)           :: baudRate
        INTEGER(8)  , intent(in)           :: SampleRate
        INTEGER(8)  , intent(in)           :: centralFrequency
        REAL(8)     , INTENT(IN)           :: initialPhase
        INTEGER(1)  , intent(in)           :: outPutSampleCapacity
        INTEGER(8)  , intent(in)           :: psn(:)
        INTEGER(8)  , intent(in)           :: chipRateInSamples
        INTEGER(8)  , intent(in)           :: impulseResponseArray(:)
        INTEGER(8)  , intent(in)           :: outputShift


        this%baudRateInSamples              = baudRate
        this%centralFrequency               = centralFrequency
        this%initialPhase                   = initialPhase
        this%SampleRate                     = SampleRate
        this%outPutSampleCapacity           = outPutSampleCapacity
        this%outputShift                    = outputShift

        CALL this%psnGnerator%Constructor (psn, chipRateInSamples)

        CALL this%phaseDemodulator%Constructor(this%centralFrequency&
                                        ,this%initialPhase&
                                        ,this%sampleRate&
                                        ,impulseResponseArray&
                                        ,int(8,8))

     END SUBROUTINE
    
    FUNCTION Demodulate (this, inputSig)
        CLASS(BPSKDemodulator_t), intent(inout) :: this
        CLASS(analyticSignal_t) , allocatable   :: inputSig
        CLASS(complexSignal_t)  , allocatable   :: Demodulate

        ALLOCATE (Demodulate)


        Demodulate = this%phaseDemodulator%Downconvert(inputSig)



     END FUNCTION Demodulate



    SUBROUTINE destructor(this)
        type(BPSKDemodulator_t), intent(in) :: this
    END SUBROUTINE

END MODULE DBPSKDemod
