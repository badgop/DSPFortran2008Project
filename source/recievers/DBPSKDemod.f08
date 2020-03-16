MODULE DBPSKDemod
    USE PSNSimpleMod
    USE impulseGeneratorModule
    USE complexSignalModule
    USE DiffDataModulatorMod
    USE analyticSignalModule
    USE signumSignalModule
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
        INTEGER(8)                   :: threshold
        TYPE(PSNSimple_t)            :: psnGnerator
        TYPE(PhaseDetector_t)        :: phaseDemodulator
        TYPE(signumSignal_t)         :: currentPRSSignal
    CONTAINS
        PROCEDURE :: Constructor
        PROCEDURE :: Demodulate
        PROCEDURE :: SetTreshold
        FINAL     :: destructor
    END TYPE BPSKDemodulator_t

CONTAINS

    SUBROUTINE Constructor(this,baudRate,sampleRate,centralFrequency,initialPhase&
                               ,outPutSampleCapacity,psn,chipRateInSamples&
                               ,impulseResponseArray,outputShift)
        CLASS(BPSKDemodulator_t), INTENT(inout) :: this
        INTEGER(8)  , INTENT(IN)           :: baudRate
        INTEGER(8)  , INTENT(IN)           :: SampleRate
        INTEGER(8)  , INTENT(IN)           :: centralFrequency
        REAL(8)     , INTENT(IN)           :: initialPhase
        INTEGER(1)  , INTENT(IN)           :: outPutSampleCapacity
        INTEGER(8)  , INTENT(IN)           :: psn(:)
        INTEGER(8)  , INTENT(IN)           :: chipRateInSamples
        INTEGER(8)  , INTENT(IN)           :: impulseResponseArray(:)
        INTEGER(8)  , INTENT(IN)           :: outputShift

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

       !ТО ЧТО СДЕЛАТЬ С ГЕНЕРАТОРОМ ПСП!!!!



     END SUBROUTINE

     ! установка порогового значения решающего устройства
     SUBROUTINE SetTreshold(this,threshold)
        CLASS(BPSKDemodulator_t), INTENT(inout) :: this
        INTEGER(8)  , INTENT(IN)                :: threshold

        this%threshold   = threshold
     END SUBROUTINE
    

    FUNCTION Demodulate (this, inputSig)
        CLASS(BPSKDemodulator_t), INTENT(inout) :: this
        CLASS(analyticSignal_t) , INTENT(in)    :: inputSig
        INTEGER(8)              , ALLOCATABLE   :: Demodulate(:)

        CLASS(complexSignal_t)  , ALLOCATABLE   :: matchedFilterOut
        CLASS(complexSignal_t)  , ALLOCATABLE   :: Demodulated


        ALLOCATE (matchedFilterOut)
        ALLOCATE (Demodulated)

        Demodulated = this%phaseDemodulator%Downconvert(inputSig)



     END FUNCTION Demodulate



    SUBROUTINE destructor(this)
        type(BPSKDemodulator_t), INTENT(IN) :: this
    END SUBROUTINE

END MODULE DBPSKDemod
