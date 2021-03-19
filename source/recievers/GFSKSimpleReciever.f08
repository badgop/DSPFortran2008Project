MODULE GFSKSimpleReciever
    USE analyticSignalModule
    USE complexSignalModule
    USE MathConstModule
    USE FreqDetectMod
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: GFSKSimpleDemodulator
        PRIVATE
        INTEGER(8)                   :: decimationRate
        INTEGER(1)                   :: outputfilterShift

        TYPE(analyticSignal_t)       :: ChannelFilterImpulseResponse

    CONTAINS
        PROCEDURE ::Constructor
        PROCEDURE ::Demodulate
        FINAL :: destructor
    END TYPE GFSKSimpleDemodulator

CONTAINS

    SUBROUTINE Constructor(this,decimationRate,impulseResponse,outputfilterShift)
        CLASS(GFSKSimpleDemodulator), INTENT(INOUT) :: this
        INTEGER(8)                  , INTENT(IN)    :: decimationRate
        INTEGER(2),DIMENSION(:)     , INTENT(IN)    :: impulseResponse
        INTEGER(1)                  , INTENT(IN)    :: outputfilterShift

        CALL  this%ChannelFilterImpulseResponse%Constructor(impulseResponse)
        this%decimationRate    = decimationRate
        this%outputfilterShift = outputfilterShift
    END SUBROUTINE
    
     FUNCTION Demodulate (this, inputSig) RESULT (demodSignal)
          CLASS(GFSKSimpleDemodulator), INTENT(IN) :: this
          CLASS(complexSignal_t)                   :: inputSig
          CLASS(analyticSignal_t)  , ALLOCATABLE   :: demodSignal

          ALLOCATE(demodSignal)

          inputSig = inputSig.CONV.this%ChannelFilterImpulseResponse
          CALL inputSig%Rshift(this%outputfilterShift)
          inputSig =  inputSig%Decimate(this%decimationRate)



          CALL FreqDetectorComplexSignalINT8(inputSig,demodSignal)

     END FUNCTION Demodulate





    SUBROUTINE destructor(this)
        TYPE(GFSKSimpleDemodulator), INTENT(INOUT) :: this
    END SUBROUTINE

END MODULE GFSKSimpleReciever
