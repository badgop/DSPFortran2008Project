MODULE GFSKSimpleReciever
    USE analyticSignalModule
    USE complexSignalModule
    USE MathConstModule
    USE FreqDetectMod
    USE WriteReadAnalyticSignalToFromFile
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: GFSKSimpleDemodulator
        PRIVATE
        INTEGER(8)                   :: decimationRate
        INTEGER(1)                   :: outputfilterShift
        INTEGER(1)                   :: outPutFreqShift


        TYPE(analyticSignal_t)       :: ChannelFilterImpulseResponse

    CONTAINS
        PROCEDURE ::Constructor
        PROCEDURE ::Demodulate
        PROCEDURE ::ComputeSymbolFrequency
        FINAL :: destructor
    END TYPE GFSKSimpleDemodulator

CONTAINS

    SUBROUTINE Constructor(this,decimationRate,impulseResponse,outputfilterShift, outPutFreqShift)
        CLASS(GFSKSimpleDemodulator), INTENT(INOUT) :: this
        INTEGER(8)                  , INTENT(IN)    :: decimationRate
        INTEGER(4),DIMENSION(:)     , INTENT(IN)    :: impulseResponse
        INTEGER(1)                  , INTENT(IN)    :: outputfilterShift
        INTEGER(1)                  , intent(in)    :: outPutFreqShift

        CALL  this%ChannelFilterImpulseResponse%Constructor(impulseResponse)
        this%decimationRate    = decimationRate
        this%outputfilterShift = outputfilterShift
        this% outPutFreqShift =  outPutFreqShift
    END SUBROUTINE
    
     FUNCTION Demodulate (this, inputSig) RESULT (demodSignal)
          CLASS(GFSKSimpleDemodulator), INTENT(IN) :: this
          CLASS(complexSignal_t)      , INTENT(IN) :: inputSig
          CLASS(analyticSignal_t)  , ALLOCATABLE   :: demodSignal
           CLASS(analyticSignal_t) , ALLOCATABLE    :: tmp
          TYPE(complexSignal_t)                   :: tmpSignal

          ALLOCATE(demodSignal)
           ALLOCATE(tmp)

          tmpSignal = inputSig.CONV.this%ChannelFilterImpulseResponse
          CALL tmpSignal%Rshift(this%outputfilterShift)
          tmpSignal =  tmpSignal%Decimate(this%decimationRate)
          CALL FreqDetectorComplexSignalINT8(tmpSignal,demodSignal)

           WRITE(*,*) 'max val demodSignal ', demodSignal%GetMax()
         tmp= this%ComputeSymbolFrequency (demodSignal)
!          demodSignal = tmp



     END FUNCTION Demodulate

     FUNCTION ComputeSymbolFrequency (this, inputSig) RESULT (freqSignal)
          CLASS(GFSKSimpleDemodulator), INTENT(IN) :: this
          CLASS(analyticSignal_t)   , INTENT(IN)    :: inputSig
          CLASS(analyticSignal_t)  , ALLOCATABLE   :: freqSignal
          TYPE(analyticSignal_t)                  :: clipSignal
          INTEGER(2), DIMENSION(:) , ALLOCATABLE :: fd_out
          INTEGER(2), DIMENSION(:) , ALLOCATABLE :: fd_dt
          INTEGER(8)                            :: i

          ALLOCATE(freqSignal)

          !clipSignal =  inputSig%ClipSignal(int(0,2),int(1024,2))
          !CALL  clipSignal%ExtractSignalData(fd_out)

          WRITE(*,*) 'max val ', inputSig%GetMax()
          CALL  inputSig%ExtractSignalData(fd_out)

          !fd_out = 12000

          !ALLOCATE(fd_dt, source=fd_out)

!          fd_dt=0
!          DO i=2,size(fd_dt)
!              fd_dt(i) =fd_out(i) !-fd_out(i-1)
!          END DO

          CALL freqSignal%Constructor(fd_out)

           CALL WriteAnalyticSignalToFile(freqSignal,int(2,1),'balagan.pcm')


          DEALLOCATE(fd_out)
!          DEALLOCATE(fd_dt)

     END FUNCTION ComputeSymbolFrequency





    SUBROUTINE destructor(this)
        TYPE(GFSKSimpleDemodulator), INTENT(INOUT) :: this
    END SUBROUTINE

END MODULE GFSKSimpleReciever
