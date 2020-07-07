MODULE AWGNChannelMod
    USE analyticSignalModule
    USE complexSignalModule
    USE ModuleExitProg
    USE POWER_METER
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: AWGNChannel_t
        PRIVATE
        INTEGER(2),ALLOCATABLE    :: noiseArray(:)
        REAL(4)                   :: powerNoise
        INTEGER(8)                :: noiseArraySize
        INTEGER(8)                :: ptr = 0
    CONTAINS
        PROCEDURE                 :: MakeBandNoise
        PROCEDURE                 :: LoadNoiseInt2
        PROCEDURE                 :: AddNoiseAnalytic
        PROCEDURE, PRIVATE        :: CheckNoiseIsLoaded
        PROCEDURE,NOPASS, PRIVATE :: CalculateNeededAmplitudeKoeff
        PROCEDURE                 :: SetPtr
        PROCEDURE                 :: GetPowerNoise
        FINAL                     :: destructor
    END TYPE AWGNChannel_t

CONTAINS

   FUNCTION MakeBandNoise(this,inputSignal,impulseResponse,outPutShift)
        CLASS(AWGNChannel_t), INTENT(in)     :: this
        CLASS(analyticSignal_t), INTENT(IN)  :: inputSignal
        CLASS(analyticSignal_t), INTENT(IN)  :: impulseResponse
        INTEGER(1),INTENT(IN)                :: outPutShift
        CLASS (analyticSignal_t),ALLOCATABLE :: MakeBandNoise

        ALLOCATE(MakeBandNoise)
        MakeBandNoise = inputSignal.CONV.impulseResponse
        CALL MakeBandNoise%RShift(outPutShift)
    END  FUNCTION MakeBandNoise

    SUBROUTINE LoadNoiseInt2(this,inputSignal)
        CLASS(AWGNChannel_t), INTENT(INOUT)     :: this
        CLASS(analyticSignal_t), INTENT(IN)     :: inputSignal
        INTEGER(2),ALLOCATABLE                  :: tmpArray(:)

        CALL inputSignal%ExtractSignalData(tmpArray)
        ALLOCATE(this%noiseArray,source  =(tmpArray))
        DEALLOCATE(tmpArray)
        this%powerNoise     = GetSignalRmsPowerINT2(this%noiseArray,int(size(this%noiseArray),8))
        this%noiseArraySize = size(this%noiseArray)
    END SUBROUTINE LoadNoiseInt2
    
    FUNCTION AddNoiseAnalytic(this,inputSignal,snrNeed,outCapacity)
        CLASS(AWGNChannel_t)    , INTENT(in)          :: this
        CLASS(analyticSignal_t) , INTENT(IN)          :: inputSignal
        REAL(4)                 , INTENT(IN)          :: snrNeed
        INTEGER(1)              , INTENT(IN)          :: outCapacity
        CLASS (analyticSignal_t),ALLOCATABLE          :: AddNoiseAnalytic
        INTEGER(2)              ,ALLOCATABLE          :: inputSignalArrayInt2(:)
        INTEGER(2)              ,ALLOCATABLE          :: noiseSignalArrayInt2(:)

        REAL(8)                                       :: powerInput
        REAL(8)                                       :: koeff,x,y
        INTEGER(8)                                    :: i,ptr
        integer(2)                                    :: z
        REAL(8)                                       :: yy = 0
        REAL(8)                                       :: summ
        REAL(8)                                       :: summ2
        REAL(8)                                       :: scaler


        CALL CheckNoiseIsLoaded(this)

        ALLOCATE(AddNoiseAnalytic)
!        WRITE (*,*) 'Извлекаю! '
!        Write (*,*) 'kind ' ,  inputSignal%GetSiGnalKind()
        CALL inputSignal%ExtractSignalData(inputSignalArrayInt2)
        powerInput =  GetSignalRmsPowerINT2 (inputSignalArrayInt2,int(size(inputSignalArrayInt2),8))
        WRITE (*,*) 'powerInput ' ,powerInput
        koeff = CalculateNeededAmplitudeKoeff (powerInput,this%powerNoise,snrNeed)
        WRITE(*,*) 'koeff ',koeff
        ptr= this%ptr
!        WRITE(*,*) this%ptr
!        WRITE(*,*) size(this%noiseArray)

        summ = 0
        summ2 = 0
        yy=0
        ALLOCATE(noiseSignalArrayInt2(1:size(inputSignalArrayInt2)))
        WRITE(*,*) 'длина noiseSignalArrayInt2 ',size(noiseSignalArrayInt2)
        DO i=1, size (inputSignalArrayInt2)
           IF ((ptr)>size(this%noiseArray)) THEN
              DO WHILE(ptr>size(this%noiseArray))
                  ptr = ptr-size(this%noiseArray)
              END DO
               WRITE(*,*) 'ptr ', ptr
              WRITE(*,*) 'ПЕРЕХОД'
           END IF

            noiseSignalArrayInt2(i)=this%noiseArray(ptr)
            ptr = ptr + 1

       END DO



        DO i=1, size (inputSignalArrayInt2)
!           IF ((ptr)>size(this%noiseArray)) THEN
!              DO WHILE(ptr>size(this%noiseArray))
!                  ptr = ptr-size(this%noiseArray)
!              END DO
!               WRITE(*,*) 'ptr ', ptr
!              WRITE(*,*) 'ПЕРЕХОД'
!           END IF
           ! НОРМИРОВКА СИГНАЛА!!!!!

           !koeff =4.0
           x = float(inputSignalArrayInt2(i))/32767.0
          ! y = float(this%noiseArray(ptr))/32767.0
        y = float( noiseSignalArrayInt2(i))/32767.0
          !
           ! на 3дБ по напряжениб больше
           x = x*koeff*2
           summ = x+y
           summ = summ*2
           scaler = float(2**(outCapacity-1)-1)
           z = int(summ*scaler,2)
           inputSignalArrayInt2(i) = z
          ! ptr = ptr + 1

           yy=yy+y**2
           summ2=summ2+x**2

           !WRITE(*,*) y,yy
           !WRITE(*,*) this%noiseArray(i+ptr)
        END DO

        CALL  AddNoiseAnalytic%Constructor(inputSignalArrayInt2)
!        WRITE (*,*) 'size ', size(inputSignalArrayInt2)
        DEALLOCATE(inputSignalArrayInt2)
!        WRITE (*,*) 'ВЫШЕЛ! '
         yy=yy/float(size (inputSignalArrayInt2))
         summ2=summ2/float(size (inputSignalArrayInt2))
         WRITE (*,*) 'нощность ШУМА дБ ',   ((20.0*log10(sqrt(yy   )))-3.0)
         yy =  GetSignalRmsPowerINT2(noiseSignalArrayInt2, int(size(noiseSignalArrayInt2),8))
         WRITE (*,*) 'нощность ШУМА2 дБ ',  yy
         WRITE (*,*) 'нощность сигнла дБ ', ((20.0*log10(sqrt(summ2)))-3.0)

    END  FUNCTION AddNoiseAnalytic

      FUNCTION AddNoiseComplex(this,inputSignal,SNRneed,outCapacity)
        CLASS(AWGNChannel_t)    , INTENT(in)          :: this
        CLASS(analyticSignal_t) , INTENT(IN)          :: inputSignal
        REAL(8)                 , INTENT(IN)          :: SNRneed
        INTEGER(1)              , INTENT(IN)          :: outCapacity
        CLASS (complexSignal_t) , ALLOCATABLE         :: AddNoiseComplex

        CALL CheckNoiseIsLoaded(this)

    END  FUNCTION AddNoiseComplex

    FUNCTION CalculateNeededAmplitudeKoeff(powerInput,powerNoise,snrNeed)
       REAL(4)                 , INTENT(IN)          :: snrNeed
       REAL(8)                 , INTENT(IN)          :: powerInput
       REAL(4)                 , INTENT(IN)          :: powerNoise
       REAL(8)                           :: CalculateNeededAmplitudeKoeff
       REAL(8)                           :: coeff_dB,snrCurrent

       snrCurrent =  powerInput-powerNoise
       WRITE(*,*) 'ОСШ нынешнее ',snrCurrent

       coeff_dB = -(snrCurrent - snrNeed)
       WRITE(*,*) 'koeff amp noise dB ', coeff_dB

       CalculateNeededAmplitudeKoeff = 10.0**(0.05*(coeff_dB))
    END  FUNCTION CalculateNeededAmplitudeKoeff

    SUBROUTINE SetPtr(this,ptrValue)
        CLASS(AWGNChannel_t)    , INTENT(inout)      :: this
        INTEGER(8)              , INTENT(inout)      :: ptrValue

        this%ptr =   ptrValue
    END  SUBROUTINE  SetPtr


    ! проверяет загружен ли шумовой сигнал
    SUBROUTINE CheckNoiseIsLoaded(this)
        CLASS(AWGNChannel_t)    , INTENT(in)          :: this
        IF(.NOT. ALLOCATED(this%noiseArray)) THEN
         WRITE(*,*) 'Шум не загружен в память генератора!'
         CALL ExitFromProgramNormal()
        END IF
    END SUBROUTINE CheckNoiseIsLoaded

      FUNCTION GetPowerNoise(this)
        CLASS(AWGNChannel_t)    , INTENT(inout)      :: this
        REAL(8)                                      :: GetPowerNoise
        GetPowerNoise =   this%powerNoise
    END  FUNCTION GetPowerNoise


    SUBROUTINE destructor(this)
        type(AWGNChannel_t), INTENT(inOUT) :: this
        IF(ALLOCATED(this%noiseArray))   DEALLOCATE(this%noiseArray)
    END SUBROUTINE

END MODULE AWGNChannelMod
