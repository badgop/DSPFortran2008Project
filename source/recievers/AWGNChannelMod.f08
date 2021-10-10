MODULE AWGNChannelMod
    USE analyticSignalModule
    USE complexSignalModule
    USE ModuleExitProg
    USE POWER_METER
    USE RandomMod
    USE RawCorrOpenMPmod
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: AWGNChannel_t
        PRIVATE
        INTEGER(2),DIMENSION(:),ALLOCATABLE    :: noiseArray
        REAL(8)                   :: powerNoise
        INTEGER(8)                :: noiseArraySize
        INTEGER(8)                :: ptr = 1
    CONTAINS
        PROCEDURE                 :: MakeBandNoise
        PROCEDURE                 :: GenerateBandNoise
        PROCEDURE                 :: LoadNoiseInt2
        PROCEDURE                 ::  GetWholeNoise
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

     FUNCTION GetWholeNoise(this)  RESULT (outNoise)
        CLASS(AWGNChannel_t), INTENT(in)     :: this

        CLASS(analyticSignal_t), ALLOCATABLE              :: outNoise

        ALLOCATE (outNoise)



         IF (ALLOCATED(this%noiseArray)) THEN
             CALL outNoise%Constructor(this%noiseArray)
        ELSE
            WRITE(*,*)  'НЕ могу выгрузить шум '
        END IF


     END  FUNCTION  GetWholeNoise

     SUBROUTINE GenerateBandNoise(this,length,impulseResponse,outPutShift)
        CLASS(AWGNChannel_t), INTENT(inout)     :: this
        INTEGER(2),DIMENSION(:), INTENT(IN)  :: impulseResponse
        INTEGER(8) ,INTENT(IN)  :: length
        INTEGER(1),INTENT(IN)                :: outPutShift
        INTEGER(8)                           :: i
        INTEGER(8),DIMENSION(:),ALLOCATABLE              :: noise_out

         REAL       ,PARAMETER  :: m = 0.0
         REAL       ,PARAMETER  :: sigma = 0.2

        IF (ALLOCATED(this%noiseArray)) THEN
            DEALLOCATE(this%noiseArray)
        ELSE
            ALLOCATE(this%noiseArray(1:length))
        END IF

        ALLOCATE(noise_out(1:length))

        CALL RanomGeneratorInit()
        DO i=1,length
             noise_out(i) = GetRandomGaussianInt2(m,sigma)
         END DO

        noise_out = CorrelationRawOpemMP (noise_out,impulseResponse)
        this%noiseArray  = int(SHIFTA( noise_out,outPutShift),2)
        this%noiseArraySize = length
        this%powerNoise     = GetSignalRmsPowerINT2(this%noiseArray,this%noiseArraySize)

         DEALLOCATE(noise_out)


    END  SUBROUTINE GenerateBandNoise

    SUBROUTINE LoadNoiseInt2(this,inputSignal)
        CLASS(AWGNChannel_t), INTENT(INOUT)     :: this
        CLASS(analyticSignal_t), INTENT(IN)     :: inputSignal
        INTEGER(2),ALLOCATABLE,DIMENSION(:)                  :: tmpArray

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
        INTEGER(2),DIMENSION(:),ALLOCATABLE          :: inputSignalArrayInt2
        INTEGER(2),DIMENSION(:),ALLOCATABLE          :: noiseSignalArrayInt2

        REAL(8)                                       :: powerInput
        REAL(8)                                       :: koeff,x,y
        INTEGER(8)                                    :: i,ptr
        integer(2)                                    :: z
        REAL(8)                                       :: yy = 0
        REAL(8)                                       :: summ
        REAL(8)                                       :: summ2
        REAL(8)                                       :: scaler
        REAL(8)                                       :: maxX
        REAL(8)                                       :: maxY


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
       ! WRITE(*,*) 'длина noiseSignalArrayInt2 ',size(noiseSignalArrayInt2)
        DO i=1, size (inputSignalArrayInt2)
           IF ((ptr)>size(this%noiseArray)) THEN
              DO WHILE(ptr>size(this%noiseArray))
                  ptr = ptr-size(this%noiseArray)
              END DO
              !WRITE(*,*) 'ptr ', ptr
              !WRITE(*,*) 'ПЕРЕХОД'
           END IF

            noiseSignalArrayInt2(i)=this%noiseArray(ptr)
            ptr = ptr + 1

       END DO


           maxX = maxVAl(float(abs(inputSignalArrayInt2)))/32767.0 +0.01
           maxY = maxVAl(float(abs(noiseSignalArrayInt2)))/32767.0 +0.01

           WRITE(*,*) maxX, (1.0/maxX)
           WRITE(*,*) maxY, (1.0/maxY)
        DO i=1, size (inputSignalArrayInt2)

           x = float(inputSignalArrayInt2(i) )/32767.0
           y = float(noiseSignalArrayInt2(i) )/32767.0

           ! приводим значение сигналов к величине 1.0
           x = x*(1.0/maxX)
           y = y*(1.0/maxY)



          IF ((abs(x)-1.0)>0.0001) THEN
              WRITE(*,*) 'FUFx'
               WRITE(*,*) x
           END IF

           IF ((abs(y)-1.0)>0.0001) THEN
              WRITE(*,*) 'FUFy'
               WRITE(*,*) y
           END IF



           ! на 3дБ по напряжениб больше
           x = x*koeff*2
           ! пусть x=y=1 , и коэфф =1, тогда summ = 1+1+2 =4.0
           summ = (x+y)/(1+koeff*2)
           IF ((abs(summ)-1.0)>0.001) THEN
              WRITE(*,*) 'FUF'
               WRITE(*,*) summ
           END IF


           scaler = float(2**(outCapacity-1)-1)
           z = int(summ*scaler,2)
           inputSignalArrayInt2(i) = z

        END DO

        CALL  AddNoiseAnalytic%Constructor(inputSignalArrayInt2)

        yy =  GetSignalRmsPowerINT2(inputSignalArrayInt2, int(size(inputSignalArrayInt2),8))
        WRITE (*,*) 'сигнал+шум дБ ',  yy
        WRITE(*,*) 'msc ', maxval(abs(inputSignalArrayInt2))
        DEALLOCATE(inputSignalArrayInt2)
!        WRITE (*,*) 'ВЫШЕЛ! '


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
       REAL(8)                 , INTENT(IN)          :: powerNoise
       REAL(8)                           :: CalculateNeededAmplitudeKoeff
       REAL(8)                           :: coeff_dB,snrCurrent

       snrCurrent =  powerInput-powerNoise
      ! WRITE(*,*) 'ОСШ нынешнее ',snrCurrent

       coeff_dB = -(snrCurrent - snrNeed)
       WRITE(*,*) 'koeff amp noise dB ', coeff_dB

       CalculateNeededAmplitudeKoeff = 10.0**(0.05*(coeff_dB))
    END  FUNCTION CalculateNeededAmplitudeKoeff

    SUBROUTINE SetPtr(this,ptrValue)
        CLASS(AWGNChannel_t)    , INTENT(inout)      :: this
        INTEGER(8)              , INTENT(inout)      :: ptrValue
        IF (ptrValue==0) ptrValue=1
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
