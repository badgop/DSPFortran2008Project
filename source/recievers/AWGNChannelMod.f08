MODULE AWGNChannelMod
    USE analyticSignalModule
    USE complexSignalModule
    USE ModuleExitProg
    USE POWER_METER
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: AWGNChannel_t
        PRIVATE
        REAL(4),ALLOCATABLE    :: noiseArray(:)
        REAL(8)                   :: powerNoise
        INTEGER(8)                :: noiseArraySize
        INTEGER(8)                :: ptr = 0
    CONTAINS
        PROCEDURE                 :: MakeBandNoise
        PROCEDURE                 :: LoadNoise
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

    SUBROUTINE LoadNoise(this,inputSignal)
        CLASS(AWGNChannel_t), INTENT(INOUT)     :: this
        CLASS(analyticSignal_t), INTENT(IN)     :: inputSignal
        INTEGER(8),ALLOCATABLE                  :: tmpArray(:)
        CALL inputSignal%ExtractSignalData(tmpArray)
        ! обратите внимание - приведение у другому типу
        ALLOCATE(this%noiseArray,source  = FLOAT(tmpArray))
        !this%powerNoise     = GetSignalRmsPowerReal4(this%noiseArray,int(50000,8))
        this%noiseArraySize = size(this%noiseArray)
        DEALLOCATE(tmpArray)

    END SUBROUTINE LoadNoise
    
    FUNCTION AddNoiseAnalytic(this,inputSignal,snrNeed,outCapacity)
        CLASS(AWGNChannel_t)    , INTENT(in)          :: this
        CLASS(analyticSignal_t) , INTENT(IN)          :: inputSignal
        REAL(8)                 , INTENT(IN)          :: snrNeed
        INTEGER(1)              , INTENT(IN)          :: outCapacity
        CLASS (analyticSignal_t),ALLOCATABLE          :: AddNoiseAnalytic
        REAL(4)                 ,ALLOCATABLE          :: inputSignalArrayReal(:)
        INTEGER(8)              ,ALLOCATABLE          :: inputSignalArrayInt8(:)
        REAL(4)                 ,ALLOCATABLE          :: outputSignalArrayReal(:)
        REAL(8)                                       :: powerInput
        REAL(8)                                       :: koeff
        INTEGER(8)                                    :: inputSignalSize


        ALLOCATE(AddNoiseAnalytic)
        CALL CheckNoiseIsLoaded(this)
        CALL inputSignal%ExtractSignalData(inputSignalArrayInt8)
        inputSignalArrayReal = FLOAT(inputSignalArrayInt8)/FLOAT(2**15-1)
        DEALLOCATE(inputSignalArrayInt8)
        inputSignalSize = inputSignal%GetSignalSize()

       ! powerInput = GetSignalRmsPowerReal4(inputSignalArrayReal, inputSignalSize)
        koeff = CalculateNeededAmplitudeKoeff (powerInput,this%powerNoise,snrNeed)

        IF ( (this%ptr+inputSignalSize)>this%noiseArraySize) THEN

         inputSignalArrayReal(1:(this%noiseArraySize-this%ptr) ) =  inputSignalArrayReal(1:(this%noiseArraySize-this%ptr))&
                             + (this%noiseArray(this%ptr:this%noiseArraySize))/FLOAT(2**15-1)

         inputSignalArrayReal((this%noiseArraySize-this%ptr+1):inputSignalSize) = &
                          inputSignalArrayReal((this%noiseArraySize-this%ptr+1):inputSignalSize)&
                         + (this%noiseArray(1:(this%ptr+inputSignalSize-this%noiseArraySize+1)))/FLOAT(2**15-1)

        ELSE
          inputSignalArrayReal =  inputSignalArrayReal + inputSignalArrayReal(this%ptr+inputSignalSize)
        END IF

          ! inputSignalArrayReal = inputSignalArrayReal /max(inputSignalArrayReal)




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
       REAL(8)                 , INTENT(IN)          :: snrNeed
       REAL(8)                 , INTENT(IN)          :: powerInput
       REAL(8)                 , INTENT(IN)          :: powerNoise
       REAL(8)                           :: CalculateNeededAmplitudeKoeff
       CalculateNeededAmplitudeKoeff = 10**(0.05*(powerInput-powerNoise-snrNeed))
    END  FUNCTION CalculateNeededAmplitudeKoeff

     FUNCTION SetPtr(this,ptrValue)
        CLASS(AWGNChannel_t)    , INTENT(inout)      :: this
        INTEGER(8)              , INTENT(inout)      :: ptrValue
        INTEGER(8)                                   :: SetPtr
        this%ptr =   ptrValue
    END  FUNCTION SetPtr


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
