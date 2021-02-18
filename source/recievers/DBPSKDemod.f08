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
    USE  ModuleWriteReadArrayFromToFile
    USE ModuleWriteReadArrayFromToFile
    USE WriteReadComplexSignalToFromFile
    USE FastModuleMod
    USE ArrayFunctionsMod
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
        INTEGER(8)                   :: thresholdSumm
        INTEGER(8)                   :: decimationCoeff
        INTEGER(1)                   :: ethalonCapacity
        LOGICAL                      :: signumCompute


        TYPE(PSNSimple_t)            :: psnGnerator
        TYPE(PhaseDetector_t)        :: phaseDemodulator
        TYPE(DiffCodeGenerator_t)    :: deCoder
        TYPE(signumSignal_t)         :: currentPRSSignal
        TYPE(analyticSignal_t)       :: currentPRSSignalAnalytic
    CONTAINS
        PROCEDURE :: Constructor
        ! преобразование по частоте вниз
        ! разложение на квадратуры
        ! Вычисление ВКФ
        PROCEDURE :: Demodulate
        PROCEDURE :: SetTreshold
        PROCEDURE :: SetTresholdSumm
        ! Пороговая обработка сигнала после согласованного фильтра
        PROCEDURE :: TresholdProcessing
        PROCEDURE :: GetData
        PROCEDURE :: SetSignumComputeMode
        FINAL     :: destructor
    END TYPE BPSKDemodulator_t

CONTAINS

    SUBROUTINE Constructor(this,baudRate,sampleRate,centralFrequency,initialPhase&
                               ,outPutSampleCapacity,psn,chipRateInSamples&
                               ,impulseResponseArray,outputShift,decimationCoeff,ethalonCapacity)
        CLASS(BPSKDemodulator_t), INTENT(inout) :: this
        INTEGER(8)  , INTENT(IN)           :: baudRate
        INTEGER(8)  , INTENT(IN)           :: SampleRate
        INTEGER(8)  , INTENT(IN)           :: centralFrequency
        REAL(8)     , INTENT(IN)           :: initialPhase
        INTEGER(1)  , INTENT(IN)           :: outPutSampleCapacity
        INTEGER(1)  , INTENT(IN)           :: psn(:)
        INTEGER(8)  , INTENT(IN)           :: chipRateInSamples
        INTEGER(8)  , INTENT(IN)           :: impulseResponseArray(:)
        INTEGER(8)  , INTENT(IN)           :: outputShift
        INTEGER(8)  , INTENT(IN)           :: decimationCoeff
        INTEGER(1)  , INTENT(IN)           :: ethalonCapacity
        INTEGER(1)  , ALLOCATABLE          :: psnSignalArray(:)


        this%baudRateInSamples              = baudRate
        this%centralFrequency               = centralFrequency
        this%initialPhase                   = initialPhase
        this%SampleRate                     = SampleRate
        this%outPutSampleCapacity           = outPutSampleCapacity
        this%outputShift                    = outputShift
        this%decimationCoeff                = decimationCoeff
        this%ethalonCapacity                = ethalonCapacity

        CALL this%psnGnerator%Constructor (psn, chipRateInSamples/this%decimationCoeff)
        CALL this%phaseDemodulator%Constructor(this%centralFrequency&
                                        ,this%initialPhase&
                                        ,this%sampleRate&
                                        ,impulseResponseArray&
                                        ,int(outputShift,8))
       psnSignalArray=this%psnGnerator%OutPutPsnArray(int(1,8))

       !CALL ReverseArrayInt1(psnSignalArray)



       CALL this%currentPRSSignal%Constructor(psnSignalArray)

       psnSignalArray = psnSignalArray*ethalonCapacity
       CALL this%currentPRSSignalAnalytic%Constructor(psnSignalArray)
       DEALLOCATE(psnSignalArray)
     END SUBROUTINE

     ! установка порогового значения решающего устройства
     SUBROUTINE SetTreshold(this,threshold)
        CLASS(BPSKDemodulator_t), INTENT(inout) :: this
        INTEGER(8)  , INTENT(IN)                :: threshold
        this%threshold   = threshold
     END SUBROUTINE

       ! установка порогового значения решающего устройства
     SUBROUTINE SetTresholdSumm(this,thresholdSumm)
        CLASS(BPSKDemodulator_t), INTENT(inout) :: this
        INTEGER(8)  , INTENT(IN)                :: thresholdSumm
        this%thresholdSumm   = thresholdSumm
     END SUBROUTINE SetTresholdSumm
    
    !Осуществляет : преобразоваине по частоте вниз
    !               разложение на квадратуры
    !               Согласованную фильтрацию знаковую
    ! Возвращает выходной сигнал согласованного фильтра
    FUNCTION Demodulate (this, inputSig)
        CLASS(BPSKDemodulator_t), INTENT(inout) :: this
        CLASS(analyticSignal_t) , INTENT(in)    :: inputSig
        CLASS(complexSignal_t)  , ALLOCATABLE   :: Demodulate

        ALLOCATE (Demodulate)
        ! преобразование вниз и разложение на квадратуры
        Demodulate = this%phaseDemodulator%Downconvert(inputSig)
        Demodulate = Demodulate%Decimate(this%decimationCoeff)
        CALL WriteComplexSignalToFile(Demodulate,int(2,1),'test_signals\output\Ipath.pcm','test_signals\output\Qpath.pcm')



        ! согласованная фильтрация
       ! WRITE (*,*) 'тип обрабоки'

        IF (this%signumCompute) THEN
            ! согласованная фильтрация
            WRITE (*,*) 'свертка знаковая'

!           Demodulate = Demodulate%ClipSignal(int(0,2),int(1,2))
!           Demodulate = Demodulate.CONV.this%currentPRSSignalAnalytic

           Demodulate = Demodulate.CONVSIGN.this%currentPRSSignal

        ELSE
            WRITE (*,*) 'свертка полноразрядная'
           Demodulate = Demodulate%ClipSignal(int(this%ethalonCapacity,2),int(this%ethalonCapacity,2))
           Demodulate = Demodulate.CONV.this%currentPRSSignalAnalytic

        END IF

    !  CALL WriteComplexSignalToFile(Demodulate,int(2,1),'test_signals\output\Icorr.pcm','test_signals\output\Qcorr.pcm')

     END FUNCTION Demodulate

     ! Осуществляет пороговую обработку выходного сигнала
     ! согласованного фильтра
     ! Возвращает массив  с принятой информацией
     ! Ождиается что на один импульс приходиться не более 3 отсчетов
     FUNCTION TresholdProcessing(this, matchedFilterOut)
        USE WriteReadAnalyticSignalToFromFile
        CLASS(BPSKDemodulator_t), INTENT(in)  :: this
        CLASS(complexSignal_t)  , INTENT(inout)  :: matchedFilterOut
        INTEGER(1)              , ALLOCATABLE :: TresholdProcessing(:)
        INTEGER(8),dimension(:) , ALLOCATABLE :: module
        !!!!!!!!!!!!!!!!!!!
        INTEGER(2)              , ALLOCATABLE :: module2(:)
        INTEGER(8), ALLOCATABLE               :: realPart(:)
        INTEGER(8), ALLOCATABLE               :: imagePart(:)
        INTEGER(8)                            :: i
        INTEGER(8)                            :: lastI
        INTEGER(1)                            :: bitBuffer(1:32767)
        INTEGER(8)                            :: cnt
        LOGICAL                               :: latchEarly = .FALSE.
        LOGICAL                               :: latchLate  = .FALSE.
        INTEGER(8)                            :: pointAccumulator
        INTEGER(8)                            :: maxMod=0
        bitBuffer=0
        pointAccumulator = 0
        module = matchedFilterOut%GetModuleFast()
        ALLOCATE(module2(1:size(module)))
        !module2 =  SHIFTA(module,4)
        module2 =  module
        CALL WriteArrayToFile (module2, 'test_signals\output\last_modulesinglePSN.pcm')


        lasti=0
        CALL matchedFilterOut%ExtractSignalData(realPart,imagePart)
        cnt=0
!        WRITE(*,*) 'size module ', size(module)
        DO i=1,size(module)
           IF (GetFastMouleFromComplexInt8(realPart(i),imagePart(i))>=this%threshold) THEN
                latchEarly = .TRUE.
               !WRITE(*,*) 'БОЛЬШЕ i',i , (realPart(i)) , (imagePart(i)), module(i),i-lasti
               lasti=i
               IF (module(i)>maxMod) maxMod = module(i)
!                pointAccumulator = pointAccumulator + 1

               !Обработка созвездия ведется с учетом базиса [COS, -SIN]
!               IF((realPart(i)>0).AND.(imagePart(i)<0)) THEN
!                   cnt=cnt+1
!                   bitBuffer(cnt)=1
!                   WRITE(*,*) 'принята 1 ', i , module(i), (realPart(i)) , (imagePart(i))
!               END IF
!               IF((realPart(i)<0).AND.(imagePart(i)>0)) THEN
!                   cnt=cnt+1
!                   bitBuffer(cnt)=0
!                   WRITE(*,*) 'принята 0 ', i ,module(i), (realPart(i)) , (imagePart(i))
!               END IF

               IF((realPart(i)>0).AND.(imagePart(i)<0)) THEN
                   pointAccumulator = pointAccumulator + 1
                   !WRITE(*,*) '1'
               END IF
               IF((realPart(i)<0).AND.(imagePart(i)>0)) THEN
                   pointAccumulator = pointAccumulator - 1
                   !WRITE(*,*) '0'
               END IF


           ELSE
              IF(latchEarly) latchLate = .TRUE.
              !WRITE(*,*) 'МЕНЬШЕ i',i , (realPart(i)) , (imagePart(i)), module(i),i

           END IF

           IF(latchEarly.AND.latchLate) THEN
            !  WRITE(*,*) 'поймали  ',pointAccumulator

              IF (pointAccumulator>=this%thresholdSumm) THEN
                 cnt=cnt+1
                 bitBuffer(cnt)=1
                  !WRITE(*,*) 'приныто 1 ', pointAccumulator
              END IF

              IF (pointAccumulator<=-this%thresholdSumm) THEN
                 cnt=cnt+1
                 bitBuffer(cnt)=0
               !  WRITE(*,*) 'приныто 0 ',pointAccumulator
              END IF


              pointAccumulator = 0
              latchEarly = .FALSE.
              latchLate  = .FALSE.
           END IF


        END DO
!        WRITE(*,*) 'ВО ТУТ ПИЗДЕЦ ', cnt
       ! WRITE(*,*) 'статус  ', ALLOCATED(TresholdProcessing)
        IF (cnt == 0) THEN
             WRITE(*,*) 'ничего не принято'
             cnt=1
        END IF
        !WRITe(*,*) 'CNT ',cnt
        ALLOCATE(TresholdProcessing(1:cnt))
        TresholdProcessing = bitBuffer(1:cnt)

        WRITE(*,*) 'MAXIMUS ', maxMod


     END FUNCTION TresholdProcessing

     FUNCTION GetData(this, inputSig)
        CLASS(BPSKDemodulator_t), INTENT(inout) :: this
        CLASS(analyticSignal_t) , INTENT(in)    :: inputSig
        INTEGER(1)              , ALLOCATABLE   :: GetData(:)
        INTEGER(1)              , ALLOCATABLE   :: demodulatedData(:)

        CLASS(complexSignal_t)  , ALLOCATABLE   :: Demodulate
        ALLOCATE(Demodulate)
        Demodulate = this%Demodulate(inputSig)
        demodulatedData    = this%TresholdProcessing(Demodulate)
        GetData            = this%deCoder%DecodeDBPSKData(demodulatedData)

     END FUNCTION GetData

    SUBROUTINE  SetSignumComputeMode(this,signumCompute)
         CLASS(BPSKDemodulator_t), INTENT(inout) :: this
         LOGICAL, INTENT(IN)                     :: signumCompute
         this%signumCompute = signumCompute
    END SUBROUTINE SetSignumComputeMode
    SUBROUTINE destructor(this)
        type(BPSKDemodulator_t), INTENT(IN) :: this
    END SUBROUTINE

END MODULE DBPSKDemod
