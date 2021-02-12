MODULE DPSK2PSnDeMod

    USE PSNSimpleMod
    USE impulseGeneratorModule
    USE complexSignalModule
    USE DiffDataModulatorMod
    USE analyticSignalModule
    USE signumSignalModule
    USE DDSModule
    USE MathConstModule
    USE PhaseDetectorModule
    USE ModuleWriteReadArrayFromToFile
    USE WriteReadComplexSignalToFromFile
    USE FastModuleMod
    USE ArrayFunctionsMod
    USE WriteReadAnalyticSignalToFromFile

    IMPLICIT NONE




    PRIVATE

    TYPE, PUBLIC :: BPSK2PSNDemodulator_t
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


        TYPE(signumSignal_t)         :: psn0_signum
        TYPE(signumSignal_t)         :: psn1_signum
        TYPE(analyticSignal_t)       :: psn0
        TYPE(analyticSignal_t)       :: psn1



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
    END TYPE BPSK2PSNDemodulator_t

CONTAINS

    SUBROUTINE Constructor(this,baudRate,sampleRate,centralFrequency,initialPhase&
                               ,outPutSampleCapacity,psn0,psn1,chipRateInSamples&
                               ,impulseResponseArray,outputShift,decimationCoeff,ethalonCapacity)
        CLASS(BPSK2PSNDemodulator_t), INTENT(inout) :: this
        INTEGER(8)  , INTENT(IN)           :: baudRate
        INTEGER(8)  , INTENT(IN)           :: SampleRate
        INTEGER(8)  , INTENT(IN)           :: centralFrequency
        REAL(8)     , INTENT(IN)           :: initialPhase
        INTEGER(1)  , INTENT(IN)           :: outPutSampleCapacity
        INTEGER(1)  , INTENT(IN)           :: psn0(:)
        INTEGER(1)  , INTENT(IN)           :: psn1(:)
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


        CALL this%psnGnerator%Constructor (psn0, chipRateInSamples/this%decimationCoeff)
        psnSignalArray=this%psnGnerator%OutPutPsnArray(int(1,8))
        CALL this%psn0_signum%Constructor(psnSignalArray)
        psnSignalArray = psnSignalArray*ethalonCapacity
        CALL this%psn0%Constructor(psnSignalArray)
        DEALLOCATE(psnSignalArray)

        CALL this%psnGnerator%Constructor (psn1, chipRateInSamples/this%decimationCoeff)
        psnSignalArray=this%psnGnerator%OutPutPsnArray(int(1,8))
        CALL this%psn1_signum%Constructor(psnSignalArray)
        psnSignalArray = psnSignalArray*ethalonCapacity
        CALL this%psn1%Constructor(psnSignalArray)
        DEALLOCATE(psnSignalArray)


!        CALL WriteAnalyticSignalToFile(this%psn1,int(2,1),'test_signals\output\psn1.pcm')
!        CALL WriteAnalyticSignalToFile(this%psn0,int(2,1),'test_signals\output\psn0.pcm')


        CALL this%phaseDemodulator%Constructor(this%centralFrequency&
                                        ,this%initialPhase&
                                        ,this%sampleRate&
                                        ,impulseResponseArray&
                                        ,int(outputShift,8))

     END SUBROUTINE

     ! установка порогового значения решающего устройства
     SUBROUTINE SetTreshold(this,threshold)
        CLASS(BPSK2PSNDemodulator_t), INTENT(inout) :: this
        INTEGER(8)  , INTENT(IN)                :: threshold
        this%threshold   = threshold
     END SUBROUTINE

       ! установка порогового значения решающего устройства
     SUBROUTINE SetTresholdSumm(this,thresholdSumm)
        CLASS(BPSK2PSNDemodulator_t), INTENT(inout) :: this
        INTEGER(8)  , INTENT(IN)                :: thresholdSumm
        this%thresholdSumm   = thresholdSumm
     END SUBROUTINE SetTresholdSumm

    !Осуществляет : преобразоваине по частоте вниз
    !               разложение на квадратуры
    !               Согласованную фильтрацию знаковую
    ! Возвращает выходной сигнал согласованного фильтра
    FUNCTION Demodulate (this, inputSig) RESULT (twoModules)
        CLASS(BPSK2PSNDemodulator_t), INTENT(inout) :: this
        CLASS(analyticSignal_t) , INTENT(in)    :: inputSig
        CLASS(complexSignal_t)  , ALLOCATABLE   :: twoModules
        CLASS(complexSignal_t)  , ALLOCATABLE   :: phaseDemOutPut
        CLASS(complexSignal_t)  , ALLOCATABLE   :: DemodulatePsn0
        CLASS(complexSignal_t)  , ALLOCATABLE   :: DemodulatePsn1

        CLASS(analyticSignal_t) , ALLOCATABLE    :: diffSig

        INTEGER(2),dimension(:) , ALLOCATABLE   :: module0, module1,diffMod


        ALLOCATE (twoModules)
        ALLOCATE(phaseDemOutPut)
        ALLOCATE(DemodulatePsn0)
        ALLOCATE(DemodulatePsn1)



        ! преобразование по частоте вниз и разложение на квадратуры
        phaseDemOutPut = this%phaseDemodulator%Downconvert(inputSig)
        IF(this%decimationCoeff/=1) phaseDemOutPut = phaseDemOutPut%Decimate(this%decimationCoeff)

      !  CALL WriteComplexSignalToFile(phaseDemOutPut,int(2,1),'test_signals\output\Ipath2PSN.pcm'&
      !                                                       ,'test_signals\output\Qpath2PSN.pcm')

        ! согласованная фильтрация
       ! WRITE (*,*) 'тип обрабоки'

        IF (this%signumCompute) THEN
            ! согласованная фильтрация
            WRITE (*,*) 'свертка знаковая'
            DemodulatePsn0 = phaseDemOutPut.CONVSIGN.this%psn0_signum
            DemodulatePsn1 = phaseDemOutPut.CONVSIGN.this%psn1_signum
        ELSE
           ! WRITE (*,*) 'свертка полноразрядная'
           ! WRITE(*,*) ' this%ethalonCapacity ' ,this%ethalonCapacity
           DemodulatePsn0 = phaseDemOutPut%ClipSignal(int(this%ethalonCapacity,2),int(this%ethalonCapacity,2))
           DemodulatePsn1 = phaseDemOutPut%ClipSignal(int(this%ethalonCapacity,2),int(this%ethalonCapacity,2))

           DemodulatePsn0 = DemodulatePsn0.CONV.this%psn0
           DemodulatePsn1 = DemodulatePsn1.CONV.this%psn1
        END IF

          DEALLOCATE(phaseDemOutPut)


         CALL WriteComplexSignalToFile(DemodulatePsn0,int(2,1),'test_signals\output\Demodulate0PsnI.pcm'&
                                                       , 'test_signals\output\DemodulatePsn0Q.pcm')


        CALL WriteComplexSignalToFile(DemodulatePsn1,int(2,1),'test_signals\output\Demodulate1PsnI.pcm'&
                                                       , 'test_signals\output\DemodulatePsn1Q.pcm')
          module0 = DemodulatePsn0%GetModuleFast()
          module1 = DemodulatePsn1%GetModuleFast()


          ALLOCATE(diffMod,source =module1 )
          diffMod =diffMod-module0

!          ALLOCATE(diffSig)
!          CALL diffSig%Constructor(diffMod)
!          DEALLOCATE(diffMod)
!
!
!          CALL WriteAnalyticSignalToFile(diffSig,int(2,1),'test_signals\output\diffSIG.pcm')

          CALL WriteArrayToFile (module0, 'test_signals\output\last_module0.pcm')
           CALL WriteArrayToFile (module1, 'test_signals\output\last_module1.pcm')


          CALL twoModules%Constructor(module0,module1)


          DEALLOCATE(module0)
          DEALLOCATE(module1)
!          DEALLOCATE(diffSig)
          DEALLOCATE(DemodulatePsn0)
          DEALLOCATE(DemodulatePsn1)
!      CALL WriteComplexSignalToFile(twoModules,int(2,1),'test_signals\output\zeroesModules.pcm'&
!                                                       , 'test_signals\output\onesModule.pcm')
     END FUNCTION Demodulate

     ! Осуществляет пороговую обработку выходного сигнала
     ! согласованного фильтра
     ! Возвращает массив  с принятой информацией
     ! Ождиается что на один импульс приходиться не более 3 отсчетов
     FUNCTION TresholdProcessing(this, matchedFilterOut)
        USE WriteReadAnalyticSignalToFromFile
        CLASS(BPSK2PSNDemodulator_t), INTENT(in)  :: this
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
        LOGICAL                               :: latchEarly0       = .FALSE.
        LOGICAL                               :: latchLate0        = .FALSE.
        LOGICAL                               :: latchEarly1       = .FALSE.
        LOGICAL                               :: latchLate1        = .FALSE.
        LOGICAL                               :: isFindingMessage  = .TRUE.
        INTEGER(8)                            :: timeStampLast     = 0
        INTEGER(8)                            :: timeStampCurr     = 0


        INTEGER(8)                            :: pointAccumulator = 0;
        INTEGER(8)                            :: maxMod=0
        bitBuffer=0




        lasti=0
        CALL matchedFilterOut%ExtractSignalData(realPart,imagePart)
        cnt=0
        isFindingMessage  = .TRUE.
        timeStampCurr=0
        timeStampLast=0
        DO i=1,size(realPart)



             IF ((realPart(i)>=this%threshold).AND.(.NOT.latchEarly0)) THEN
                latchEarly0 = .TRUE.
                timeStampCurr = i
              !  WRITE(*,*) 'поймал начало 0 ',i

             END IF

              IF (imagePart(i)>=this%threshold.AND.(.NOT.latchEarly1)) THEN
                latchEarly1 = .TRUE.
                timeStampCurr = i
              !  WRITE(*,*) 'поймал начало 1 ',i
             END IF

             IF (realPart(i)<=this%threshold.AND.(latchEarly0)) THEN
                latchLate0 = .TRUE.
               !  WRITE(*,*) 'поймал конец 0 ',i
             END IF

             IF (imagePart(i)<=this%threshold.AND.(latchEarly1)) THEN
                latchLate1 = .TRUE.
              ! WRITE(*,*) 'поймал конец 1 ',i
             END IF

             IF(latchEarly0.AND.latchLate0) THEN
!                 WRITE(*,*)' пик 0 обработан'
!                 WRITE(*,*)'diff ', timeStampCurr-timeStampLast
                 latchEarly0 = .FALSE.
                 latchLate0 = .FALSE.
               !  IF((timeStampCurr-timeStampLast)<6000)  WRITE(*,*) '-----ЭРОР!!"!!'


                  IF((timeStampCurr-timeStampLast)<8000) THEN
                   !     WRITE(*,*) '-----ЭРОР!!"!!'
                  ELSE

                    cnt=cnt+1

                 bitBuffer(cnt)=0
                timeStampLast = timeStampCurr




                  END IF




             END IF
             IF(latchEarly1.AND.latchLate1) THEN
!                 WRITE(*,*)' пик 1 обработан'
                 latchEarly1 = .FALSE.
!                 WRITE(*,*)'diff ', timeStampCurr-timeStampLast
                 latchLate1 = .FALSE.

                !  IF((timeStampCurr-timeStampLast)<6000)  WRITE(*,*) '-----ЭРОР!!"!!'


                IF((timeStampCurr-timeStampLast)<8000) THEN
                  !    WRITE(*,*) '-----ЭРОР!!"!!'
                  ELSE

                    cnt=cnt+1

                 bitBuffer(cnt)=1
                timeStampLast = timeStampCurr

                  END IF


             END IF


        END DO
!
   WRITE(*,*) 'ВО ТУТ ПИЗДЕЦ ', cnt
        WRITE(*,*) 'статус  ', ALLOCATED(TresholdProcessing)
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
        CLASS(BPSK2PSNDemodulator_t), INTENT(inout) :: this
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
         CLASS(BPSK2PSNDemodulator_t), INTENT(inout) :: this
         LOGICAL, INTENT(IN)                     :: signumCompute
         this%signumCompute = signumCompute
    END SUBROUTINE SetSignumComputeMode
    SUBROUTINE destructor(this)
        type(BPSK2PSNDemodulator_t), INTENT(IN) :: this
    END SUBROUTINE


END MODULE DPSK2PSnDeMod
