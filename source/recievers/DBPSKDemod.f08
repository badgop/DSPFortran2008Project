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
        INTEGER(8)                   :: decimationCoeff

        TYPE(PSNSimple_t)            :: psnGnerator
        TYPE(PhaseDetector_t)        :: phaseDemodulator
        TYPE(DiffCodeGenerator_t)    :: deCoder
        TYPE(signumSignal_t)         :: currentPRSSignal
    CONTAINS
        PROCEDURE :: Constructor
        ! преобразование по частоте вниз
        ! разложение на квадратуры
        ! Вычисление ВКФ
        PROCEDURE :: Demodulate
        PROCEDURE :: SetTreshold
        ! Пороговая обработка сигнала после согласованного фильтра
        PROCEDURE :: TresholdProcessing
        PROCEDURE :: GetData
        FINAL     :: destructor
    END TYPE BPSKDemodulator_t

CONTAINS

    SUBROUTINE Constructor(this,baudRate,sampleRate,centralFrequency,initialPhase&
                               ,outPutSampleCapacity,psn,chipRateInSamples&
                               ,impulseResponseArray,outputShift,decimationCoeff)
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
        INTEGER(1)  , ALLOCATABLE          :: psnSignalArray(:)
        this%baudRateInSamples              = baudRate
        this%centralFrequency               = centralFrequency
        this%initialPhase                   = initialPhase
        this%SampleRate                     = SampleRate
        this%outPutSampleCapacity           = outPutSampleCapacity
        this%outputShift                    = outputShift
        this%decimationCoeff                = decimationCoeff
        CALL this%psnGnerator%Constructor (psn, chipRateInSamples/this%decimationCoeff)
        CALL this%phaseDemodulator%Constructor(this%centralFrequency&
                                        ,this%initialPhase&
                                        ,this%sampleRate&
                                        ,impulseResponseArray&
                                        ,int(outputShift,8))
       psnSignalArray=this%psnGnerator%OutPutPsnArray(int(1,8))
       CALL this%currentPRSSignal%Constructor(psnSignalArray)
       DEALLOCATE(psnSignalArray)
     END SUBROUTINE

     ! установка порогового значения решающего устройства
     SUBROUTINE SetTreshold(this,threshold)
        CLASS(BPSKDemodulator_t), INTENT(inout) :: this
        INTEGER(8)  , INTENT(IN)                :: threshold
        this%threshold   = threshold
     END SUBROUTINE
    
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

        !CALL WriteComplexSignalToFile(Demodulate,int(2,1),'test_signals\output\Icorr.pcm','test_signals\output\Qcorr.pcm')
        ! согласованная фильтрация
        Demodulate = Demodulate.CONVSIGN.this%currentPRSSignal

     END FUNCTION Demodulate

     ! Осуществляет пороговую обработку выходного сигнала
     ! согласованного фильтра
     ! Возвращает массив  с принятой информацией
     ! Ождиается что на один импульс приходиться не более 3 отсчетов
     FUNCTION TresholdProcessing(this, matchedFilterOut)
        USE WriteReadAnalyticSignalToFromFile
        CLASS(BPSKDemodulator_t), INTENT(in)  :: this
        CLASS(complexSignal_t)  , INTENT(inout)  :: matchedFilterOut
        INTEGER(8)              , ALLOCATABLE :: TresholdProcessing(:)
        INTEGER(8)              , ALLOCATABLE :: module(:)
        !!!!!!!!!!!!!!!!!!!
        INTEGER(2)              , ALLOCATABLE :: module2(:)
        INTEGER(8), ALLOCATABLE               :: realPart(:)
        INTEGER(8), ALLOCATABLE               :: imagePart(:)
        INTEGER(8)                            :: i
        INTEGER(1)                            :: bitBuffer(1:32767)
        INTEGER(8)                            :: cnt
        bitBuffer=0
        module = matchedFilterOut%GetModuleFast()
!        ALLOCATE(module2(1:size(module)))
!        module2 =  module
!        CALL WriteArrayToFile (module2, 'test_signals\output\last_module.pcm')



        CALL matchedFilterOut%ExtractSignalData(realPart,imagePart)
        cnt=0
!        WRITE(*,*) 'size module ', size(module)
        DO i=1,size(module)
           IF (module(i)>=this%threshold) THEN
!               WRITE(*,*) 'БОЛЬШЕ i',i , (realPart(i)) , (imagePart(i))
               !Обработка созвездия ведется с учетом базиса [COS, -SIN]
               IF((realPart(i)>0).AND.(imagePart(i)<0)) THEN
                   cnt=cnt+1
                   bitBuffer(cnt)=1
!                   WRITE(*,*) 'принята 1 ', i , module(i), (realPart(i)) , (imagePart(i))
               END IF
               IF((realPart(i)<0).AND.(imagePart(i)>0)) THEN
                   cnt=cnt+1
                   bitBuffer(cnt)=0
!                   WRITE(*,*) 'принята 0 ', i ,module(i), (realPart(i)) , (imagePart(i))
               END IF
!
                IF((realPart(i)<0).AND.(imagePart(i)<0)) THEN
                   cnt=cnt+1
                   bitBuffer(cnt)=0
!                   WRITE(*,*) 'НЕСТАНДАРТНО принята 0 ', i ,module(i), (realPart(i)) , (imagePart(i))
               END IF

               IF((realPart(i)>0).AND.(imagePart(i)>0)) THEN
                   cnt=cnt+1
                   bitBuffer(cnt)=1
!                   WRITE(*,*) 'НЕСТАНДАРТНО принята 1 ', i ,module(i), (realPart(i)) , (imagePart(i))
               END IF

           END IF

        END DO
!        WRITE(*,*) 'ВО ТУТ ПИЗДЕЦ ', cnt
       ! WRITE(*,*) 'статус  ', ALLOCATED(TresholdProcessing)
        IF (cnt == 0) THEN
             WRITE(*,*) 'ничего не принято'
             cnt=1
        END IF
        ALLOCATE(TresholdProcessing(1:cnt))
        TresholdProcessing = bitBuffer(1:cnt)

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

    SUBROUTINE destructor(this)
        type(BPSKDemodulator_t), INTENT(IN) :: this
    END SUBROUTINE

END MODULE DBPSKDemod
