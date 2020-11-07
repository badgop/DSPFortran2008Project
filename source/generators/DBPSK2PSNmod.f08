MODULE DBPSK2PSNmod

    USE PSNSimpleMod
    USE impulseGeneratorModule
    USE complexSignalModule
    USE DiffDataModulatorMod
    USE analyticSignalModule
    USE DDSModule
    USE MathConstModule
    USE ImpulseGeneratorModuleOOP

    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: BPSK2PSNmodulator_t
        PRIVATE
        INTEGER(8)                   :: baudRateInSamples
        INTEGER(8)                   :: SampleRate
        INTEGER(8)                   :: chipRateInSamples
        INTEGER(8)                   :: centralFrequency
        INTEGER(1)                   :: outPutSampleCapacity
        INTEGER(1)                   :: outputShift
        INTEGER(1),ALLOCATABLE       :: psn0(:)
        INTEGER(1),ALLOCATABLE       :: psn1(:)
        TYPE(analyticSignal_t)       :: impluseResponse
        TYPE(impulseGeretator_t)     :: psnGen0
        TYPE(impulseGeretator_t)     :: psnGen1
        TYPE(impulseGeretator_t)     :: dataSignalGen
        TYPE(DiffCodeGenerator_t)    :: coder

        TYPE(DDS_t)                  :: mixer
    CONTAINS
        PROCEDURE :: Constructor
        PROCEDURE :: Generate
        PROCEDURE :: GenerateDiffData
        FINAL     :: destructor
    END TYPE BPSK2PSNmodulator_t

CONTAINS

    SUBROUTINE Constructor(this,baudRate,SampleRate,centralFrequency,outPutSampleCapacity,psn0,psn1,chipRateInSamples&
                          , impulseResponseArray,outputShift)
        class(BPSK2PSNmodulator_t), intent(inout) :: this
        INTEGER(8)  , intent(in)           :: baudRate
        INTEGER(8)  , intent(in)           :: SampleRate
        INTEGER(8)  , intent(in)           :: centralFrequency
        INTEGER(1)  , intent(in)           :: outPutSampleCapacity
        INTEGER(1)  , intent(in)           :: psn0(:)
        INTEGER(1)  , intent(in)           :: psn1(:)
        INTEGER(8)  , intent(in)           :: chipRateInSamples
        INTEGER(8)  , intent(in)           :: impulseResponseArray(:)
        INTEGER(1)  , intent(in)           :: outputShift

        this%baudRateInSamples              = baudRate
        this%centralFrequency               = centralFrequency
        this%SampleRate                     = SampleRate
        this%outPutSampleCapacity           = outPutSampleCapacity
        this%outputShift                    = outputShift
        this%chipRateInSamples              = chipRateInSamples

        ! начальная фаза сигнала!!!!
        CALL this%coder%Constructor(int(1,8))



        ALLOCATE(this%psn0,source = psn0)
        ALLOCATE(this%psn1,source = psn1)

        CALL this%psnGen0%SetOverSampleRate(int(this%chipRateInSamples,2))
        CALL this%psnGen1%SetOverSampleRate(int(this%chipRateInSamples,2))
        CALL this%dataSignalGen%SetOverSampleRate(int(this%baudRateInSamples,2))

        CALL this%impluseResponse%Constructor(impulseResponseArray)

    END SUBROUTINE
    

    FUNCTION GenerateDiffData (this, data)
        CLASS(BPSK2PSNmodulator_t), intent(inout) :: this
        INTEGER(1)  , intent(in)              :: data(:)
        INTEGER(1)  , allocatable             :: GenerateDiffData(:)

        GenerateDiffData = this%coder%GenerateDBPSKData(data)

    END FUNCTION GenerateDiffData

    FUNCTION Generate (this, data)
        CLASS(BPSK2PSNmodulator_t), intent(inout) :: this
        INTEGER(1)  , intent(in)              :: data(:)
        INTEGER(1)  , allocatable             :: Diffdata(:)
        CLASS(analyticSignal_t) , allocatable :: OutPutModulationSig
        INTEGER(1)              , allocatable :: outputDataSig(:)
        INTEGER(8)                            :: stat
        CLASS(analyticSignal_t), allocatable  :: Generate
        !CLASS(analyticSignal_t), allocatable  :: heterodyneSignal
        INTEGER(8)                            :: outPutSignalLength
        INTEGER(8)                            :: i,j1,j2,k,cnt1,cnt2



        ALLOCATE(Generate)

      !  ALLOCATE(heterodyneSignal)


        ALLOCATE(OutPutModulationSig)


        diffData = this%GenerateDiffData(data)

        outPutSignalLength = size(diffData)*this%baudRateInSamples
        ALLOCATE(outputDataSig(1:outPutSignalLength))

        cnt1=1
        j1=1
        j2=1
        WRITE(*,*) 'length data ', size(diffData)
        DO i=1,outPutSignalLength

          IF (j1==size(this%psn0)) j1 = 1
          IF (j2==size(this%psn1)) j2 = 1

          IF(this%dataSignalGen%GetReadyForData())  THEN
             CALL this%dataSignalGen%PutData(diffData(cnt1))
             cnt1 = cnt1+1
          END IF

          IF (this%psnGen0%GetReadyForData()) THEN
              CALL this%psnGen0%PutData(this%psn0(j1))

              j1=j1+1
          END IF

           IF  (this%psnGen1%GetReadyForData()) THEN
              CALL this%psnGen1%PutData(this%psn1(j2))

              j2=j2+1
          END IF




          IF(this%dataSignalGen%GetOutputSample()>0) THEN
                  outputDataSig(i)=this%psnGen1%GetOutputSample()
                  !outputDataSig(i) = 0
          ELSE
                  outputDataSig(i)=this%psnGen0%GetOutputSample()
                  !outputDataSig(i)=0
          END IF



          !WRITE(*,*) i,j2,j1,cnt1

        END DO



        CALL OutPutModulationSig%Constructor(outputDataSig)

        CALL OutPutModulationSig%ZeroesStuffing(this%baudRateInSamples,this%baudRateInSamples)

        OutPutModulationSig = OutPutModulationSig.CONV.this%impluseResponse

          WRITE(*,*)' уровень OutPutModulationSig' ,OutPutModulationSig%GetSignalKind() ,OutPutModulationSig%GetMax()


        Generate = OutPutModulationSig

          WRITE(*,*)' пищлец this%outputShift',this%outputShift
        WRITE(*,*)' уровень' ,Generate%GetSignalKind() ,Generate%GetMax()
        DeALLOCATE(OutPutModulationSig)




        CALL Generate%RShift(this%outputShift)








!        outputDataSig = GenerateImpluseSequence(this%baudRateInSamples,diffData)
!        CALL OutPutModulationSig%Constructor(outputDataSig)
!        OutPutPsn = this%psnGnerator%OutPutPsnAs (int(size(diffData),8))
!        Generate =  OutPutPsn * OutPutModulationSig
!        CALL Generate%ZeroesStuffing(this%baudRateInSamples,this%baudRateInSamples)
!        Generate=Generate.CONV.this%impluseResponse
!        stat = this%mixer%Constructor (romLengthInBits = int(32,1)&
!                                     , romLengthTruncedInBits =int(16,1) &
!                                     , samplingFrequency =int(this%SampleRate,4) &
!                                     , outputSignalSampleCapacity=int(16,1))
!
!        CALL this%mixer%SetPhase(PI/2)
!        CALL this%mixer%ComputeOutput(int(this%centralFrequency,8),(Generate%GetSignalSize()),heterodyneSignal)
!        Generate = Generate*heterodyneSignal
!        CALL Generate%RShift(this%outputShift)
!
!        DEALLOCATE(OutPutPsn)
!        DeALLOCATE(OutPutModulationSig)
!        DEALLOCATE(heterodyneSignal)

    END FUNCTION Generate



    SUBROUTINE destructor(this)
        type(BPSK2PSNmodulator_t), intent(in) :: this
    END SUBROUTINE

END MODULE DBPSK2PSNmod
