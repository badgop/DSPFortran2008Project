MODULE BERTestMod
    USE ModuleWriteReadArrayFromToFile
    USE ModuleExitProg
    USE BPSKmod
    USE OctetDataModule
    USE CRC16Mod
    USE PayloadGeneratorMod
    USE analyticSignalModule
    USE WriteReadAnalyticSignalToFromFile
    USE DBPSKDemod
    IMPLICIT NONE

    CONTAINS


    SUBROUTINE BERTestSignumCorrelation (parameterFileName, resultFileName)
           CHARACTER(*), INTENT(IN) :: parameterFileName
           CHARACTER(*), INTENT(IN) :: resultFileName
           !***********************************
           REAL(4)                  :: snrStart
           REAL(4)                  :: snrEnd
           REAL(4)                  :: snrStep
           INTEGER(2)               :: numberOfIterations
           INTEGER(4)               :: messageLength
           !*** Параметры передатчика
           INTEGER(8)               :: sampleRateModulator
           INTEGER(8)               :: baudRateModulator
           INTEGER(8)               :: centralFrequency
           INTEGER(1)               :: outPutSampleCapacityModulator
           INTEGER(1)               :: outputShiftModulator
           INTEGER(1)               :: outPutSampleCapacityChannel
           INTEGER(8)               :: chipRateInSamples
           CHARACTER(50)            :: pspFileName
           CHARACTER(50)            :: trancieverFilterName
           CHARACTER(50)            :: inputNoiseFileName
           CHARACTER(50)            :: recievFilterName
           REAL(8)                  :: initialPhase
           INTEGER(8)               :: decimationCoeff
           INTEGER(8)               :: outputShiftPhaseDetector
           INTEGER(1)               :: outPutSampleCapacityDetector
           !*** Параметры приемника
           INTEGER(8)               :: sampleRateDeModulator
           LOGICAL                  :: fileExists
           INTEGER(8)               :: iostat_Num

           TYPE(BPSKmodulator_t)    :: modulatorBPSK
           TYPE(analyticSignal_t)   :: bpskSignal
           TYPE(BPSKDemodulator_t)  :: DemodulatorBPSK

           INTEGER(1),ALLOCATABLE      :: psn(:)
           INTEGER(1),ALLOCATABLE      :: payloadDataBitArray(:)
           INTEGER(1),ALLOCATABLE      :: payloadDataBitArrayWithCrc(:)

           INTEGER(8),ALLOCATABLE      :: transcieverImpulseResponse(:)



           fileExists = IsFileExists(parameterFileName)
           IF (fileExists .EQV. .FALSE.) THEN
                WRITE(*,*) 'Не существует файл ',  parameterFileName
                CALL   ExitFromProgramNormal()
           END IF
           fileExists = IsFileExists(parameterFileName)
           IF (fileExists.EQV. .FALSE.) THEN
                WRITE(*,*) 'Не существует файл ',  resultFileName
                CALL   ExitFromProgramNormal()
           END IF

           OPEN(10, FILE = parameterFileName, ACCESS="STREAM",ACTION= "READ", FORM="FORMATTED", IOSTAT=iostat_Num)
           READ(10,*)  snrStart
           READ(10,*)  snrEnd
           READ(10,*)  snrStep
           READ(10,*)  numberOfIterations
           READ(10,*)  messageLength
           ! параметры модулятора
           READ(10,*)  sampleRateModulator
           READ(10,*)  centralFrequency
           READ(10,*)  outPutSampleCapacityModulator
           READ(10,*)  outputShiftModulator
           READ(10,*)  outPutSampleCapacityChannel
           READ(10,*)  baudrateModulator
           READ(10,*)  chipRateInSamples
           READ(10,*)  pspFileName
           READ(10,*)  trancieverFilterName
           READ(10,*)  inputNoiseFileName
           READ(10,*)  sampleRateDeModulator
           READ(10,*)  recievFilterName
           READ(10,*)  initialPhase
           READ(10,*)  outputShiftPhaseDetector
           READ(10,*)  outPutSampleCapacityDetector
           READ(10,*)  decimationCoeff
           CLOSE(10)
           WRITE(*,*) 'параметры модели прочитаны...'

           ! загрузка ПСП
           CALL ReadArrayFromFile (psn,pspFileName,'(I1)')
           ! формирование пакета данных без контрольной суммы
           payloadDataBitArray =  GenerateRandomPayloadBitArray(messageLength)
           ! добавляем контрольную сумму
           payloadDataBitArrayWithCrc = GeneratePayloadDataBitArrayWithCRC(payloadDataBitArray)
           ! загрузка ИХ фильтра передатчика
           CALL ReadArrayFromFile (transcieverImpulseResponse,trancieverFilterName,'(I12)')

           CALL modulatorBPSK%Constructor(baudRate              = baudrateModulator&
                                          ,SampleRate           = sampleRateModulator&
                                          ,centralFrequency     = centralFrequency&
                                          ,outPutSampleCapacity = outPutSampleCapacityModulator&
                                          ,psn                  = payloadDataBitArrayWithCrc&
                                          ,chipRateInSamples    = chipRateInSamples&
                                          ,impulseResponseArray = transcieverImpulseResponse&
                                          ,outputShift          = outputShiftModulator&
                                          )

           DEALLOCATE(transcieverImpulseResponse)

           bpskSignal = modulatorBPSK%Generate(payloadDataBitArrayWithCrc)

           CALL WriteAnalyticSignalToFile(bpskSignal,int(2,1),'bpskTest.pcm')


           ! загрузка ИХ фильтра приемника
           CALL ReadArrayFromFile (transcieverImpulseResponse,recievFilterName,'(I12)')
           CALL DemodulatorBPSK%Constructor( baudRate                  = baudrateModulator&
                                              ,SampleRate              = sampleRateDeModulator&
                                              ,centralFrequency        = centralFrequency&
                                              ,initialPhase            = initialPhase&
                                              ,outPutSampleCapacity    = outPutSampleCapacityDetector &
                                              ,psn                     = psn&
                                              ,chipRateInSamples       = chipRateInSamples&
                                              ,impulseResponseArray    = transcieverImpulseResponse&
                                              ,outPutShift             = outputShiftPhaseDetector&
                                              ,decimationCoeff          = decimationCoeff)

          DEALLOCATE(transcieverImpulseResponse)


            CONTAINS

        SUBROUTINE BerTestSignumCorrelationInt(snrStart,snrEnd,snrStep, iterationNumber)
         REAL(4)   ,INTENT(IN) :: snrStart
         REAL(4)   ,INTENT(IN) :: snrEnd
         REAL(4)   ,INTENT(IN) :: snrStep
         INTEGER(2),INTENT(IN) :: iterationNumber

        END SUBROUTINE BerTestSignumCorrelationInt




    END SUBROUTINE BERTestSignumCorrelation




END MODULE BERTestMod
