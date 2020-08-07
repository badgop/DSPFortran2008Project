MODULE Bert2PsnMod


    USE ModuleWriteReadArrayFromToFile
    USE ModuleExitProg
   USE DBPSK2PSNmod
    USE OctetDataModule
    USE CRC16Mod
    USE PayloadGeneratorMod
    USE analyticSignalModule
    USE WriteReadAnalyticSignalToFromFile
     USE DPSK2PSnDeMod
    USE AWGNChannelMod
    USE  ModuleExitProg
    USE ReadWriteArrayToFromTxt
    USE ModuleWriteReadArrayFromToFile
    IMPLICIT NONE


     INTERFACE
            REAL(8) FUNCTION omp_get_wtime()
            END FUNCTION
    END INTERFACE

!    As cpu_time adds up the time spent by all the threads, this time will almost
!    certainly increase with number of threads. You would use system_clock or o
!    mp_get_wtime to test the effectiveness of threaded parallelism.
!    https://community.intel.com/t5/Intel-Fortran-Compiler/slow-openMP-matrix-multiplication/td-p/826153

!These two intrinsics report different types of time. system_clock reports "wall time" or
!elapsed time. cpu_time reports time used by the CPU. On a multi-tasking machine these c
!ould be very different, e.g., if your process shared the CPU equally with three other processes
!and therefore received 25% of the CPU and used 10 cpu seconds, it would take about 40 seconds of
! actual elapsed or wall clock time
!https://stackoverflow.com/questions/6878246/fortran-intrinsic-timing-routines-which-is-better-cpu-time-or-system-clock

    INTEGER(4), PARAMETER :: maximumMeasurePoints = 256

    CONTAINS


    SUBROUTINE BERTestSignumCorrelation2PSN (parameterFileName, resultFileName)
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
           CHARACTER(50)            :: psp0FileName
           CHARACTER(50)            :: psp1FileName
           CHARACTER(50)            :: trancieverFilterName
           CHARACTER(50)            :: inputNoiseFileName
           CHARACTER(50)            :: recievFilterName
           REAL(8)                  :: initialPhase
           INTEGER(8)               :: decimationCoeff
           INTEGER(8)               :: outputShiftPhaseDetector
           INTEGER(1)               :: outPutSampleCapacityDetector
           INTEGER(8)               :: demodTreshold
           INTEGER(8)               :: thresholdSumm
           LOGICAL                  :: signumCompute = .TRUE.
           !*** Параметры приемника
           INTEGER(8)               :: sampleRateDeModulator
           INTEGER(1)               :: ethalonCapacity
           LOGICAL                  :: fileExists
           INTEGER(8)               :: iostat_Num
           INTEGER(2),ALLOCATABLE   :: arrayInt2(:)
           INTEGER(8)               :: i,j,cnt
           REAL(4)                  :: snrCurr

           TYPE(BPSK2PSNmodulator_t)  ,ALLOCATABLE    :: modulatorBPSK
           TYPE(analyticSignal_t)                  :: bpskSignal
           TYPE(BPSK2PSNDemodulator_t),ALLOCATABLE    :: DemodulatorBPSK
           TYPE(AWGNChannel_t)    ,ALLOCATABLE    :: awgnChannel
           TYPE(analyticSignal_t) ,aLLOCATABLE    :: noiseSignal
           TYPE(analyticSignal_t) ,aLLOCATABLE    :: bpskSignalWithNoise

           INTEGER(1),ALLOCATABLE      :: psn0(:)
           INTEGER(1),ALLOCATABLE      :: psn1(:)
           INTEGER(1),ALLOCATABLE      :: payloadDataBitArray(:)
           INTEGER(1),ALLOCATABLE      :: payloadDataBitArrayWithCrc(:)
           INTEGER(8),ALLOCATABLE      :: transcieverImpulseResponse(:)
           INTEGER(1),ALLOCATABLE      :: decodedData(:)
           INTEGER(1),ALLOCATABLE      :: decodedDataOctets(:)
           LOGICAL                     :: crcOk = .FALSE.
           REAL(4)                     :: berPointsArray(1:maximumMeasurePoints) = 0.0
           REAL(4)                     :: merValueArray (1:maximumMeasurePoints) = 0.0
           INTEGER(4)                  :: numOfsuccessRecieve = 0
           REAL(8) :: start, finish
           REAL(8) :: startAll, finishAll



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
           READ(10,*)  baudrateModulator
           READ(10,*)  chipRateInSamples
           READ(10,*)  psp0FileName
           READ(10,*)  psp1FileName
           READ(10,*)  trancieverFilterName
           !генератор ШУМА
           READ(10,*)  inputNoiseFileName
           READ(10,*)  outPutSampleCapacityChannel
           ! Демодулятор
           READ(10,*)  sampleRateDeModulator
           READ(10,*)  recievFilterName
           READ(10,*)  initialPhase
           READ(10,*)  outputShiftPhaseDetector
           READ(10,*)  outPutSampleCapacityDetector
           READ(10,*)  decimationCoeff
           READ(10,*)  ethalonCapacity
           READ(10,*)  demodTreshold
           READ(10,*)  thresholdSumm
           READ(10,*)  signumCompute
           CLOSE(10)
           WRITE(*,*) 'параметры модели прочитаны...'

           ! загрузка ПСП
           CALL ReadArrayFromFile (psn0,psp0FileName,'(I1)')
           CALL ReadArrayFromFile (psn1,psp1FileName,'(I1)')
           ! формирование пакета данных без контрольной суммы
           payloadDataBitArray =  GenerateRandomPayloadBitArray(messageLength)
           WRITE(*,*) 'size payloadDataBitArray ',size(payloadDataBitArray)
           ! добавляем контрольную сумму
           payloadDataBitArrayWithCrc = GeneratePayloadDataBitArrayWithCRC(payloadDataBitArray)
            WRITE(*,*) 'size payloadDataBitArrayWithCrc ',size(payloadDataBitArrayWithCrc)

           ! загрузка ИХ фильтра передатчика
           CALL ReadArrayFromFile (transcieverImpulseResponse,trancieverFilterName,'(I12)')

           ALLOCATE(modulatorBPSK)
           CALL modulatorBPSK%Constructor(baudRate              = baudrateModulator&
                                          ,SampleRate           = sampleRateModulator&
                                          ,centralFrequency     = centralFrequency&
                                          ,outPutSampleCapacity = outPutSampleCapacityModulator&
                                          ,psn0                  = psn0&
                                          ,psn1                  = psn1&
                                          ,chipRateInSamples    = chipRateInSamples&
                                          ,impulseResponseArray = transcieverImpulseResponse&
                                          ,outputShift          = outputShiftModulator&
                                          )

           DEALLOCATE(transcieverImpulseResponse)

           CALL  WriteArrayToFileTxt(payloadDataBitArrayWithCrc,'test_signals\output\codedDataBertTest.txt','(I1.1)')

           bpskSignal = modulatorBPSK%Generate(payloadDataBitArrayWithCrc)
           DEALLOCATE(modulatorBPSK)
           ! что бы уменьшить размер файла
           CALL  bpskSignal%ExtractSignalData2(arrayInt2)
           !arrayInt2=1
           CALL  bpskSignal%Constructor(arrayInt2)
           DEALLOCATE(arrayInt2)
!           WRITE(*,*) 'kind ', bpskSignal%GetSiGnalKind()

           CALL bpskSignal%ZeroesStuffing(int(baudrateModulator,8),int(baudrateModulator,8))

          ! CALL WriteAnalyticSignalToFile(bpskSignal,int(2,1),'bpskTest2psn.pcm')


!           CALL ReadAnalyticSignalFromFile(bpskSignal,int(2,1),'noise_0_1_2Mhz_test.pcm')
!            WRITE(*,*) 'уровень шума в генераторе '


           ! загрузка ИХ фильтра приемника
           CALL ReadArrayFromFile (transcieverImpulseResponse,recievFilterName,'(I12)')
           ALLOCATE(DemodulatorBPSK)
           CALL DemodulatorBPSK%Constructor( baudRate                  = baudrateModulator&
                                              ,SampleRate              = sampleRateDeModulator&
                                              ,centralFrequency        = (centralFrequency)&
                                              ,initialPhase            = initialPhase&
                                              ,outPutSampleCapacity    = outPutSampleCapacityDetector &
                                              ,psn0                    = psn0&
                                              ,psn1                    = psn1&
                                              ,chipRateInSamples       = chipRateInSamples&
                                              ,impulseResponseArray    = transcieverImpulseResponse&
                                              ,outPutShift             = outputShiftPhaseDetector&
                                              ,decimationCoeff         = decimationCoeff &
                                              ,ethalonCapacity         = ethalonCapacity )
          DEALLOCATE(transcieverImpulseResponse)

          CALL  DemodulatorBPSK%SetSignumComputeMode(signumCompute)
          CALL  DemodulatorBPSK%SetTreshold(demodTreshold)
          WRITE(*,*) 'thresholdSumm ' ,thresholdSumm
          CALL  DemodulatorBPSK%SetTresholdSumm(thresholdSumm)



          ! настройка генератора АБГШ
          ALLOCATE(awgnChannel)
          ALLOCATE(noiseSignal)
          CALL ReadAnalyticSignalFromFile(noiseSignal,int(2,1),inputNoiseFileName)
          CALL awgnChannel%LoadNoiseInt2(noiseSignal)
          DEALLOCATE(noiseSignal)
          WRITE(*,*) 'уровень шума в генераторе ',awgnChannel%GetPowerNoise()


!          deCodedData = DemodulatorBPSK%GetData(bpskSignal)
!          decodedDataOctets = BitsToOctets(deCodedData, .TRUE.)
!          IF (CheckCRC(decodedDataOctets))  WRITE(*,*) 'CRC OK'
          snrCurr = snrStart
!          ALLOCATE(bpskSignalWithNoise)
!          WRITE(*,*) 'outPutSampleCapacityChannel ',outPutSampleCapacityChannel
!          bpskSignalWithNoise = awgnChannel%AddNoiseAnalytic(bpskSignal,snrCurr,outPutSampleCapacityChannel)
!          CALL WriteAnalyticSignalToFile(bpskSignalWithNoise,int(2,1),'bpskTestNoise.pcm')
!          DEALLOCATE(bpskSignalWithNoise)

          CALL RanomGeneratorInit()
          WRITE(*,*) 'SNR CURR ',snrCurr
          cnt=1
          startAll = omp_get_wtime()
          DO WHILE(snrCurr<snrEnd)

!          !$omp parallel
!            !$omp  do PRIVATE(crcOk,numOfsuccessRecieve)
             DO j = 1,numberOfIterations
                start=omp_get_wtime()
                  crcOk = AddNoiseRecievCheckCRC(bpskSignal     = bpskSignal &
                                       ,DemodulatorBPSK         = DemodulatorBPSK&
                                       ,awgnChannel             = awgnChannel&
                                       ,snr                     = snrCurr&
                                       ,capacity                = outPutSampleCapacityChannel)
!                 !$omp critical

                  IF ((crcOk)) numOfsuccessRecieve = numOfsuccessRecieve +1
                  WRITE(*,*) 'crc ',crcOk

!                  IF (.NOT.crcOk) THEN
!                     WRITE(*,*) 'ERRROR CRC  ',crcOk
!                     CALL ExitFromProgramNormal()
!
!                  END IF


!                  !$omp end critical

                   finish=omp_get_wtime()
             WRITE(*,*)' j= ', j, ' time = ', (finish-start)
             END DO
!             !$omp end  do
!             !$omp END parallel

             berPointsArray(cnt)= snrCurr
             merValueArray(cnt)=   float(numberOfIterations-numOfsuccessRecieve)/float(numberOfIterations)
             numOfsuccessRecieve = 0

             WRITE (*,*) 'SNR ', berPointsArray(cnt), ' MER ',merValueArray(cnt)
             cnt = cnt + 1
             snrCurr = snrCurr + snrStep
          END DO

            finishAll = omp_get_wtime()


            OPEN(11, FILE = resultFileName, ACCESS="STREAM",ACTION= "WRITE", FORM="FORMATTED", IOSTAT=iostat_Num)

            WRITE(*,*)'all time = ', (finishAll-startAll)/numberOfIterations

            WRITE(11,*)  'SNR           MER '
            DO i=1,cnt-1
            WRITE(11,'(F7.3,F7.3)')  berPointsArray(i), merValueArray(i)
            END DO

            CLOSE(11)


         CONTAINS

        SUBROUTINE BerTestSignumCorrelationInt(snrStart,snrEnd,snrStep, iterationNumber)
         REAL(4)   ,INTENT(IN) :: snrStart
         REAL(4)   ,INTENT(IN) :: snrEnd
         REAL(4)   ,INTENT(IN) :: snrStep
         INTEGER(2),INTENT(IN) :: iterationNumber

        END SUBROUTINE BerTestSignumCorrelationInt

    END SUBROUTINE BERTestSignumCorrelation2PSN

    FUNCTION AddNoiseRecievCheckCRC(bpskSignal,DemodulatorBPSK,awgnChannel,snr,capacity) RESULT (isCrcOk)
        CLASS(analyticSignal_t)  , INTENT(IN) :: bpskSignal
        CLASS(BPSK2PSNDemodulator_t) , INTENT(INOUT) :: DemodulatorBPSK
        CLASS(AWGNChannel_t)     , INTENT(INOUT) :: awgnChannel
        REAL(4)                  , INTENT(IN) :: snr
        INTEGER(1)               , INTENT(IN) :: capacity
        LOGICAL                               :: isCrcOk
        INTEGER(8)                            :: z
        TYPE(analyticSignal_t)                :: bpskSignalWithNoise
        INTEGER(1),ALLOCATABLE      :: decodedData(:)
        INTEGER(1),ALLOCATABLE      :: decodedDataOctets(:)


        !by default
        isCrcOk = .FALSE.
        z= GetRandomInt(int(4,1))
        CALL awgnChannel%SetPtr(z)

        bpskSignalWithNoise = awgnChannel%AddNoiseAnalytic(bpskSignal,snr,capacity)
        !CALL WriteAnalyticSignalToFile(bpskSignalWithNoise,int(2,1),'bpskSignalWithNoise.pcm')
        deCodedData = DemodulatorBPSK%GetData(bpskSignalWithNoise)
        CALL  WriteArrayToFileTxt(deCodedData,'test_signals\output\deCodedDataBertTest.txt','(I1.1)')
        WRITE(*,*) 'принято бит ', size(deCodedData)
        decodedDataOctets = BitsToOctets(deCodedData, .TRUE.)
        isCrcOk = CheckCRC(decodedDataOctets)
    END FUNCTION AddNoiseRecievCheckCRC

END MODULE Bert2PsnMod
