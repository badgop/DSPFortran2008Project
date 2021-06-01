module GFSKVadimmod
    USE ModuleWriteReadArrayFromToFile
    USE ModuleExitProg
    USE GFSKmod
    USE OctetDataModule
    USE CRC16Mod
    USE RandomMod
    USE PSNMakerMod
    USE PayloadGeneratorMod
    USE analyticSignalModule
    USE WriteReadAnalyticSignalToFromFile
    USE GFSKSimpleReciever
    USE HDLCFrameMakerModule

    USE AWGNChannelMod
    USE  ModuleExitProg
    USE ReadWriteArrayToFromTxt

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


    SUBROUTINE BERTestGFSKVadim (parameterFileName, resultFileName)
          CHARACTER(*), INTENT(IN)                   :: parameterFileName
          CHARACTER(*), INTENT(IN)                   :: resultFileName
          !**Параметры модели*****
          !Общие параметры
          REAL(4)                                    :: snrStart
          REAL(4)                                    :: snrEnd
          REAL(4)                                    :: snrStep
          INTEGER(2)                                 :: numberOfIterations
          INTEGER(4)                                 :: messageLength
          INTEGER(2)                                 :: preambuleLength
          !Параметры передатчика
          integer(4)                                 :: sampleRate
          real(4)                                    :: bt
          INTEGER(8)                                 :: baudRate
          REAL(8)                                    :: mIndex
          INTEGER(4)                                 :: centralFrequency
          integer(1)                                 :: fir_order
          integer(1)                                 :: capacityFilter
          integer(1)                                 :: outPutDDSCapacity
          INTEGER(1)                                 :: outputFilterShiftGauss
          integer(1)                                 :: romLengthTruncedInBits
          !Параметры канала
          INTEGER(1)                                 :: outPutSampleCapacityChannel
          !Параметры приемника
          CHARACTER(100)                             :: impulseResponseFileName
          INTEGER(4), ALLOCATABLE , DIMENSION(:)     :: impulseResponse
          INTEGER(1)                                 :: outPutFilterShiftReciever
          INTEGER(8)                                 :: decimationRate
          INTEGER(1)                                 :: outPutFreqShift
          !Входные файлы
          CHARACTER(100)                             :: inputNoiseFileName
          CHARACTER(100)                             :: inputPreambuleFileName
          !Выходные файлы
          CHARACTER(100)                             :: inputGFSKSignalFileName
          CHARACTER(100)                             :: inputFreqDetectFileName
          CHARACTER(100)                             :: dataFileName
          CHARACTER(100)                             :: dataWithCRCFileName
          !Переменные функции
          LOGICAL                                    :: fileExists
          INTEGER(8)                                 :: iostat_Num
          INTEGER(2), DIMENSION(:) ,ALLOCATABLE      :: arrayInt2
          INTEGER(8)                                 :: i,j,cnt
          REAL(4)                                    :: snrCurr


          TYPE(GFSKmodulator_t)  ,ALLOCATABLE        :: modulatorGFSK
          TYPE(analyticSignal_t)                     :: gfskSignal
          TYPE(GFSKSimpleDemodulator),ALLOCATABLE    :: DemodulatorGFSK
          TYPE(AWGNChannel_t)    ,ALLOCATABLE        :: awgnChannel
          TYPE(analyticSignal_t) ,aLLOCATABLE        :: noiseSignal
          TYPE(analyticSignal_t) ,aLLOCATABLE        :: gfskSignalWithNoise

          INTEGER(1), DIMENSION(:), ALLOCATABLE      :: preambule
          INTEGER(1), DIMENSION(:), ALLOCATABLE      :: dataArray
          INTEGER(1), DIMENSION(:), ALLOCATABLE      ::dataArrayHDLC


          INTEGER(1),ALLOCATABLE      :: payloadDataBitArrayWithCrc(:)
          INTEGER(8),ALLOCATABLE      :: RecieverImpulseResponse(:)
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

            !***** Чтение параметров модели
            OPEN(10, FILE = parameterFileName, ACCESS="STREAM",ACTION= "READ", FORM="FORMATTED", IOSTAT=iostat_Num)
            !Общие параметры
            READ(10,*)  snrStart
            READ(10,*)  snrEnd
            READ(10,*)  snrStep
            READ(10,*)  numberOfIterations
            READ(10,*)  messageLength
             READ(10,*) preambuleLength
            !передатчик
            READ(10,*)  sampleRate
            READ(10,*)  bt
            READ(10,*)  baudRate
            READ(10,*)  mIndex
            READ(10,*)  fir_order
            READ(10,*)  capacityFilter
            READ(10,*)  outputFilterShiftGauss
            READ(10,*)  romLengthTruncedInBits
            READ(10,*)  outPutDDSCapacity
            READ(10,*)  centralFrequency
            !имитатор канала
            READ(10,*)  outPutSampleCapacityChannel
            ! приемник
            READ(10,*)  impulseResponseFileName
            READ(10,*)  outPutFilterShiftReciever
            READ(10,*)  decimationRate
            READ(10,*)  outPutFreqShift
            !Входные файлы
            READ(10,*)  inputNoiseFileName
            READ(10,*)  inputPreambuleFileName
            !Выходные файлы
            READ(10,*)  inputGFSKSignalFileName
            READ(10,*)  inputFreqDetectFileName
            READ(10,*)  dataFileName
            READ(10,*)  dataWithCRCFileName
            WRITE(*,*) 'параметры модели прочитаны...'


            !********** подготовка преамбулы и кадра данных
            CALL RanomGeneratorInit()
            preambule     = MakePSN(preambuleLength)
            dataArray     = GenerateRandomPayloadBitArray(int(messageLength,4))
            dataArrayHDLC = MakeFrameHDLC(dataArray,preambule )
            CALL WriteArrayToFileTxt(int( dataArrayHDLC,8), dataWithCRCFileName,'(I1.1)' )
            CALL WriteArrayToFileTxt(int( dataArray,8)    , dataFileName       ,'(I1.1)' )
            DEALLOCATE (dataArray)
            !*******

           ! загрузка ИХ фильтра передатчика
           CALL ReadArrayFromFile (impulseResponse,impulseResponseFileName,'(I12)')


!
!           ALLOCATE(modulatorBPSK)
!           CALL modulatorBPSK%Constructor(baudRate              = baudrateModulator&
!                                          ,SampleRate           = sampleRateModulator&
!                                          ,centralFrequency     = centralFrequency&
!                                          ,outPutSampleCapacity = outPutSampleCapacityModulator&
!                                          ,psn                  = psn&
!                                          ,chipRateInSamples    = chipRateInSamples&
!                                          ,impulseResponseArray = transcieverImpulseResponse&
!                                          ,outputShift          = outputShiftModulator&
!                                          )
!
!           DEALLOCATE(transcieverImpulseResponse)
!
!           CALL  WriteArrayToFileTxt(payloadDataBitArrayWithCrc,'test_signals\output\codedDataBertTest.txt','(I1.1)')
!
!           bpskSignal = modulatorBPSK%Generate(payloadDataBitArrayWithCrc)
!           DEALLOCATE(modulatorBPSK)
!           ! что бы уменьшить размер файла
!           CALL  bpskSignal%ExtractSignalData2(arrayInt2)
!           !arrayInt2=1
!           CALL  bpskSignal%Constructor(arrayInt2)
!           DEALLOCATE(arrayInt2)
!!           WRITE(*,*) 'kind ', bpskSignal%GetSiGnalKind()
!
!           CALL bpskSignal%ZeroesStuffing(int(0,8),int(0,8))
!
!           CALL WriteAnalyticSignalToFile(bpskSignal,int(2,1),'bpskTest.pcm')
!
!
!!           CALL ReadAnalyticSignalFromFile(bpskSignal,int(2,1),'noise_0_1_2Mhz_test.pcm')
!!            WRITE(*,*) 'уровень шума в генераторе '
!
!
!           ! загрузка ИХ фильтра приемника
!           CALL ReadArrayFromFile (transcieverImpulseResponse,recievFilterName,'(I12)')
!           ALLOCATE(DemodulatorBPSK)
!           CALL DemodulatorBPSK%Constructor( baudRate                  = baudrateModulator&
!                                              ,SampleRate              = sampleRateDeModulator&
!                                              ,centralFrequency        = (centralFrequency)&
!                                              ,initialPhase            = initialPhase&
!                                              ,outPutSampleCapacity    = outPutSampleCapacityDetector &
!                                              ,psn                     = psn&
!                                              ,chipRateInSamples       = chipRateInSamples&
!                                              ,impulseResponseArray    = transcieverImpulseResponse&
!                                              ,outPutShift             = outputShiftPhaseDetector&
!                                              ,decimationCoeff         = decimationCoeff &
!                                              ,ethalonCapacity         = ethalonCapacity )
!          DEALLOCATE(transcieverImpulseResponse)
!
!          CALL  DemodulatorBPSK%SetSignumComputeMode(signumCompute)
!          CALL  DemodulatorBPSK%SetTreshold(demodTreshold)
!          WRITE(*,*) 'thresholdSumm ' ,thresholdSumm
!          CALL  DemodulatorBPSK%SetTresholdSumm(thresholdSumm)
!
!
!
!          ! настройка генератора АБГШ
!          ALLOCATE(awgnChannel)
!          ALLOCATE(noiseSignal)
!          CALL ReadAnalyticSignalFromFile(noiseSignal,int(2,1),inputNoiseFileName)
!          CALL awgnChannel%LoadNoiseInt2(noiseSignal)
!          DEALLOCATE(noiseSignal)
!          WRITE(*,*) 'уровень шума в генераторе ',awgnChannel%GetPowerNoise()
!
!
!!          deCodedData = DemodulatorBPSK%GetData(bpskSignal)
!!          decodedDataOctets = BitsToOctets(deCodedData, .TRUE.)
!!          IF (CheckCRC(decodedDataOctets))  WRITE(*,*) 'CRC OK'
!          snrCurr = snrStart
!          ALLOCATE(bpskSignalWithNoise)
!          WRITE(*,*) 'outPutSampleCapacityChannel ',outPutSampleCapacityChannel
!          bpskSignalWithNoise = awgnChannel%AddNoiseAnalytic(bpskSignal,snrCurr,outPutSampleCapacityChannel)
!          CALL WriteAnalyticSignalToFile(bpskSignalWithNoise,int(2,1),'bpskTestNoise.pcm')
!          DEALLOCATE(bpskSignalWithNoise)
!
!          CALL RanomGeneratorInit()
!          WRITE(*,*) 'SNR CURR ',snrCurr
!          cnt=1
!          startAll = omp_get_wtime()
!          DO WHILE(snrCurr<snrEnd)
!
!!          !$omp parallel
!!            !$omp  do PRIVATE(crcOk,numOfsuccessRecieve)
!             DO j = 1,numberOfIterations
!                start=omp_get_wtime()
!                  crcOk = AddNoiseRecievCheckCRC(bpskSignal     = bpskSignal &
!                                       ,DemodulatorBPSK         = DemodulatorBPSK&
!                                       ,awgnChannel             = awgnChannel&
!                                       ,snr                     = snrCurr&
!                                       ,capacity                = outPutSampleCapacityChannel)
!!                 !$omp critical
!
!                  IF ((crcOk)) numOfsuccessRecieve = numOfsuccessRecieve +1
!                  WRITE(*,*) 'crc ',crcOk
!
!!                  IF (.NOT.crcOk) THEN
!!                     WRITE(*,*) 'ERRROR CRC  ',crcOk
!!                     CALL ExitFromProgramNormal()
!!
!!                  END IF
!
!
!!                  !$omp end critical
!
!                   finish=omp_get_wtime()
!             WRITE(*,*)' j= ', j, ' time = ', (finish-start)
!             END DO
!!             !$omp end  do
!!             !$omp END parallel
!
!             berPointsArray(cnt)= snrCurr
!             merValueArray(cnt)=   float(numberOfIterations-numOfsuccessRecieve)/float(numberOfIterations)
!             numOfsuccessRecieve = 0
!
!             WRITE (*,*) 'SNR ', berPointsArray(cnt), ' MER ',merValueArray(cnt)
!             cnt = cnt + 1
!             snrCurr = snrCurr + snrStep
!          END DO
!
!            finishAll = omp_get_wtime()
!
!
!            OPEN(11, FILE = resultFileName, ACCESS="STREAM",ACTION= "WRITE", FORM="FORMATTED", IOSTAT=iostat_Num)
!
!            WRITE(*,*)'all time = ', (finishAll-startAll)/numberOfIterations
!
!            WRITE(11,*)  'SNR           MER '
!            DO i=1,cnt-1
!            WRITE(11,'(F7.3,F7.3)')  berPointsArray(i), merValueArray(i)
!            END DO
!
!            CLOSE(11)





    END SUBROUTINE BERTestGFSKVadim

!    FUNCTION AddNoiseRecievCheckCRC(bpskSignal,DemodulatorBPSK,awgnChannel,snr,capacity) RESULT (isCrcOk)
!        CLASS(analyticSignal_t)  , INTENT(IN) :: bpskSignal
!        CLASS(BPSKDemodulator_t) , INTENT(INOUT) :: DemodulatorBPSK
!        CLASS(AWGNChannel_t)     , INTENT(INOUT) :: awgnChannel
!        REAL(4)                  , INTENT(IN) :: snr
!        INTEGER(1)               , INTENT(IN) :: capacity
!        LOGICAL                               :: isCrcOk
!        INTEGER(8)                            :: z
!        TYPE(analyticSignal_t)                :: bpskSignalWithNoise
!        INTEGER(1),ALLOCATABLE      :: decodedData(:)
!        INTEGER(1),ALLOCATABLE      :: decodedDataOctets(:)
!
!
!        !by default
!        isCrcOk = .FALSE.
!        z= GetRandomInt(int(4,1))
!        WRITE(*,*) 'z= GetRandomInt(int(4,1))  ' ,z
!
!        CALL awgnChannel%SetPtr(z)
!
!        bpskSignalWithNoise = awgnChannel%AddNoiseAnalytic(bpskSignal,snr,capacity)
!       ! CALL WriteAnalyticSignalToFile(bpskSignalWithNoise,int(2,1),'bpskSignalWithNoise.pcm')
!        deCodedData = DemodulatorBPSK%GetData(bpskSignalWithNoise)
!        !CALL  WriteArrayToFileTxt(deCodedData,'test_signals\output\deCodedDataBertTest.txt','(I1.1)')
!        WRITE(*,*) 'принято бит ', size(deCodedData)
!        decodedDataOctets = BitsToOctets(deCodedData, .TRUE.)
!        isCrcOk = CheckCRC(decodedDataOctets)
!    END FUNCTION AddNoiseRecievCheckCRC
end module GFSKVadimmod
