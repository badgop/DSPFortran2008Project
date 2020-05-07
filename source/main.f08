PROGRAM main
    USE PrefixModule
    USE TestsModule
    USE ModuleWriteReadArrayFromToFile
    USE  ReadWriteArrayToFromTxt
    USE MathConstModule
    USE BERTestMod





!CALL DDSOutputTest(romLengthInBits=int(32,1),romLenthTruncedInBits=int(14,1),outputSignalSampleCapacity=int(12,1)&
!                             ,samplingFrequency=192*KILO&
!                             ,phase=real(1.0,8)&
!                             ,signalLengthInSamples=2346&
!                             ,centralFrequency=int(2*KILO,4)&
!                             ,centralFrequency2=int(1*KILO,4)&
!                             ,file1Name='test_signals\input\dds_test_output1.pcm'&
!                             ,file2Name='test_signals\input\dds_test_output2.pcm' )

!    CALL  AnalyticComplexSignalTestConstructors(inputSignalFileNameI         ='test_signals\input\dds_test_output1.pcm'&
!                                               ,inputSignalFileNameQ         ='test_signals\input\dds_test_output2.pcm'&
!                                               ,outputSignalFileNameAnaytic  ='test_signals\output\analytic_constructor_test.pcm'&
!                                               ,outputSignalFileNameComplexI ='test_signals\output\complexExtractredI.pcm'&
!                                               ,outputSignalFileNameComplexQ ='test_signals\output\complexExtractredQ.pcm')



!    CALL AnalyticSignalTestWriteRead(inputSignalFileName='test_signals\input\dds_test_output1.pcm'&
!                                    ,outputSignalFileName='test_signals\output\test_read_write_analytic.pcm' )

!    CALL ComplexSignalTestWriteRead(inputSignalFileNameI='test_signals\input\dds_test_output1.pcm'&
!                                   ,inputSignalFileNameQ='test_signals\input\dds_test_output2.pcm'&
!                                   ,outputSignalFileNameI='test_signals\output\test_read_write_complexI.pcm'&
!                                   ,outputSignalFileNameQ='test_signals\output\test_read_write_complexQ.pcm'&
!                                   ,capacity=INT(2,1))
!!!
!   CALL AnalyticSignalMultiplyPlusShiftTest(inputSignalFileName='test_signals\input\dds_test_output1.pcm'&
!                                           ,inputSignalFileName2='test_signals\input\dds_test_output2.pcm'&
!                                           ,outputSignalFileName='test_signals\output\analytic_multiply.pcm'&
!                                           ,shift=INT(10,1))
!     CALL ComplexDDSTest(centralFrequency= int(0.05*KILO,4),samplingFrequency=192*KILO&
!                         ,sig_len=16384,romLengthInBits=int(32,1),romLenthTruncedInBits=int(14,1)&
!                        ,outputSignalSampleCapacity=int(12,1)&
!                        ,outputSignalFileNameI='test_signals\output\complexdds_outI.pcm'&
!                        ,outputSignalFileNameQ='test_signals\output\complexdds_outQ.pcm')


!
!
!    CALL AnalyticSignalTestAddSub(inputSignalFileNameI='test_signals\input\dds_test_output1.pcm'&
!                                 ,inputSignalFileNameQ='test_signals\input\dds_test_output2.pcm'&
!                                 ,outputSignalAddName= 'test_signals\output\analytic_add_test.pcm' &
!                                 ,outputSignalSubName = 'test_signals\output\analytic_sub_test.pcm'  )


!*******************ТЕСТ комплексного умножителя*************
!
!
!   Убедиться в наличии файлов исходного сигнала в inputsignals
!   через DDSOutputTest создать опорные колебания с нужной частотой и длительностью
!  и запустить уже ComplexMultiplyTest

!     CALL DDSOutputTest(romLengthInBits=int(32,1),romLenthTruncedInBits=int(14,1),outputSignalSampleCapacity=int(12,1)&
!                             ,samplingFrequency=192*KILO&
!                             ,phase=real(1.0,8)&
!                             ,signalLengthInSamples=2346&
!                             ,centralFrequency=40*KILO&
!                             ,centralFrequency2=40*KILO&
!                             ,file1Name='test_signals\input\dds_test_output1.pcm'&
!                             ,file2Name='test_signals\input\dds_test_output2.pcm' )
!
!     CALL ComplexMultiplyTest(inputSignalFileNameI='test_signals\input\outi.pcm'&
!                             ,inputSignalFileNameQ='test_signals\input\outq.pcm'&
!                             ,inputRefI='test_signals\input\dds_test_output1.pcm'&
!                             ,inputRefQ='test_signals\input\dds_test_output2.pcm'&
!                             ,outputSignalFileNameI='test_signals\output\complex_mult_testI.pcm'&
!                             ,outputSignalFileNameQ='test_signals\output\complex_mult_testQ.pcm'&
!                             ,shift=int(9,1) )



!    CALL ConvolveTest(inputSignalFileName  = 'test_signals\input\noise_163930.pcm'&
!                     ,inputRefFileName     = 'test_signals\input\new3.txt'&
!                     ,outputSignalFileName = 'test_signals\output\convolve_test.pcm'&
!                     ,shift = int(18,1))


!    CALL AutoConvolveTest(inputSignalFileName  = 'test_signals\input\noise_7897.pcm'&
!                     ,inputRefFileName     = 'test_signals\input\noise_7897.pcm'&
!                     ,outputSignalFileName = 'test_signals\output\auto_convolve_test.pcm'&
!                     ,shift = int(22,1))

!    CALL AutoConvolveTest(inputSignalFileName  = 'test_signals\input\signal (2).pcm'&
!                     ,inputRefFileName     = 'test_signals\input\signal (2).pcm'&
!                     ,outputSignalFileName = 'test_signals\output\auto_convolve_test.pcm'&
!                     ,shift = int(30,1))
!
!    CALL OpenMPIConvolveTest(inputSignalFileName  = 'test_signals\input\noise_7897.pcm'&
!                     ,inputRefFileName     = 'test_signals\input\noise_7897.pcm'&
!                     ,outputSignalFileName = 'test_signals\output\auto_convolve_test.pcm'&
!                     ,shift = int(22,1)&
!                     ,iterationCount=int(10000,4))
!

!     CALL SignumSignalConstructorTest()
!     CALL RegisterPushPopAnsSummTest()
!     CALL RegisterArrayPushPopTest()
!     CALL SignumCorrTest()
!  WRITE (*,*) 'SIGNIM!'
!    CALL SignumConvolveTest(inputSignalFileName  = 'test_signals\input\noise_7897.pcm'&
!                    ,inputRefFileName     = 'test_signals\input\noise_7897.pcm'&
!                    ,outputSignalFileName = 'test_signals\output\auto_convolve_test_sig.pcm'&
!                    ,shift = int(0,1)&
!                    ,iterationCount=int(10000,4))

!      CALL ImpulseGeneratorTest ('test_signals\input\psp_valera.txt', 'test_signals\output\impGenTest1.pcm', osr = int(10,8))


!      CALL SimplePSNGeneratorTest('test_signals\input\psp_valera.txt', 'test_signals\output\PsbSimpleGenTest1.pcm', osr = int(10,8)&
!                                      ,lenInblocks = int(10,8))


!       CALL BPSKGeneratorTest(      pspFileName          = 'test_signals\input\psp_valera.txt'&
!                                   ,dataFileName         = 'test_signals\input\data.txt'&
!                                   ,outPutFileName       = 'test_signals\output\BPSKTest1.pcm'&
!                                   ,filterFileName       = 'test_signals\input\20_mhz_1_25_cut_int.txt'&
!                                   ,codedDataFileName    = 'test_signals\output\codedData.txt'&
!                                   ,baudRateInSamples    = int(10240,8), chipRateInSamples = int(10,8) &
!                                   ,sampleRate           = int(20*MEGA,8)&
!                                   ,centralFrequency     = int(3*MEGA,8)&
!                                   ,outPutSampleCapacity = int(14,1)&
!                                   ,outPutShift          = int(17,1))
!        CALL PhaseDetectorTest(inputFileName    = 'test_signals\output\BPSKTest1.pcm ' &
!                              , outPutFileNameI = 'test_signals\output\phaseDemodTestI.pcm'&
!                              ,outPutFileNameQ  = 'test_signals\output\phaseDemodTestQ.pcm'&
!                              ,filterFileName   = 'test_signals\input\20_mhz_1_25_cut_int.txt' &
!                              ,sampleRate       = int(20*MEGA,8)&
!                              ,centralFrequency = int (3*MEGA+0,8)&
!                              ,initialPhase     = 0.3*PI&
!                              ,outputShift      = int(28,8))

!         WRITE(*,*) 'аналитич со знаковым'
!'test_signals\input\noise_7897.pcm'
!         CALL AnalyticSignumConvolveTest(inputSignalFileName  = 'test_signals\output\phaseDemodTestQ.pcm'&
!                                         ,inputRefFileName     = 'test_signals\input\impGenTest1.pcm'&
!                                         ,outputSignalFileName = 'test_signals\output\auto_convolve_test22.pcm'&
!                                         ,shift = int(0,1)&
!                                         ,iterationCount=int(1,4))


!              CALL  BPSKDemodulatorTest(      pspFileName  = 'test_signals\input\psp_valera.txt'&
!                                   ,dataFileName           = 'test_signals\input\data.txt'&
!                                   ,inPutFileName          = 'test_signals\output\BPSKTest1.pcm'&
!                                   ,filterFileName         = 'test_signals\input\20_mhz_1_25_cut_int.txt'&
!                                   ,deCodedDataFileName    = 'test_signals\output\decodedData.txt'&
!                                   ,phaseDetectorIName     = 'test_signals\output\bpskDemodI.pcm'&
!                                   ,phaseDetectorQName     = 'test_signals\output\bpskDemodQ.pcm'&
!                                   ,complexModuleCorrNAme  = 'test_signals\output\moduleCorr.pcm'&
!                                   ,baudRateInSamples      = int(10240,8), chipRateInSamples = int(10,8) &
!                                   ,sampleRate             = int(20*MEGA,8)&
!                                   ,centralFrequency       = int (3*MEGA+0,8)&
!                                   ,initialPhase           = 0.3*PI&
!                                   , outPutSampleCapacity  = int(14,1)&
!                                   , outPutShift           = int(26,1)&
!                                   , decimationCoeff       = int(5,8))


!              CALL OctetDataMaker(inputDataFileName  = 'test_signals\input\data.txt'&
!                                 ,outputDataFileName = 'test_signals\output\octetData.txt')


!               CALL Crc16Test(inputDataFileName = 'test_signals\input\vdl4_octets.txt'&
!                             , outputDataFileName = 'test_signals\output\decodedData3.txt'  )


!                 CALL   PowerMeterTest(inputDataFileName = 'test_signals\output\BPSKTest1.pcm'&
!                             , outputDataFileName = 'test_signals\output\decodedData3.txt'&
!                             , samplingFrequency = int((100000*KILO),8)&
!                             , length = int(20000,8)  )


!                 CALL NoiseMakerTest(inputSignalFileName  = 'noise\noise.pcm'&
!                     ,inputRefFileName     = 'test_signals\input\20_mhz_bandpass_3Mhz_1_9_to_4_1_int.txt'&
!                     ,outputSignalFileName = 'test_signals\output\noise_1_1Mhz.pcm'&
!                     ,shift = int(31,1))

!                 CALL RandomGeneratorTest()
!                CALL  AddNoiseTEst(inputNoiseFileName = 'test_signals\output\noise_1_1Mhz.pcm'&
!                                  ,inputSignalFileName ='test_signals\output\BPSKTest1.pcm'&
!                                  ,outputSignalFileName = 'test_signals\output\noiseAmpTEst.pcm'&
!                                  ,amplifiedNoise = 'test_signals\output\awgnTest.pcm' &
!                                  ,snr =  18.00 )
!
!
!              CALL  BPSKDemodulatorTest(      pspFileName  = 'test_signals\input\psp_valera.txt'&
!                                   ,dataFileName           = 'test_signals\input\data.txt'&
!                                   ,inPutFileName          = 'test_signals\output\awgnTest.pcm'&
!                                   ,filterFileName         = 'test_signals\input\20_mhz_1_25_cut_int.txt'&
!                                   ,deCodedDataFileName    = 'test_signals\output\decodedData.txt'&
!                                   ,phaseDetectorIName     = 'test_signals\output\bpskDemodI.pcm'&
!                                   ,phaseDetectorQName     = 'test_signals\output\bpskDemodQ.pcm'&
!                                   ,complexModuleCorrNAme  = 'test_signals\output\moduleCorr.pcm'&
!                                   ,baudRateInSamples      = int(10240,8), chipRateInSamples = int(10,8) &
!                                   ,sampleRate             = int(20*MEGA,8)&
!                                   ,centralFrequency       = int (3*MEGA+0,8)&
!                                   ,initialPhase           = 0.3*PI&
!                                   , outPutSampleCapacity  = int(14,1)&
!                                   , outPutShift           = int(26,1)&
!                                   , decimationCoeff       = int(5,8))


!        CALL PhaseDetectorTest(inputFileName    = 'test_signals\output\awgnTest.pcm' &
!                              , outPutFileNameI = 'test_signals\output\phaseDemodTestI.pcm'&
!                              ,outPutFileNameQ  = 'test_signals\output\phaseDemodTestQ.pcm'&
!                              ,filterFileName   = 'test_signals\input\20_mhz_1_25_cut_int.txt' &
!                              ,sampleRate       = int(20*MEGA,8)&
!                              ,centralFrequency = int (3*MEGA+0,8)&
!                              ,initialPhase     = 0.3*PI&
!                              ,outputShift      = int(28,8))

         CALL BERTestSignumCorrelation (parameterFileName= 'test\berTestSignum.txt'&
                                    , resultFileName = 'test\result.txt' )

    CONTAINS


    FUNCTION qq(a)
      INTEGER(1),INTENT(IN) :: a
      INTEGER(1)            :: qq
      qq = 1
    END FUNCTION qq

END PROGRAM main
