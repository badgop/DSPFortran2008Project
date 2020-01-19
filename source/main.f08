PROGRAM main
    USE PrefixModule
    USE TestsModule
    IMPLICIT NONE


!CALL DDSOutputTest(romLengthInBits=int(32,1),romLenthTruncedInBits=int(14,1),outputSignalSampleCapacity=int(12,1)&
!                             ,samplingFrequency=192*KILO&
!                             ,phase=real(1.0,8)&
!                             ,signalLengthInSamples=2346&
!                             ,centralFrequency=1*KILO&
!                             ,centralFrequency2=2*KILO&
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
!!
!   CALL AnalyticSignalMultiplyPlusShiftTest(inputSignalFileName='test_signals\input\dds_test_output1.pcm'&
!                                           ,inputSignalFileName2='test_signals\input\dds_test_output2.pcm'&
!                                           ,outputSignalFileName='test_signals\output\analytic_multiply.pcm'&
!                                           ,shift=INT(10,1))
!     CALL ComplexDDSTest(centralFrequency= 1*KILO,samplingFrequency=192*KILO&
!                         ,sig_len=16384,romLengthInBits=int(32,1),romLenthTruncedInBits=int(14,1)&
!                        ,outputSignalSampleCapacity=int(12,1)&
!                        ,phase=real(1.0,8)&
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
!   Убедиться в наличии файлов... в inputsignals
!  и запустить уже ComplexMultiplyTest

!     CALL DDSOutputTest(romLengthInBits=int(32,1),romLenthTruncedInBits=int(14,1),outputSignalSampleCapacity=int(12,1)&
!                             ,samplingFrequency=192*KILO&
!                             ,phase=real(1.0,8)&
!                             ,signalLengthInSamples=2346&
!                             ,centralFrequency=40*KILO&
!                             ,centralFrequency2=40*KILO&
!                             ,file1Name='test_signals\input\dds_test_output1.pcm'&
!                             ,file2Name='test_signals\input\dds_test_output2.pcm' )

     CALL ComplexMultiplyTest(inputSignalFileNameI='test_signals\input\outi.pcm'&
                             ,inputSignalFileNameQ='test_signals\input\outq.pcm'&
                             ,inputRefI='test_signals\input\dds_test_output1.pcm'&
                             ,inputRefQ='test_signals\input\dds_test_output2.pcm'&
                             ,outputSignalFileNameI='test_signals\output\complex_mult_testI.pcm'&
                             ,outputSignalFileNameQ='test_signals\output\complex_mult_testQ.pcm'&
                             ,shift=int(10,1) )

    WRITE(*,*) 'DONE!'


    CONTAINS




END PROGRAM main
