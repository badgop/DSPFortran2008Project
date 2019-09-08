PROGRAM main
    USE PrefixModule
    USE TestsModule
    IMPLICIT NONE


CALL DDSOutputTest(romLengthInBits=int(32,1),romLenthTruncedInBits=int(14,1),outputSignalSampleCapacity=int(12,1)&
                             ,samplingFrequency=192*KILO,centralFrequency=1*KILO,phase=real(1.0,8)&
                             ,periods=10,centralFrequency2=2*KILO )

!    CALL  AnalyticComplexSignalTestConstructors('dds_output_test1.pcm', 'analytic_const_test.pcm')
!    CALL AnalyticSignalTestWriteRead('dds_output_test1.pcm','analytic_test_write.pcm' )
!    CALL ComplexSignalTestWriteRead('dds_output_test1.pcm','dds_output_test2.pcm',&
!                                    'complex_write_testI.pcm','complex_write_testQ.pcm')
!
!   CALL AnalyticSignalMultiplyPlusShiftTest()
!     CALL ComplexDDSTest(int((800*KILO),4),380001)
!
!
!    CALL AnalyticSignalTestOperators()
!     CALL ComplexMultiplyTest(int((50*KILO)))



    WRITE(*,*) 'DONE!'


    CONTAINS




END PROGRAM main
