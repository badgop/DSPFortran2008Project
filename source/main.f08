PROGRAM main
    USE PrefixModule
    USE TestsModule
    IMPLICIT NONE


   CALL DDSOutputTest(romLengthInBits=int(32,1),romLenthTruncedInBits=int(14,1),outputSignalSampleCapacity=int(8,1)&
                             ,samplingFrequency=20*MEGA,centralFrequency=100*KILO,phase=real(0.0,8)&
                             ,periods=10)
!    CALL  AnalyticComplexSignalTestConstructors
!    CALL AnalyticSignalTestWriteRead()
!    CALL ComplexSignalTestWriteRead()
!   CALL AnalyticSignalMultiplyPlusShiftTest()
!     CALL ComplexDDSTest(int((800*KILO),4),380001)
!
!
!    CALL AnalyticSignalTestOperators()
!     CALL ComplexMultiplyTest(int((50*KILO)))



    WRITE(*,*) 'DONE!'


    CONTAINS




END PROGRAM main
