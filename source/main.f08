PROGRAM main
    USE PrefixModule
    USE TestsModule
    IMPLICIT NONE

    CALL InitDDSTest()
!   CALL DDSOutputTest()
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
