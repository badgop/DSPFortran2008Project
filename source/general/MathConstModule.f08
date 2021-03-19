    !======================================================
    !===== Модуль, содержащий математические константы
    !======
    !======
    !======================================================
    !
    !
    ! Авторы:
    ! Татарчук И.А
    !======================================================


MODULE MathConstModule
    IMPLICIT NONE

    PUBLIC

    REAL(8)   , PARAMETER :: PI       = 3.1415926535897932384626433832795
    REAL(8)   , PARAMETER :: E_NUMBER = 2.718281828459045235360287471352
    INTEGER(1), PARAMETER :: bitsInByte_const=8

    ! Вспомогательные константы
    INTEGER(2),PRIVATE, PARAMETER :: int2 =-32768
    INTEGER(4),PRIVATE, PARAMETER :: int4 =2**30-1
    INTEGER(8),PRIVATE, PARAMETER :: int8 = 1
    INTEGER(8), PARAMETER :: HUGE_Int1=HUGE(bitsInByte_const)
    INTEGER(8), PARAMETER :: HUGE_Int2=HUGE(int2)
    INTEGER(8), PARAMETER :: HUGE_Int4=HUGE(int4)
    INTEGER(8), PARAMETER :: HUGE_Int8=HUGE(int8)
END MODULE MathConstModule
