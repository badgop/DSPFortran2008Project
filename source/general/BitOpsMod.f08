! В это модуле содержаться функции реализующие разноообразные битовые операции

module BitOpsMod
    USE MathConstModule
    USE BitRegisterMod

    implicit none
    CONTAINS
    ! вычисляет число единиц в 64 разрядном числе
    PURE FUNCTION SumOnesInInt_8(x)
         INTEGER(1), PARAMETER ::INT_KIND = 8
         INTEGER(INT_KIND),INTENT(IN) :: x
         INTEGER(INT_KIND) :: SumOnesInInt_8
         INTEGER(INT_KIND) ::i
         SumOnesInInt_8=0
         DO i=0,(INT_KIND*bitsInByte_const-1)
            IF (BTEST(x,i)) SumOnesInInt_8=SumOnesInInt_8+1
         END DO
    END FUNCTION SumOnesInInt_8

    PURE FUNCTION ReverseBitOrderINT1(x)
         INTEGER(1), PARAMETER ::INT_KIND = 8
         INTEGER(INT_KIND),INTENT(IN) :: x
          INTEGER(INT_KIND) :: ReverseBitOrderINT1


    END FUNCTION ReverseBitOrderINT1

end module BitOpsMod
