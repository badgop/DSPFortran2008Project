module BitOpsMod
    implicit none


    CONTAINS


    FUNCTION SumOnesInInt_8(x)

       INTEGER(8) :: x,i
       INTEGER(8) :: SumOnesInInt_8

       SumOnesInInt_8=0
       DO i=0,63
          IF (BTEST(x,i)) SumOnesInInt_8=SumOnesInInt_8+1
       END DO

    END FUNCTION SumOnesInInt_8

end module BitOpsMod
