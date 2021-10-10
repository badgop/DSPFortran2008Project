MODULE BitRegisterMod
    IMPLICIT NONE



    INTERFACE PushPopRegister
       MODULE PROCEDURE       PushPopInt_8
       MODULE PROCEDURE       PushPopInt_1
    END INTERFACE

    CONTAINS
     ! 64 разрядный регистр сдвига 63
     ! in - задвигаемый бит, на позицию start_pos [63,0]     !
     ! возвращает ВЫДВИГАЕМЫЙ БИТ c номером pop_pos
     ! допустимые значения pop_pos [0,63]
     ! PushPopInt_8 <------ |63|62|61|60|...|1|0| <------in

     FUNCTION PushPopInt_8(reg,in,popPos,startPos)
         INTEGER(1), PARAMETER           ::INT_KIND = 8
         INTEGER(INT_KIND),INTENT(INOUT) :: reg
         INTEGER(1),INTENT(IN)           :: in
         INTEGER(1),INTENT(IN)           :: popPos
         INTEGER(1),INTENT(IN)           :: startPos
         INTEGER(1)                      :: PushPopInt_8
         PushPopInt_8=0
         IF(BTEST(reg,popPos)) PushPopInt_8=1
         reg=SHIFTL(reg,1)
         IF(in/=0) reg = IBSET(reg,startPos)
    END  FUNCTION PushPopInt_8

     ! 8 разрядный регистр сдвига
     ! in - задвигаемый бит, на позицию start_pos [7,0]     !
     ! возвращает ВЫДВИГАЕМЫЙ БИТ c номером pop_pos
     ! допустимые значения pop_pos [0,7]
     ! PushPopInt_1 <------ |7|6|5|4|...|1|0| <------in

     FUNCTION PushPopInt_1(reg,in,popPos,startPos)
         INTEGER(1), PARAMETER           ::INT_KIND = 1
         INTEGER(INT_KIND),INTENT(INOUT) :: reg
         INTEGER(1),INTENT(IN)           :: in
         INTEGER(1),INTENT(IN)           :: popPos
         INTEGER(1),INTENT(IN)           :: startPos
         INTEGER(1)                      :: PushPopInt_1
         PushPopInt_1=0
         IF(BTEST(reg,popPos)) PushPopInt_1=1
         reg=SHIFTL(reg,1)
         IF(in/=0) reg = IBSET(reg,startPos)
    END  FUNCTION PushPopInt_1



END MODULE BitRegisterMod
