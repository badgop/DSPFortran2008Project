! В это модуле содержаться функции реализующие разноообразные битовые операции

module BitOpsMod
    USE MathConstModule
    implicit none
    CONTAINS
     ! 64 разрядный регистр сдвига 63
     ! in - задвигаемый бит, на позицию start_pos [0,63]     !
     ! возвращает выдвигаемый бит c номером pop_pos
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


       ! 64*n разрядный регистр сдвига, составленный из n
     ! элементов массива (reg(:))
     ! считаем что биты задвигаются в последний элемент
     ! и выдвигаются из первого
     ! in - задвигаемый в последний элемент масссива бит
     ! start_pos - номер разряда куда вставляется задвигаемый бит (63 старший (левый), 0 младший(правый))
     ! возвращает выдвигаемый бит - 63 (старший) из первого элемента массива
    FUNCTION PushPopBitSignumArrayInt_8(reg,in,start_pos)
        INTEGER(1), PARAMETER ::INT_KIND = 8
        INTEGER(INT_KIND),INTENT(INOUT) :: reg(:)
        INTEGER(1),INTENT(IN)           :: in
        INTEGER(1),INTENT(IN)           :: start_pos
        INTEGER(1)                      :: PushPopBitSignumArrayInt_8
        INTEGER(8)                      :: length
        INTEGER(8)                      :: i
        !последний выдвинутый крайний левый бит из предыдущего элемента массива
        INTEGER(1)                      :: lastPopBitPrev
        !последний выдвинутый крайний левый бит из текущего элемента массива
        INTEGER(1)                      :: lastPopBitCurrent
        PushPopBitSignumArrayInt_8=0
        length = size(reg)

        ! обрабатываем крайний правый элемент массива
        ! новый бит будет помещаться  в  startPos
        ! lastPopBitCurrent <------ |63|62|61|60|...|startPos|startPos-1|...|0| <------lastPopBitPrev
        lastPopBitPrev= PushPopInt_8(reg(length),in=in,popPos=int(63,1),startPos=int(start_pos,1))
!        WRITE(*,*)'lastPopBitPrev     ' ,lastPopBitPrev
        DO i=length-1,1,-1
           ! lastPopBitCurrent <------ |63|62|61|60|...|2|1||0| <------lastPopBitPrev
           lastPopBitCurrent = PushPopInt_8(reg(i),in=lastPopBitPrev,popPos=int(63,1),startPos=int(0,1))
           lastPopBitPrev    = lastPopBitCurrent
        END DO
        PushPopBitSignumArrayInt_8 = lastPopBitPrev
    END  FUNCTION PushPopBitSignumArrayInt_8


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

end module BitOpsMod
