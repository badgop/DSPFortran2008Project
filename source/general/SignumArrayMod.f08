MODULE SignumArrayMod
    USE MathConstModule
    USE BitRegisterMod
    IMPLICIT NONE

    !
    !ОБРАБОТКА ЗНАКОВОГО(БИТОВОГО) МАССИВА!!!!
    !
    !
    !

    INTERFACE PushPopBitSignumArray
       MODULE PROCEDURE       PushPopBitSignumArrayInt_8
       MODULE PROCEDURE       PushPopBitSignumArrayInt_1
    END INTERFACE

    CONTAINS


    ! 64*n разрядный регистр сдвига, составленный из n
     ! элементов массива (reg(:))
     ! считаем что биты задвигаются в последний элемент
     ! и выдвигаются из первого
     ! in - задвигаемый в последний элемент масссива бит
     ! start_pos - номер разряда куда вставляется задвигаемый бит (63 старший (левый), 0 младший(правый))
     ! возвращает выдвигаемый бит - 63 (старший) из первого элемента массива
    FUNCTION PushPopBitSignumArrayInt_8(reg,in,start_pos)
        INTEGER(1), PARAMETER           ::INT_KIND = 8
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
        lastPopBitPrev= PushPopRegister(reg(length)&
                                    ,in=in&
                                    ,popPos=int(INT_KIND*bitsInByte_const-1,1)&
                                    ,startPos=int(start_pos,1))
!        WRITE(*,*)'lastPopBitPrev     ' ,lastPopBitPrev
        DO i=length-1,1,-1
           ! lastPopBitCurrent <------ |63|62|61|60|...|2|1||0| <------lastPopBitPrev
           lastPopBitCurrent = PushPopRegister(reg(i)&
                                           ,in=lastPopBitPrev&
                                           ,popPos=int(INT_KIND*bitsInByte_const-1,1)&
                                           ,startPos=int(0,1))
           lastPopBitPrev    = lastPopBitCurrent
        END DO
        PushPopBitSignumArrayInt_8 = lastPopBitPrev
    END  FUNCTION PushPopBitSignumArrayInt_8


     ! 8*n разрядный регистр сдвига, составленный из n
     ! элементов массива (reg(:))
     ! считаем что биты задвигаются в последний элемент
     ! и выдвигаются из первого
     ! in - задвигаемый в последний элемент масссива бит
     ! start_pos - номер разряда куда вставляется задвигаемый бит (7 старший (левый), 0 младший(правый))
     ! возвращает выдвигаемый бит - 7 (старший) из первого элемента массива
    FUNCTION PushPopBitSignumArrayInt_1(reg,in,start_pos)
        INTEGER(1), PARAMETER           ::INT_KIND = 1
        INTEGER(INT_KIND),INTENT(INOUT) :: reg(:)
        INTEGER(1),INTENT(IN)           :: in
        INTEGER(1),INTENT(IN)           :: start_pos
        INTEGER(1)                      :: PushPopBitSignumArrayInt_1
        INTEGER(8)                      :: length
        INTEGER(8)                      :: i
        !последний выдвинутый крайний левый бит из предыдущего элемента массива
        INTEGER(1)                      :: lastPopBitPrev
        !последний выдвинутый крайний левый бит из текущего элемента массива
        INTEGER(1)                      :: lastPopBitCurrent
        PushPopBitSignumArrayInt_1=0
        length = size(reg)

        ! обрабатываем крайний правый элемент массива
        ! новый бит будет помещаться  в  startPos
        ! lastPopBitCurrent <------ |63|62|61|60|...|startPos|startPos-1|...|0| <------lastPopBitPrev
        lastPopBitPrev= PushPopRegister(reg(length)&
                                    ,in=in&
                                    ,popPos=int(INT_KIND*bitsInByte_const-1,1)&
                                    ,startPos=int(start_pos,1))
!        WRITE(*,*)'lastPopBitPrev     ' ,lastPopBitPrev
        DO i=length-1,1,-1
           ! lastPopBitCurrent <------ |63|62|61|60|...|2|1||0| <------lastPopBitPrev
           lastPopBitCurrent = PushPopRegister(reg(i)&
                                           ,in=lastPopBitPrev&
                                           ,popPos=int(INT_KIND*bitsInByte_const-1,1)&
                                           ,startPos=int(0,1))
           lastPopBitPrev    = lastPopBitCurrent
        END DO
        PushPopBitSignumArrayInt_1 = lastPopBitPrev
    END  FUNCTION PushPopBitSignumArrayInt_1



END MODULE SignumArrayMod
