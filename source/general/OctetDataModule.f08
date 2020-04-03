MODULE OctetDataModule

    USE BitOpsMod
    USE SignumArrayMod
    USE MathConstModule
    USE ModuleExitProg





CONTAINS
    ! ВНИМАНИЕ отсчеты ЗНАКОВОГО сигнала заносятя в старшие разряды!
    FUNCTION BitsToOctets(loadedSignal,isMSBFIRST)
        INTEGER(8), INTENT(IN)  :: loadedSignal(:)
        LOGICAL   , INTENT(IN)  :: isMSBFIRST
        INTEGER(1), ALLOCATABLE :: BitsToOctets(:)
        INTEGER(1)               :: trailLen
        INTEGER(4) :: stat
        INTEGER(8) :: length_new
        INTEGER(8) :: i,j,k
        INTEGER(1) :: registerKind
        registerKind=KIND(BitsToOctets)
         WRITE(*,*) 'BitsToOctets registerKind=', registerKind
        length_new= size(loadedSignal)/(registerKind*bitsInByte_const)
        WRITE(*,*) 'length_new ', length_new
        !если длина массива не кратна числу бит в целом типе данных, то надо еще одно число добавить
        trailLen = INT(MOD(size(loadedSignal),registerKind*bitsInByte_const),1)
        IF ( trailLen>0) length_new=length_new + 1
        WRITE(*,*) 'trail length = ', trailLen
        WRITE(*,*) 'Новая длина = ',length_new
        allocate (BitsToOctets(1:length_new))
        !обязательно зануляем массив, что бы ставить только 1
        BitsToOctets=0
        k=0
        j=1
        DO i=1, size(loadedSignal)
           IF(k>((registerKind*bitsInByte_const)-1)) THEN
              k=0
              j=j+1
           END IF
           IF(loadedSignal(i)>0) THEN
                IF(isMSBFIRST) THEN
                  BitsToOctets(j)=IBSET(BitsToOctets(j),(registerKind*bitsInByte_const-1)-k)
                ELSE
                  BitsToOctets(j)=IBSET(BitsToOctets(j),(k))
                END IF
           END IF
           k=k+1
        END DO
   END FUNCTION BitsToOctets

       FUNCTION OctetsToBits(loadedSignal,isMSBFIRST)
        INTEGER(1), INTENT(IN)  :: loadedSignal(:)
        LOGICAL   , INTENT(IN)  :: isMSBFIRST
        INTEGER(1), ALLOCATABLE :: OctetsToBits(:)
        INTEGER(1) :: registerKind
        registerKind=KIND(OctetsToBits)
        ALLOCATE(OctetsToBits(1:(size(loadedSignal)*registerKind*bitsInByte_const)))
        OctetsToBits = 0
        k=1

        DO i=1, size(loadedSignal)
           IF(.NOT.(isMSBFIRST)) THEN
              DO j=0,(registerKind*bitsInByte_const)-1
                  IF (BTEST(loadedSignal(i),j)) OctetsToBits(k) = 1
                  k = k+1
              END DO
           ELSE
              DO j=(registerKind*bitsInByte_const)-1,0,-1
                  IF (BTEST(loadedSignal(i),j)) OctetsToBits(k) = 1
                  k = k+1
              END DO
           END IF
       END DO

   END FUNCTION OctetsToBits



END MODULE OctetDataModule
