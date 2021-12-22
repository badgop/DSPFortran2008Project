MODULE signumSignalModule

    USE BitOpsMod
    USE SignumArrayMod
    USE MathConstModule
    USE ModuleExitProg
    USE openmpSignumCorr
    USE ModuleExitProg
    IMPLICIT NONE
    PRIVATE



    TYPE, PUBLIC :: signumSignal_t
        PRIVATE
        ! отсчеты аналитического сингнала содержаться в динамеческом массиве
        INTEGER(8),ALLOCATABLE :: signal(:)
        ! поле типа данных - isAllocated определяет выделена ли память под сигнал т.е проведена ли
        ! инициализация экземпляра обьекта
        LOGICAL                :: isAllocated=.FALSE.
        ! поле определяет длительность аналитического сигнала целых числах (размер числа в байтах
        ! определяется типом данных signal(:) )
        INTEGER(8)             :: signalSize= 0
        !длина хвоста (отчеты занимают N+1 чисел, и если длина сигнала не кратна 8, то в последнем
        ! необходио хранить остсаток бит)
        INTEGER(1)             :: trailLen= 0
        ! Поле, определяющее имя сигнала
        ! ЖЕЛАТЕЛЬНО добавлять расширение к имени
        CHARACTER(50)         :: signalName=''
    CONTAINS

        PROCEDURE                 :: Constructor8
        PROCEDURE                 :: Constructor4
        PROCEDURE                 :: Constructor2
        PROCEDURE                 :: Constructor1
        GENERIC :: Constructor       => Constructor1,Constructor2,Constructor4,Constructor8
        PROCEDURE                 :: ExtractSignalData
        PROCEDURE                 :: Correlate
        PROCEDURE                 :: CorrelateOMP
        FINAL                     :: destructor
        !операторы
        generic :: operator   (.CONV.) =>  Correlate
    END TYPE signumSignal_t

CONTAINS
    ! ВНИМАНИЕ отсчеты ЗНАКОВОГО сигнала заносятя в старшие разряды!
    SUBROUTINE Constructor8(this,loadedSignal)
        CLASS(signumSignal_t), INTENT(INOUT) :: this
        INTEGER(1),PARAMETER :: intKind=8
        INTEGER(intKind), INTENT(IN) :: loadedSignal(:)
        INTEGER(4) :: stat
        INTEGER(8) :: length_new
        INTEGER(8) :: i,j,k
        INTEGER(1) :: registerKind
        CHARACTER(len=60) ::errorCode


        IF (ALLOCATED(this%signal)) THEN
           !!WRITE(*,*) 'ПАмять уже выделена, обнуляю'
           DEALLOCATE(this%signal, STAT=stat)
           IF (STAT==0) THEN
               !WRITE(*,*) ' ANALYTIC SELF DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize, this%signalName
           END IF
           this%isAllocated=.FALSE.
           this%signalName=''
           this%signalSize= 0
        END IF
           !WRITE(*,*) 'SIGNUM CONSTRUCTOR WORKS!', this%signalName
        ! число байт в типе данных
        registerKind=KIND(this%signal)
        length_new= size(loadedSignal)/(registerKind*bitsInByte_const)
       ! WRITE(*,*)  'НИТЬ  ',omp_get_thread_num()
       ! WRITE(*,*) 'длина сигнала знаковый конструктора 8',  size(loadedSignal)

        !если длина массива не кратна числу бит в целом типе данных, то надо еще одно число добавить
        this%trailLen = INT(MOD(size(loadedSignal),registerKind*bitsInByte_const),1)
        IF (this%trailLen>0) length_new=length_new+1
       !WRITE(*,*) 'trail length = ',this%trailLen
       ! WRITE(*,*) 'Новая длина = ',length_new
        allocate (this%signal(1:length_new), STAT=stat,ERRMSG = errorCode)

          IF (STAT/=0) THEN
               WRITE (*,*) 'Знаковый конструктор не смог выделить память,ERRMSG = ',errorCode
               CALL ExitFromProgramNormal()
           END IF
        !обязательно зануляем массив, что бы ставить только 1
        this%signal=0
        k=0
        j=1
        DO i=1, size(loadedSignal)
           IF(k>((registerKind*bitsInByte_const)-1)) THEN
              k=0
              j=j+1
           END IF
           IF(loadedSignal(i)>=0) this%signal(j)=IBSET(this%signal(j),(registerKind*bitsInByte_const-1)-k)
           k=k+1
        END DO
        this%isAllocated=.TRUE.
        this%signalSize=length_new
   END SUBROUTINE Constructor8

   SUBROUTINE Constructor4(this,loadedSignal)
        CLASS(signumSignal_t), INTENT(INOUT) :: this
        INTEGER(1),PARAMETER :: intKind=4
        INTEGER(intKind), INTENT(IN) :: loadedSignal(:)
        INTEGER(4) :: stat
        INTEGER(8) :: length_new
        INTEGER(8) :: i,j,k
        INTEGER(1) :: registerKind
        CHARACTER(len=60) ::errorCode


        IF (ALLOCATED(this%signal)) THEN
           !!WRITE(*,*) 'ПАмять уже выделена, обнуляю'
           DEALLOCATE(this%signal, STAT=stat)
           IF (STAT==0) THEN
               !WRITE(*,*) ' ANALYTIC SELF DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize, this%signalName
           END IF
           this%isAllocated=.FALSE.
           this%signalName=''
           this%signalSize= 0
        END IF
           !WRITE(*,*) 'SIGNUM CONSTRUCTOR WORKS!', this%signalName
        ! число байт в типе данных
        registerKind=KIND(this%signal)
        length_new= size(loadedSignal)/(registerKind*bitsInByte_const)
        !если длина массива не кратна числу бит в целом типе данных, то надо еще одно число добавить
        this%trailLen = INT(MOD(size(loadedSignal),registerKind*bitsInByte_const),1)
        IF (this%trailLen>0) length_new=length_new+1
        !WRITE(*,*) 'trail length = ',this%trailLen
        !WRITE(*,*) 'Новая длина = ',length_new
       allocate (this%signal(1:length_new), STAT=stat,ERRMSG = errorCode)

          IF (STAT/=0) THEN
               WRITE (*,*) 'Знаковый конструктор не смог выделить память,ERRMSG = ',errorCode
               CALL ExitFromProgramNormal()
           END IF
        !обязательно зануляем массив, что бы ставить только 1
        this%signal=0
        k=0
        j=1
        DO i=1, size(loadedSignal)
           IF(k>((registerKind*bitsInByte_const)-1)) THEN
              k=0
              j=j+1
           END IF
           IF(loadedSignal(i)>=0) this%signal(j)=IBSET(this%signal(j),(registerKind*bitsInByte_const-1)-k)
           k=k+1
        END DO
        this%isAllocated=.TRUE.
        this%signalSize=length_new
   END SUBROUTINE Constructor4
    
     SUBROUTINE Constructor2(this,loadedSignal)
        CLASS(signumSignal_t), INTENT(INOUT) :: this
        INTEGER(1),PARAMETER :: intKind=2
        INTEGER(intKind), INTENT(IN) :: loadedSignal(:)
        INTEGER(4) :: stat
        INTEGER(8) :: length_new
        INTEGER(8) :: i,j,k
        INTEGER(1) :: registerKind
         CHARACTER(len=60) ::errorCode

        IF (ALLOCATED(this%signal)) THEN
           !!WRITE(*,*) 'ПАмять уже выделена, обнуляю'
           DEALLOCATE(this%signal, STAT=stat)
           IF (STAT==0) THEN
               !WRITE(*,*) ' ANALYTIC SELF DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize, this%signalName
           END IF
           this%isAllocated=.FALSE.
           this%signalName=''
           this%signalSize= 0
        END IF
           !WRITE(*,*) 'SIGNUM CONSTRUCTOR WORKS!', this%signalName
        ! число байт в типе данных
        registerKind=KIND(this%signal)
        length_new= size(loadedSignal)/(registerKind*bitsInByte_const)
        !если длина массива не кратна числу бит в целом типе данных, то надо еще одно число добавить
        this%trailLen = INT(MOD(size(loadedSignal),registerKind*bitsInByte_const),1)
        IF (this%trailLen>0) length_new=length_new+1
        ! WRITE(*,*) 'длина сигнала знаковый конструктора 2 ',  size(loadedSignal)
       ! WRITE(*,*) 'trail length = ',this%trailLen

       ! WRITE(*,*) 'Новая длина = ',length_new
        allocate (this%signal(1:length_new), STAT=stat,ERRMSG = errorCode)

          IF (STAT/=0) THEN
               WRITE (*,*) 'Знаковый конструктор не смог выделить память,ERRMSG = ',errorCode
               CALL ExitFromProgramNormal()
           END IF
        !обязательно зануляем массив, что бы ставить только 1
        this%signal=0
        k=0
        j=1
        DO i=1, size(loadedSignal)
           IF(k>((registerKind*bitsInByte_const)-1)) THEN
              k=0
              j=j+1
           END IF
           IF(loadedSignal(i)>=0) this%signal(j)=IBSET(this%signal(j),(registerKind*bitsInByte_const-1)-k)
           k=k+1
        END DO
        this%isAllocated=.TRUE.
        this%signalSize=length_new
   END SUBROUTINE Constructor2

   SUBROUTINE Constructor1(this,loadedSignal)
        CLASS(signumSignal_t), INTENT(INOUT) :: this
        INTEGER(1),PARAMETER :: intKind=1
        INTEGER(intKind), INTENT(IN) :: loadedSignal(:)
        INTEGER(4) :: stat
        INTEGER(8) :: length_new
        INTEGER(8) :: i,j,k
        INTEGER(1) :: registerKind
         CHARACTER(len=60) ::errorCode


        IF (ALLOCATED(this%signal)) THEN
           !!WRITE(*,*) 'ПАмять уже выделена, обнуляю'
           DEALLOCATE(this%signal, STAT=stat)
           IF (STAT==0) THEN
               !WRITE(*,*) ' ANALYTIC SELF DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize, this%signalName
           END IF
           this%isAllocated=.FALSE.
           this%signalName=''
           this%signalSize= 0
        END IF
           !WRITE(*,*) 'SIGNUM CONSTRUCTOR WORKS!', this%signalName
        ! число байт в типе данных
        registerKind=KIND(this%signal)
        length_new= size(loadedSignal)/(registerKind*bitsInByte_const)
        !если длина массива не кратна числу бит в целом типе данных, то надо еще одно число добавить
        this%trailLen = INT(MOD(size(loadedSignal),registerKind*bitsInByte_const),1)
        IF (this%trailLen>0) length_new=length_new+1
        !WRITE(*,*) 'trail length = ',this%trailLen
       ! WRITE(*,*) 'Новая длина = ',length_new
        allocate (this%signal(1:length_new), STAT=stat,ERRMSG = errorCode)

          IF (STAT/=0) THEN
               WRITE (*,*) 'Знаковый конструктор не смог выделить память,ERRMSG = ',errorCode
               CALL ExitFromProgramNormal()
           END IF
        !обязательно зануляем массив, что бы ставить только 1
        this%signal=0
        k=0
        j=1
        DO i=1, size(loadedSignal)
           IF(k>((registerKind*bitsInByte_const)-1)) THEN
              k=0
              j=j+1
           END IF
           IF(loadedSignal(i)>=0) this%signal(j)=IBSET(this%signal(j),(registerKind*bitsInByte_const-1)-k)
           k=k+1
        END DO
        this%isAllocated=.TRUE.
        this%signalSize=length_new
   END SUBROUTINE Constructor1

    SUBROUTINE ExtractSignalData(this,extractedSignal)
        INTEGER(8),ALLOCATABLE, INTENT(INOUT) :: extractedSignal(:)
        CLASS(signumSignal_t), INTENT(INOUT) :: this
        ALLOCATE(extractedSignal,source=this%signal)
     END SUBROUTINE ExtractSignalData


      ! ВЫХОДНАЯ РАЗРЯДНОСТЬ - 32 бита!!!!
      FUNCTION Correlate(input,reference)
         CLASS(signumSignal_t), INTENT(IN)  :: input
         CLASS(signumSignal_t), INTENT(IN)  :: reference

         INTEGER(4), allocatable :: Correlate(:)
         INTEGER(8)              :: rezSignalLength
         INTEGER(8)              :: referenceSignalLength
         INTEGER(8), allocatable :: window(:)
         INTEGER(8)              :: i,j,corrCnt
         INTEGER(8)              :: mask       = 0
         INTEGER(1)              :: registerKind
         INTEGER(8)              :: result
         INTEGER(1)              :: bitPushPos = 0
         INTEGER(8)              :: arrayPtr   = 0
         INTEGER(8)              :: regPtr     = 0
         INTEGER(1)              :: extractedBit = 0
         INTEGER(4)              :: summ=0



         registerKind=KIND(input%signal)
         ! длительность сигнала ВКФ = длительного входного сигнала
         IF (input%signalSize==1) THEN
             rezSignalLength = input%trailLen
         ELSE                                    ! - 1
             rezSignalLength = (input%signalSize)*(registerKind*bitsInByte_const)+input%trailLen

         END IF

         !WRITE(*,*) 'корр  длина = ',rezSignalLength
         ALLOCATE(Correlate(1:rezSignalLength))
         Correlate=0
         ALLOCATE(window(1:reference%signalSize))
         ! если длина опорного сигнала не кратна registerKind*bitsInByte_const
         ! и в последнем элементе массива содержиться битовый хвост
         ! нужно обрезать лишние отсчеты сигнала, загруженные выше
         IF(reference%trailLen/=0) THEN
            mask=0
         ! подготовка маски
         !  |63|62|61|60|...|2|1|0|
         !  | 1| 1| 1| 0|...|0|0|0|
         !  единицы должны быть в старших разрядах
         !
             DO i=0,reference%trailLen-1
                mask=IBSET(mask,(registerKind*bitsInByte_const-1)-i)
             END DO
!         !WRITE(*,*) 'mask ', mask
         !номер разряда регистра куда нужно задвигать бит ,в слуаче наличия хвоста
         ! например хвост - 3 бита, тогда задигать нужно в 8*8-1 -(3-1) = 63-2=61
            bitPushPos = INT((RegisterKind*bitsInByte_const-1)-(reference%trailLen-1),1)
!            !WRITE(*,*) 'bitPushPos ', bitPushPos
         END IF
         ! первая загрузка окна
         DO i=1,reference%signalSize
            window(i)=input%signal(i)
!            !WRITE(*,*) ' i ,input%signal(i)  ', i, input%signal(i)
         END DO
         ! номер отсчета ВКФ (взаимнокорреляционой функции)
         corrCnt=1
         !длительность опорного сигнала в отсчетах
         IF (input%signalSize==1) THEN
             referenceSignalLength =reference%trailLen
         ELSE                                                      ! - 1
             referenceSignalLength =(reference%signalSize)*(registerKind*bitsInByte_const)+reference%trailLen
         END IF

!         !WRITE(*,*) 'windoow '
!         !WRITE(*,fmt) (window)

          ! обработка индекса элемента знакового массива с хвостом и без
          ! установка начального значения индекса знакового массива со входным сигналом
          IF (reference%trailLen==0) THEN
               ! длительность опорного сигнала кратна RegisterKind*bitsInByte_const
               ! ergo в знаковом представлении он укладывается в целое число
               ! элементов массива
               ! указатель ПОСЛЕДНЕГО задвинутого бита (или ИЗВЛЕЧЕННОГО из ВХОДНОГО сигнала)
               ! regPtr =
               arrayPtr =  reference%signalSize
               ! последний задвинутый - НУЛЕВОЙ бит!
               regPtr   =  0
          ELSE
               ! trailLen [1:62]
               arrayPtr =  reference%signalSize
               ! старший - самый левый, иеет индекс 63
               regPtr   =  registerKind*bitsInByte_const -  reference%trailLen
          END IF




          !!WRITE(*,*) 'reference%signalSize = ',reference%signalSize
         ! i - индекс бита входного сигнала, так как окно уже загружен
         ! в конце основного цикла надо загрузить следующих бит  - поэтому +1
MAIN_CYCLE:  DO i=referenceSignalLength+1, rezSignalLength
                 ! Вычисление одного значения ВКФ
                 !!WRITE(*,*) 'i ',i
                 IF(reference%trailLen/=0) THEN

                    DO j=1,reference%signalSize-1
                       !!WRITE(*,*) j
                       result= NOT(XOR(window(j),reference%signal(j)))
                      ! WRITE(*,*) POPCNT(result) -(64-POPCNT(result))
                       summ = summ +  POPCNT(result) -(64-POPCNT(result))
                       !Correlate(corrCnt) = Correlate(corrCnt) + POPCNT(result) -(64-POPCNT(result))
                    END DO

                    Correlate(corrCnt)=summ
                    summ=0



                    ! последний элемент массива содержит хвост, его обработка ведется отдельно с маской
                    !!WRITE(*,*) 'И ПОСЛЕДНЕЕ'
                    !!WRITE(*,*) 'WINDOW ', window(reference%signalSize)
                    result = NOT(XOR(window(reference%signalSize),reference%signal(reference%signalSize)))
                    result = AND(result,mask)
                    !!WRITE(*,*) 'result ',result
                    Correlate(corrCnt) = Correlate(corrCnt) + POPCNT(result) -(reference%trailLen-POPCNT(result))
                   ! WRITE(*,*) POPCNT(result) -(reference%trailLen-POPCNT(result))
                     !!WRITE(*,*) 'DEBUG '
                 ELSE

            ! распараллелить нахуй!

                    DO j=1,reference%signalSize
                       result= NOT(XOR(window(j),reference%signal(j)))



                       summ = summ + POPCNT(result)&
                                           -(64-POPCNT(result))

                    END DO


                      Correlate(corrCnt)=summ
                    summ=0

                 END IF

                 !!WRITE(*,*) 'AND NOW '
                 !
                 ! вычисляем индекс массива входного сигнала
                 regPtr = regPtr - 1
                 IF (regPtr  < 0) THEN
                     regPtr   = 63
                     arrayPtr = arrayPtr + 1
                 END IF

                 IF (BTEST (input%signal(arrayPtr),regPtr)) THEN
                    extractedBit = 1
                 ELSE
                    extractedBit = 0
                 END IF
                 result = PushPopBitSignumArrayInt_8(window,extractedBit,bitPushPos)
                 !!WRITE(*,*) 'AND THEN '
                 corrCnt=corrCnt+1
                 !WRITE(*,*) 'corrCnt ',corrCnt,  omp_get_thread_num()
              END DO MAIN_CYCLE


              DEALLOCATE(window)
    END FUNCTION   Correlate

     ! ВЫХОДНАЯ РАЗРЯДНОСТЬ - 32 бита!!!!
      FUNCTION CorrelateOMP(input,reference)
         CLASS(signumSignal_t), INTENT(IN)  :: input
         CLASS(signumSignal_t), INTENT(IN)  :: reference

         INTEGER(4), allocatable :: CorrelateOMP(:)
         INTEGER(8)              :: rezSignalLength
         INTEGER(8)              :: referenceSignalLength
         INTEGER(8), allocatable :: window(:)
         INTEGER(8)              :: i,j,corrCnt
         INTEGER(8)              :: mask       = 0
         INTEGER(1)              :: registerKind
         INTEGER(8)              :: result
         INTEGER(1)              :: bitPushPos = 0
         INTEGER(8)              :: arrayPtr   = 0
         INTEGER(8)              :: regPtr     = 0
         INTEGER(1)              :: extractedBit = 0
         INTEGER(4)              :: summ=0
         CHARACTER(10)           :: fmt="(I64.1)"


         registerKind=KIND(input%signal)
         ! длительность сигнала ВКФ = длительного входного сигнала
         IF (input%signalSize==1) THEN
             rezSignalLength = input%trailLen
         ELSE                                    ! - 1
             rezSignalLength = (input%signalSize)*(registerKind*bitsInByte_const)+input%trailLen

         END IF

         !WRITE(*,*) 'корр  длина = ',rezSignalLength
         ALLOCATE(CorrelateOMP(1:rezSignalLength))
         CorrelateOMP=0
         ALLOCATE(window(1:reference%signalSize))
         ! если длина опорного сигнала не кратна registerKind*bitsInByte_const
         ! и в последнем элементе массива содержиться битовый хвост
         ! нужно обрезать лишние отсчеты сигнала, загруженные выше
         IF(reference%trailLen/=0) THEN
            mask=0
         ! подготовка маски
         !  |63|62|61|60|...|2|1|0|
         !  | 1| 1| 1| 0|...|0|0|0|
         !  единицы должны быть в старших разрядах
         !
             DO i=0,reference%trailLen-1
                mask=IBSET(mask,(registerKind*bitsInByte_const-1)-i)
             END DO
!         !WRITE(*,*) 'mask ', mask
         !номер разряда регистра куда нужно задвигать бит ,в слуаче наличия хвоста
         ! например хвост - 3 бита, тогда задигать нужно в 8*8-1 -(3-1) = 63-2=61
            bitPushPos = INT((RegisterKind*bitsInByte_const-1)-(reference%trailLen-1),1)
!            !WRITE(*,*) 'bitPushPos ', bitPushPos
         END IF
         ! первая загрузка окна
         DO i=1,reference%signalSize
            window(i)=input%signal(i)
!            !WRITE(*,*) ' i ,input%signal(i)  ', i, input%signal(i)
         END DO
         ! номер отсчета ВКФ (взаимнокорреляционой функции)
         corrCnt=1
         !длительность опорного сигнала в отсчетах
         IF (input%signalSize==1) THEN
             referenceSignalLength =reference%trailLen
         ELSE                                                      ! - 1
             referenceSignalLength =(reference%signalSize)*(registerKind*bitsInByte_const)+reference%trailLen
         END IF

!         !WRITE(*,*) 'windoow '
!         !WRITE(*,fmt) (window)

          ! обработка индекса элемента знакового массива с хвостом и без
          ! установка начального значения индекса знакового массива со входным сигналом
          IF (reference%trailLen==0) THEN
               ! длительность опорного сигнала кратна RegisterKind*bitsInByte_const
               ! ergo в знаковом представлении он укладывается в целое число
               ! элементов массива
               ! указатель ПОСЛЕДНЕГО задвинутого бита (или ИЗВЛЕЧЕННОГО из ВХОДНОГО сигнала)
               ! regPtr =
               arrayPtr =  reference%signalSize
               ! последний задвинутый - НУЛЕВОЙ бит!
               regPtr   =  0
          ELSE
               ! trailLen [1:62]
               arrayPtr =  reference%signalSize
               ! старший - самый левый, иеет индекс 63
               regPtr   =  registerKind*bitsInByte_const -  reference%trailLen
          END IF




          !!WRITE(*,*) 'reference%signalSize = ',reference%signalSize
         ! i - индекс бита входного сигнала, так как окно уже загружен
         ! в конце основного цикла надо загрузить следующих бит  - поэтому +1

!  !$omp parallel do SHARED(input,reference,window) PRIVATE(i,j,result) REDUCTION(+:summ)
MAIN_CYCLE:  DO i=referenceSignalLength+1, rezSignalLength
                 ! Вычисление одного значения ВКФ
                 !!WRITE(*,*) 'i ',i
                 IF(reference%trailLen/=0) THEN

                    DO j=1,reference%signalSize-1
                       !!WRITE(*,*) j
                       result= NOT(XOR(window(j),reference%signal(j)))
                      ! WRITE(*,*) POPCNT(result) -(64-POPCNT(result))
                       summ = summ +  POPCNT(result) -(64-POPCNT(result))
                       !Correlate(corrCnt) = Correlate(corrCnt) + POPCNT(result) -(64-POPCNT(result))
                    END DO

                    CorrelateOMP(corrCnt)=summ
                    summ=0
                    ! последний элемент массива содержит хвост, его обработка ведется отдельно с маской
                    !!WRITE(*,*) 'И ПОСЛЕДНЕЕ'
                    !!WRITE(*,*) 'WINDOW ', window(reference%signalSize)
                    result = NOT(XOR(window(reference%signalSize),reference%signal(reference%signalSize)))
                    result = AND(result,mask)
                    !!WRITE(*,*) 'result ',result
                    CorrelateOMP(corrCnt) = CorrelateOMP(corrCnt) + POPCNT(result) -(reference%trailLen-POPCNT(result))
                   ! WRITE(*,*) POPCNT(result) -(reference%trailLen-POPCNT(result))
                     !!WRITE(*,*) 'DEBUG '
                 ELSE

            ! распараллелить нахуй!

                    DO j=1,reference%signalSize
                       result= NOT(XOR(window(j),reference%signal(j)))



                       summ = summ + POPCNT(result)&
                                           -(64-POPCNT(result))

                    END DO


                      CorrelateOMP(corrCnt)=summ
                    summ=0

                 END IF

                 !!WRITE(*,*) 'AND NOW '
                 !
                 ! вычисляем индекс массива входного сигнала
                 regPtr = regPtr - 1
                 IF (regPtr  < 0) THEN
                     regPtr   = 63
                     arrayPtr = arrayPtr + 1
                 END IF

                 IF (BTEST (input%signal(arrayPtr),regPtr)) THEN
                    extractedBit = 1
                 ELSE
                    extractedBit = 0
                 END IF
                 result = PushPopBitSignumArrayInt_8(window,extractedBit,bitPushPos)
                 !!WRITE(*,*) 'AND THEN '
                 corrCnt=corrCnt+1
                 !WRITE(*,*) 'corrCnt ',corrCnt,  omp_get_thread_num()
              END DO MAIN_CYCLE
! !$omp end parallel do

              DEALLOCATE(window)

    END FUNCTION   CorrelateOMP

    SUBROUTINE destructor(this)
        TYPE(signumSignal_t), INTENT(INOUT) :: this
        INTEGER(4) :: stat
        DEALLOCATE(this%signal, STAT=stat)
        IF (STAT==0) THEN
            !WRITE(*,*) ' SIGNUM DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize, this%signalName
            this%isAllocated=.FALSE.
        ELSE
            IF(  (.NOT. ALLOCATED(this%signal)).AND.(.NOT.( this%isAllocated)   )  ) THEN
                    !WRITE(*,*) 'уже освободили память ',this%signalName
                ELSE
                    !WRITE(*,*) 'Не могу освободить память ',this%signalName
            END IF
        END IF
    END SUBROUTINE
END MODULE signumSignalModule
