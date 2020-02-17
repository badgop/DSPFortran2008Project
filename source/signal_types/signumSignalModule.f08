MODULE signumSignalModule
    USE analyticSignalModule
    USE SIGNUM_FUNC
    USE BitOpsMod
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
        PROCEDURE                 :: Constructor
        PROCEDURE                 :: ExtractSignalData
        PROCEDURE                 :: Correlate
        FINAL                     :: destructor

        generic :: operator   (.CORR.) =>  Correlate

    END TYPE signumSignal_t

CONTAINS

    SUBROUTINE Constructor(this,loadedSignal)
        CLASS(signumSignal_t), INTENT(INOUT) :: this
        INTEGER(8), INTENT(IN) :: loadedSignal(:)
        INTEGER(4) :: stat
        INTEGER(8) :: length_new
        INTEGER(8) :: i,j,k
        INTEGER(1) :: registerKind
        INTEGER(1),PARAMETER :: bitsInByte=8

        IF ((ALLOCATED(this%signal)).AND.(this%isAllocated)) THEN
!           WRITE(*,*) 'ПАмять уже выделена, обнуляю'
           DEALLOCATE(this%signal, STAT=stat)
!           IF (STAT==0) THEN
!               WRITE(*,*) ' ANALYTIC SELF DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize, this%signalName
!           END IF
           this%isAllocated=.FALSE.
           this%signalName=''
           this%signalSize= 0
        END IF
!           WRITE(*,*) 'ANALYTIC CONSTRUCTOR WORKS!', this%signalName
        ! число байт в типе данных
        registerKind=KIND(this%signal)
        length_new= size(loadedSignal)/(registerKind*bitsInByte)
        !если длина массива не кратна числу бит в целом типе данных, то надо еще одно число добавить
        this%trailLen = MOD(size(loadedSignal),registerKind*bitsInByte)
        IF (this%trailLen>0) length_new=length_new+1
!        WRITE(*,*) 'trail length = ',this%trailLen
!        WRITE(*,*) 'Новая длина = ',length_new
        allocate (this%signal(1:length_new))
        !обязательно зануляем массив, что бы ставить только 1
        this%signal=0
        k=0
        j=1
        DO i=1, size(loadedSignal)
           IF(k>((registerKind*bitsInByte)-1)) THEN
              k=0
              j=j+1
           END IF
           IF(loadedSignal(i)>=0) this%signal(j)=IBSET(this%signal(j),(registerKind*bitsInByte-1)-k)
           k=k+1
        END DO
        this%isAllocated=.TRUE.
        this%signalSize=length_new
    END SUBROUTINE Constructor
    
    SUBROUTINE ExtractSignalData(this,extractedSignal)
        INTEGER(8),ALLOCATABLE, INTENT(INOUT) :: extractedSignal(:)
        CLASS(signumSignal_t), INTENT(INOUT) :: this
        ALLOCATE(extractedSignal,source=this%signal)
     END SUBROUTINE ExtractSignalData

      FUNCTION Correlate(input,reference)

         CLASS(signumSignal_t), INTENT(IN)  :: input
         CLASS(signumSignal_t), INTENT(IN)  :: reference
         INTEGER(8), allocatable :: Correlate(:)

         INTEGER(8)              :: rezSignalLength
         INTEGER(8), allocatable :: window(:)
         INTEGER(8)              :: i,cnt
         INTEGER(8)              :: mask=0
         INTEGER(1),PARAMETER    :: bitsInByte=8
         INTEGER(1)              :: registerKind
         INTEGER(8)              :: result


         rezSignalLength = input%signalSize*(KIND(input%signal))+input%trailLen
         ALLOCATE(Correlate(1:rezSignalLength))
         Correlate=0
         ALLOCATE(window(1:reference%signalSize))

         registerKind=KIND(input%signal)


           ! если длина опорного сигнала не кратна registerKind*bitsInByte
         ! и в последнем элементе массива содержиться битовый хвост
         ! нужно обрезать лишние отсчеты сигнала, загржуенные вышеы
         IF(reference%trailLen/=0) THEN
         mask=0
         ! подготовка маски
            DO i=0,reference%trailLen-1
               mask=IBSET(mask,i)
            END DO
            !mask=SHIFTL(mask,((registerKind*bitsInByte-1)-(reference%trailLen-1)))
            mask=NOT(mask)
         END IF


         ! первая загрузка окна
         cnt=1
         DO i=1,reference%signalSize
            window(i)=input%signal(i)
         END DO

         !!!ВСТАВИТЬ ЦИКЛ ДАЛЬШЕ
         ! задвигать по одному отсчету
         IF(reference%trailLen/=0) THEN
            DO i=1,reference%signalSize-1
               result= NOT(XOR(window(i),reference%signal(i)))
               Correlate(cnt) = Correlate(cnt) + SumOnesInInt_8(result)
            END DO
            result= NOT(XOR(window(reference%signalSize),reference%signal(reference%signalSize)))
            result=AND(result,mask)
            Correlate(cnt) = Correlate(cnt) + SumOnesInInt_8(result)
         ELSE
            DO i=1,reference%signalSize
               result= NOT(XOR(window(i),reference%signal(i)))
               Correlate(cnt) = Correlate(cnt) + SumOnesInInt_8(result)
            END DO
         END IF




    END FUNCTION   Correlate



    SUBROUTINE destructor(this)
        TYPE(signumSignal_t), INTENT(INOUT) :: this
        INTEGER(4) :: stat

        DEALLOCATE(this%signal, STAT=stat)
        IF (STAT==0) THEN
!            WRITE(*,*) ' ANALYTIC DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize, this%signalName
            this%isAllocated=.FALSE.
        ELSE
            IF(  (.NOT. ALLOCATED(this%signal)).AND.(.NOT.( this%isAllocated)   )  ) THEN
                    WRITE(*,*) 'уже освободили память ',this%signalName
                ELSE
                    WRITE(*,*) 'Не могу освободить память ',this%signalName
            END IF
        END IF
    END SUBROUTINE
END MODULE signumSignalModule
