MODULE OctetDataModule

    USE BitOpsMod
    USE SignumArrayMod
    USE MathConstModule
    USE ModuleExitProg
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: octetData_t
        PRIVATE
        ! массив с октетами информации
        INTEGER(1),ALLOCATABLE :: data(:)
        ! поле типа данных - isAllocated определяет выделена ли память под сигнал т.е проведена ли
        ! инициализация экземпляра обьекта
        LOGICAL                :: isAllocated=.FALSE.
        ! поле определяет длительность аналитического сигнала целых числах (размер числа в байтах
        ! определяется типом данных data(:) )
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
        FINAL                     :: destructor
        !операторы

    END TYPE octetData_t

CONTAINS
    ! ВНИМАНИЕ отсчеты ЗНАКОВОГО сигнала заносятя в старшие разряды!
    SUBROUTINE Constructor(this,loadedSignal)
        CLASS(octetData_t), INTENT(INOUT) :: this
        INTEGER(8), INTENT(IN) :: loadedSignal(:)
        INTEGER(4) :: stat
        INTEGER(8) :: length_new
        INTEGER(8) :: i,j,k
        INTEGER(1) :: registerKind

        IF (ALLOCATED(this%data)) THEN
           !!WRITE(*,*) 'ПАмять уже выделена, обнуляю'
           DEALLOCATE(this%data, STAT=stat)
           IF (STAT==0) THEN
               !WRITE(*,*) ' ANALYTIC SELF DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize, this%signalName
           END IF
           this%isAllocated=.FALSE.
           this%signalName=''
           this%signalSize= 0
        END IF
           !WRITE(*,*) 'SIGNUM CONSTRUCTOR WORKS!', this%signalName
        ! число байт в типе данных
        registerKind=KIND(this%data)
        length_new= size(loadedSignal)/(registerKind*bitsInByte_const)
        !если длина массива не кратна числу бит в целом типе данных, то надо еще одно число добавить
        this%trailLen = INT(MOD(size(loadedSignal),registerKind*bitsInByte_const),1)
        IF (this%trailLen>0) length_new=length_new + 1
        !WRITE(*,*) 'trail length = ',this%trailLen
        !WRITE(*,*) 'Новая длина = ',length_new
        allocate (this%data(1:length_new))
        !обязательно зануляем массив, что бы ставить только 1
        this%data=0
        k=0
        j=1
        DO i=1, size(loadedSignal)
           IF(k>((registerKind*bitsInByte_const)-1)) THEN
              k=0
              j=j+1
           END IF
           IF(loadedSignal(i)>=0) this%data(j)=IBSET(this%data(j),(registerKind*bitsInByte_const-1)-k)
           k=k+1
        END DO
        this%isAllocated=.TRUE.
        this%signalSize=length_new
   END SUBROUTINE Constructor
    
    SUBROUTINE ExtractSignalData(this,extractedSignal)
        INTEGER(1),ALLOCATABLE, INTENT(INOUT) :: extractedSignal(:)
        CLASS(octetData_t), INTENT(INOUT) :: this
        ALLOCATE(extractedSignal,source=this%data)
     END SUBROUTINE ExtractSignalData

    SUBROUTINE destructor(this)
        TYPE(octetData_t), INTENT(INOUT) :: this
        INTEGER(4) :: stat
        DEALLOCATE(this%data, STAT=stat)
        IF (STAT==0) THEN
            !WRITE(*,*) ' SIGNUM DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize, this%signalName
            this%isAllocated=.FALSE.
        ELSE
            IF(  (.NOT. ALLOCATED(this%data)).AND.(.NOT.( this%isAllocated)   )  ) THEN
                    !WRITE(*,*) 'уже освободили память ',this%signalName
                ELSE
                    !WRITE(*,*) 'Не могу освободить память ',this%signalName
            END IF
        END IF
    END SUBROUTINE

END MODULE OctetDataModule
