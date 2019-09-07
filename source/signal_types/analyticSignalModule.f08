!======================================================
    !===== Модуль содержащий прозводный тип данных (КЛАСС) analyticSignal_t
    !          тип воплощает в себе аналитический сигнал
    !======================================================
    !
    ! Авторы:
    ! Татарчук И.А
    !======================================================


MODULE analyticSignalModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: analyticSignal_t

        PRIVATE

        ! отсчеты аналитического сингнала содержаться в динамеческом массиве
        INTEGER(8),ALLOCATABLE :: signal(:)

        ! поле типа данных - isAllocated определяет выделена ли память под сигнал т.е проведена ли
        ! инициализация экземпляра обьекта
        LOGICAL                :: isAllocated=.FALSE.

        ! поле определяет длительность аналитического сигнала в отсчетах
        INTEGER(8)             :: signalSize= 0

        ! Поле, определяющее имя сигнала
        ! ЖЕЛАТЕЛЬНО добавлять расширение к имени
        CHARACTER(50)         :: signalName=''

    CONTAINS
        !== далее идет список членов функций класса - методов ,
        !  в термионологии Фортрана - привязанных к типу процедур (type bound procedures)


        ! член функция (метода) типа конструктор
        ! только эта функция может выделить память для signal(:)
        PROCEDURE Constructor

        ! извлчение отсчетов сигнала во внешений массив
        PROCEDURE ExtractSignalData

        ! Получить сведения о том выделена ли память под сигнал или нет
        PROCEDURE GetAllocationStatus

        ! Получить значение  длительность сигнла
        PROCEDURE GetSignalSize

        ! Установить значение поля signalName
        PROCEDURE SetName


        ! методы, описывающие такие операции как умножение, вычитание, сложение и присваивание
        ! соотвественно для типа данных analyticSignal_t

        ! Multiply - умножение
        PROCEDURE :: MultiplyAnalyticSignals
        ! Substruct - вычитание
        PROCEDURE :: SubtractAnalyticSignals
        !ADD  - сложение
        PROCEDURE :: AddAnalyticSignals
        ! Assignment - присвание
        PROCEDURE AssignDataFromAssignment

        ! далее выполняется перегрузка операторов
        ! умножения, вычитания, сложения и присваивания
        ! для типа данных analyticSignal_t
        ! перегрузка осуществляться через идентификатор generic :: operator


        generic :: operator   (*) =>  MultiplyAnalyticSignals
        generic :: operator   (-) =>  SubtractAnalyticSignals
        generic :: operator   (+) =>   AddAnalyticSignals

        ! перегрузка оператора присвания выполняется чуть иначе
        !https://stackoverflow.com/questions/19064132/nested-derived-type-with-overloaded-assignment
        !https://stackoverflow.com/questions/19111471/fortran-derived-type-assignment
        generic :: assignment (=) =>  AssignDataFromAssignment

        ! Финализирующый метод класс (так же - деструктор)
        ! должен освободить память, занятую массивом  signal(:)
        FINAL :: destructor

    END TYPE analyticSignal_t


CONTAINS

    ! конструктор класса (типа analyticSignal_t)
    ! конструктор принимает ПОЛИМОРФНУЮ Переменную (идентификатор CLASS) типа analyticSignal_t
    SUBROUTINE Constructor(this,loadedSignal)

        CLASS(analyticSignal_t), INTENT(INOUT)  :: this
        INTEGER(8), INTENT(IN) :: loadedSignal(:)

        INTEGER(8) :: fileSize

        !что если обьект уже проинициализирован - проверить!!!
        fileSize=size(loadedSignal)
        this%signalSize=fileSize
        ALLOCATE( this%signal, source=loadedSignal)
        this%isAllocated=.TRUE.
        WRITE(*,*) 'ANALYTIC CONSTRUCTOR WORKS!'

    END SUBROUTINE Constructor

     SUBROUTINE ExtractSignalData(this,extractedSignal)

        INTEGER(8),ALLOCATABLE, INTENT(INOUT) :: extractedSignal(:)
        CLASS(analyticSignal_t), INTENT(IN)  :: this

        !ЗАЩИТА
        ALLOCATE(extractedSignal,source=this%signal)


     END SUBROUTINE ExtractSignalData


     FUNCTION GetAllocationStatus(this) RESULT(stat)
        CLASS(analyticSignal_t), INTENT(IN)  :: this
        LOGICAL :: stat

        stat = this%isAllocated
     END FUNCTION GetAllocationStatus

     FUNCTION GetSignalSize(this) RESULT( signalSize)
        CLASS(analyticSignal_t), INTENT(IN)  :: this
        INTEGER(8) :: signalSize

         signalSize = this%signalSize
     END FUNCTION GetSignalSize


    SUBROUTINE AssignDataFromAssignment(leftOp,rightOp)
        CLASS(analyticSignal_t), INTENT(INOUT)  :: leftOp
        CLASS(analyticSignal_t), INTENT(IN)     :: rightOp

        ! ЗАЩИТА

        allocate (leftOp%signal,source=rightOp%signal)
        leftOp%isAllocated=.TRUE.
        leftOp%signalSize=size(rightOp%signal)

    END SUBROUTINE AssignDataFromAssignment


     SUBROUTINE SetName(this,signalName)

            CLASS(analyticSignal_t), INTENT(INOUT)  :: this
            CHARACTER(*),INTENT(IN)          :: signalName

            this%signalName=signalName

     END SUBROUTINE SetName

     FUNCTION MultiplyAnalyticSignals(xOp,yOp)
         CLASS(analyticSignal_t), INTENT(IN)  :: xOp
         CLASS(analyticSignal_t), INTENT(IN)  :: yOp
         CLASS(analyticSignal_t), allocatable ::MultiplyAnalyticSignals


            !r%signal=xOp%signal*yOp%signal
            allocate( MultiplyAnalyticSignals)

            ! Вот тут конструктор копирования(=) не отрабаывает - не может взять размер и посавить статус выделения - почему??
            ! Хотя деструктор вызывается спокойно с нужными парамерами
            ! требуется расследование
            MultiplyAnalyticSignals%Signal=xOp%signal*yOp%signal
            CALL MultiplyAnalyticSignals%Setname('промежут умножение')
            MultiplyAnalyticSignals%isAllocated=.TRUE.

     END FUNCTION MultiplyAnalyticSignals

     FUNCTION SubtractAnalyticSignals(xOp,yOp)
         CLASS(analyticSignal_t), INTENT(IN)  :: xOp
         CLASS(analyticSignal_t), INTENT(IN)  :: yOp
         CLASS(analyticSignal_t), allocatable ::SubtractAnalyticSignals


            !r%signal=xOp%signal*yOp%signal
            allocate( SubtractAnalyticSignals)

            ! Вот тут конструктор копирования(=) не отрабаывает - не может взять размер и посавить статус выделения - почему??
            ! Хотя деструктор вызывается спокойно с нужными парамерами
            ! требуется расследование
            CALL SubtractAnalyticSignals%Setname('промежут вычитание')
            SubtractAnalyticSignals%Signal=xOp%signal-yOp%signal
            SubtractAnalyticSignals%isAllocated=.TRUE.

     END FUNCTION SubtractAnalyticSignals

      FUNCTION   AddAnalyticSignals(xOp,yOp)
         CLASS(analyticSignal_t), INTENT(IN)  :: xOp
         CLASS(analyticSignal_t), INTENT(IN)  :: yOp
         CLASS(analyticSignal_t), allocatable :: AddAnalyticSignals


            !r%signal=xOp%signal*yOp%signal
            allocate(   AddAnalyticSignals)

            ! Вот тут конструктор копирования(=) не отрабаывает - не может взять размер и посавить статус выделения - почему??
            ! Хотя деструктор вызывается спокойно с нужными парамерами
            ! требуется расследование
            CALL AddAnalyticSignals%Setname('промежутсложение')
              AddAnalyticSignals%Signal=xOp%signal+yOp%signal
              AddAnalyticSignals%isAllocated=.TRUE.

     END FUNCTION   AddAnalyticSignals



    SUBROUTINE destructor(this)
        TYPE(analyticSignal_t), INTENT(INOUT) :: this
        INTEGER(4) :: stat

        DEALLOCATE(this%signal, STAT=stat)
        IF (STAT==0) THEN
            WRITE(*,*) ' ANALYTIC DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize, this%signalName
            this%isAllocated=.FALSE.
        ELSE
            IF(  (.NOT. ALLOCATED(this%signal)).AND.(.NOT.( this%isAllocated)   )  ) THEN
                    WRITE(*,*) 'уже освободили память ',this%signalName

                ELSE

                    WRITE(*,*) 'Не могу освободить память ',this%signalName
            END IF


        END IF




    END SUBROUTINE

END MODULE analyticSignalModule



