    !======================================================
    !===== Модуль содержащий прозводный тип данных (КЛАСС) analyticSignal_t
    !          тип воплощает в себе аналитический сигнал
    !======================================================
    !
    ! Авторы:
    ! Татарчук И.А
    !======================================================


MODULE analyticSignalModule
    USE ModuleExitProg
    USE signumSignalModule
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
        PROCEDURE :: AssignDataFromAssignment
        ! Convolve -свертка сигналов analyticSignal_t
        PROCEDURE :: Convolve

        ! ConvolveRaw - свертка массивов int(8)
        ! эта функция не принимает полиморфную переменную типа analyticSignal_t
        PROCEDURE,NOPASS, PRIVATE :: CorrelationRaw
        !Арифметический сдвиг отсчетов сигнала в ПРАВО
        PROCEDURE :: RShift
        ! Вставка нулей в начало и конец сигнала (увеличивает его длину)
        PROCEDURE :: ZeroesStuffing
        PROCEDURE :: ConvolveSignum
         ! Convolve -свертка сигналов analyticSignal_t и signumSignalModule
        PROCEDURE :: ConvolveAnalyticSignalSignumSignal
        !Ивертирует сигнал (умножает на минус 1)
        PROCEDURE :: Invert
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
        !оператор СВЕРТКИ
        generic :: operator   (.CONV.) =>  Convolve
        generic :: operator   (.CONVSIGN.) =>  ConvolveSignum
        ! оператор свертки Аналитического и Знакового сигнала
        generic :: operator   (.CONVANALYTICSIGNUM.) =>  ConvolveAnalyticSignalSignumSignal
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
        INTEGER(4) :: stat
        CHARACTER(len=60) ::errorCode
        !  Если память под объект уже выделена?
        !  надо обнулить
!        IF ((ALLOCATED(this%signal)).AND.(this%isAllocated)) THEN
        IF ((ALLOCATED(this%signal))) THEN
           !WRITE(*,*) 'ПАмять уже выделена, обнуляю'
           DEALLOCATE(this%signal, STAT=stat)
           IF (STAT==0) THEN
               !WRITE(*,*) ' ANALYTIC SELF DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize, this%signalName
           END IF
           this%isAllocated=.FALSE.
           this%signalName=''
           this%signalSize= 0

        ELSE
           !WRITE(*,*) 'КАКОЙ_ТА БАЛАГАН!'
        END IF
        !и только потом выделять
        ! выделять нужно обязательно
!           WRITE(*,*) 'ANALYTIC CONSTRUCTOR WORKS!', this%signalName
          allocate (this%signal,source=loadedSignal,STAT=stat,ERRMSG = errorCode )
           IF (STAT/=0) THEN

               WRITE (*,*) 'Аналитич конструктор не смог выделить память,ERRMSG = ',errorCode

               CALL ExitFromProgramNormal()
           END IF

           this%isAllocated=.TRUE.
           this%signalSize=size(loadedSignal)
    END SUBROUTINE Constructor

     SUBROUTINE ExtractSignalData(this,extractedSignal)

        INTEGER(8),ALLOCATABLE, INTENT(INOUT) :: extractedSignal(:)
        CLASS(analyticSignal_t), INTENT(IN)  :: this

        !ЗАЩИТА
        IF (this%isAllocated) THEN
            ALLOCATE(extractedSignal,source=this%signal)
        ELSE
           WRITE (*,*) 'НЕ МОГУ ИЗВЛЕЧЬ ДАННЫЕ иЗ', this%signalName
        END IF

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

    SUBROUTINE SetName(this,signalName)

            CLASS(analyticSignal_t), INTENT(INOUT)  :: this
            CHARACTER(*),INTENT(IN)          :: signalName

            this%signalName=signalName

     END SUBROUTINE SetName

     FUNCTION MultiplyAnalyticSignals(xOp,yOp)
         CLASS(analyticSignal_t), INTENT(IN)  :: xOp
         CLASS(analyticSignal_t), INTENT(IN)  :: yOp
         CLASS(analyticSignal_t), allocatable ::MultiplyAnalyticSignals



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



            allocate(   AddAnalyticSignals)

            ! Вот тут конструктор копирования(=) не отрабаывает - не может взять размер и посавить статус выделения - почему??
            ! Хотя деструктор вызывается спокойно с нужными парамерами
            ! требуется расследование
            CALL AddAnalyticSignals%Setname('промежутсложение')
              AddAnalyticSignals%Signal=xOp%signal+yOp%signal
              AddAnalyticSignals%isAllocated=.TRUE.

     END FUNCTION   AddAnalyticSignals

    SUBROUTINE AssignDataFromAssignment(leftOp,rightOp)
        CLASS(analyticSignal_t), INTENT(INOUT)  :: leftOp
        CLASS(analyticSignal_t), INTENT(IN)     :: rightOp
        INTEGER(8),ALLOCATABLE :: extractedSignal(:)

        ! ЗАЩИТА
!        IF ((ALLOCATED(leftOp%signal)).AND.(leftOp%isAllocated)) THEN
!           !WRITE(*,*) 'ПАмять уже выделена, обнуляю'
!           DEALLOCATE(leftOp%signal)
!           leftOp%signalSize=0
!           leftOp%isAllocated=.FALSE.
!        ELSE
!           allocate (leftOp%signal,source=rightOp%signal)
!           leftOp%isAllocated=.TRUE.
!           leftOp%signalSize=size(rightOp%signal)
!        END IF
         CALL rightOp%ExtractSignalData(extractedSignal)
         CALL leftOp%Constructor(extractedSignal)
         DEALLOCATE(extractedSignal)

    END SUBROUTINE AssignDataFromAssignment

    ! функция реализующая вычисление корреляционной (или свертки)
    ! функции аналитических сигналов
    ! input - входной сигнал
    ! reference - опорный сигнал

    FUNCTION Convolve(input,reference)

         CLASS(analyticSignal_t), INTENT(IN)  :: input
         CLASS(analyticSignal_t), INTENT(IN)  :: reference
         CLASS(analyticSignal_t), allocatable :: convolve

         allocate(Convolve)
         convolve%signalName='fun name'
         ! ЗАщита
         IF (input%signalSize<reference%signalSize) THEN
              !WRITE(*,*) 'ОШИБКА Опорный сигнал  длительности > входящий'
              CALL ExitFromProgramNormal()
         ELSE
              CALL convolve%Constructor(CorrelationRaw(input%signal,reference%signal))
         END IF

    END FUNCTION   Convolve

    ! Вычисление корр. функции (Raw-  англ. сырая.)
    ! пределы [0 : длина входного сигнала- длина опорного сигнала]
     PURE FUNCTION CorrelationRaw(input,reference)
          INTEGER(8),INTENT(IN)   :: input(:),reference(:)
          INTEGER(8),ALLOCATABLE  :: CorrelationRaw(:)
          INTEGER(8)              :: i,j
          INTEGER(8)              :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw=0

          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw(i)=CorrelationRaw(i)+input(i+j)*reference(j)
                END DO
          END DO


    END FUNCTION   CorrelationRaw

    ! Вставка нулей в начало и конец сигнала (увеличивает его длину)
    ! before - число нулей до начала сигнала
    ! after - после начала сигнала
    SUBROUTINE ZeroesStuffing (this, beforeLen,afterLen)

        CLASS(analyticSignal_t), INTENT(INOUT)  :: this
        INTEGER(8),INTENT(IN)                   :: beforeLen
        INTEGER(8),INTENT(IN)                   :: afterLen
        !итоговая длина сигнала
        INTEGER(8)                              :: summLen
        INTEGER(8),ALLOCATABLE                  :: tempArray(:)
        INTEGER(8)                              ::allocationStatus

        !итоговая длина сигнала
        summLen = beforeLen + afterLen + this%signalSize
        ! проверка на максимальную длину массива
        IF(summLen>HUGE(0)) THEN
          !WRITE(*,*) 'Превышение максимальной длины массива'
          !WRITE(*,*) '(при наполнении нулями)'
          CALL ExitFromProgramNormal()
        END IF

        ALLOCATE(tempArray(1:summLen),stat=allocationStatus)
        IF (allocationStatus>0) THEN
          !WRITE(*,*) 'не могу выделить память tmpArray'
          CALL ExitFromProgramNormal()
        END IF

        tempArray=0
        tempArray(beforeLen+1:beforeLen+1+this%signalSize)=this%signal
        DEALLOCATE(this%signal)
        ALLOCATE(this%signal,source=tempArray,stat=allocationStatus)
        IF (allocationStatus>0) THEN
          !WRITE(*,*) 'не могу выделить память this%signal'
          CALL ExitFromProgramNormal()
        END IF
        DEALLOCATE(tempArray)


      END SUBROUTINE ZeroesStuffing

    ! ВЫполняет арифметический сдвиг вправо, для выбора старших разрядов сигнала
    ! shift - величина сдвига в отсчетах
    SUBROUTINE RShift(this,shift)
        CLASS(analyticSignal_t), INTENT(INOUT)  :: this
        INTEGER(1),INTENT(IN)                :: shift

        this%signal=SHIFTA( this%signal,shift)

    END SUBROUTINE RShift


     FUNCTION ConvolveSignum(input,reference)
         CLASS(analyticSignal_t), INTENT(IN)  :: input
         CLASS(analyticSignal_t), INTENT(IN)  :: reference
         CLASS(analyticSignal_t), ALLOCATABLE :: ConvolveSignum
         TYPE(signumSignal_t  )               :: inputSig
         TYPE(signumSignal_t  )               :: referenceSig
         INTEGER(8)                           :: status
         INTEGER(8), ALLOCATABLE              :: rez(:)


         allocate(ConvolveSignum,stat=status)
         ConvolveSignum%signalName='fun name ConvolveSignum'
         !WRITE (*,*)  'NAme = ', ConvolveSignum%signalName
         !WRITE (*,*)  'alloc status = ', status

         IF (input%signalSize<reference%signalSize) THEN
              !WRITE(*,*) 'ОШИБКА Опорный сигнал  длительности > входящий'
              CALL ExitFromProgramNormal()
         END IF

         CALL inputSig%Constructor(input%signal)
         CALL referenceSig%Constructor(reference%signal)
         !WRITE(*,*) 'Вычисляю знак корреляцию'
         rez = inputSig.CORR.referenceSig
         !WRITE(*,*) 'ВЫЗЫВАЮ Аналитич КОНТРСРКУТР'
         CALL ConvolveSignum%Constructor(rez)
         DEALLOCATE(rez)

    END FUNCTION ConvolveSignum

     FUNCTION ConvolveAnalyticSignalSignumSignal(input,reference)
         CLASS(analyticSignal_t), INTENT(IN)   :: input
         CLASS(signumSignal_t)  , INTENT(IN)   :: reference
         CLASS(analyticSignal_t), ALLOCATABLE  :: ConvolveAnalyticSignalSignumSignal
         TYPE(signumSignal_t  )                :: inputSig
         INTEGER(8)                            :: status
         INTEGER(8), ALLOCATABLE               :: rez(:)

         allocate(ConvolveAnalyticSignalSignumSignal,stat=status)
         ConvolveAnalyticSignalSignumSignal%signalName='fun name ConvolveSignum'
         !WRITE (*,*)  'NAme = ', ConvolveSignum%signalName
         !WRITE (*,*)  'alloc status = ', status

         CALL inputSig%Constructor(input%signal)

!          IF (inputSig%signalSize<reference%signalSize) THEN
!              !WRITE(*,*) 'ОШИБКА Опорный сигнал  длительности > входящий'
!              CALL ExitFromProgramNormal()
!         END IF

         !WRITE(*,*) 'Вычисляю знак корреляцию'
         rez = inputSig.CORR.reference
         !WRITE(*,*) 'ВЫЗЫВАЮ Аналитич КОНТРСРКУТР'
         CALL ConvolveAnalyticSignalSignumSignal%Constructor(rez)
         DEALLOCATE(rez)

    END FUNCTION ConvolveAnalyticSignalSignumSignal

    SUBROUTINE INVERT(this)
         CLASS(analyticSignal_t), INTENT(INOUT)   :: this
         this%signal =  - this%signal
    END  SUBROUTINE

    SUBROUTINE destructor(this)
        TYPE(analyticSignal_t), INTENT(INOUT) :: this
        INTEGER(4) :: stat
         CHARACTER(len=60) ::errorCode

        DEALLOCATE(this%signal, STAT=stat)
        IF (STAT==0) THEN
!            !WRITE(*,*) ' ANALYTIC DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize, this%signalName
            this%isAllocated=.FALSE.
        ELSE
            IF(  (.NOT. ALLOCATED(this%signal)).AND.(.NOT.( this%isAllocated)   )  ) THEN
                    !WRITE(*,*) 'уже освободили память ',this%signalName
                ELSE
                    WRITE(*,*) 'Не могу освободить память ',errorCode
            END IF
        END IF

    END SUBROUTINE

END MODULE analyticSignalModule



