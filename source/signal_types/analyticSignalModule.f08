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
    USE ModuleExitProg
    USE MathConstModule
    USE RawCorrMod
    USE ClippingMode
    USE ArrayFunctionsMod
    IMPLICIT NONE

        INTERFACE
            INTEGER FUNCTION OMP_GET_THREAD_NUM()
            END FUNCTION
    END INTERFACE

    PRIVATE

    TYPE, PUBLIC :: analyticSignal_t

        PRIVATE
        ! отсчеты аналитического сингнала содержаться в динамеческом массиве
        INTEGER(8),ALLOCATABLE :: signalInt8(:)
        INTEGER(4),ALLOCATABLE :: signalInt4(:)
        INTEGER(2),ALLOCATABLE :: signalInt2(:)
        INTEGER(1),ALLOCATABLE :: signalInt1(:)
        INTEGER(1)             :: signalArrayKind = 0
        ! поле типа данных - isAllocated определяет выделена ли память под сигнал т.е проведена ли
        ! инициализация экземпляра обьекта
        LOGICAL                :: isAllocated=.FALSE.
        ! поле определяет длительность аналитического сигнала в отсчетах
        INTEGER(8)             :: signalSize= 0
        INTEGER(8)             :: indexPtr = 0
        ! Поле, определяющее имя сигнала
        ! ЖЕЛАТЕЛЬНО добавлять расширение к имени
        CHARACTER(50)         :: signalName=''

    CONTAINS
        !== далее идет список членов функций класса - методов ,
        !  в термионологии Фортрана - привязанных к типу процедур (type bound procedures)

        ! член функция (метода) типа конструктор
        ! только эта функция может выделить память для signal(:)
        PROCEDURE ConstructorInt1
        PROCEDURE ConstructorInt2
        PROCEDURE ConstructorInt4
        PROCEDURE ConstructorInt8

        GENERIC :: Constructor       => ConstructorInt1,ConstructorInt2,ConstructorInt4,ConstructorInt8

        ! извлчение отсчетов сигнала во внешений массив
        PROCEDURE ExtractSignalData1
        PROCEDURE ExtractSignalData2
        PROCEDURE ExtractSignalData4
        PROCEDURE ExtractSignalData8

        GENERIC :: ExtractSignalData => ExtractSignalData1,ExtractSignalData2,ExtractSignalData4,ExtractSignalData8


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

             !Арифметический сдвиг отсчетов сигнала в ПРАВО
        PROCEDURE :: RShift
        ! Вставка нулей в начало и конец сигнала (увеличивает его длину)
        PROCEDURE :: ZeroesStuffing
        PROCEDURE :: ConvolveSignum
         ! Convolve -свертка сигналов analyticSignal_t и signumSignalModule
        PROCEDURE :: ConvolveAnalyticSignalSignumSignal
        ! умножить Аналтич сигнал на ИНТ8
        PROCEDURE :: MultiplyAnalyticSignalsAndConstant8
        ! Здесь надо указать в качестве какого аргумента передается полиморфная переменная
        ! т.к по умолчанию она передается первым аргументом
        ! умножить  ИНТ8  на Аналтич сигнал на
        PROCEDURE,PASS(yOp) :: Multiplydonstant8AndAnalyticSignals
        ! простая децимация
        PROCEDURE :: Decimate

        ! далее выполняется перегрузка операторов
        ! умножения, вычитания, сложения и присваивания
        ! для типа данных analyticSignal_t
        ! перегрузка осуществляться через идентификатор generic :: operator
        generic :: operator   (*) =>  MultiplyAnalyticSignals,MultiplyAnalyticSignalsAndConstant8&
                                     ,Multiplydonstant8AndAnalyticSignals
        generic :: operator   (-) =>  SubtractAnalyticSignals
        generic :: operator   (+) =>   AddAnalyticSignals
        ! перегрузка оператора присвания выполняется чуть иначе
        !https://stackoverflow.com/questions/19064132/nested-derived-type-with-overloaded-assignment
        !https://stackoverflow.com/questions/19111471/fortran-derived-type-assignment
        generic :: assignment (=) =>  AssignDataFromAssignment
        !оператор СВЕРТКИ
        ! signal = INPUT.CONV.REFERENCE
        generic :: operator   (.CONV.) =>  Convolve
        ! оператор знаковой СВЕРТКИ
        ! signal = INPUT.CONV.REFERENCE  где REFERENCE - analyticSignal :: ConvolveSignum
        ! signal = INPUT.CONV.REFERENCE  где REFERENCE - signumSignal :: ConvolveAnalyticSignalSignumSignal
        generic :: operator   (.CONVSIGN.) =>  ConvolveSignum,ConvolveAnalyticSignalSignumSignal
        ! Финализирующый метод класс (так же - деструктор)
        ! должен освободить память, занятую массивом  signal(:)
        PROCEDURE GetMAx
        PROCEDURE GetMin
        PROCEDURE GetValue
        PROCEDURE GetMaxAbs
        PROCEDURE GetSignalKind
        ! выполняет функции ограничения сигнала по уровню
        PROCEDURE :: ClipSignal
        PROCEDURE :: MirorReflectSignal
        FINAL :: destructor

    END TYPE analyticSignal_t


CONTAINS

    ! конструктор класса (типа analyticSignal_t)
    ! конструктор принимает ПОЛИМОРФНУЮ Переменную (идентификатор CLASS) типа analyticSignal_t
    SUBROUTINE ConstructorInt1(this,loadedSignal)
        CLASS(analyticSignal_t), INTENT(INOUT)  :: this
        INTEGER(1),PARAMETER                  :: arrayKind=1
        INTEGER(arrayKind), INTENT(IN) :: loadedSignal(:)
        INTEGER(4) :: stat
        CHARACTER(len=60) ::errorCode
        !  Если память под объект уже выделена?
        !  надо обнулить
!        IF ((ALLOCATED(this%signal)).AND.(this%isAllocated)) THEN
        IF ((ALLOCATED(this%signalInt1))) THEN
           !WRITE(*,*) 'ПАмять уже выделена, обнуляю'
           DEALLOCATE(this%signalInt1, STAT=stat)
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
           WRITE(*,*) 'ANALYTIC CONSTRUCTOR WORKS INT1 !', this%signalName
          allocate (this%signalInt1,source=loadedSignal,STAT=stat,ERRMSG = errorCode )
           IF (STAT/=0) THEN

               WRITE (*,*) 'Аналитич конструктор не смог выделить память,ERRMSG = ',errorCode

               CALL ExitFromProgramNormal()
           END IF

           this%isAllocated=.TRUE.
           this%signalSize=size(loadedSignal)
           this%signalArrayKind = arrayKind
    END SUBROUTINE ConstructorInt1


    SUBROUTINE ConstructorInt2(this,loadedSignal)
        CLASS(analyticSignal_t), INTENT(INOUT)  :: this
        INTEGER(1),PARAMETER                  :: arrayKind=2
        INTEGER(arrayKind), INTENT(IN) :: loadedSignal(:)
        INTEGER(4) :: stat
        CHARACTER(len=60) ::errorCode
        !  Если память под объект уже выделена?
        !  надо обнулить
!        IF ((ALLOCATED(this%signal)).AND.(this%isAllocated)) THEN
        IF ((ALLOCATED(this%signalInt2))) THEN
           !WRITE(*,*) 'ПАмять уже выделена, обнуляю'
           DEALLOCATE(this%signalInt2, STAT=stat)
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
          ! WRITE(*,*) 'ANALYTIC CONSTRUCTOR WORKS INT2!', this%signalName
          allocate (this%signalInt2,source=loadedSignal,STAT=stat,ERRMSG = errorCode )
           IF (STAT/=0) THEN
               WRITE (*,*) 'Аналитич конструктор не смог выделить память,ERRMSG = ',errorCode
               CALL ExitFromProgramNormal()
           END IF
           this%isAllocated=.TRUE.
           this%signalSize=size(this%signalInt2)
           this%signalArrayKind = arrayKind
!           WRITE(*,'(I20)') this%signalInt2
!           WRITE(*,*) 'COnstruvctor 2 size ', this%signalSize
!           WRITE(*,*) 'real ', size(this%signalInt2)
    END SUBROUTINE ConstructorInt2


    SUBROUTINE ConstructorInt4(this,loadedSignal)
        CLASS(analyticSignal_t), INTENT(INOUT)  :: this
        INTEGER(1),PARAMETER                  :: arrayKind=4
        INTEGER(arrayKind), INTENT(IN) :: loadedSignal(:)
        INTEGER(4) :: stat
        CHARACTER(len=60) ::errorCode
        !  Если память под объект уже выделена?
        !  надо обнулить
!        IF ((ALLOCATED(this%signal)).AND.(this%isAllocated)) THEN
        IF ((ALLOCATED(this%signalInt4))) THEN
           !WRITE(*,*) 'ПАмять уже выделена, обнуляю'
           DEALLOCATE(this%signalInt4, STAT=stat)
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
         !  WRITE(*,*) 'ANALYTIC CONSTRUCTOR WORKS! INT4', this%signalName
          allocate (this%signalInt4,source=loadedSignal,STAT=stat,ERRMSG = errorCode )
           IF (STAT/=0) THEN

               WRITE (*,*) 'Аналитич конструктор не смог выделить память,ERRMSG = ',errorCode

               CALL ExitFromProgramNormal()
           END IF

           this%isAllocated=.TRUE.
           this%signalSize=size(loadedSignal)
           this%signalArrayKind = arrayKind
    END SUBROUTINE ConstructorInt4

    SUBROUTINE ConstructorInt8(this,loadedSignal)
        CLASS(analyticSignal_t), INTENT(INOUT)  :: this
        INTEGER(1),PARAMETER                  :: arrayKind=8
        INTEGER(arrayKind), INTENT(IN) :: loadedSignal(:)
        INTEGER(4) :: stat
        CHARACTER(len=60) ::errorCode
        !  Если память под объект уже выделена?
        !  надо обнулить
!        IF ((ALLOCATED(this%signal)).AND.(this%isAllocated)) THEN
        IF ((ALLOCATED(this%signalInt8))) THEN
           !WRITE(*,*) 'ПАмять уже выделена, обнуляю'
           DEALLOCATE(this%signalInt8, STAT=stat)
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
!           WRITE(*,*) 'ANALYTIC CONSTRUCTOR WORKS! INT8', this%signalName
          allocate (this%signalInt8,source=loadedSignal,STAT=stat,ERRMSG = errorCode )
           IF (STAT/=0) THEN

               WRITE (*,*) 'Аналитич конструктор не смог выделить память,ERRMSG = ',errorCode

               CALL ExitFromProgramNormal()
           END IF

           this%isAllocated=.TRUE.
           this%signalSize=size(loadedSignal)
           this%signalArrayKind = arrayKind
    END SUBROUTINE ConstructorInt8

     SUBROUTINE ExtractSignalData1(this,extractedSignal)
        INTEGER(1),PARAMETER                  :: arrayKind=1
        INTEGER(arrayKind),ALLOCATABLE, INTENT(INOUT) :: extractedSignal(:)
        CLASS(analyticSignal_t), INTENT(IN)  :: this
        INTEGER(4)                           :: stat
        CHARACTER(len=60) ::errorCode

        !ЗАЩИТА
        IF (this%isAllocated) THEN

             SELECT CASE(this%signalArrayKind)
                  CASE(1)
                  ALLOCATE(extractedSignal,source=this%signalInt1,STAT=stat,ERRMSG = errorCode)
                  IF (stat/=0) THEN
                       WRITE (*,*) 'НЕ могу выделить память ExtractSignalData1 ,ERRMSG = ',errorCode
                     CALL ExitFromProgramNormal()
                   END IF
                CASE(2)
                  ALLOCATE(extractedSignal,source=int(this%signalInt2,1),STAT=stat,ERRMSG = errorCode)
                  IF (stat/=0) THEN
                       WRITE (*,*) 'НЕ могу выделить память ExtractSignalData1 ,ERRMSG = ',errorCode
                     CALL ExitFromProgramNormal()
                   END IF
                CASE(4)
                   ALLOCATE(extractedSignal,source=int(this%signalInt4,1),STAT=stat,ERRMSG = errorCode)
                  IF (stat/=0) THEN
                       WRITE (*,*) 'НЕ могу выделить память ExtractSignalData1 ,ERRMSG = ',errorCode
                     CALL ExitFromProgramNormal()
                   END IF
                CASE(8)
                   ALLOCATE(extractedSignal,source=int(this%signalInt8,1),STAT=stat,ERRMSG = errorCode)
                  IF (stat/=0) THEN
                       WRITE (*,*) 'НЕ могу выделить память ExtractSignalData1 ,ERRMSG = ',errorCode
                     CALL ExitFromProgramNormal()
                   END IF
            END SELECT


        ELSE
                 WRITE (*,*) 'НЕ МОГУ ИЗВЛЕЧЬ ДАННЫЕ INT1', this%signalName
                 CALL ExitFromProgramNormal()
        END IF
     END SUBROUTINE ExtractSignalData1

       SUBROUTINE ExtractSignalData2(this,extractedSignal)
        INTEGER(1),PARAMETER                  :: arrayKind=2
        INTEGER(arrayKind),ALLOCATABLE, INTENT(INOUT) :: extractedSignal(:)
        CLASS(analyticSignal_t), INTENT(IN)  :: this
        INTEGER(4)                           :: stat
        CHARACTER(len=60) ::errorCode

        !ЗАЩИТА
        IF (this%isAllocated) THEN
            SELECT CASE(this%signalArrayKind)
                  CASE(1)
                  ALLOCATE(extractedSignal,source=int(this%signalInt1,2),STAT=stat,ERRMSG = errorCode)
                  IF (stat/=0) THEN
                       WRITE (*,*) 'НЕ могу выделить память ExtractSignalData1 ,ERRMSG = ',errorCode
                     CALL ExitFromProgramNormal()
                   END IF
                CASE(2)
                  ALLOCATE(extractedSignal,source=this%signalInt2,STAT=stat,ERRMSG = errorCode)
                  IF (stat/=0) THEN
                       WRITE (*,*) 'НЕ могу выделить память ExtractSignalData1 ,ERRMSG = ',errorCode
                     CALL ExitFromProgramNormal()
                   END IF
                CASE(4)
                   ALLOCATE(extractedSignal,source=int(this%signalInt4,2),STAT=stat,ERRMSG = errorCode)
                  IF (stat/=0) THEN
                       WRITE (*,*) 'НЕ могу выделить память ExtractSignalData1 ,ERRMSG = ',errorCode
                     CALL ExitFromProgramNormal()
                   END IF
                CASE(8)
                   ALLOCATE(extractedSignal,source=int(this%signalInt8,2),STAT=stat,ERRMSG = errorCode)
                  IF (stat/=0) THEN
                       WRITE (*,*) 'НЕ могу выделить память ExtractSignalData1 ,ERRMSG = ',errorCode
                     CALL ExitFromProgramNormal()
                   END IF
            END SELECT
        ELSE
           WRITE (*,*) 'НЕ МОГУ ИЗВЛЕЧЬ ДАННЫЕ INT2', this%signalName
           CALL ExitFromProgramNormal()
        END IF
     END SUBROUTINE ExtractSignalData2

     SUBROUTINE ExtractSignalData4(this,extractedSignal)
        INTEGER(1),PARAMETER                  :: arrayKind=4
        INTEGER(arrayKind),ALLOCATABLE, INTENT(INOUT) :: extractedSignal(:)
        CLASS(analyticSignal_t), INTENT(IN)  :: this
        INTEGER(4)                           :: stat
        CHARACTER(len=60) ::errorCode

        !ЗАЩИТА
        IF (this%isAllocated) THEN
               SELECT CASE(this%signalArrayKind)
                  CASE(1)
                  ALLOCATE(extractedSignal,source=int(this%signalInt1,4),STAT=stat,ERRMSG = errorCode)
                  IF (stat/=0) THEN
                       WRITE (*,*) 'НЕ могу выделить память ExtractSignalData1 ,ERRMSG = ',errorCode
                     CALL ExitFromProgramNormal()
                   END IF
                CASE(2)
                  ALLOCATE(extractedSignal,source=int(this%signalInt2,4),STAT=stat,ERRMSG = errorCode)
                  IF (stat/=0) THEN
                       WRITE (*,*) 'НЕ могу выделить память ExtractSignalData1 ,ERRMSG = ',errorCode
                     CALL ExitFromProgramNormal()
                   END IF
                CASE(4)
                   ALLOCATE(extractedSignal,source=this%signalInt4,STAT=stat,ERRMSG = errorCode)
                  IF (stat/=0) THEN
                       WRITE (*,*) 'НЕ могу выделить память ExtractSignalData1 ,ERRMSG = ',errorCode
                     CALL ExitFromProgramNormal()
                   END IF
                CASE(8)
                   ALLOCATE(extractedSignal,source=int(this%signalInt8,4),STAT=stat,ERRMSG = errorCode)
                  IF (stat/=0) THEN
                       WRITE (*,*) 'НЕ могу выделить память ExtractSignalData1 ,ERRMSG = ',errorCode
                     CALL ExitFromProgramNormal()
                   END IF
            END SELECT
        ELSE
           WRITE (*,*) 'НЕ МОГУ ИЗВЛЕЧЬ ДАННЫЕ INT4', this%signalName
           CALL ExitFromProgramNormal()
        END IF
     END SUBROUTINE ExtractSignalData4

     SUBROUTINE ExtractSignalData8(this,extractedSignal)
        INTEGER(1),PARAMETER                  :: arrayKind=8
        INTEGER(arrayKind),ALLOCATABLE, INTENT(INOUT) :: extractedSignal(:)
        CLASS(analyticSignal_t), INTENT(IN)  :: this
         INTEGER(4)                           :: stat
        CHARACTER(len=60) ::errorCode

        !ЗАЩИТА
        IF (this%isAllocated) THEN
                       SELECT CASE(this%signalArrayKind)
                  CASE(1)
                  ALLOCATE(extractedSignal,source=int(this%signalInt1,8),STAT=stat,ERRMSG = errorCode)
                  IF (stat/=0) THEN
                       WRITE (*,*) 'НЕ могу выделить память ExtractSignalData1 ,ERRMSG = ',errorCode
                     CALL ExitFromProgramNormal()
                   END IF
                CASE(2)
                  ALLOCATE(extractedSignal,source=int(this%signalInt2,8),STAT=stat,ERRMSG = errorCode)
                  IF (stat/=0) THEN
                       WRITE (*,*) 'НЕ могу выделить память ExtractSignalData1 ,ERRMSG = ',errorCode
                     CALL ExitFromProgramNormal()
                   END IF
                CASE(4)
                   ALLOCATE(extractedSignal,source=int(this%signalInt4,8),STAT=stat,ERRMSG = errorCode)
                  IF (stat/=0) THEN
                       WRITE (*,*) 'НЕ могу выделить память ExtractSignalData1 ,ERRMSG = ',errorCode
                     CALL ExitFromProgramNormal()
                   END IF
                CASE(8)
                   ALLOCATE(extractedSignal,source=this%signalInt8,STAT=stat,ERRMSG = errorCode)
                  IF (stat/=0) THEN
                       WRITE (*,*) 'НЕ могу выделить память ExtractSignalData1 ,ERRMSG = ',errorCode
                     CALL ExitFromProgramNormal()
                   END IF
            END SELECT
        ELSE
           WRITE (*,*) 'НЕ МОГУ ИЗВЛЕЧЬ ДАННЫЕ INT8', this%signalName
           CALL ExitFromProgramNormal()
        END IF
     END SUBROUTINE ExtractSignalData8


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

         INTEGER(2), ALLOCATABLE              :: arrayInt1(:)
         INTEGER(2), ALLOCATABLE              :: arrayInt2(:)
         INTEGER(4), ALLOCATABLE              :: arrayInt4(:)
         INTEGER(8), ALLOCATABLE              :: arrayInt8(:)

         INTEGER(8)                           :: maxAbsX,maxAbsY,maxRez,i

         maxAbsX = xOp%GetMaxAbs()
         maxAbsY = yOp%GetMaxAbs()
         maxRez  = maxAbsX*maxAbsY

         allocate( MultiplyAnalyticSignals)

!         WRITE(*,*) 'ЗАшел в умножение'
!         WRITE(*,*) 'maxREz ', maxRez
!         WRITE(*,*) 'HUGE_Int4 ',HUGE_Int4

          SELECT CASE (maxRez)
                CASE(:HUGE_Int1)
                   ALLOCATE(arrayInt1(1:xOp%GetSignalSize()))
                     DO i=1,xOp%GetSignalSize()
                        arrayInt1(i) = (xOp%GetValue(i)*yOp%GetValue(i))
                     END DO
                        CALL MultiplyAnalyticSignals%Constructor (arrayInt1)
                        DEALLOCATE(arrayInt1)
                 CASE(HUGE_Int1+1:HUGE_Int2)
                   ALLOCATE(arrayInt2(1:xOp%GetSignalSize()))
                     DO i=1,xOp%GetSignalSize()
                        arrayInt2(i) = (xOp%GetValue(i)*yOp%GetValue(i))
                     END DO
                     CALL MultiplyAnalyticSignals%Constructor (arrayInt2)
                     DEALLOCATE(arrayInt2)
                 CASE(HUGE_Int2+1:HUGE_Int4)
!                     WRITE(*,*) 'ЗАшел в нужну ветку'

                   ALLOCATE(arrayInt4(1:xOp%GetSignalSize()))
                     DO i=1,xOp%GetSignalSize()
                        arrayInt4(i) = (xOp%GetValue(i)*yOp%GetValue(i))
!                        WRITE(*,*) 'arrayInt4(i) ',arrayInt4(i)
                     END DO
                     CALL MultiplyAnalyticSignals%Constructor (arrayInt4)
                     DEALLOCATE(arrayInt4)
                 CASE(HUGE_Int4+1:HUGE_Int8)
                   ALLOCATE(arrayInt8(1:xOp%GetSignalSize()))
                     DO i=1,xOp%GetSignalSize()
                        arrayInt8(i) = (xOp%GetValue(i)*yOp%GetValue(i))
                     END DO
                     CALL MultiplyAnalyticSignals%Constructor (arrayInt8)
                     DEALLOCATE(arrayInt8)
          END SELECT

            ! Вот тут конструктор копирования(=) не отрабаывает - не может взять размер и посавить статус выделения - почему??
            ! Хотя деструктор вызывается спокойно с нужными парамерами
            ! требуется расследование

            CALL MultiplyAnalyticSignals%Setname('промежут умножение')
            MultiplyAnalyticSignals%isAllocated=.TRUE.

     END FUNCTION MultiplyAnalyticSignals

      FUNCTION SubtractAnalyticSignals(xOp,yOp)
         CLASS(analyticSignal_t), INTENT(IN)  :: xOp
         CLASS(analyticSignal_t), INTENT(IN)  :: yOp
         CLASS(analyticSignal_t), allocatable ::SubtractAnalyticSignals
          INTEGER(2), ALLOCATABLE              :: arrayInt1(:)
          INTEGER(2), ALLOCATABLE              :: arrayInt2(:)
          INTEGER(4), ALLOCATABLE              :: arrayInt4(:)
          INTEGER(8), ALLOCATABLE              :: arrayInt8(:)
          INTEGER(8)                           :: maxAbsX,maxAbsY,maxRez,i
        maxAbsX = xOp%GetMaxAbs()
        maxAbsY = yOp%GetMaxAbs()
        maxRez  = maxAbsX+maxAbsX

         allocate( SubtractAnalyticSignals)
         SELECT CASE (maxRez)
                CASE(:HUGE_Int1)
                   ALLOCATE(arrayInt1(1:xOp%GetSignalSize()))
                     DO i=1,xOp%GetSignalSize()
                        arrayInt1(i) = int(xOp%GetValue(i)-xOp%GetValue(i),1)
                     END DO
                        CALL  SubtractAnalyticSignals%Constructor (arrayInt1)
                        DEALLOCATE(arrayInt1)
                 CASE(HUGE_Int1+1:HUGE_Int2)
                   ALLOCATE(arrayInt2(1:xOp%GetSignalSize()))
                     DO i=1,xOp%GetSignalSize()
                        arrayInt2(i) = int(xOp%GetValue(i)-xOp%GetValue(i),2)
                     END DO
                     CALL SubtractAnalyticSignals%Constructor (arrayInt2)
                     DEALLOCATE(arrayInt2)
                 CASE(HUGE_Int2+1:HUGE_Int4)
                   ALLOCATE(arrayInt4(1:xOp%GetSignalSize()))
                     DO i=1,xOp%GetSignalSize()
                        arrayInt4(i) = int(xOp%GetValue(i)-xOp%GetValue(i),4)
                     END DO
                     CALL SubtractAnalyticSignals%Constructor (arrayInt4)
                     DEALLOCATE(arrayInt4)
                 CASE(HUGE_Int4+1:HUGE_Int8)
                   ALLOCATE(arrayInt8(1:xOp%GetSignalSize()))
                     DO i=1,xOp%GetSignalSize()
                        arrayInt8(i) = int(xOp%GetValue(i)-xOp%GetValue(i),8)
                     END DO
                     CALL SubtractAnalyticSignals%Constructor (arrayInt8)
                     DEALLOCATE(arrayInt8)
          END SELECT

            CALL SubtractAnalyticSignals%Setname('промежут вычитание')
            SubtractAnalyticSignals%isAllocated=.TRUE.
     END FUNCTION SubtractAnalyticSignals

     FUNCTION   AddAnalyticSignals(xOp,yOp)
         CLASS(analyticSignal_t), INTENT(IN)  :: xOp
         CLASS(analyticSignal_t), INTENT(IN)  :: yOp
         CLASS(analyticSignal_t), allocatable :: AddAnalyticSignals

         INTEGER(2), ALLOCATABLE              :: arrayInt1(:)
         INTEGER(2), ALLOCATABLE              :: arrayInt2(:)
         INTEGER(4), ALLOCATABLE              :: arrayInt4(:)
         INTEGER(8), ALLOCATABLE              :: arrayInt8(:)
         INTEGER(8)                           :: maxAbsX,maxAbsY,maxRez,i
         maxAbsX = xOp%GetMaxAbs()
         maxAbsY = yOp%GetMaxAbs()
         maxRez  = maxAbsX+maxAbsX

        allocate(AddAnalyticSignals)

!           WRITE(*,*) 'ЗАшел в сложение'
!           WRITE(*,*) 'maxREz ', maxRez
!           WRITE(*,*) 'HUGE_Int4 ',HUGE_Int4


         SELECT CASE (maxRez)
                CASE(:HUGE_Int1)
                   ALLOCATE(arrayInt1(1:xOp%GetSignalSize()))
                     DO i=1,xOp%GetSignalSize()
                        arrayInt1(i) = int(xOp%GetValue(i)+xOp%GetValue(i),1)
                     END DO
                        CALL  AddAnalyticSignals%Constructor (arrayInt1)
                        DEALLOCATE(arrayInt1)
                 CASE(HUGE_Int1+1:HUGE_Int2)
                     WRITE(*,*) 'ЗАшел в нужну ветку'
                   ALLOCATE(arrayInt2(1:xOp%GetSignalSize()))
                     DO i=1,xOp%GetSignalSize()
                        arrayInt2(i) = int(xOp%GetValue(i)+xOp%GetValue(i),2)
                     END DO
                     CALL AddAnalyticSignals%Constructor (arrayInt2)
                     DEALLOCATE(arrayInt2)
                 CASE(HUGE_Int2+1:HUGE_Int4)
                   ALLOCATE(arrayInt4(1:xOp%GetSignalSize()))
                     DO i=1,xOp%GetSignalSize()
                        arrayInt4(i) = int(xOp%GetValue(i)+xOp%GetValue(i),4)
                     END DO
                     CALL AddAnalyticSignals%Constructor (arrayInt4)
                     DEALLOCATE(arrayInt4)
                 CASE(HUGE_Int4+1:HUGE_Int8)
                   ALLOCATE(arrayInt8(1:xOp%GetSignalSize()))
                     DO i=1,xOp%GetSignalSize()
                        arrayInt8(i) = int(xOp%GetValue(i)+xOp%GetValue(i),8)
                     END DO
                     CALL AddAnalyticSignals%Constructor (arrayInt8)
                     DEALLOCATE(arrayInt8)
          END SELECT



            ! Вот тут конструктор копирования(=) не отрабаывает - не может взять размер и посавить статус выделения - почему??
            ! Хотя деструктор вызывается спокойно с нужными парамерами
            ! требуется расследование
            CALL AddAnalyticSignals%Setname('промежутсложение')

            AddAnalyticSignals%isAllocated=.TRUE.

     END FUNCTION   AddAnalyticSignals

    SUBROUTINE AssignDataFromAssignment(leftOp,rightOp)
        CLASS(analyticSignal_t), INTENT(INOUT)  :: leftOp
        CLASS(analyticSignal_t), INTENT(IN)     :: rightOp
        INTEGER(8),ALLOCATABLE :: extractedSignalInt8(:)
        INTEGER(4),ALLOCATABLE :: extractedSignalInt4(:)
        INTEGER(2),ALLOCATABLE :: extractedSignalInt2(:)
        INTEGER(1),ALLOCATABLE :: extractedSignalInt1(:)

        SELECT CASE(rightOp%signalArrayKind)
               CASE(1)
                    CALL rightOp%ExtractSignalData(extractedSignalInt1)
                    CALL leftOp%Constructor(extractedSignalInt1)
                    DEALLOCATE(extractedSignalInt1)
                CASE(2)
                    CALL rightOp%ExtractSignalData(extractedSignalInt2)
                    CALL leftOp%Constructor(extractedSignalInt2)
                    DEALLOCATE(extractedSignalInt2)
                 CASE(4)
                    CALL rightOp%ExtractSignalData(extractedSignalInt4)
                    CALL leftOp%Constructor(extractedSignalInt4)
                    DEALLOCATE(extractedSignalInt4)
                 CASE(8)
                    CALL rightOp%ExtractSignalData(extractedSignalInt8)
                    CALL leftOp%Constructor(extractedSignalInt8)
                    DEALLOCATE(extractedSignalInt8)
                 CASE(0)
                     WRITE(*,*) 'правая сторона не инициализирована'
                    CALL ExitFromProgramNormal()
                 CASE DEFAULT
                    WRITE(*,*) 'Неправильно выбран тип целого для записи'
                    CALL ExitFromProgramNormal()
         END SELECT


    END SUBROUTINE AssignDataFromAssignment

    ! функция реализующая вычисление корреляционной (или свертки)
    ! функции аналитических сигналов
    ! input - входной сигнал
    ! reference - опорный сигнал

    FUNCTION Convolve(input,reference)

         CLASS(analyticSignal_t), INTENT(IN)  :: input
         CLASS(analyticSignal_t), INTENT(IN)  :: reference
         CLASS(analyticSignal_t), allocatable :: convolve
         INTEGER(8),ALLOCATABLE               :: tempArray(:)
         INTEGER(8)                           :: i,j
         INTEGER(4)                           :: stat
         INTEGER(1)                           :: inKind,refKind
         CHARACTER(len=60) ::errorCode

         allocate(Convolve)
         convolve%signalName='conv fun name'
         ! ЗАщита
!         WRITE(*,*) input%signalSize
!         WRITE(*,*) reference%signalSize

         IF (input%signalSize<reference%signalSize) THEN
             WRITE(*,*) 'ОШИБКА Опорный сигнал  длительности > входящий'
             CALL ExitFromProgramNormal()

         END IF



!            ALLOCATE(tempArray(1:input%GetSignalSize()),STAT=stat)
!            IF (stat/=0) THEN
!                WRITE (*,*) 'НЕ могу выделить память под выч. корр. ф-ции,ERRMSG = ',errorCode
!                CALL ExitFromProgramNormal()
!            tempArray = 0
!             END IF
!
!             DO i=1,input%GetSignalSize()-reference%GetSignalSize()
!                DO j=1,reference%GetSignalSize()
!                    tempArray(i)=tempArray(i)+input%GetValue(i+j)*reference%GetValue(j)
!                    !WRITE(*,*) 'tempArray', tempArray(i)
!                END DO
!             END DO
!            tempArray = 0


!
            inKind = input%GetSignalKind()
            refKind = reference%GetSignalKind()
            WRITE(*,*) 'inKind ', inKind
            WRITE(*,*) 'refKind ', refKind
!            WRITE (*,*) 'input%signalInt2 ', ALLOCATED (input%signalInt2)
!            WRITE (*,*) 'reference%signalInt2 ', ALLOCATED (reference%signalInt2)
!
!            CALL input%ExtractSignalData2(in)
!            CALL reference%ExtractSignalData2(ref)
!
!            tempArray = CorrelationRaw222(in,ref)

           ! tempArray = CorrelationRaw(in,ref)

!             DO i=1,input%GetSignalSize()-reference%GetSignalSize()
!                DO j=1,reference%GetSignalSize()
!                    tempArray(i)=tempArray(i)+in(i+j)*ref(j)
!                    !WRITE(*,*) 'tempArray', tempArray(i)
!                END DO
!             END DO

            IF ((inKind == 1).AND.(refKind== 1)) tempArray=CorrelationRaw (input%signalInt1,reference%signalInt1)
            IF ((inKind == 1).AND.(refKind== 2)) tempArray=CorrelationRaw (input%signalInt1,reference%signalInt2)
            IF ((inKind == 1).AND.(refKind== 4)) tempArray=CorrelationRaw (input%signalInt1,reference%signalInt4)
            IF ((inKind == 1).AND.(refKind== 4)) tempArray=CorrelationRaw (input%signalInt1,reference%signalInt8)
            IF ((inKind == 8).AND.(refKind== 4)) tempArray=CorrelationRaw (input%signalInt8,reference%signalInt4)
            IF ((inKind == 8).AND.(refKind== 2)) tempArray=CorrelationRaw (input%signalInt8,reference%signalInt2)
            IF ((inKind == 8).AND.(refKind== 1)) tempArray=CorrelationRaw (input%signalInt8,reference%signalInt1)
            IF ((inKind == 2).AND.(refKind== 1)) tempArray=CorrelationRaw (input%signalInt2,reference%signalInt1)
            IF ((inKind == 2).AND.(refKind== 2)) tempArray=CorrelationRaw (input%signalInt2,reference%signalInt2)
            IF ((inKind == 2).AND.(refKind== 4)) tempArray=CorrelationRaw (input%signalInt2,reference%signalInt4)
            IF ((inKind == 2).AND.(refKind== 8)) tempArray=CorrelationRaw (input%signalInt2,reference%signalInt8)
            IF ((inKind == 4).AND.(refKind== 1)) tempArray=CorrelationRaw (input%signalInt4,reference%signalInt1)
            IF ((inKind == 4).AND.(refKind== 2)) tempArray=CorrelationRaw (input%signalInt4,reference%signalInt2)
            IF ((inKind == 4).AND.(refKind== 4)) tempArray=CorrelationRaw (input%signalInt4,reference%signalInt4)
            IF ((inKind == 4).AND.(refKind== 8)) tempArray=CorrelationRaw (input%signalInt4,reference%signalInt8)

              !WRITE(*,*) 'обычн свертка, нить №  ',  omp_get_thread_num()
             CALL convolve%Constructor(tempArray)
             DEALLOCATE(tempArray)

    END FUNCTION   Convolve



    ! Вставка нулей в начало и конец сигнала (увеличивает его длину)
    ! before - число нулей до начала сигнала
    ! after - после начала сигнала
    SUBROUTINE ZeroesStuffing (this, beforeLen,afterLen)

        CLASS(analyticSignal_t), INTENT(INOUT)  :: this
        INTEGER(8),INTENT(IN)                   :: beforeLen
        INTEGER(8),INTENT(IN)                   :: afterLen
        !итоговая длина сигнала
        INTEGER(8)                              :: summLen
        INTEGER(8),ALLOCATABLE                  :: tempArray8(:)
        INTEGER(4),ALLOCATABLE                  :: tempArray4(:)
        INTEGER(2),ALLOCATABLE                  :: tempArray2(:)
        INTEGER(1),ALLOCATABLE                  :: tempArray1(:)
        INTEGER(8)                              ::allocationStatus

        !итоговая длина сигнала
        summLen = beforeLen + afterLen + this%signalSize
        ! проверка на максимальную длину массива
        IF(summLen>HUGE(summLen)) THEN
          WRITE(*,*) 'Превышение максимальной длины массива'
          WRITE(*,*) '(при наполнении нулями)'
          CALL ExitFromProgramNormal()
        END IF




        SELECT CASE(this%signalArrayKind)
                CASE(1)
                      ALLOCATE(tempArray1(1:summLen),stat=allocationStatus)
                      IF (allocationStatus>0) THEN
                         WRITE(*,*) 'не могу выделить память tmpArray'
                         CALL ExitFromProgramNormal()
                      END IF

                      tempArray1=0
                      tempArray1(beforeLen+1:beforeLen-1+this%signalSize)=this%signalInt1
                      DEALLOCATE(this%signalInt1)
                      ALLOCATE(this%signalInt1,source=tempArray1,stat=allocationStatus)
                      IF (allocationStatus>0) THEN
                          WRITE(*,*) 'не могу выделить память this%signal'
                          CALL ExitFromProgramNormal()
                      END IF
                      DEALLOCATE(tempArray1)

                CASE(2)
                      ALLOCATE(tempArray2(1:summLen),stat=allocationStatus)
                      IF (allocationStatus>0) THEN
                         WRITE(*,*) 'не могу выделить память tmpArray'
                         CALL ExitFromProgramNormal()
                      END IF

                      tempArray2=0
                      tempArray2(beforeLen+1:beforeLen-1+this%signalSize)=this%signalInt2
                      DEALLOCATE(this%signalInt2)
                      ALLOCATE(this%signalInt2,source=tempArray2,stat=allocationStatus)
                      IF (allocationStatus>0) THEN
                          WRITE(*,*) 'не могу выделить память this%signal'
                          CALL ExitFromProgramNormal()
                      END IF
                      DEALLOCATE(tempArray2)
                CASE(4)
                      ALLOCATE(tempArray4(1:summLen),stat=allocationStatus)
                      IF (allocationStatus>0) THEN
                         WRITE(*,*) 'не могу выделить память tmpArray'
                         CALL ExitFromProgramNormal()
                      END IF

                      tempArray4=0
                      tempArray4(beforeLen+1:beforeLen-1+this%signalSize)=this%signalInt4
                      DEALLOCATE(this%signalInt4)
                      ALLOCATE(this%signalInt4,source=tempArray4,stat=allocationStatus)
                      IF (allocationStatus>0) THEN
                          WRITE(*,*) 'не могу выделить память this%signal'
                          CALL ExitFromProgramNormal()
                      END IF
                      DEALLOCATE(tempArray4)

                CASE(8)

                      ALLOCATE(tempArray8(1:summLen),stat=allocationStatus)
                      IF (allocationStatus>0) THEN
                         WRITE(*,*) 'не могу выделить память tmpArray'
                         CALL ExitFromProgramNormal()
                      END IF

                      tempArray8=0
                      tempArray8(beforeLen+1:beforeLen+1+this%signalSize)=this%signalInt8
                      DEALLOCATE(this%signalInt8)
                      ALLOCATE(this%signalInt8,source=tempArray8,stat=allocationStatus)
                      IF (allocationStatus>0) THEN
                          WRITE(*,*) 'не могу выделить память this%signal'
                          CALL ExitFromProgramNormal()
                      END IF
                      DEALLOCATE(tempArray8)

                CASE(0)
                    WRITE(*,*) 'правая сторона не инициализирована'
                    CALL ExitFromProgramNormal()
         END SELECT

          this%signalSize = summLen

      END SUBROUTINE ZeroesStuffing

    ! ВЫполняет арифметический сдвиг вправо, для выбора старших разрядов сигнала
    ! shift - величина сдвига в отсчетах
    SUBROUTINE RShift(this,shift)
        CLASS(analyticSignal_t), INTENT(INOUT)  :: this
        INTEGER(1),INTENT(IN)                   :: shift
         SELECT CASE(this%signalArrayKind)
                CASE(1)
                  this%signalInt1=SHIFTA( this%signalInt1,shift)
                CASE(2)
                  this%signalInt2=SHIFTA( this%signalInt2,shift)
                CASE(4)
                  this%signalInt4=SHIFTA( this%signalInt4,shift)
                CASE(8)
                  this%signalInt8=SHIFTA( this%signalInt8,shift)
         END SELECT
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

          SELECT CASE(input%signalArrayKind)
                CASE(1)
                  CALL inputSig%Constructor(input%signalInt1)
                CASE(2)
                  CALL inputSig%Constructor(input%signalInt2)
                CASE(4)
                  CALL inputSig%Constructor(input%signalInt4)
                CASE(8)
                  CALL inputSig%Constructor(input%signalInt8)
         END SELECT

          SELECT CASE(reference%signalArrayKind)
                CASE(1)
                  CALL referenceSig%Constructor(reference%signalInt1)
                CASE(2)
                  CALL referenceSig%Constructor(reference%signalInt2)
                CASE(4)
                  CALL referenceSig%Constructor(reference%signalInt4)
                CASE(8)
                  CALL referenceSig%Constructor(reference%signalInt8)
         END SELECT

        WRITE(*,*) 'Вычисляю знак корреляцию'

         rez = inputSig.CONV.referenceSig

         WRITE(*,*) 'ВЫЗЫВАЮ Аналитич КОНТРСРКУТР'
         CALL ConvolveSignum%Constructor(rez)
         DEALLOCATE(rez)

    END FUNCTION ConvolveSignum

     FUNCTION ConvolveAnalyticSignalSignumSignal(input,reference)
         CLASS(analyticSignal_t), INTENT(IN)   :: input
         CLASS(signumSignal_t)  , INTENT(IN)   :: reference
         CLASS(analyticSignal_t), ALLOCATABLE  :: ConvolveAnalyticSignalSignumSignal
         TYPE(signumSignal_t  )                :: inputSig
         INTEGER(8)                            :: status

         ! ВЫХОДНАЯ РАЗРЯДНОСТЬ ЗНАКОВОГО КОРРЕЛЯТОРА 32 БИТА

         INTEGER(4), ALLOCATABLE               :: rez(:)

         !WRITE(*,*) 'ЗАЩЕЛ'

         allocate(ConvolveAnalyticSignalSignumSignal,stat=status)
         ConvolveAnalyticSignalSignumSignal%signalName='fun name ConvolveSignum'

         ! WRITE(*,*) 'дал память'
         !WRITE (*,*)  'NAme = ', ConvolveSignum%signalName
         !WRITE (*,*)  'alloc status = ', status

          SELECT CASE(input%signalArrayKind)
                CASE(1)
                  CALL inputSig%Constructor(input%signalInt1)
                CASE(2)
                  CALL inputSig%Constructor(input%signalInt2)
                CASE(4)
                  CALL inputSig%Constructor(input%signalInt4)
                CASE(8)
                  CALL inputSig%Constructor(input%signalInt8)
         END SELECT

!          IF (inputSig%signalSize<reference%signalSize) THEN
!              !WRITE(*,*) 'ОШИБКА Опорный сигнал  длительности > входящий'
!              CALL ExitFromProgramNormal()
!         END IF

        ! WRITE(*,*) 'Вычисляю знак корреляцию'

         rez = inputSig.CONV.reference

         !WRITE(*,*) 'ВЫЗЫВАЮ Аналитич КОНТРСРКУТР'
         CALL ConvolveAnalyticSignalSignumSignal%Constructor(rez)
         DEALLOCATE(rez)

    END FUNCTION ConvolveAnalyticSignalSignumSignal

    ! умножить аналитич сигнал на константу (СИГНАЛ*КОНСТАНТА)
    FUNCTION MultiplyAnalyticSignalsAndConstant8(xOp,yOp)
         CLASS(analyticSignal_t), INTENT(IN)  :: xOp
         INTEGER(8),              INTENT(IN)  :: yOp
         CLASS(analyticSignal_t), allocatable ::MultiplyAnalyticSignalsAndConstant8
         INTEGER(2), ALLOCATABLE              :: arrayInt1(:)
         INTEGER(2), ALLOCATABLE              :: arrayInt2(:)
         INTEGER(4), ALLOCATABLE              :: arrayInt4(:)
         INTEGER(8), ALLOCATABLE              :: arrayInt8(:)
         INTEGER(8)                           :: maxAbsX,maxAbsY,maxRez,i

         allocate( MultiplyAnalyticSignalsAndConstant8)

         maxAbsX = xOp%GetMaxAbs()
         maxAbsY = yOp
         maxRez  = maxAbsX*maxAbsY

          SELECT CASE (maxRez)
                CASE(:HUGE_Int1)
                     ALLOCATE(arrayInt1(1:xOp%GetSignalSize()))
                     DO i=1,xOp%GetSignalSize()
                        arrayInt1(i) = int(xOp%GetValue(i)*yOp,1)
                     END DO
                        CALL MultiplyAnalyticSignalsAndConstant8%Constructor (arrayInt1)
                        DEALLOCATE(arrayInt1)
                 CASE(HUGE_Int1+1:HUGE_Int2)
                   ALLOCATE(arrayInt2(2:xOp%GetSignalSize()))
                     DO i=1,xOp%GetSignalSize()
                        arrayInt2(i) = int(xOp%GetValue(i)*yOp,2)
                     END DO
                     CALL MultiplyAnalyticSignalsAndConstant8%Constructor (arrayInt2)
                     DEALLOCATE(arrayInt2)
                 CASE(HUGE_Int2+1:HUGE_Int4)
                   ALLOCATE(arrayInt4(2:xOp%GetSignalSize()))
                     DO i=1,xOp%GetSignalSize()
                        arrayInt4(i) = int(xOp%GetValue(i)*yOp,4)
                     END DO
                     CALL MultiplyAnalyticSignalsAndConstant8%Constructor (arrayInt4)
                     DEALLOCATE(arrayInt4)
                 CASE(HUGE_Int4+1:HUGE_Int8)
                   ALLOCATE(arrayInt8(2:xOp%GetSignalSize()))
                     DO i=1,xOp%GetSignalSize()
                        arrayInt8(i) = int(xOp%GetValue(i)*yOp,8)
                     END DO
                     CALL MultiplyAnalyticSignalsAndConstant8%Constructor (arrayInt8)
                     DEALLOCATE(arrayInt8)
          END SELECT


            ! Вот тут конструктор копирования(=) не отрабаывает - не может взять размер и посавить статус выделения - почему??
            ! Хотя деструктор вызывается спокойно с нужными парамерами
            ! требуется расследование


            CALL MultiplyAnalyticSignalsAndConstant8%Setname('промежут умножение')
            MultiplyAnalyticSignalsAndConstant8%isAllocated=.TRUE.
     END FUNCTION MultiplyAnalyticSignalsAndConstant8

         ! умножить аналитич сигнал на константу (КОНСТАНТА*СИГНАЛ)
    FUNCTION Multiplydonstant8AndAnalyticSignals(xOp,yOp)
         INTEGER(8),              INTENT(IN)  :: xOp
         CLASS(analyticSignal_t), INTENT(IN)  :: yOp
         CLASS(analyticSignal_t), allocatable ::Multiplydonstant8AndAnalyticSignals
         INTEGER(2), ALLOCATABLE              :: arrayInt1(:)
         INTEGER(2), ALLOCATABLE              :: arrayInt2(:)
         INTEGER(4), ALLOCATABLE              :: arrayInt4(:)
         INTEGER(8), ALLOCATABLE              :: arrayInt8(:)
         INTEGER(8)                           :: maxAbsX,maxAbsY,maxRez,i

         allocate( Multiplydonstant8AndAnalyticSignals)
            ! Вот тут конструктор копирования(=) не отрабаывает - не может взять размер и посавить статус выделения - почему??
            ! Хотя деструктор вызывается спокойно с нужными парамерами
            ! требуется расследование
         maxAbsX = yOp%GetMaxAbs()
         maxAbsY = xOp
         maxRez  = maxAbsX*maxAbsY

          SELECT CASE (maxRez)
                CASE(:HUGE_Int1)
                     ALLOCATE(arrayInt1(1:yOp%GetSignalSize()))
                     DO i=1,yOp%GetSignalSize()
                        arrayInt1(i) = int(yOp%GetValue(i)*xOp,1)
                     END DO
                        CALL Multiplydonstant8AndAnalyticSignals%Constructor (arrayInt1)
                        DEALLOCATE(arrayInt1)
                 CASE(HUGE_Int1+1:HUGE_Int2)
                   ALLOCATE(arrayInt2(2:yOp%GetSignalSize()))
                     DO i=1,yOp%GetSignalSize()
                        arrayInt2(i) = int(yOp%GetValue(i)*xOp,2)
                     END DO
                     CALL Multiplydonstant8AndAnalyticSignals%Constructor (arrayInt2)
                     DEALLOCATE(arrayInt2)
                 CASE(HUGE_Int2+1:HUGE_Int4)
                   ALLOCATE(arrayInt4(2:yOp%GetSignalSize()))
                     DO i=1,yOp%GetSignalSize()
                        arrayInt4(i) = int(yOp%GetValue(i)*xOp,4)
                     END DO
                     CALL Multiplydonstant8AndAnalyticSignals%Constructor (arrayInt4)
                     DEALLOCATE(arrayInt4)
                 CASE(HUGE_Int4+1:HUGE_Int8)
                   ALLOCATE(arrayInt8(2:yOp%GetSignalSize()))
                     DO i=1,yOp%GetSignalSize()
                        arrayInt8(i) = int(yOp%GetValue(i)*xOp,8)
                     END DO
                     CALL Multiplydonstant8AndAnalyticSignals%Constructor (arrayInt8)
                     DEALLOCATE(arrayInt8)
          END SELECT

            CALL Multiplydonstant8AndAnalyticSignals%Setname('промежут умножение')
            Multiplydonstant8AndAnalyticSignals%isAllocated=.TRUE.
     END FUNCTION Multiplydonstant8AndAnalyticSignals

     FUNCTION Decimate(this,r)
         CLASS(analyticSignal_t), INTENT(IN)  :: this
         CLASS(analyticSignal_t), allocatable :: Decimate
         INTEGER(8) , INTENT(IN)              :: r
         ALLOCATE(Decimate)

          SELECT CASE(this%signalArrayKind)
                CASE(1)
                  CALL Decimate%Constructor(this%signalInt1(1:this%signalSize:r))
                CASE(2)
                  CALL Decimate%Constructor(this%signalInt2(1:this%signalSize:r))
                CASE(4)
                  CALL Decimate%Constructor(this%signalInt4(1:this%signalSize:r))
                CASE(8)
                  CALL Decimate%Constructor(this%signalInt8(1:this%signalSize:r))
         END SELECT


     END FUNCTION Decimate


     FUNCTION GetMax (this)
           CLASS(analyticSignal_t), INTENT(IN)  :: this
            INTEGER(8)                          :: GetMax
             SELECT CASE(this%signalArrayKind)
               CASE(1)
                  GetMAx= maxval(this%signalInt1)
               CASE(2)
                  GetMAx= maxval(this%signalInt2)
               CASE(4)
                  GetMAx= maxval(this%signalInt4)
               CASE(8)
                    GetMAx= maxval(this%signalInt8)
                CASE(0)
                    WRITE(*,*) ' не инициализированj'
                    CALL ExitFromProgramNormal()
         END SELECT
     END FUNCTION

         FUNCTION GetMin (this)
           CLASS(analyticSignal_t), INTENT(IN)  :: this
            INTEGER(8)                          :: GetMin
             SELECT CASE(this%signalArrayKind)
               CASE(1)
                  GetMin= minval(this%signalInt1)
               CASE(2)
                  GetMin= minval(this%signalInt2)
               CASE(4)
                  GetMin= minval(this%signalInt4)
               CASE(8)
                    GetMin= minval(this%signalInt8)
                CASE(0)
                    WRITE(*,*) ' не инициализированj'
                    CALL ExitFromProgramNormal()
         END SELECT
     END FUNCTION GetMin

       FUNCTION GetMaxAbs (this)
           CLASS(analyticSignal_t), INTENT(IN)  :: this
            INTEGER(8)                          :: GetMaxAbs
             SELECT CASE(this%signalArrayKind)
               CASE(1)
                   GetMaxAbs= maxval(Abs(this%signalInt1))
               CASE(2)
                   GetMaxAbs= maxval(Abs(this%signalInt2))
               CASE(4)
                   GetMaxAbs= maxval(Abs(this%signalInt4))
               CASE(8)
                     GetMaxAbs= maxval(Abs(this%signalInt8))
                CASE(0)
                    WRITE(*,*) ' не инициализированj'
                    CALL ExitFromProgramNormal()

         END SELECT
     END FUNCTION GetMaxAbs

     FUNCTION GetValue (this,ptr)
           CLASS(analyticSignal_t), INTENT(IN)  :: this
           INTEGER(8)             , INTENT(IN)  :: ptr
           INTEGER(8)                           :: GetValue
           IF (ptr>this%signalSize) THEN
              WRITE(*,*) 'Идекс превосходит размеры массива'
              CALL ExitFromProgramNormal()
           END IF

             SELECT CASE(this%signalArrayKind)
               CASE(1)
                  GetValue = (this%signalInt1(ptr))
               CASE(2)
                  GetValue = (this%signalInt2(ptr))
               CASE(4)
                  GetValue = (this%signalInt4(ptr))
               CASE(8)
                   GetValue =(this%signalInt8(ptr))
                CASE(0)
                    WRITE(*,*) ' не инициализированj'
                    CALL ExitFromProgramNormal()
         END SELECT
     END FUNCTION GetValue

     FUNCTION GetSignalKind(this)
          CLASS(analyticSignal_t), INTENT(IN)  :: this
          INTEGER(1)                           :: GetSignalKind
          GetSignalKind = this%signalArrayKind
     END FUNCTION GetSignalKind

     FUNCTION ClipSignal(this, level,outLevel)
        CLASS(analyticSignal_t), INTENT(IN)  :: this
        INTEGER(2)             , INTENT(IN)  :: level
        INTEGER(2)             , INTENT(IN)  :: outLevel
        CLASS(analyticSignal_t), ALLOCATABLE :: ClipSignal

        ALLOCATE(ClipSignal)
        SELECT CASE(this%signalArrayKind)
               CASE(1)
                    CALL ClipSignal%ConstructorInt1( ClipToLevelInt1(this%signalInt1,level,outLevel))
                CASE(2)
                    CALL ClipSignal%ConstructorInt2( ClipToLevelInt2(this%signalInt2,level,outLevel))
                 CASE(4)
                    CALL ClipSignal%ConstructorInt4( ClipToLevelInt4(this%signalInt4,level,outLevel))
                 CASE(8)
                    CALL ClipSignal%ConstructorInt8( ClipToLevelInt8(this%signalInt8,level,outLevel))
                 CASE(0)
                     WRITE(*,*) 'клипирование - сигнал не инициализирован'
                    CALL ExitFromProgramNormal()
                 CASE DEFAULT
                    WRITE(*,*) 'Неправильно выбран тип целого для клиппирования'
                    CALL ExitFromProgramNormal()
         END SELECT
     END FUNCTION ClipSignal

    SUBROUTINE MirorReflectSignal (this)
       CLASS(analyticSignal_t), INTENT(INOUT)  :: this


       SELECT CASE(this%signalArrayKind)
               CASE(1)
                   CALL ReverseArrayInt( this%signalInt1(:) )
               CASE(2)
                  CALL ReverseArrayInt( this%signalInt2(:) )
               CASE(4)
                  CALL ReverseArrayInt( this%signalInt4(:) )
               CASE(8)
                  CALL ReverseArrayInt( this%signalInt8(:) )
               CASE(0)
                     WRITE(*,*) 'клипирование - сигнал не инициализирован'
                    CALL ExitFromProgramNormal()
               CASE DEFAULT
                    WRITE(*,*) 'Неправильно выбран тип целого для клиппирования'
                    CALL ExitFromProgramNormal()
         END SELECT


    END SUBROUTINE MirorReflectSignal

    SUBROUTINE destructor(this)
        TYPE(analyticSignal_t), INTENT(INOUT) :: this
        INTEGER(4) :: stat
         CHARACTER(len=60) ::errorCode


         SELECT CASE(this%signalArrayKind)
                CASE(1)
                   DEALLOCATE(this%signalInt1, STAT=stat)
                    IF (STAT==0) THEN
!                   !WRITE(*,*) ' ANALYTIC DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize, this%signalName
                      this%isAllocated=.FALSE.
                      ELSE
                        IF(  (.NOT. ALLOCATED(this%signalInt1)).AND.(.NOT.( this%isAllocated)   )  ) THEN
                    !WRITE(*,*) 'уже освободили память ',this%signalName
                         ELSE
                            WRITE(*,*) 'Не могу освободить память ',errorCode
                         END IF
                      END IF

                CASE(2)
                   DEALLOCATE(this%signalInt2, STAT=stat)
                    IF (STAT==0) THEN
!                   !WRITE(*,*) ' ANALYTIC DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize, this%signalName
                      this%isAllocated=.FALSE.
                      ELSE
                        IF(  (.NOT. ALLOCATED(this%signalInt2)).AND.(.NOT.( this%isAllocated)   )  ) THEN
                    !WRITE(*,*) 'уже освободили память ',this%signalName
                         ELSE
                            WRITE(*,*) 'Не могу освободить память ',errorCode
                         END IF
                      END IF

                CASE(4)

                    DEALLOCATE(this%signalInt4, STAT=stat)
                    IF (STAT==0) THEN
!                   !WRITE(*,*) ' ANALYTIC DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize, this%signalName
                      this%isAllocated=.FALSE.
                      ELSE
                        IF(  (.NOT. ALLOCATED(this%signalInt4)).AND.(.NOT.( this%isAllocated)   )  ) THEN
                    !WRITE(*,*) 'уже освободили память ',this%signalName
                         ELSE
                            WRITE(*,*) 'Не могу освободить память ',errorCode
                         END IF
                      END IF

                CASE(8)

                    DEALLOCATE(this%signalInt8, STAT=stat)
                    IF (STAT==0) THEN
!                   !WRITE(*,*) ' ANALYTIC DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize, this%signalName
                      this%isAllocated=.FALSE.
                      ELSE
                        IF(  (.NOT. ALLOCATED(this%signalInt8)).AND.(.NOT.( this%isAllocated)   )  ) THEN
                    !WRITE(*,*) 'уже освободили память ',this%signalName
                         ELSE
                            WRITE(*,*) 'Не могу освободить память ',errorCode
                         END IF
                      END IF

                CASE(0)
                    WRITE(*,*) 'деструктор сторона не инициализирована'
                    CALL ExitFromProgramNormal()

         END SELECT

    END SUBROUTINE



END MODULE analyticSignalModule



