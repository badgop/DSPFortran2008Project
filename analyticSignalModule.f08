MODULE analyticSignalModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: analyticSignal_t

        PRIVATE
        INTEGER(8),ALLOCATABLE :: signal(:)
        LOGICAL                :: isAllocated=.FALSE.
        INTEGER(8),PUBLIC      :: signalSize= 0

    CONTAINS

        PROCEDURE Constructor
        PROCEDURE ExtractSignalData
        PROCEDURE GetAllocationStatus
        PROCEDURE GetSignalSize
        PROCEDURE :: MultiplyAnalyticSignals


        PROCEDURE AssignDataFromAssignment

        generic :: assignment(=) =>  AssignDataFromAssignment
        generic :: operator(*) =>  MultiplyAnalyticSignals
!https://stackoverflow.com/questions/19064132/nested-derived-type-with-overloaded-assignment
!https://stackoverflow.com/questions/19111471/fortran-derived-type-assignment
        FINAL :: destructor

    END TYPE analyticSignal_t

!    interface assignment(=)
!        module procedure AssignDataFromAssignment
!    end interface assignment(=)

CONTAINS

    SUBROUTINE AssignDataFromAssignment(leftOp,rightOp)
        CLASS(analyticSignal_t), INTENT(INOUT)  :: leftOp
        CLASS(analyticSignal_t), INTENT(IN)     :: rightOp

        ! ЗАЩИТА

        allocate (leftOp%signal,source=rightOp%signal)
        leftOp%isAllocated=.TRUE.
        leftOp%signalSize=size(rightOp%signal)

    END SUBROUTINE AssignDataFromAssignment


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

     END FUNCTION MultiplyAnalyticSignals


    SUBROUTINE destructor(this)
        TYPE(analyticSignal_t), INTENT(INOUT) :: this
        INTEGER(4) :: stat

        DEALLOCATE(this%signal, STAT=stat)
        IF (STAT==0) THEN
            WRITE(*,*) ' ANALYTIC DESTRUCTOR WORKS! STAT ,SIZE ', stat,this%signalSize
            this%isAllocated=.FALSE.
        ELSE
            WRITE(*,*) 'Не могу освободить память '
        END IF




    END SUBROUTINE

END MODULE analyticSignalModule



