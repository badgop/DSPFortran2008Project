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

        PROCEDURE AssignData

        FINAL :: destructor

    END TYPE analyticSignal_t

    interface assignment(=)
        module procedure AssignDataFromAssignment

    end interface assignment(=)

CONTAINS

    SUBROUTINE AssignDataFromAssignment(leftOp,rightOp)
        CLASS(analyticSignal_t), INTENT(INOUT)  :: leftOp
        CLASS(analyticSignal_t), INTENT(IN)     :: rightOp

        ! ЗАЩИТА

        allocate (leftOp%signal,source=rightOp%signal)
        leftOp%isAllocated=.TRUE.
        leftOp%signalSize=size(rightOp%signal)

    END SUBROUTINE AssignDataFromAssignment

     SUBROUTINE AssignData(this,rightOp)
        CLASS(analyticSignal_t), INTENT(INOUT)  :: this
        CLASS(analyticSignal_t), INTENT(IN)     :: rightOp

        ! ЗАЩИТА

        allocate (this%signal,source=rightOp%signal)
        this%isAllocated=.TRUE.
        this%signalSize=size(rightOp%signal)
        WRITE(*,*) ' AssignData = ', size(rightOp%signal), this%signalSize

    END SUBROUTINE AssignData





    SUBROUTINE Constructor(this,loadedSignal)
        INTEGER(8), INTENT(IN) :: loadedSignal(:)
        CLASS(analyticSignal_t), INTENT(INOUT)  :: this

        INTEGER(8) :: fileSize

        !что если обьект уже проинициализирован - проверить!!!
        fileSize=size(loadedSignal)
        this%signalSize=fileSize
        ALLOCATE( this%signal, source=loadedSignal)
        this%isAllocated=.TRUE.

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




    SUBROUTINE destructor(this)
        TYPE(analyticSignal_t), INTENT(INOUT) :: this

        DEALLOCATE(this%signal)
        this%isAllocated=.FALSE.
        WRITE(*,*) 'ANALYTIC DESTRUCTOR WORKS!'

    END SUBROUTINE

END MODULE analyticSignalModule



