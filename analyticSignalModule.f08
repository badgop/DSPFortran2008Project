MODULE analyticSignalModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: analyticSignal_t

        PRIVATE
        INTEGER(8),ALLOCATABLE :: signal(:)
        LOGICAL                :: isAllocated
        INTEGER(8),PUBLIC             :: signalSize

    CONTAINS

        PROCEDURE Constructor
        PROCEDURE ExtractSignalData
        PROCEDURE GetAllocationStatus
!        PROCEDURE AssignData
!        GENERIC :: assignment(=) => AssignData
        FINAL :: destructor

    END TYPE analyticSignal_t

    interface assignment(=)
        module procedure AssignData
    end interface assignment(=)

CONTAINS

    SUBROUTINE AssignData(leftOp,rightOp)
        CLASS(analyticSignal_t), INTENT(INOUT),ALLOCATABLE  :: leftOp
        CLASS(analyticSignal_t), INTENT(IN)                :: rightOp

        ! ЗАЩИТА

        allocate (leftOp%signal,source=rightOp%signal)
        leftOp%isAllocated=.TRUE.
        leftOp%signalSize=size(rightOp%signal)


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

    SUBROUTINE destructor(this)
        TYPE(analyticSignal_t), INTENT(INOUT) :: this

        DEALLOCATE(this%signal)
        this%isAllocated=.FALSE.
        WRITE(*,*) 'ANALYTIC DESTRUCTOR WORKS!'

    END SUBROUTINE

END MODULE analyticSignalModule



