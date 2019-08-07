MODULE analyticSignalModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: analyticSignal_t

        !PRIVATE
        INTEGER(8),ALLOCATABLE :: signal(:)
        LOGICAL                :: isAllocated
        INTEGER(8)             :: signalSize

    CONTAINS
        PROCEDURE Constructor
        FINAL :: destructor

    END TYPE analyticSignal_t

    interface assignment(=)
        module procedure AssignData
    end interface assignment(=)

CONTAINS

    SUBROUTINE AssignData(leftOp,rightOp)
        CLASS(analyticSignal_t), INTENT(OUT), ALLOCATABLE  :: leftOp
        CLASS(analyticSignal_t), INTENT(IN)                :: rightOp

        ! ЗАЩИТА
        allocate (leftOp,source=rightOp)

        leftOp%isAllocated=.TRUE.
    END SUBROUTINE AssignData

    SUBROUTINE Constructor(this,loadedSignal)
        INTEGER(8), INTENT(IN) :: loadedSignal(:)
        CLASS(analyticSignal_t), INTENT(INOUT)  :: this

        INTEGER(8) :: fileSize


        fileSize=size(loadedSignal)
        this%signalSize=fileSize
        ALLOCATE( this%signal(1:fileSize))
        this%isAllocated=.TRUE.


    END SUBROUTINE

    SUBROUTINE destructor(this)
        TYPE(analyticSignal_t), INTENT(INOUT) :: this

        DEALLOCATE(this%signal)
        this%isAllocated=.FALSE.

    END SUBROUTINE

END MODULE analyticSignalModule



