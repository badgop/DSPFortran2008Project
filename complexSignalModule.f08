MODULE complexSignalModule
    USE analyticSignalModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: complexSignal_t

        !PRIVATE
        TYPE(analyticSignal_t) ::i
        TYPE(analyticSignal_t) ::q
        INTEGER(8),ALLOCATABLE :: signal(:)
        LOGICAL                :: isAllocated
        INTEGER(8)             :: signalSize

    CONTAINS
        PROCEDURE Constructor
        PROCEDURE Constructor2
        ! через 2 массива и через 2 сигнала аналитических
        GENERIC :: cons => Constructor,Constructor2
        PROCEDURE ExtractSignalData
        FINAL :: destructor

    END TYPE complexSignal_t

    interface assignment(=)
        module procedure AssignData
    end interface assignment(=)

CONTAINS

    SUBROUTINE AssignData(leftOp,rightOp)
        CLASS(complexSignal_t), INTENT(INOUT), ALLOCATABLE  :: leftOp
        CLASS(complexSignal_t), INTENT(IN)                :: rightOp

        ! ЗАЩИТА

        allocate (leftOp%signal,source=rightOp%signal)
        leftOp%isAllocated=.TRUE.
        leftOp%signalSize=size(rightOp%signal)


    END SUBROUTINE AssignData

    SUBROUTINE Constructor(this,loadedSignal)
        INTEGER(8), INTENT(IN) :: loadedSignal(:)
        CLASS(complexSignal_t), INTENT(INOUT)  :: this

        INTEGER(8) :: fileSize

        !что если обьект уже проинициализирован - проверить!!!
        fileSize=size(loadedSignal)
        this%signalSize=fileSize
        ALLOCATE( this%signal, source=loadedSignal)
        this%isAllocated=.TRUE.

    END SUBROUTINE Constructor

     SUBROUTINE Constructor2(this,loadedSignal,aa)
        INTEGER(8), INTENT(IN) :: aa(:)
        INTEGER(8), INTENT(IN) :: loadedSignal(:)
        CLASS(complexSignal_t), INTENT(INOUT)  :: this

        INTEGER(8) :: fileSize

        !что если обьект уже проинициализирован - проверить!!!
        fileSize=size(loadedSignal)
        this%signalSize=fileSize
        ALLOCATE( this%signal, source=loadedSignal)
        this%isAllocated=.TRUE.

    END SUBROUTINE Constructor2


    SUBROUTINE ExtractSignalData(this,extractedSignal)

        INTEGER(8),ALLOCATABLE, INTENT(INOUT) :: extractedSignal(:)
        CLASS(complexSignal_t), INTENT(IN)  :: this

        !ЗАЩИТА
        ALLOCATE(extractedSignal,source=this%signal)


     END SUBROUTINE ExtractSignalData

    SUBROUTINE destructor(this)
        TYPE(complexSignal_t), INTENT(INOUT) :: this

        DEALLOCATE(this%signal)
        this%isAllocated=.FALSE.

    END SUBROUTINE

END MODULE complexSignalModule



