 MODULE complexSignalModule
    USE analyticSignalModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: complexSignal_t

        !PRIVATE
        TYPE(analyticSignal_t),ALLOCATABLE ::i
        TYPE(analyticSignal_t),ALLOCATABLE ::q
        INTEGER(8),ALLOCATABLE :: signal(:)
        LOGICAL                :: isAllocated
        INTEGER(8)             :: signalSize

    CONTAINS
        PROCEDURE ConstructorFromArrays
        PROCEDURE ConstructorFromAnalyticSignals
        ! через 2 массива и через 2 сигнала аналитических
        GENERIC :: Constructor => ConstructorFromArrays,ConstructorFromAnalyticSignals
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

!        allocate (leftOp%i%signal,source=rightOp%i%signal)
!        allocate (leftOp%q%signal,source=rightOp%q%signal)
!
!        leftOp%isAllocated=.TRUE.
!        leftOp%signalSize=size(rightOp%i%signal)


    END SUBROUTINE AssignData

    SUBROUTINE ConstructorFromArrays(this,componentI,componentQ)
        CLASS(complexSignal_t), INTENT(INOUT)  :: this
        INTEGER(8), INTENT(IN)                 :: componentI(:)
        INTEGER(8), INTENT(IN)                 :: componentQ(:)
        INTEGER(8) :: fileSizeI

        !что если обьект уже проинициализирован - проверить!!!

        ALLOCATE(this%i)
        ALLOCATE(this%q)
        fileSizeI=size(componentI)
        this%signalSize=fileSizeI

        CALL this%i%Constructor(componentI)
        CALL this%q%Constructor(componentQ)

        this%isAllocated=.TRUE.

    END SUBROUTINE ConstructorFromArrays

     SUBROUTINE ConstructorFromAnalyticSignals(this,iSig_t,qSig_t)
        USE analyticSignalModule

        CLASS(complexSignal_t), INTENT(INOUT)  :: this
        CLASS(analyticSignal_t), INTENT(IN)  :: iSig_t
        CLASS(analyticSignal_t), INTENT(IN)  :: qSig_t

        !что если обьект уже проинициализирован - проверить!!!

!        CALL this%i%Constructor(iSig_t%signal)
!        CALL this%q%Constructor(qSig_t%signal)

        ALLOCATE(this%i)
        ALLOCATE(this%q)

        this%i=iSig_t
        this%q=qSig_t
!        CALL this%i%AssignData(iSig_t)
!        CALL this%q%AssignData(qSig_t)

        this%isAllocated=.TRUE.

    END SUBROUTINE ConstructorFromAnalyticSignals


    SUBROUTINE ExtractSignalData(this,extractedSignal)

        INTEGER(8),ALLOCATABLE, INTENT(INOUT) :: extractedSignal(:)
        CLASS(complexSignal_t), INTENT(IN)  :: this

        !ЗАЩИТА
        ALLOCATE(extractedSignal,source=this%signal)


     END SUBROUTINE ExtractSignalData

    SUBROUTINE destructor(this)
        TYPE(complexSignal_t), INTENT(INOUT) :: this

        !**деструкторы комноментов вызывают сами

!        DEALLOCATE(this%i%signal)
!        DEALLOCATE(this%q%signal)
        this%isAllocated=.FALSE.

    END SUBROUTINE

END MODULE complexSignalModule



