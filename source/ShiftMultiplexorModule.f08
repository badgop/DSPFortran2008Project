MODULE ShiftMultiplexorModule
    USE analyticSignalModule
    USE complexSignalModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: shiftMultiplexor_t

        PRIVATE
        INTEGER(1) :: shift=0


    CONTAINS

        PROCEDURE :: Constructor
        PROCEDURE :: PerformAnalyticSignalShift
        PROCEDURE :: PerformComplexSignalShift
        FINAL     :: Destructor

    END TYPE shiftMultiplexor_t



CONTAINS

     SUBROUTINE Constructor(this,shift)

        CLASS(shiftMultiplexor_t), INTENT(INOUT) :: this
        INTEGER(1),INTENT(IN) :: shift
        this%shift=shift

    END SUBROUTINE Constructor

    SUBROUTINE PerformAnalyticSignalShift(this,baseSignal, shiftedSignal)

        CLASS(shiftMultiplexor_t), INTENT(INOUT) :: this
        CLASS(analyticSignal_t), INTENT(INOUT)   :: baseSignal
        CLASS(analyticSignal_t), INTENT(INOUT)   :: shiftedSignal

        INTEGER(8), ALLOCATABLE                   :: tmpSignal(:)

        CALL baseSignal%ExtractSignalData(tmpSignal)


        tmpSignal = SHIFTA( tmpSignal,this%shift)

        CALL shiftedSignal%Constructor(tmpSignal)
        DEALLOCATE(tmpSignal)


    END SUBROUTINE PerformAnalyticSignalShift

     SUBROUTINE PerformComplexSignalShift(this,baseSignal, shiftedSignal)

        CLASS(shiftMultiplexor_t), INTENT(INOUT) :: this
        CLASS(complexSignal_t), INTENT(INOUT)   :: baseSignal
        CLASS(complexSignal_t), INTENT(INOUT)   :: shiftedSignal

        INTEGER(8), ALLOCATABLE                   :: tmpSignalI(:)
        INTEGER(8), ALLOCATABLE                   :: tmpSignalQ(:)

        CALL baseSignal%ExtractSignalData(tmpSignalI,tmpSignalQ)


        tmpSignalI = SHIFTA( tmpSignalI,this%shift)
        tmpSignalQ = SHIFTA( tmpSignalQ,this%shift)

        CALL shiftedSignal%Constructor(tmpSignalI,tmpSignalQ)
        DEALLOCATE(tmpSignalI)
        DEALLOCATE(tmpSignalQ)


    END SUBROUTINE PerformComplexSignalShift

    SUBROUTINE Destructor(this)

        TYPE(shiftMultiplexor_t), INTENT(INOUT) :: this


    END SUBROUTINE

END MODULE ShiftMultiplexorModule




