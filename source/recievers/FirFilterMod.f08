MODULE FirFilterMod
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: FirFilter_t
        PRIVATE
        INTEGER(4),ALLOCATABLE :: impulseResponseInt4(:)
        INTEGER(2),ALLOCATABLE :: impulseResponseInt2(:)
        INTEGER(2),ALLOCATABLE :: impulseResponseInt1(:)
        INTEGER(1)             :: impulseResponseKind=0
    CONTAINS
        PROCEDURE :: Constructor
        FINAL     :: destructor
    END TYPE FirFilter_t

CONTAINS

    SUBROUTINE Constructor(this)
        class(FirFilter_t), intent(in) :: this
    END SUBROUTINE
    
    SUBROUTINE destructor(this)
        type(FirFilter_t), intent(in) :: this
    END SUBROUTINE

END MODULE FirFilterMod
