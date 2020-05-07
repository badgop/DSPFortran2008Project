MODULE PSNSimpleMod
    USE analyticSignalModule
    USE impulseGeneratorModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: PSNSimple_t
        PRIVATE
        INTEGER(1), ALLOCATABLE :: psn(:)
        INTEGER(8)              :: osr
    CONTAINS
        PROCEDURE :: Constructor

        PROCEDURE :: OutPutPsnArray
        PROCEDURE :: OutPutPsnAs


        FINAL     :: destructor
    END TYPE PSNSimple_t

CONTAINS

    SUBROUTINE Constructor(this, input_psn,osr)
        class(PSNSimple_t), intent(inOUT) :: this
        INTEGER (1)       , intent(in)    :: input_psn(:)
        INTEGER(8)        , intent(in)    :: osr
        ALLOCATE (this%psn, source = input_psn )
        this%osr = osr
        this%psn =  GenerateImpluseSequence(osr,input_psn)
!        WRITE (*,*) 'psn lrn ',size(this%psn)
    END SUBROUTINE
    
    FUNCTION OutPutPsnAs(this, lengthInBlocks)
        class(PSNSimple_t)     , intent(inOUT)     :: this
        INTEGER (8)            , intent(in)        :: lengthInBlocks
        CLASS(analyticSignal_t), allocatable       :: OutPutPsnAs
        INTEGER(8)             , ALLOCATABLE       :: psnTmp(:)
        INTEGER(8)                                 :: i
        INTEGER(8)                                 :: psnLen
        ALLOCATE(OutPutPsnAs)
        psnLen = size(this%psn)
        ALLOCATE (psnTmp(1:(lengthInBlocks*(psnLen))))
!        WRITE (*,*) 'size ', size(psnTmp)
        DO i=1,lengthInBlocks
           psnTmp((psnLen*(i-1)+1):(psnLen*i))= this%psn(1:psnLen)
        END DO
        CALL OutPutPsnAs%Constructor(psnTmp)
        DEALLOCATE(psnTmp)
    END FUNCTION OutPutPsnAs

      FUNCTION OutPutPsnArray(this, lengthInBlocks)
        class(PSNSimple_t)     , intent(inOUT)     :: this
        INTEGER (8)            , intent(in)        :: lengthInBlocks

        INTEGER(8)             , ALLOCATABLE       :: OutPutPsnArray(:)
        INTEGER(8)                                 :: i
        INTEGER(8)                                 :: psnLen

        psnLen = size(this%psn)
        ALLOCATE (OutPutPsnArray(1:(lengthInBlocks*(psnLen))))
!        WRITE (*,*) 'size ', size(OutPutPsnArray)
        DO i=1,lengthInBlocks
           OutPutPsnArray((psnLen*(i-1)+1):(psnLen*i))= this%psn(1:psnLen)
        END DO

    END FUNCTION OutPutPsnArray

    SUBROUTINE destructor(this)
        type(PSNSimple_t), intent(inout) :: this
        IF (ALLOCATED(this%psn)) DEALLOCATE (this%psn)

    END SUBROUTINE

END MODULE PSNSimpleMod
