MODULE PSNSimpleMod
    USE analyticSignalModule
    USE impulseGeneratorModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: PSNSimple_t
        PRIVATE
        INTEGER(8), ALLOCATABLE :: psn(:)
        INTEGER(8)              :: osr
    CONTAINS
        PROCEDURE :: Constructor
        PROCEDURE :: OutPutPsn

        FINAL     :: destructor
    END TYPE PSNSimple_t

CONTAINS

    SUBROUTINE Constructor(this, input_psn,osr)
        class(PSNSimple_t), intent(inOUT) :: this
        INTEGER (8)       , intent(in)    :: input_psn(:)
        INTEGER(8)        , intent(in)    :: osr
        ALLOCATE (this%psn, source = input_psn )
        this%osr = osr

        this%psn =  GenerateImpluseSequence(osr,input_psn)
        WRITE (*,*) 'psn lrn ',size(this%psn)


    END SUBROUTINE
    
    FUNCTION OutPutPsn(this, lengthInBlocks)
        class(PSNSimple_t)     , intent(inOUT)     :: this
        INTEGER (8)            , intent(in)        :: lengthInBlocks
        CLASS(analyticSignal_t), allocatable       :: OutPutPsn
        INTEGER(8)             , ALLOCATABLE       :: psnTmp(:)
        INTEGER(8)                                 :: i
        INTEGER(8)                                 :: psnLen
        ALLOCATE(OutPutPsn)
        psnLen = size(this%psn)
        ALLOCATE (psnTmp(1:(lengthInBlocks*(psnLen))))
        WRITE (*,*) 'size ', size(psnTmp)
        DO i=1,lengthInBlocks
           psnTmp((psnLen*(i-1)+1):(psnLen*i))= this%psn(1:psnLen)
        END DO
        CALL OutPutPsn%Constructor(psnTmp)
        DEALLOCATE(psnTmp)
    END FUNCTION

    SUBROUTINE destructor(this)
        type(PSNSimple_t), intent(inout) :: this
        IF (ALLOCATED(this%psn)) DEALLOCATE (this%psn)

    END SUBROUTINE

END MODULE PSNSimpleMod
