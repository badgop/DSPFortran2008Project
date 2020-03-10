MODULE DiffDataModulatorMod
    USE analyticSignalModule
    USE impulseGeneratorModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: DiffCodeGenerator_t
        PRIVATE
        INTEGER :: initialPhase
    CONTAINS
        PROCEDURE :: Constructor
        PROCEDURE :: GenerateDBPSKData
        FINAL     :: destructor
    END TYPE DiffCodeGenerator_t

CONTAINS

    SUBROUTINE constructor(this,initialPhase)
        class(DiffCodeGenerator_t), intent(inout) :: this
        INTEGER(8), intent(in)                 :: initialPhase
        IF (initialPhase>0) THEN
            this%initialPhase = 1
            ELSE
             this%initialPhase = 0
        END IF

    END SUBROUTINE
    
    FUNCTION GenerateDBPSKData(this,data)
        class(DiffCodeGenerator_t), intent(in) :: this
        INTEGER(8), allocatable   ::  GenerateDBPSKData(:)
        INTEGER(8),INTENT(IN)                  :: data(:)

        INTEGER(8)                             :: i
        ALLOCATE (GenerateDBPSKData(1:(size(data)+1)))
        GenerateDBPSKData(1) = (this%initialPhase)
        DO i=2,size(data)
           GenerateDBPSKData(i) = XOR (GenerateDBPSKData(i-1),data(i))
        END DO

     END FUNCTION



     SUBROUTINE destructor(this)
        type(DiffCodeGenerator_t), intent(in) :: this
    END SUBROUTINE

END MODULE DiffDataModulatorMod
