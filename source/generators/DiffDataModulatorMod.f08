MODULE DiffDataModulatorMod
    USE analyticSignalModule
    USE impulseGeneratorModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC  :: DiffCodeGenerator_t
        PRIVATE
        INTEGER   :: initialPhase
    CONTAINS
        PROCEDURE :: Constructor
        PROCEDURE :: GenerateDBPSKData
        PROCEDURE :: DecodeDBPSKData
        FINAL     :: destructor
    END TYPE DiffCodeGenerator_t

CONTAINS

    SUBROUTINE constructor(this,initialPhase)
        CLASS(DiffCodeGenerator_t), intent(inout) :: this
        INTEGER(8), intent(in)                    :: initialPhase
        IF (initialPhase>0) THEN
            this%initialPhase = 1
            ELSE
             this%initialPhase = 0
        END IF
    END SUBROUTINE

    FUNCTION GenerateDBPSKData(this,data)
        CLASS(DiffCodeGenerator_t), intent(in) :: this
        INTEGER(1), allocatable                :: GenerateDBPSKData(:)
        INTEGER(1),INTENT(IN)                  :: data(:)
        INTEGER(8)                             :: i
        ALLOCATE (GenerateDBPSKData(1:(size(data)+1)))
        GenerateDBPSKData(1) = (this%initialPhase)
        DO i=2,size(data)+1
           GenerateDBPSKData(i) = XOR (GenerateDBPSKData(i-1),data(i-1))
        END DO
     END FUNCTION

     FUNCTION DecodeDBPSKData(this,data)
        CLASS(DiffCodeGenerator_t), intent(in) :: this
        INTEGER(1), allocatable                :: DecodeDBPSKData(:)
        INTEGER(1),INTENT(IN)                  :: data(:)
        INTEGER(8)                             :: i

        ALLOCATE (DecodeDBPSKData(1:(size(data)-1)))

        DO i=1,size(data)-1
           DecodeDBPSKData(i) = XOR (data(i),data(i+1))
        END DO


      END FUNCTION DecodeDBPSKData

     SUBROUTINE destructor(this)
        type(DiffCodeGenerator_t), intent(in) :: this
    END SUBROUTINE

END MODULE DiffDataModulatorMod
