MODULE impulseGeneratorModule
    IMPLICIT NONE


    CONTAINS


    ! формирует последовательность импульсов с амплитудой [-1,1]
    ! длительность одиночного импульса tau отсчетов
    ! prbs- последовательность бит по которой формируется сигнал
    ! один элемент массива - один бит
    ! при этом [-...0] = -1
    !          [1....] =  1
    FUNCTION GenerateImpluseSequence(tau,prbs)
         INTEGER(8),INTENT(IN)    :: tau
         INTEGER(1),INTENT(IN)    :: prbs(:)
         INTEGER(1),ALLOCATABLE   :: GenerateImpluseSequence(:)
         INTEGER(8)               :: i,j
         INTEGER(8)               :: zn
         ALLOCATE (GenerateImpluseSequence(1:(size(prbs)*tau)))
         DO i=1,size(prbs)
            IF (prbs(i) == 0) THEN
               zn = -1
            ELSE
               zn =  1
            END IF
            DO j=1,tau
               GenerateImpluseSequence(j+(i-1)*tau) = zn
            END DO
         END DO
    END FUNCTION


END MODULE impulseGeneratorModule
