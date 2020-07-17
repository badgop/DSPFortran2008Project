MODULE PSNMakerMod
    USE RandomMod
    IMPLICIT NONE

    CONTAINS

    ! перед этим нужно настроить ГЕНЕРАТОР СЛУЧАЙНЫХ ЧИСЕЛ
    FUNCTION MakePSN(psnLength) RESULT (psn)
         INTEGER(2)               , INTENT(IN)  :: psnLength
         INTEGER(1), DIMENSION(:) , ALLOCATABLE :: psn
         INTEGER(1)                             :: volume = 1
         INTEGER(8)                             :: randomValue
         INTEGER(2)                             :: i

         ALLOCATE(psn(1:psnLength))
         psn=0
         DO i=1,psnLength
            randomValue = GetRandomInt(volume)-64
            IF (randomValue>64) psn(i) = 1
         END DO

    END FUNCTION

END MODULE PSNMakerMod
