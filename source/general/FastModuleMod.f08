MODULE FastModuleMod
    IMPLICIT NONE

    CONTAINS

! Быстрое вычисление модуля коплексного числа
! реальная и мнимая части - ИНТ8
ELEMENTAL FUNCTION GetFastMouleFromComplexInt8(realPart,imagePart)
      INTEGER(8), INTENT(IN) :: realPart
      INTEGER(8), INTENT(IN) :: imagePart
      INTEGER(8)             :: GetFastMouleFromComplexInt8
      INTEGER(8)             :: absR,absI
      absR= abs(realPart)
      absI= abs(imagePart)
      IF (absR>absI)  THEN
         GetFastMouleFromComplexInt8 = absR   + absI/2
      ELSE
         GetFastMouleFromComplexInt8 = absR/2 + absI
      END IF
    END FUNCTION  GetFastMouleFromComplexInt8




!
FUNCTION GetFastMouleFromComplexInt8_2(realPart,imagePart) RESULT (module)
      INTEGER(8), INTENT(IN) :: realPart(:)
      INTEGER(8), INTENT(IN) :: imagePart(:)
      INTEGER(2), DIMENSION(:),ALLOCATABLE :: module
      INTEGER(8)             :: absR,absI,i

      ALLOCATE(module(1:size(realPart)))

      DO i=1,size(realPart)
        absR= abs(realPart(i))
        absI= abs(imagePart(i))

      IF (absR>absI)  THEN
         module(i) = absR   + absI/2
      ELSE
         module(i) = absR/2 + absI
      END IF

      END DO

    END FUNCTION  GetFastMouleFromComplexInt8_2

END MODULE FastModuleMod

