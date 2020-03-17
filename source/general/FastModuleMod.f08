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
         GetFastMouleFromComplexInt8 = absR + absI/2
      ELSE
         GetFastMouleFromComplexInt8 = absR/2 + absI
      END IF
    END FUNCTION  GetFastMouleFromComplexInt8
END MODULE FastModuleMod
