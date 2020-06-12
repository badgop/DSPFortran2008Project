MODULE ArrayFunctionsMod
    IMPLICIT NONE



      INTERFACE ReverseArrayInt
          MODULE PROCEDURE   ReverseArrayInt1
          MODULE PROCEDURE   ReverseArrayInt2
          MODULE PROCEDURE   ReverseArrayInt4
          MODULE PROCEDURE   ReverseArrayInt8
    END INTERFACE

 CONTAINS

      SUBROUTINE ReverseArrayInt1(array)
         INTEGER(1), PARAMETER                         ::INT_KIND = 1
         INTEGER(INT_KIND),DIMENSION(:),INTENT(INOUT)  :: array
         INTEGER(INT_KIND),DIMENSION(:),ALLOCATABLE    :: arrayTmp
         INTEGER(8)                                    :: i

         ALLOCATE(arrayTmp,source = array)

         DO i=LBOUND(Array,1),UBOUND(Array,1)
            array(i)= arrayTmp(UBOUND(arrayTmp,1)-i+1)
         END DO
         DEALLOCATE(arrayTmp)
      END SUBROUTINE

     SUBROUTINE ReverseArrayInt2(array)
         INTEGER(1), PARAMETER                         ::INT_KIND = 2
         INTEGER(INT_KIND),DIMENSION(:),INTENT(INOUT)  :: array
         INTEGER(INT_KIND),DIMENSION(:),ALLOCATABLE    :: arrayTmp
         INTEGER(8)                                    :: i

         ALLOCATE(arrayTmp,source = array)

         DO i=LBOUND(Array,1),UBOUND(Array,1)
            array(i)= arrayTmp(UBOUND(arrayTmp,1)-i+1)
         END DO
         DEALLOCATE(arrayTmp)
      END SUBROUTINE

      SUBROUTINE ReverseArrayInt4(array)
         INTEGER(1), PARAMETER                         ::INT_KIND = 4
         INTEGER(INT_KIND),DIMENSION(:),INTENT(INOUT)  :: array
         INTEGER(INT_KIND),DIMENSION(:),ALLOCATABLE    :: arrayTmp
         INTEGER(8)                                    :: i

         ALLOCATE(arrayTmp,source = array)

         DO i=LBOUND(Array,1),UBOUND(Array,1)
            array(i)= arrayTmp(UBOUND(arrayTmp,1)-i+1)
         END DO
         DEALLOCATE(arrayTmp)
      END SUBROUTINE

     SUBROUTINE ReverseArrayInt8(array)
         INTEGER(1), PARAMETER                         ::INT_KIND = 8
         INTEGER(INT_KIND),DIMENSION(:),INTENT(INOUT)  :: array
         INTEGER(INT_KIND),DIMENSION(:),ALLOCATABLE    :: arrayTmp
         INTEGER(8)                                    :: i

         ALLOCATE(arrayTmp,source = array)

         DO i=LBOUND(Array,1),UBOUND(Array,1)
            array(i)= arrayTmp(UBOUND(arrayTmp,1)-i+1)
         END DO
         DEALLOCATE(arrayTmp)
      END SUBROUTINE




END MODULE ArrayFunctionsMod
