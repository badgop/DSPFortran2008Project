module ClippingMode
    implicit none


    CONTAINS


    FUNCTION ClipToLevelInt1(inputArray,level,outLevel) RESULT(ouputNumber)
       INTEGER(1), PARAMETER   :: intKind = 1
       INTEGER(2), INTENT(IN)  :: level
       INTEGER(2), INTENT(IN)  :: outLevel
       INTEGER(intKind), INTENT(IN)  :: inputArray(:)
       INTEGER(intKind),ALLOCATABLE        :: ouputNumber(:)

       ALLOCATE(ouputNumber, source = inputArray)

       WHERE(inputArray>=level)  ouputNumber = outLevel
       WHERE(inputArray<-level) ouputNumber  =-outLevel

    END FUNCTION ClipToLevelInt1

    FUNCTION ClipToLevelInt2(inputArray,level,outLevel) RESULT(ouputNumber)
       INTEGER(1), PARAMETER   :: intKind = 2
       INTEGER(2), INTENT(IN)  :: level
       INTEGER(2), INTENT(IN)  :: outLevel
       INTEGER(intKind), INTENT(IN)  :: inputArray(:)
       INTEGER(intKind),ALLOCATABLE        :: ouputNumber(:)

       ALLOCATE(ouputNumber, source = inputArray)

       WHERE(inputArray>=level)  ouputNumber = outLevel
       WHERE(inputArray<-level) ouputNumber  =-outLevel

       !WHERE()


    END FUNCTION ClipToLevelInt2

     FUNCTION ClipToLevelInt4(inputArray,level,outLevel) RESULT(ouputNumber)
       INTEGER(1), PARAMETER   :: intKind = 4
       INTEGER(2), INTENT(IN)  :: level
       INTEGER(2), INTENT(IN)  :: outLevel
       INTEGER(intKind), INTENT(IN)  :: inputArray(:)
       INTEGER(intKind),ALLOCATABLE        :: ouputNumber(:)

       ALLOCATE(ouputNumber, source = inputArray)

       WHERE(inputArray>=level)  ouputNumber = outLevel
       WHERE(inputArray<-level) ouputNumber  =-outLevel


    END FUNCTION ClipToLevelInt4

     FUNCTION ClipToLevelInt8(inputArray,level,outLevel) RESULT(ouputNumber)
       INTEGER(1), PARAMETER   :: intKind = 8
       INTEGER(2), INTENT(IN)  :: level
       INTEGER(2), INTENT(IN)  :: outLevel
       INTEGER(intKind), INTENT(IN)  :: inputArray(:)
       INTEGER(intKind),ALLOCATABLE        :: ouputNumber(:)

       ALLOCATE(ouputNumber,source = inputArray)

       WHERE(inputArray>=level)  ouputNumber = outLevel
       WHERE(inputArray<-level) ouputNumber  =-outLevel


    END FUNCTION ClipToLevelInt8

end module ClippingMode
