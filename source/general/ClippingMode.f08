module ClippingMode
    implicit none


    ELEMENTAL FUNCTION ClipToLevelInt1(inputNumber,level) RESULT(ouputNumber)
       INTEGER(1), PARAMETER   :: intKind = 1
       INTEGER(1), INTENT(IN)  :: level
       INTEGER(intKind)        :: ouputNumber

       SELECT CASE (inputNumber):

              CASE(:-level)
                  ouputNumber = - level
              CASE(level:)


    END FUNCTION ClipToLevelInt1

end module ClippingMode
