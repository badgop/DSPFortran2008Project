MODULE CRC16Mod

    IMPLICIT NONE

    CONTAINS


    FUNCTION CRC16Compute(inputBitArrayINT8,poly)
       INTEGER(1),INTENT(IN)  :: inputBitArrayINT8(:)
       INTEGER(4),INTENT(IN)  :: poly
       INTEGER(2)             :: CRC16Compute
       INTEGER(4)             :: i,j, crc16,tmp


       crc16=z'ffff'
       DO  i = 1,size(inputBitArrayINT8)
           tmp = inputBitArrayINT8(i)
           tmp= SHIFTL(tmp,8)
           crc16=XOR(crc16,tmp)
           ! необязательно, но только в целях вывода
           !crc16=AND(crc16,z'ffff')
           DO j=1,8
              IF (BTEST(crc16,15)) THEN
                 crc16 = SHIFTL(crc16,1)
                 crc16 = XOR(crc16,poly)
              ELSE
                 crc16 = SHIFTL(crc16,1)
              END IF
           END DO
       END DO


       crc16=AND(crc16,z'ffff')
       crc16=XOR(crc16,z'ffff')
       CRC16Compute= crc16
!       CRC16Compute(1:size(inputBitArrayINT8)) = inputBitArrayINT8
!       CRC16Compute(size(inputBitArrayINT8)+1) = int(SHIFTR(crc16,8),1)
!       CRC16Compute(size(inputBitArrayINT8)+2) = int(crc16,1)

    END FUNCTION

END MODULE CRC16Mod
