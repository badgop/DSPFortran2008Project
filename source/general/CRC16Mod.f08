MODULE CRC16Mod

    IMPLICIT NONE

    CONTAINS


    FUNCTION CRC16Compute(inputBitArrayINT8,poly)
       INTEGER(1),INTENT(IN)  :: inputBitArrayINT8(:)
       INTEGER(4),INTENT(IN)  :: poly
       INTEGER(2),ALLOCATABLE :: CRC16Compute(:)
       INTEGER(4)             :: i,j, crc16,tmp

       ALLOCATE (   CRC16Compute(1:(size(inputBitArrayINT8)+2)   ))
       crc16=z'ffff'
       DO  i = 1,size(inputBitArrayINT8)
           tmp = inputBitArrayINT8(i)
           tmp= SHIFTL(tmp,8)
           crc16=XOR(crc16,tmp)
            WRITE(*,'(z8)') crc16
           DO j=0,7
              IF (BTEST(crc16,15)) THEN
                 crc16 = XOR(crc16,poly)
                 crc16 = SHIFTL(crc16,1)
              ELSE
                 crc16 = SHIFTL(crc16,1)
              END IF
           END DO
       END DO


       crc16=AND(crc16,z'ffff')
       crc16=XOR(crc16,z'ffff')
       WRITE(*,*) size(CRC16Compute)


       CRC16Compute(1:size(inputBitArrayINT8)) = inputBitArrayINT8
       CRC16Compute(size(inputBitArrayINT8)+1) = int(SHIFTR(crc16,8),1)
       CRC16Compute(size(inputBitArrayINT8)+2) = int(crc16,1)

    END FUNCTION

END MODULE CRC16Mod
