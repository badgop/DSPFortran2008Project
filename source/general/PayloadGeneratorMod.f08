MODULE PayloadGeneratorMod
    USE RandomMod
    USE OctetDataModule
    USE CRC16Mod
    IMPLICIT NONE

    CONTAINS


     FUNCTION GenerateRandomPayloadBitArray(dataLength) RESULT (dataArary)
          INTEGER(1),DIMENSION(:) ,ALLOCATABLE               :: dataArary
          INTEGER(4)            ,INTENT(IN)    :: dataLength
          INTEGER(4)                           :: i
          CALL RanomGeneratorInit()
          ALLOCATE(dataArary(1:dataLength))
          dataArary = 0
          DO i = 1,dataLength
             IF (GetRandomInt(int(1,1)) > 64  ) THEN
                dataArary(i) = 1
             END IF
          END DO
    END FUNCTION GenerateRandomPayloadBitArray

     ! тоже самое но с CRC
     FUNCTION GenerateRandomPayloadBitArrayWithCRC(dataLength) RESULT (dataAraryCRC)
          INTEGER(1),DIMENSION(:) ,ALLOCATABLE               :: dataAraryCRC
          INTEGER(4)            ,INTENT(IN)    :: dataLength
          INTEGER(1),DIMENSION(:) ,ALLOCATABLE :: data
          data = GenerateRandomPayloadBitArray(dataLength)
          dataAraryCRC = GeneratePayloadDataBitArrayWithCRC (data)
    END FUNCTION GenerateRandomPayloadBitArrayWithCRC

    FUNCTION GeneratePayloadDataOctetsWithCRC(bitDataArary) RESULT(dataOctetsWithCrc)
          INTEGER(1)            ,INTENT(INOUT) :: bitDataArary(:)
          INTEGER(1),ALLOCATABLE               :: OctetdataArary(:)
          INTEGER(1),ALLOCATABLE               :: dataOctetsWithCrc(:)
          INTEGER(2)                           :: CRC16

          OctetdataArary = BitsToOctets(bitDataArary,.TRUE.)
          OctetdataArary = ReverseBitOrderINT1(OctetdataArary)
          CRC16 = CRC16Compute(OctetdataArary, 4129,65535)
          ALLOCATE(dataOctetsWithCrc(1:(size(OctetdataArary)+2)))
          dataOctetsWithCrc(1:size(OctetdataArary))= OctetdataArary
          dataOctetsWithCrc(size(OctetdataArary)+1) = int(SHIFTR(crc16,8),1)
          dataOctetsWithCrc(size(OctetdataArary)+2) = int(crc16,1)
      END FUNCTION  GeneratePayloadDataOctetsWithCRC

      ! формирует битовый массив с контрольной суммой
      ! передача данных ведется со СТАРШЕГО БИТА (старший это, например, самый младший),
      ! см. ниже
      ! |7| |6| |5| |4| |3| |2| |1| |0|  - разряды числа
      ! |1| |2| |3| |4| |5| |6| |7| |8|  - разряды сообщения в пакете
      FUNCTION GeneratePayloadDataBitArrayWithCRC(bitDataArary) RESULT(BitArrayWithCrc)
          INTEGER(1),DIMENSION(:)       ,INTENT(IN)         :: bitDataArary
          INTEGER(1),DIMENSION(:),ALLOCATABLE               :: OctetdataArary
          INTEGER(1),DIMENSION(:),ALLOCATABLE               :: dataOctetsWithCrc
          INTEGER(1),DIMENSION(:),ALLOCATABLE               :: BitArrayWithCrc

          INTEGER(2)                           :: CRC16

          OctetdataArary = BitsToOctets(bitDataArary,.TRUE.)
          OctetdataArary = ReverseBitOrderINT1(OctetdataArary)
          CRC16 = CRC16Compute(OctetdataArary, 4129,65535)
          ALLOCATE(dataOctetsWithCrc(1:(size(OctetdataArary)+2)))
          dataOctetsWithCrc(1:size(OctetdataArary))= OctetdataArary
          dataOctetsWithCrc(size(OctetdataArary)+1) = int(SHIFTR(crc16,8),1)
          dataOctetsWithCrc(size(OctetdataArary)+2) = int(crc16,1)
          BitArrayWithCrc = OctetsToBits(dataOctetsWithCrc,.TRUE.)

      END FUNCTION GeneratePayloadDataBitArrayWithCRC


      FUNCTION CheckCRC(octetData)  RESULT(crcOk)
         INTEGER(1),INTENT(IN) :: octetData(:)
         LOGICAL               :: crcOk
         INTEGER(2)            :: crc16

         crcOk = .FALSE.
         crc16 =  CRC16Compute(octetData, 4129,65535)
         crc16=XOR(crc16,z'ffff')
         IF (crc16 == z'1D0F') crcOk = .TRUE.
      END FUNCTION CheckCRC


END MODULE PayloadGeneratorMod
