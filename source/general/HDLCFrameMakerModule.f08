module HDLCFrameMakerModule
    USE PayloadGeneratorMod
    implicit none


    CONTAINS
    ! на входе массив с битами (1 эл - один бит)
    ! на выход - массив с битами , дополенный обучающей полседовательностью, флагами.
    ! передача СТАРШим битом вперед
    FUNCTION MakeFrameHDLC (inputDataBits, preambuleByBit)    RESULT (outputBits)
          INTEGER(1),DIMENSION(:), INTENT(IN)        :: inputDataBits
          INTEGER(1),DIMENSION(:), INTENT(IN)        :: preambuleByBit
          INTEGER(1),ALLOCATABLE,DIMENSION(:)        :: outputBits
          INTEGER(1),ALLOCATABLE,DIMENSION(:)        :: tmpArray1
          INTEGER(1),ALLOCATABLE,DIMENSION(:)        :: tmpArray2
          INTEGER(4)                                 :: cnt,ptr, i,totalCount,totalSize, messageSize
          INTEGER(2), PARAMETER                      :: bitStuffMaxCount = 256
          INTEGER(1), PARAMETER,DIMENSION(8)         :: flagByBit = [1,0,0,0,0,0,0,1]

          ! формируем посылку в порядке передачи старшим вперед
          tmpArray1 = GeneratePayloadDataBitArrayWithCRC(inputDataBits)
          ! будем считать, что число вставляемых единиц не превосходит
          ALLOCATE(tmpArray2(1:(size(tmpArray1)+bitStuffMaxCount)))
          tmpArray2 = 0

          ! осуществляем бит- стаффинг
          cnt=0
          i=1
          ptr=1
          totalCount = 0
          messageSize  = size(tmpArray1)
          DO WHILE(i < totalSize)
             IF(tmpArray1(i)==1) THEN
                cnt = cnt+1
             ELSE
                cnt = 0
             END IF

             IF(CNT==5) THEN
                tmpArray2(ptr) = 0
                totalCount = totalCount + 1
             ELSE
                tmpArray2(ptr) = tmpArray1(i)
                i = i +1
             END IF
          END DO
          ! длина сообщения вместе с бит -стаффингом
          messageSize =  messageSize +  totalCount


          DEALLOCATE(tmpArray1)
          ALLOCATE(tmpArray1(1:totalSize))
          tmpArray1 = 0

          DO i=1,totalSize
              tmpArray1(i)=tmpArray2(i)
          END DO

    END FUNCTION MakeFrameHDLC







end module HDLCFrameMakerModule
