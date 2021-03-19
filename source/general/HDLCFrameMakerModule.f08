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
          INTEGER(2), PARAMETER                      :: bitStuffMaxCount = 256
          INTEGER(1), PARAMETER,DIMENSION(8)         :: flagByBit = [0,1,1,1,1,1,1,0]

          ! формируем посылку в порядке передачи старшим вперед
          tmpArray1 = GeneratePayloadDataBitArrayWithCRC(inputDataBits)
          tmpArray2 = MakeBitStuffing (tmpArray1)
          ! вставка открывающего флага
          DEALLOCATE(tmpArray1)
          WRITE(*,*) 'Вставка открывающего флага'
          tmpArray1 = InsertField (tmpArray2,flagByBit,.TRUE.)
          ! вставка закрывающего флага
          DEALLOCATE(tmpArray2)
           WRITE(*,*) 'Вставка закрывающео флага'
          tmpArray2 = InsertField (tmpArray1,flagByBit,.FALSE.)
          ! вставка преамбулы
          DEALLOCATE(tmpArray1)
           WRITE(*,*) 'Вставка преамбулы флага'
          tmpArray1 = InsertField (tmpArray2,preambuleByBit,.TRUE.)
          DEALLOCATE (tmpArray2)
          ALLOCATE (outputBits, source = tmpArray1 )

          DEALLOCATE(tmpArray1)




    END FUNCTION MakeFrameHDLC


    FUNCTION MakeBitStuffing (inputDataBits)    RESULT (outputBits)
          INTEGER(1),DIMENSION(:), INTENT(IN)        :: inputDataBits
          INTEGER(1),ALLOCATABLE,DIMENSION(:)        :: outputBits

          INTEGER(4), PARAMETER                      :: bitStuffMaxCount = 16383
          INTEGER(1),ALLOCATABLE,DIMENSION(:)        :: tmpArray1
          INTEGER(1),ALLOCATABLE,DIMENSION(:)        :: tmpArray2
          INTEGER(8)                                 :: cnt,ptr, i,totalCount,totalSize, messageSize

           ! будем считать, что число вставляемых единиц не превосходит
          ALLOCATE(tmpArray2(1:(size(inputDataBits)+bitStuffMaxCount)))
          tmpArray2 = 0
          ! осуществляем бит- стаффинг
          cnt=0
          i=1
          ptr=1
          totalCount = 0
          messageSize  = size(inputDataBits)

          DO i=1, messageSize

             IF(inputDataBits(i)==1) THEN
                cnt = cnt+1
             ELSE
                cnt = 0
             END IF

             IF(CNT==5) THEN
                tmpArray2(ptr) = 0
                totalCount = totalCount + 1
                ptr = ptr+1
                WRITE(*,*) 'BIT HAS BEEN STUFFED!'
             ELSE
                tmpArray2(ptr) = inputDataBits(i)
             END IF

             ptr= ptr +1

          END DO
          ! длина сообщения вместе с бит -стаффингом
          totalSize =  messageSize +  totalCount

          ALLOCATE(outputBits(1:totalSize))
          outputBits = 0

          DO i=1,totalSize
              outputBits(i)=tmpArray2(i)
          END DO

          DEALLOCATE (tmpArray2)
    END FUNCTION MakeBitStuffing

     FUNCTION InsertField (inputDataBits,field,atBegin)    RESULT (outputBits)
          INTEGER(1),DIMENSION(:), INTENT(IN)        :: inputDataBits
          INTEGER(1),DIMENSION(:), INTENT(IN)        :: field
          LOGICAL                , INTENT(IN)        :: atBegin
          INTEGER(1),ALLOCATABLE,DIMENSION(:)        :: outputBits

          INTEGER(8)                                 :: inputSize, fieldSize, totalSize

          inputSize = size(inputDataBits)
          fieldSize = size(field)
          totalSize = inputSize + fieldSize
          ALLOCATE(outputBits(1:totalSize))
          outputBits = 0
          IF (atBegin) THEN
             outputBits(1:fieldSize)                          = field
             outputBits(fieldSize+1:fieldSize + inputSize )   = inputDataBits
          ELSE
             outputBits(1:inputSize )                         = inputDataBits
             outputBits(inputSize+1:inputSize+fieldSize )   = field
          END IF

     END FUNCTION InsertField




end module HDLCFrameMakerModule
