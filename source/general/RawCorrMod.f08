module RawCorrMod
    implicit none

    PRIVATE

    PUBLIC :: CorrelationRaw



    INTERFACE CorrelationRaw

          MODULE PROCEDURE       CorrelationRaw8

    END INTERFACE


    CONTAINS

        ! Вычисление корр. функции (Raw-  англ. сырая.)
    ! пределы [0 : длина входного сигнала- длина опорного сигнала]
     PURE FUNCTION CorrelationRaw8(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=8
          INTEGER(1),PARAMETER                       :: arrayKindReference=8
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw8(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw8(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw8=0
          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw8(i)=CorrelationRaw8(i)+input(i+j)*reference(j)
                END DO
          END DO
    END FUNCTION   CorrelationRaw8

            ! Вычисление корр. функции (Raw-  англ. сырая.)
    ! пределы [0 : длина входного сигнала- длина опорного сигнала]
     PURE FUNCTION CorrelationRaw11(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=1
          INTEGER(1),PARAMETER                       :: arrayKindReference=1
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw11(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw11(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw11=0
          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw11(i)=CorrelationRaw11(i)+input(i+j)*reference(j)
                END DO
          END DO
    END FUNCTION   CorrelationRaw11

               ! Вычисление корр. функции (Raw-  англ. сырая.)
    ! пределы [0 : длина входного сигнала- длина опорного сигнала]
     PURE FUNCTION CorrelationRaw12(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=1
          INTEGER(1),PARAMETER                       :: arrayKindReference=2
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw12(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw12(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw12=0
          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw12(i)=CorrelationRaw12(i)+input(i+j)*reference(j)
                END DO
          END DO
    END FUNCTION   CorrelationRaw12

                   ! Вычисление корр. функции (Raw-  англ. сырая.)
    ! пределы [0 : длина входного сигнала- длина опорного сигнала]
     PURE FUNCTION CorrelationRaw14(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=1
          INTEGER(1),PARAMETER                       :: arrayKindReference=4
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw14(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw14(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw14=0
          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw14(i)=CorrelationRaw14(i)+input(i+j)*reference(j)
                END DO
          END DO
    END FUNCTION   CorrelationRaw14

      PURE FUNCTION CorrelationRaw22(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=2
          INTEGER(1),PARAMETER                       :: arrayKindReference=2
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw22(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw22(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw22=0
          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw22(i)=CorrelationRaw22(i)+input(i+j)*reference(j)
                END DO
          END DO
    END FUNCTION   CorrelationRaw22

end module RawCorrMod
