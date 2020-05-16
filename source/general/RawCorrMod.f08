module RawCorrMod
    implicit none

    PRIVATE

    PUBLIC :: CorrelationRaw



    INTERFACE CorrelationRaw

          MODULE PROCEDURE       CorrelationRaw88
          MODULE PROCEDURE       CorrelationRaw84
          MODULE PROCEDURE       CorrelationRaw82
          MODULE PROCEDURE       CorrelationRaw81
          MODULE PROCEDURE       CorrelationRaw11
          MODULE PROCEDURE       CorrelationRaw12
          MODULE PROCEDURE       CorrelationRaw14
          MODULE PROCEDURE       CorrelationRaw18
          MODULE PROCEDURE       CorrelationRaw21
          MODULE PROCEDURE       CorrelationRaw22
          MODULE PROCEDURE       CorrelationRaw24
          MODULE PROCEDURE       CorrelationRaw28
          MODULE PROCEDURE       CorrelationRaw41
          MODULE PROCEDURE       CorrelationRaw42
          MODULE PROCEDURE       CorrelationRaw44
          MODULE PROCEDURE       CorrelationRaw48

    END INTERFACE


    CONTAINS

        ! Вычисление корр. функции (Raw-  англ. сырая.)
    ! пределы [0 : длина входного сигнала- длина опорного сигнала]
     PURE FUNCTION CorrelationRaw88(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=8
          INTEGER(1),PARAMETER                       :: arrayKindReference=8
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw88(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw88(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw88=0
          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw88(i)=CorrelationRaw88(i)+input(i+j)*reference(j)
                END DO
          END DO
    END FUNCTION   CorrelationRaw88

       PURE FUNCTION CorrelationRaw84(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=8
          INTEGER(1),PARAMETER                       :: arrayKindReference=4
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw84(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw84(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw84=0
          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw84(i)=CorrelationRaw84(i)+input(i+j)*int(reference(j),8)
                END DO
          END DO
    END FUNCTION   CorrelationRaw84

     PURE FUNCTION CorrelationRaw82(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=8
          INTEGER(1),PARAMETER                       :: arrayKindReference=2
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw82(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw82(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw82=0
          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw82(i)=CorrelationRaw82(i)+input(i+j)*int(reference(j),8)
                END DO
          END DO
    END FUNCTION   CorrelationRaw82

         PURE FUNCTION CorrelationRaw81(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=8
          INTEGER(1),PARAMETER                       :: arrayKindReference=1
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw81(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw81(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw81=0
          ! Выбрать пределы корреляции
         ! WRITE(*,*) 'RAW CORR 81 IN'
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw81(i)=CorrelationRaw81(i)+input(i+j)*int(reference(j),8)
                END DO
          END DO
         ! WRITE(*,*) 'RAW CORR 81 OUT'
    END FUNCTION   CorrelationRaw81

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
                    CorrelationRaw11(i)=CorrelationRaw11(i)+int(input(i+j),8)*int(reference(j),8)
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
                    CorrelationRaw12(i)=CorrelationRaw12(i)+int(input(i+j),8)*int(reference(j),8)
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
                    CorrelationRaw14(i)=CorrelationRaw14(i)+int(input(i+j),8)*int(reference(j),8)
                END DO
          END DO
    END FUNCTION   CorrelationRaw14

        PURE FUNCTION CorrelationRaw18(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=1
          INTEGER(1),PARAMETER                       :: arrayKindReference=8
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw18(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw18(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw18=0
          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw18(i)=CorrelationRaw18(i)+int(input(i+j),8)*int(reference(j),8)
                END DO
          END DO
    END FUNCTION   CorrelationRaw18


          PURE FUNCTION CorrelationRaw21(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=2
          INTEGER(1),PARAMETER                       :: arrayKindReference=1
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw21(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw21(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw21=0
          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw21(i)=CorrelationRaw21(i)+int(input(i+j),8)*int(reference(j),8)
                END DO
          END DO
    END FUNCTION   CorrelationRaw21

     PURE  FUNCTION CorrelationRaw22(input,reference)
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

                    CorrelationRaw22(i) = CorrelationRaw22(i)+ int(input(i+j),8)*int(reference(j),8)

                END DO
          END DO
    END FUNCTION   CorrelationRaw22

   PURE FUNCTION CorrelationRaw24(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=2
          INTEGER(1),PARAMETER                       :: arrayKindReference=4
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw24(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw24(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw24=0
          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw24(i)=CorrelationRaw24(i)+int(input(i+j),8)*int(reference(j),8)
                END DO
          END DO
    END FUNCTION   CorrelationRaw24

       PURE FUNCTION CorrelationRaw28(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=2
          INTEGER(1),PARAMETER                       :: arrayKindReference=8
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw28(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw28(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw28=0
          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw28(i)=CorrelationRaw28(i)+int(input(i+j),8)*int(reference(j),8)
                END DO
          END DO
    END FUNCTION   CorrelationRaw28

       PURE FUNCTION CorrelationRaw41(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=4
          INTEGER(1),PARAMETER                       :: arrayKindReference=1
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw41(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw41(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw41=0
          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw41(i)=CorrelationRaw41(i)+int(input(i+j),8)*int(reference(j),8)
                END DO
          END DO
    END FUNCTION   CorrelationRaw41

    PURE FUNCTION CorrelationRaw42(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=4
          INTEGER(1),PARAMETER                       :: arrayKindReference=2
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw42(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw42(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw42=0
          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw42(i)=CorrelationRaw42(i)+int(input(i+j),8)*int(reference(j),8)
                END DO
          END DO
    END FUNCTION   CorrelationRaw42

        PURE FUNCTION CorrelationRaw44(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=4
          INTEGER(1),PARAMETER                       :: arrayKindReference=4
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw44(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw44(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw44=0
          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw44(i)=CorrelationRaw44(i)+int(input(i+j),8)*int(reference(j),8)
                END DO
          END DO
    END FUNCTION   CorrelationRaw44

   PURE FUNCTION CorrelationRaw48(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=4
          INTEGER(1),PARAMETER                       :: arrayKindReference=8
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw48(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw48(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw48=0
          ! Выбрать пределы корреляции
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                    CorrelationRaw48(i)=CorrelationRaw48(i)+int(input(i+j),8)*reference(j)
                END DO
          END DO
    END FUNCTION   CorrelationRaw48




end module RawCorrMod
