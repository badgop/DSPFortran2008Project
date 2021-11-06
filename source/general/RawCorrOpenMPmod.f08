module RawCorrOpenMPmod
    implicit none


    INTERFACE
            INTEGER FUNCTION OMP_GET_THREAD_NUM()
            END FUNCTION
    END INTERFACE


    INTERFACE
            INTEGER FUNCTION omp_get_num_threads()
            END FUNCTION
    END INTERFACE





    PRIVATE

    PUBLIC :: CorrelationRawOpemMP



    INTERFACE CorrelationRawOpemMP

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
      FUNCTION CorrelationRaw88(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=8
          INTEGER(1),PARAMETER                       :: arrayKindReference=8
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw88(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          INTEGER(8)                                 :: summ
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw88(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw88=0
           summ=0
          ! Выбрать пределы корреляции
  !$omp parallel do SHARED(input,reference,CorrelationRaw88) PRIVATE(i,j) REDUCTION(+:summ)
          DO i=1,inputLen-referenceLen

                DO j=1,referenceLen
                    summ=summ+input(i+j)*int(reference(j),8)
                END DO

                CorrelationRaw88(i)=summ
                summ=0

          END DO
 !$omp end parallel do
    END FUNCTION   CorrelationRaw88

       FUNCTION CorrelationRaw84(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=8
          INTEGER(1),PARAMETER                       :: arrayKindReference=4
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw84(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          INTEGER(8)                                 :: summ
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw84(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw84=0
          summ=0
          ! Выбрать пределы корреляции
  !$omp parallel do SHARED(input,reference,CorrelationRaw84) PRIVATE(i,j) REDUCTION(+:summ)
          DO i=1,inputLen-referenceLen

                DO j=1,referenceLen
                    summ=summ+input(i+j)*int(reference(j),8)
                END DO

                CorrelationRaw84(i)=summ
                summ=0

          END DO
 !$omp end parallel do
    END FUNCTION   CorrelationRaw84

    FUNCTION CorrelationRaw82(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=8
          INTEGER(1),PARAMETER                       :: arrayKindReference=2
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw82(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          INTEGER(8)                                 :: summ
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw82(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw82=0
           summ=0
          ! Выбрать пределы корреляции
  !$omp parallel do SHARED(input,reference,CorrelationRaw82) PRIVATE(i,j) REDUCTION(+:summ)
          DO i=1,inputLen-referenceLen

                DO j=1,referenceLen
                    summ=summ+input(i+j)*int(reference(j),8)
                END DO

                CorrelationRaw82(i)=summ
                summ=0

          END DO
 !$omp end parallel do
    END FUNCTION   CorrelationRaw82

         FUNCTION CorrelationRaw81(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=8
          INTEGER(1),PARAMETER                       :: arrayKindReference=1
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw81(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          INTEGER(8)                                 :: summ

          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw81(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw81=0
          ! Выбрать пределы корреляции
         ! WRITE(*,*) 'RAW CORR 81 IN'


          summ=0

  !$omp parallel do SHARED(input,reference,CorrelationRaw81) PRIVATE(i,j) REDUCTION(+:summ)
          DO i=1,inputLen-referenceLen

                DO j=1,referenceLen
                    summ=summ+input(i+j)*int(reference(j),8)
                END DO

                CorrelationRaw81(i)=summ
                summ=0

          END DO
 !$omp end parallel do

        !  WRITE(*,*) 'RAW CORR 81 OUT'
    END FUNCTION   CorrelationRaw81

            ! Вычисление корр. функции (Raw-  англ. сырая.)
    ! пределы [0 : длина входного сигнала- длина опорного сигнала]
     FUNCTION CorrelationRaw11(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=1
          INTEGER(1),PARAMETER                       :: arrayKindReference=1
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw11(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          INTEGER(8)                                 :: summ
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw11(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw11=0
          ! Выбрать пределы корреляции
                summ=0
          ! Выбрать пределы корреляции
  !$omp parallel do SHARED(input,reference,CorrelationRaw11) PRIVATE(i,j) REDUCTION(+:summ)
          DO i=1,inputLen-referenceLen

                DO j=1,referenceLen
                    summ=summ+input(i+j)*int(reference(j),8)
                END DO

                CorrelationRaw11(i)=summ
                summ=0

          END DO
 !$omp end parallel do
    END FUNCTION   CorrelationRaw11

               ! Вычисление корр. функции (Raw-  англ. сырая.)
    ! пределы [0 : длина входного сигнала- длина опорного сигнала]
    FUNCTION CorrelationRaw12(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=1
          INTEGER(1),PARAMETER                       :: arrayKindReference=2
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw12(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
           INTEGER(8)                                 :: summ
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw12(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw12=0
          summ=0
          ! Выбрать пределы корреляции
  !$omp parallel do SHARED(input,reference,CorrelationRaw12) PRIVATE(i,j) REDUCTION(+:summ)
          DO i=1,inputLen-referenceLen

                DO j=1,referenceLen
                    summ=summ+input(i+j)*int(reference(j),8)
                END DO

                CorrelationRaw12(i)=summ
                summ=0

          END DO
 !$omp end parallel do

    END FUNCTION   CorrelationRaw12

                   ! Вычисление корр. функции (Raw-  англ. сырая.)
    ! пределы [0 : длина входного сигнала- длина опорного сигнала]
     FUNCTION CorrelationRaw14(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=1
          INTEGER(1),PARAMETER                       :: arrayKindReference=4
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw14(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          INTEGER(8)                                 :: summ

          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw14(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw14=0
          summ=0
          ! Выбрать пределы корреляции
  !$omp parallel do SHARED(input,reference,CorrelationRaw14) PRIVATE(i,j) REDUCTION(+:summ)
          DO i=1,inputLen-referenceLen

                DO j=1,referenceLen
                    summ=summ+input(i+j)*int(reference(j),8)
                END DO

                CorrelationRaw14(i)=summ
                summ=0

          END DO
 !$omp end parallel do
    END FUNCTION   CorrelationRaw14

        FUNCTION CorrelationRaw18(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=1
          INTEGER(1),PARAMETER                       :: arrayKindReference=8
          INTEGER(arrayKindInput),DIMENSION(:)     ,INTENT(IN)    :: input
          INTEGER(arrayKindReference),DIMENSION(:)  ,INTENT(IN)    :: reference
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw18(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          INTEGER(8)                                 :: summ
          ! длительность выходного сигнала в отсчетах
          WRITE(*,*) 'CorrelationRaw18(input,reference)'
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw18(1:inputLen))
          summ = 0
          ! что бы не было мусора в элементах массива
          CorrelationRaw18=0
          ! Выбрать пределы корреляции
           !$omp parallel do SHARED(input,reference,CorrelationRaw18) PRIVATE(i,j) REDUCTION(+:summ)
           DO i=1,inputLen-referenceLen
                DO j=1,referenceLen

                   summ = summ +CorrelationRaw18(i)+int(input(i+j),8)*reference(j)

                END DO
                   CorrelationRaw18(i) = summ

                   summ = 0
          END DO
          !$omp end parallel do

          WRITE (*,*) 'max coorr ',maxVal(CorrelationRaw18)
    END FUNCTION   CorrelationRaw18


          FUNCTION CorrelationRaw21(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=2
          INTEGER(1),PARAMETER                       :: arrayKindReference=1
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw21(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
           INTEGER(8)                                 :: summ
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw21(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw21=0
          ! Выбрать пределы корреляции
          summ = 0
         !$omp parallel do SHARED(input,reference,CorrelationRaw21) PRIVATE(i,j) REDUCTION(+:summ)
          DO i=1,inputLen-referenceLen

                DO j=1,referenceLen
                    summ=summ+input(i+j)*int(reference(j),8)
                END DO

                CorrelationRaw21(i)=summ
                summ=0

          END DO
 !$omp end parallel do
    END FUNCTION   CorrelationRaw21

      FUNCTION CorrelationRaw22(input,reference)
         INTEGER(1),PARAMETER                       :: arrayKindInput=2
          INTEGER(1),PARAMETER                       :: arrayKindReference=2
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw22(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          INTEGER(8)                                 :: summ



          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw22(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw22=0
          ! Выбрать пределы корреляции
         ! WRITE(*,*) 'RAW CORR 81 IN'
          summ=0

  !$omp parallel do SHARED(input,reference,CorrelationRaw22) PRIVATE(i,j) REDUCTION(+:summ)
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                     summ=summ+int(input(i+j),8)*int(reference(j),8)
                END DO
                CorrelationRaw22(i)=summ
                summ=0!
          END DO
 !$omp end parallel do

        !  WRITE(*,*) 'RAW CORR 81 OUT'
    END FUNCTION   CorrelationRaw22

   FUNCTION CorrelationRaw24(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=2
          INTEGER(1),PARAMETER                       :: arrayKindReference=4
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw24(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          INTEGER(8)                                 :: summ=0
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw24(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw24=0
          ! Выбрать пределы корреляции
                  summ=0

  !$omp parallel do SHARED(input,reference,CorrelationRaw24) PRIVATE(i,j) REDUCTION(+:summ)
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                     summ=summ+int(input(i+j),8)*int(reference(j),8)
                END DO
                CorrelationRaw24(i)=summ
                summ=0!
          END DO
 !$omp end parallel do
    END FUNCTION   CorrelationRaw24

      FUNCTION CorrelationRaw28(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=2
          INTEGER(1),PARAMETER                       :: arrayKindReference=8
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw28(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
           INTEGER(8)                                :: summ
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw28(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw28=0
          ! Выбрать пределы корреляции
          summ=0
         !$omp parallel do SHARED(input,reference,CorrelationRaw28) PRIVATE(i,j) REDUCTION(+:summ)
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                     summ=summ+int(input(i+j),8)*int(reference(j),8)
                END DO
                CorrelationRaw28(i)=summ
                summ=0!
          END DO
 !$omp end parallel do
    END FUNCTION   CorrelationRaw28

       FUNCTION CorrelationRaw41(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=4
          INTEGER(1),PARAMETER                       :: arrayKindReference=1
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw41(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          INTEGER(8)                                 :: summ

          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw41(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw41=0
          ! Выбрать пределы корреляции
          summ=0
         !$omp parallel do SHARED(input,reference,CorrelationRaw41) PRIVATE(i,j) REDUCTION(+:summ)
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                     summ=summ+int(input(i+j),8)*int(reference(j),8)
                END DO
                CorrelationRaw41(i)=summ
                summ=0!
          END DO
 !$omp end parallel do
    END FUNCTION   CorrelationRaw41

    FUNCTION CorrelationRaw42(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=4
          INTEGER(1),PARAMETER                       :: arrayKindReference=2
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw42(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
           INTEGER(8)                                 :: summ

          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw42(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw42=0
          ! Выбрать пределы корреляции
          summ=0
         !$omp parallel do SHARED(input,reference,CorrelationRaw42) PRIVATE(i,j) REDUCTION(+:summ)
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                     summ=summ+int(input(i+j),8)*int(reference(j),8)
                END DO
                CorrelationRaw42(i)=summ
                summ=0!
          END DO
 !$omp end parallel do
    END FUNCTION   CorrelationRaw42

       FUNCTION CorrelationRaw44(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=4
          INTEGER(1),PARAMETER                       :: arrayKindReference=4
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw44(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          INTEGER(8)                                 :: summ
          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw44(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw44=0
          summ=0
          ! Выбрать пределы корреляции
         !$omp parallel do SHARED(input,reference,CorrelationRaw44) PRIVATE(i,j) REDUCTION(+:summ)
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                     summ=summ+int(input(i+j),8)*int(reference(j),8)
                END DO
                CorrelationRaw44(i)=summ
                summ=0!
          END DO
 !$omp end parallel do
    END FUNCTION   CorrelationRaw44

    FUNCTION CorrelationRaw48(input,reference)
          INTEGER(1),PARAMETER                       :: arrayKindInput=4
          INTEGER(1),PARAMETER                       :: arrayKindReference=8
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(8),ALLOCATABLE                     :: CorrelationRaw48(:)
          INTEGER(8)                                 :: i,j
          INTEGER(8)                                 :: inputLen, referenceLEn
          INTEGER(8)                                 :: summ

          ! длительность выходного сигнала в отсчетах
          inputLen=SIZE(input)
          referenceLen=SIZE(reference)
          ALLOCATE (CorrelationRaw48(1:inputLen))
          ! что бы не было мусора в элементах массива
          CorrelationRaw48=0
          summ=0
          ! Выбрать пределы корреляции
         !$omp parallel do SHARED(input,reference,CorrelationRaw48) PRIVATE(i,j) REDUCTION(+:summ)
          DO i=1,inputLen-referenceLen
                DO j=1,referenceLen
                     summ=summ+int(input(i+j),8)*int(reference(j),8)
                END DO
                CorrelationRaw48(i)=summ
                summ=0!
          END DO
 !$omp end parallel do

    END FUNCTION   CorrelationRaw48


end module RawCorrOpenMPmod
