module openmpSignumCorr
    implicit none

    INTERFACE
            INTEGER FUNCTION OMP_GET_THREAD_NUM()
            END FUNCTION
    END INTERFACE

     CONTAINS



      FUNCTION CorrelationRawSignum(input,reference) RESULT(outSumm)
          INTEGER(1),PARAMETER                       :: arrayKindInput = 8
          INTEGER(1),PARAMETER                       :: arrayKindReference=8
          INTEGER(arrayKindInput)     ,INTENT(IN)    :: input(:)
          INTEGER(arrayKindReference) ,INTENT(IN)    :: reference(:)
          INTEGER(4) ,ALLOCATABLE, DIMENSION(:)    :: tmp
          INTEGER(4)                                 :: outSumm, rez
          INTEGER(8)                                 :: j
          INTEGER(8)                                 :: referenceLEn
          ! длительность выходного сигнала в отсчетах

          referenceLen=SIZE(reference)
          ALLOCATE (tmp(1:referenceLen))

          ! что бы не было мусора в элементах массива
        outSumm = 0
          ! Выбрать пределы корреляции
           !$omp parallel do SHARED(input,reference,tmp) PRIVATE(j,rez)
                DO j=1,referenceLen
!                    WRITE(*,*) 'threads ', OMP_GET_THREAD_NUM()
!
                    tmp (j) =  2*POPCNT (NOT(XOR(input(j),reference(j)))) - 64

!                    write(*,*) j,OMP_GET_THREAD_NUM()
                END DO
           !$omp end parallel do
           outSumm = sum (tmp)


    END FUNCTION   CorrelationRawSignum
end module openmpSignumCorr
