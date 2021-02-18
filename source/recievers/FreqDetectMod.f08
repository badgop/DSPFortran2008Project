MODULE FreqDetectMod
    USE GaussFilter

    USE complexSignalModule
    USE analyticSignalModule
    USE MathConstModule
    USE MathConstModule

    IMPLICIT NONE


    CONTAINS


SUBROUTINE FreqDetectorComplexSignalINT8(in,out)

        TYPE(complexSignal_t), INTENT(IN)                   :: in
        TYPE(analyticSignal_t),INTENT(INOUT), allocatable   :: out

        INTEGER(8),DIMENSION(:),ALLOCATABLE  ::  in_i
        INTEGER(8),DIMENSION(:),ALLOCATABLE  ::  in_q
        INTEGER(8),DIMENSION(:),ALLOCATABLE  ::  out_arr

        ALLOCATE(out)
        CALL in%ExtractSignalData(in_i,in_q)
        CALL FreqDetectorInt8(in_i,in_q,out_arr)
        CALL out%Constructor (out_arr)

        DEALLOCATE(in_i)
        DEALLOCATE(in_q)
        DEALLOCATE(out_arr)

END SUBROUTINE FreqDetectorComplexSignalINT8

SUBROUTINE FreqDetectorComplexSignalReal(in,out,sampleRate)

        TYPE(complexSignal_t), INTENT(IN)                   :: in
        TYPE(analyticSignal_t),INTENT(INOUT), allocatable   :: out
        integer(4)            ,intent(IN)                   :: sampleRate

        INTEGER(8),DIMENSION(:),ALLOCATABLE  ::  in_i
        INTEGER(8),DIMENSION(:),ALLOCATABLE  ::  in_q
        REAL(8),DIMENSION(:),ALLOCATABLE     ::  out_arr
        INTEGER(8),DIMENSION(:),ALLOCATABLE  ::  out_arr_int

        ALLOCATE(out)
        CALL in%ExtractSignalData(in_i,in_q)
        CALL FreqDetectorReal(real(in_i,8)/maxval(in_i),real(in_q,8)/maxval(in_q),sampleRate,out_arr)
        WRITE(*,*) 'посчитано '

        ALLOCATE(out_arr_int(1:size(out_arr)))

        out_arr_int = int((out_arr),8)
        CALL out%Constructor (out_arr_int)

        DEALLOCATE(in_i)
        DEALLOCATE(in_q)
        DEALLOCATE(out_arr)
        DEALLOCATE(out_arr_int)


END SUBROUTINE FreqDetectorComplexSignalREal


 SUBROUTINE FreqDetectorInt8(in_i, in_q,out_arr)


  INTEGER(8),DIMENSION(:), INTENT (IN) ::  in_i
  INTEGER(8),DIMENSION(:), INTENT (IN) ::  in_q
  INTEGER(8),DIMENSION(:), INTENT (OUT), ALLOCATABLE ::  out_arr

  INTEGER(8) :: i_last,q_last
  INTEGER(8) :: i

  ALLOCATE(out_arr(1:size(in_i)))
  q_last = 0
  i_last = 0

  DO I=1,size(in_i)
         if ((in_q(i).EQ.0).and.(in_i(i).EQ.0)) then
         out_arr(i)=0
         ELSE
              out_arr(i)=  in_q(i)*i_last - in_i(i)*q_last
         i_last=in_i(i)
         q_last=in_q(i)
         END IF
  END DO


 END SUBROUTINE FreqDetectorInt8


 SUBROUTINE FreqDetectorReal(in_i, in_q,FD,out_arr)

  REAL(8),DIMENSION(:), INTENT (IN) ::  in_i
  REAL(8),DIMENSION(:), INTENT (IN) ::  in_q
  INTEGER(4), INTENT (IN) :: FD
  REAL(8),DIMENSION(:), INTENT (OUT), ALLOCATABLE ::  out_arr



  INTEGER(8) :: i

  REAL(8) :: i_last,q_last

  q_last=0.0
  i_last=0.0
  ALLOCATE(out_arr(1:size(in_i)))


  DO i=1,size(in_i)

         if ((in_q(i).EQ.0).and.(in_i(i).EQ.0)) then
         out_arr(i)=0


         ELSE
          out_arr(i)= ( in_q(i)*i_last - in_i(i)*q_last) /(in_q(i)**2+in_i(i)**2)

          out_arr(i) = (out_arr(i)*(real(fd)))/(2*PI)

         !WRITE(*,*) out_arr(i),  in_q(i), in_i(i)


         i_last=in_i(i)
         q_last=in_q(i)
         END IF
  END DO
END SUBROUTINE FreqDetectorReal

END MODULE FreqDetectMod
