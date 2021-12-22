module DFTmod
    USE MathConstModule
    USE FastModuleMod
    implicit none

    CONTAINS

    function HammingWindow(sizeW,n)  result (r)
           integer(8), INTENT (IN) :: sizeW
           integer(8), INTENT (IN) :: n
           integer(8) :: r
           REAL(8)     :: s

           s = 0.54-0.46*cos((2.0*PI*float(n))/float(sizeW)-1.0)
           r = int(s*32767,8)
    end function


        function HanningWindow(sizeW,n)  result (r)
           integer(8), INTENT (IN) :: sizeW
           integer(8), INTENT (IN) :: n
           integer(8) :: r
           REAL(8)     :: s

           s = 0.5-0.5*cos((2.0*PI*float(n))/float(sizeW)-1.0)
           r = int(s*32767,8)
    end function

    SUBROUTINE DFTint2Re(input,outputRe, outputIm)
       INTEGER(1), PARAMETER   :: intKind = 2
       INTEGER(intKind), DIMENSION(:), INTENT(IN)  :: input
       INTEGER(intKind), DIMENSION(:), INTENT(OUT), ALLOCATABLE  :: outputRe
       INTEGER(intKind), DIMENSION(:), INTENT(OUT), ALLOCATABLE  :: outputIm
       INTEGER(8) :: i,j, sizeIn,ss, ss2
       INTEGER(2), PARAMETER :: multiplayer = 32767
       INTEGER(2), PARAMETER ::shift = 16
       INTEGER(8)            :: summ,summ2

       REAL (8)              :: arg


       ALLOCATE(outputRe(1:size(input)))
       ALLOCATE(outputIm(1:size(input)))
!
       summ = 0
       summ2 = 0


       sizeIn = size(input)

       arg =  2*PI/sizeIn

       DO i=1,sizeIn
           DO j=1,sizeIn

              ss = input(j)*int((cos(arg*i*j)*32767),8)*HanningWindow(sizeIn,j)
              ss2 = input(j)*int((sin(arg*i*j)*32767),8)*HanningWindow(sizeIn,j)


              summ  = summ+ss
              summ2 = summ2 +ss2

           END DO

            outputRe(i) = int(SHIFTA(summ,shift),2)
            outputIm(i) = int(SHIFTA(summ2,shift),2)

            summ = 0
            summ2 = 0

       END DO



    END SUBROUTINE DFTint2Re


    SUBROUTINE DFTint2ReNew(input,outputRe, outputIm)
       INTEGER(1), PARAMETER   :: intKindOut = 8
       INTEGER(1), PARAMETER   :: intKindIn = 2

       INTEGER(intKindIn), DIMENSION(:), INTENT(IN)  :: input
       INTEGER(intKindOut), DIMENSION(:), INTENT(OUT), ALLOCATABLE  :: outputRe
       INTEGER(intKindOut), DIMENSION(:), INTENT(OUT), ALLOCATABLE  :: outputIm
       INTEGER(8) :: i,j, sizeIn,ss
       INTEGER(8), PARAMETER :: multiplayer = 32767
       INTEGER(2), PARAMETER ::shift = 32
       INTEGER(8)            :: summ

       REAL (8)              :: arg


       ALLOCATE(outputRe(1:size(input)))
       ALLOCATE(outputIm(1:size(input)))
!
       summ = 0



       sizeIn = size(input)


       arg =  2*PI/float(sizeIn)

!$omp parallel do SHARED(input,arg, outputRe,sizeIn) PRIVATE(i,j,ss) REDUCTION(+:summ)
       DO i=1,sizeIn
           DO j=0,sizeIn-1
              ss = int(input(j+1),8)*int((cos(arg*i*j)* multiplayer),8)*HanningWindow(sizeIn,j)
              summ  = summ+ss
           END DO
            outputRe(i) = SHIFTA(summ,shift)
!            write(*,*) summ, outputRe(i)
            summ = 0
       END DO
!$omp end parallel do

summ=0
!$omp parallel do SHARED(input,arg, outputIm,sizeIn) PRIVATE(i,j,ss) REDUCTION(+:summ)
       DO i=1,sizeIn
           DO j=0,sizeIn-1
              ss = int(input(j+1),8)*int((sin(arg*i*j)* multiplayer),8)*HanningWindow(sizeIn,j)
              summ = summ +ss
           END DO
            outputIm(i) = SHIFTA(summ,shift)
            summ = 0
       END DO
!$omp end parallel do


    END SUBROUTINE DFTint2ReNew

end module DFTmod
