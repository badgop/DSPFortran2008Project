MODULE GaussFilter

    USE MathConstModule
    USE PSNSimpleMod
    USE impulseGeneratorModule
    USE complexSignalModule
    USE DiffDataModulatorMod
    USE analyticSignalModule
    USE DDSModule

    IMPLICIT NONE


    CONTAINS

    SUBROUTINE IR_GAUSS_CALCULATE_INT_2(sampleRate,bt,symbolPeriod,fir_order,capacity,IR_GAUSS_int)

      integer(4) ,intent(IN)                    :: sampleRate
      real(4)    ,INTENT(IN)                    :: bt
      integer(1) ,INTENT(IN)                    :: fir_order
      real(8)    ,INTENT(IN)                    :: symbolPeriod
      integer(1) ,INTENT(IN)                    :: capacity
      INTEGER(8)    ,INTENT(INOUT), ALLOCATABLE    :: IR_GAUSS_int(:)

      real(8)                                   :: multiplyer
      real(8)                                   :: m


      real(8), ALLOCATABLE      :: IR_GAUSS_float(:)

       CALL IR_CALCULATE_FLOAT(sampleRate = sampleRate&
                 ,bt = bt&
                 ,symbolPeriod = symbolPeriod&
                 ,fir_order = fir_order&
                 ,IR_GAUSS_float = IR_GAUSS_float)

       multiplyer = float(int(2,2)**(capacity-1)-1)*0.9835

       m = IR_GAUSS_float((fir_order-1)/2+1)

       multiplyer = multiplyer/m
       IR_GAUSS_float = IR_GAUSS_float*multiplyer


       ALLOCATE (IR_GAUSS_int(1:size(IR_GAUSS_float)))
       IR_GAUSS_int = int(IR_GAUSS_float,2)



       DEALLOCATE(IR_GAUSS_float)
    END SUBROUTINE  IR_GAUSS_CALCULATE_INT_2

 SUBROUTINE IR_CALCULATE_FLOAT(sampleRate,bt,symbolPeriod,fir_order,IR_GAUSS_float)

   real(8) ::pi= 3.1415926535897932384626433832795
   integer(4) ,intent(IN)                 :: sampleRate
   real(4)    ,INTENT(IN)                 :: bt
   integer(1) ,INTENT(IN)                 :: fir_order
   real(8)    ,INTENT(IN)                 :: symbolPeriod
   real(8)    ,INTENT(INOUT), ALLOCATABLE    :: IR_GAUSS_float(:)


   !integer(2),INTENT(IN) :: N_IMP

   real(8) ::Td
   real(8) :: a=0
   real(8) :: B=0
   real(8)     :: h
   real(8)     :: exponenta=0
   INTEGER(1) :: j
   INTEGER(4) ::k

    Td= 1.0/float(sampleRate)
    ALLOCATE( IR_GAUSS_float(1:fir_order))

    B=bt/symbolPeriod
    write(*,*)'B ',  B

    a =(2*pi)/log(2.0)   !LOG Elemental Intrinsic Function (Generic): Returns the natural logarithm of the argument.

   k= 1
    do j=-FIR_ORDER/2,FIR_ORDER/2,1
           WRITE(*,*) 'j ' ,j , 'k ',k
            exponenta =  exp ( (-1)*(pi*a)*(b**2)*((j*td)**2) )
            h = B*SQRT(a)*exponenta
            IR_GAUSS_float(k) = h
            k = k+1
    end do

        IR_GAUSS_float=IR_GAUSS_float /sum(IR_GAUSS_float)

!    do k=1,FIR_ORDER
!
!          write(*,*) IR_GAUSS_float(k)
!    end do
   ! write(*,*) 'sum ', sum(IR_GAUSS_float)

    END SUBROUTINE IR_CALCULATE_FLOAT



END MODULE GaussFilter
