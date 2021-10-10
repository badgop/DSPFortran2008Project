MODULE POWER_METER
  USE MathConstModule
  USE FastModuleMod
  implicit none

  REAL(8),PRIVATE   , PARAMETER :: FullScaleSinusRms      = 0.70710676721636812
  REAL(8),PRIVATE   , PARAMETER :: FullScaleSinusRmsdB    = -3.0103000510993061



CONTAINS
SUBROUTINE  POWER_METER_REAL(signal,fd,power,period)
                    IMPLICIT NONE         
                    REAL(4),DIMENSION(:), INTENT (IN) ::  signal
                    REAL(8), INTENT (IN) :: fd
                    REAL(4), INTENT (OUT) :: power
                    INTEGER(4), INTENT (IN), OPTIONAL  :: period
                   real(4) :: summ1 = 0
                    real(4) :: summ2 = 0
                    real(4):: real_statement=0
                    real(4) :: arg
                    real(4):: Pref
                    integer(4) :: i=0
                    integer(4) :: len     
                     real(8) ::pi= 3.1415926535897932384626433832795
                      len=size(signal)
                      arg=2*pi*10e3/fd
                           do i=1,len
                            real_statement=(  signal(i) )**2
                            summ1=summ1+real_statement
                            
                            real_statement=(sin(arg*i))**2
                            summ2=summ2+real_statement 
                   end do
                        summ1   =   sqrt(     summ1/     (float(len))    )
		              Pref       =       sqrt(   summ2/ (float(len))        )
                           power=20*log10(summ1/Pref)
         END SUBROUTINE POWER_METER_REAL


   FUNCTION GetSignalRmsPowerINT2(signal,length)
        INTEGER(2),  INTENT(IN)    :: signal(:)
        INTEGER(8),  INTENT(IN)    :: length
        REAL(8)   ,  ALLOCATABLE   :: tmpArray(:)
        REAL(8)                    :: GetSignalRmsPowerINT2,summ
        ALLOCATE(tmpArray(1:length))
        tmpArray = float(signal(1:length))/32767.0
        tmpArray = (tmpArray*tmpArray)
        summ = sum(tmpArray)/float(length)
       ! WRITE(*,*) 'длина ', length, float(length)
        GetSignalRmsPowerINT2 =20*log10(sqrt(summ)) - FullScaleSinusRmsdB
        DEALLOCATE(tmpArray)
   END FUNCTION GetSignalRmsPowerINT2


   FUNCTION GetSignalRmsPowerReferencedB(length, fs)
        INTEGER(8),INTENT(IN) :: length
        INTEGER(8),INTENT(IN) :: fs
        INTEGER(8)            :: i
        REAL(8)               :: arg,GetSignalRmsPowerReferencedB

        arg = 2.0*PI*(1.0/float(fs))*1000.0

        GetSignalRmsPowerReferencedB=0
        DO i=1,length
           GetSignalRmsPowerReferencedB = GetSignalRmsPowerReferencedB + sin(arg*i)**2
        END DO
           GetSignalRmsPowerReferencedB = GetSignalRmsPowerReferencedB/length
           GetSignalRmsPowerReferencedB=  SQRT(GetSignalRmsPowerReferencedB)
   END FUNCTION GetSignalRmsPowerReferencedB

END MODULE
