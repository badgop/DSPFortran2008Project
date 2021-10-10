MODULE bychkov_interpolate
    IMPLICIT NONE


    CONTAINS

          !
     SUBROUTINE  InterpolateByTwoFir(inputSignalFileName,inputRefFileName,outputSignalFileName,outputSignalFileNameZero,shift)
         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt


         CHARACTER(*), INTENT(IN) :: inputSignalFileName
         CHARACTER(*), INTENT(IN) :: inputRefFileName
         CHARACTER(*), INTENT(IN) :: outputSignalFileName
         CHARACTER(*), INTENT(IN) :: outputSignalFileNameZero
         INTEGER(1)  , INTENT(IN) :: shift


         TYPE(analyticSignal_t) :: input_sig
         TYPE(analyticSignal_t) :: reference_sig
         TYPE(analyticSignal_t) :: conv_result

         CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputSignalFileName)         !!!!!!
         CALL ReadAnalyticSignalFromFile(reference_sig,int(2,1),inputRefFileName,'(I10)')

!         WRITE(*,*) ' input_sig max ', input_sig%GetMax()
!         WRITE(*,*) ' reference_sig max ', reference_sig%GetMax()


         conv_result = input_sig%Interpolate(int(2,8))

!          WRITE(*,*) ' after zeroes max ', conv_result%GetMax()

          CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileNameZero)

         conv_result= conv_result.CONV.reference_sig
!          WRITE(*,*) 'max ', conv_result%GetMax()

         CALL conv_result%Rshift(shift)
         CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileName)

     END SUBROUTINE InterpolateByTwoFir


END MODULE bychkov_interpolate
