MODULE bychkov_interpolate
    IMPLICIT NONE


    CONTAINS

          ! Тест оператора свертки (классический)
     SUBROUTINE  InterpolateByTwoFir(inputSignalFileName,inputRefFileName,outputSignalFileName,shift,multiplier)
         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt


         CHARACTER(*), INTENT(IN) :: inputSignalFileName
         CHARACTER(*), INTENT(IN) :: inputRefFileName
         CHARACTER(*), INTENT(IN) :: outputSignalFileName
         INTEGER(1)  , INTENT(IN) :: shift
         REAL(4)     , INTENT(IN) :: multiplier

         TYPE(analyticSignal_t) :: input_sig
         TYPE(analyticSignal_t) :: reference_sig
         TYPE(analyticSignal_t) :: conv_result
         INTEGER(4)             :: multiplier_int


         CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputSignalFileName)         !!!!!!
         CALL ReadAnalyticSignalFromFile(reference_sig,int(4,1),inputRefFileName,'(I10)')

         conv_result = input_sig%Interpolate(int(2,8))

         conv_result= conv_result.CONV.reference_sig

         !CALL conv_result%Rshift(shift)
         CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileName)

     END SUBROUTINE InterpolateByTwoFir


END MODULE bychkov_interpolate
