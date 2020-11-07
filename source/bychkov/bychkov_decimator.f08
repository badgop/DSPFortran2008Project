module bychkov_decimator

    implicit none

    CONTAINS


         ! Тест оператора свертки (классический)
     SUBROUTINE  DecimateByTwoFir(inputSignalFileName,inputRefFileName,outputSignalFileName,shift,multiplier)
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



         conv_result= input_sig.CONV.reference_sig
         conv_result=  conv_result%Decimate(int(2,8))
         CALL conv_result%Rshift(shift)
         CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileName)

     END SUBROUTINE DecimateByTwoFir


          ! Тест оператора свертки (классический)
     SUBROUTINE  DecimateByThreeFir(inputSignalFileName,inputRefFileName,outputSignalFileName,shift,multiplier)
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



         conv_result= input_sig.CONV.reference_sig
         conv_result=  conv_result%Decimate(int(3,8))
         CALL conv_result%Rshift(shift)
         CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileName)

     END SUBROUTINE DecimateByThreeFir
end module bychkov_decimator
