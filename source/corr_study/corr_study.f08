module corr_study
    implicit none


    CONTAINS


    SUBROUTINE CrossCorrTwoPSP(inputSignalFileName,inputRefFileName,outputSignalFileName,summFileNAme,shift,shiftLen)
         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt


         CHARACTER(*), INTENT(IN) :: inputSignalFileName
         CHARACTER(*), INTENT(IN) :: inputRefFileName
         CHARACTER(*), INTENT(IN) :: outputSignalFileName
         CHARACTER(*), INTENT(IN) :: summFileNAme
         INTEGER(1)  , INTENT(IN) :: shift
         INTEGER(8)  , INTENT(IN) :: shiftLen

         TYPE(analyticSignal_t) ::input_sig
         TYPE(analyticSignal_t) ::input_sigShifted
         TYPE(analyticSignal_t) ::reference_sig
         TYPE(analyticSignal_t) ::summ_result
         TYPE(analyticSignal_t) ::conv_result


         CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputSignalFileName)

         input_sigShifted = input_sig

         CALL input_sig%ZeroesStuffing(input_sig%GetSignalSize()*2, input_sig%GetSignalSize()*2)

         CALL input_sigShifted%ZeroesStuffing(input_sigShifted%GetSignalSize()*2-shiftLen,&
          input_sigShifted%GetSignalSize()*2+shiftLen)



         summ_result= input_sig +  input_sigShifted
         CALL WriteAnalyticSignalToFile(summ_result,int(2,1),summFileNAme)
         WRITE(*,*) 'msx , ',summ_result%GetMAxAbs()

         !!!!!!
         CALL ReadAnalyticSignalFromFile(reference_sig,int(2,1),inputRefFileName)

        summ_result= summ_result%ClipSignal(int(2,2),int(2,2) )


        reference_sig= reference_sig%ClipSignal(int(2,2),int(2,2) )


         conv_result= summ_result.CONV.reference_sig
        CALL conv_result%Rshift(shift)
         CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileName)

    END SUBROUTINE


end module corr_study
