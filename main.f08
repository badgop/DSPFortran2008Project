PROGRAM main
    USE PrefixModule
    IMPLICIT NONE

!    CALL InitDDSTest()
 !   CALL DDSOutputTest()
!    CALL  AnalyticComplexSignalTestConstructors
!    CALL AnalyticSignalTestWriteRead()
!    CALL ComplexSignalTestWriteRead()
 !   CALL AnalyticSignalMultiplyPlusShiftTest()
!     CALL ComplexDDSTest(int((800*KILO),4),380001)


!    CALL AnalyticSignalTestOperators()
     CALL ComplexMultiplyTest(int((50*KILO)))



    WRITE(*,*) 'DONE!'


    CONTAINS


    ! процедура для тестирования функции конструктора класса DDS_t
    SUBROUTINE InitDDSTest()

            USE DDSModule
            USE MathConstModule

            TYPE(DDS_t) ::ddsGenerator
            !разрядность аккамулятора фазы
            INTEGER(1) :: romLengthInBits
            !частота дискретизации
            INTEGER(4) :: samplingFrequency
            !число бит до которых усекатется таблица ПЗУ
            INTEGER(1) :: romLenthTruncedInBits
            !разрядность выходного сигнала
            INTEGER(1) :: outputSignalSampleCapacity

            INTEGER(1) :: status

            romLengthInBits=32
            romLenthTruncedInBits=8
            outputSignalSampleCapacity=8
            samplingFrequency= 20000000

            status= ddsGenerator%Constructor(romLengthInBits,romLenthTruncedInBits,&
                                             samplingFrequency,outputSignalSampleCapacity)

            !Сравни заданные выше значения и значения что выводит ddsGenerator%DebugOutput
            !проверь содержимое таблицы
            status=ddsGenerator%DebugOutput('ddsromtable.pcm')


    END SUBROUTINE InitDDSTest

    SUBROUTINE DDSOutputTest()

            USE analyticSignalModule
            USE DDSModule
            USE MathConstModule
            USE PrefixModule
            USE ModuleWriteReadArrayFromToFile

            TYPE(DDS_t) ::ddsGenerator
            !разрядность аккамулятора фазы
            INTEGER(1) :: romLengthInBits
            !частота дискретизации
            INTEGER(4) :: samplingFrequency
            !число бит до которых усекатется таблица ПЗУ
            INTEGER(1) :: romLenthTruncedInBits
            !разрядность выходного сигнала
            INTEGER(1) :: outputSignalSampleCapacity
            INTEGER(1) :: status

            INTEGER(4) :: signalLengthInSamples
            INTEGER(4) :: oscillationPeriod
            INTEGER(4) :: centralFrequency

            INTEGER(8), ALLOCATABLE :: frequencys(:)

            INTEGER(2), ALLOCATABLE :: outputSignal2byte(:)

            TYPE(analyticSignal_t) ::imputFreqSignal
            TYPE(analyticSignal_t) ::outputSignal
            INTEGER(8), ALLOCATABLE :: outputArray(:)

            REAL(8)                  :: phase


            romLengthInBits=32
            romLenthTruncedInBits=14
            outputSignalSampleCapacity=14
            samplingFrequency= 40*MEGA

            centralFrequency= 100*KILO
            oscillationPeriod=samplingFrequency/ centralFrequency

            status= ddsGenerator%Constructor(romLengthInBits,romLenthTruncedInBits,&
                                             samplingFrequency,outputSignalSampleCapacity)

            WRITE(*,*) 'Тест DDS_t запущен - '
            WRITE(*,*) 'проверка значений полученных конструктором'
            WRITE(*,*) 'отсчетов на период', oscillationPeriod

             phase=0
             CALL ddsGenerator%SetPhase(phase)

            signalLengthInSamples=2000  !oscillationPeriod*3000

            ALLOCATE(frequencys(1:signalLengthInSamples))


            frequencys=centralFrequency

            CALL imputFreqSignal%Constructor(frequencys)


            CALL ddsGenerator%ComputeOutput(imputFreqSignal, outputsignal)

            CALL outputsignal%ExtractSignalData(outputArray)

            ALLOCATE(outputSignal2byte(1:size(outputArray)))

            outputSignal2byte=int(outputArray,2)

            CALL WriteArrayToFile(outputSignal2byte,'dds_output_rest.pcm')



    END SUBROUTINE DDSOutputTest

     SUBROUTINE AnalyticComplexSignalTestConstructors()

           USE analyticSignalModule
           USE complexSignalModule
           USE ModuleWriteReadArrayFromToFile
           USE WriteReadAnalyticSignalToFromFile
           IMPLICIT NONE

           TYPE(analyticSignal_t) ::signal_1
           TYPE(analyticSignal_t) ::signal_2


           TYPE(complexSignal_t) ::signalComplex_1
           TYPE(complexSignal_t) ::signalComplex_2
           TYPE(complexSignal_t) ::signalComplex_3

           CHARACTER(50) :: inputSignalFileName
           CHARACTER(50) :: outputSignalFileName
           LOGICAL       :: state=.FALSE.


           INTEGER(2),ALLOCATABLE :: testSignal(:)
           INTEGER(8),ALLOCATABLE :: testSignalExtract(:)

           INTEGER(8),ALLOCATABLE :: testSignalExtractI(:)
           INTEGER(8),ALLOCATABLE :: testSignalExtractQ(:)



           inputSignalFileName  = 'dds_output_rest.pcm'
           outputSignalFileName = 'dds_output_extracted.pcm'

           CALL ReadArrayFromFile(testSignal,inputSignalFileName)

           CALL signal_1%Constructor(  int(testSignal,8))

           signal_2=signal_1

           CALL signal_2% ExtractSignalData(testSignalExtract)

           CALL WriteArrayToFile(int(testSignalExtract,2),outputSignalFileName)


           state= signalComplex_1%GetAllocationStatus()

           WRITE(*,*) ' комплексный 1! - статус выделение памяти',  state
           CALL signalComplex_1%Constructor(int(testSignal,8),int(testSignal,8))

           WRITE(*,*) 'А теперь комплексный 1!', signalComplex_1%GetSignalSize(),&
           signalComplex_1%GetAllocationStatus()


           state= signalComplex_2%GetAllocationStatus()
            WRITE(*,*) ' комплексный 2! - статус выделение памяти',  state

           CALL signalComplex_2%Constructor(signal_1,signal_2)
           state= signalComplex_2%GetAllocationStatus()
           WRITE(*,*) 'А теперь комплексный 2!', signalComplex_2%GetSignalSize(), state



           signalComplex_3=signalComplex_2
           WRITE(*,*) 'А теперь комплексный 3!', signalComplex_3%GetSignalSize(),&
           signalComplex_3%GetAllocationStatus()

           CALL signalComplex_3%ExtractSignalData(testSignalExtractI,testSignalExtractQ)


           CALL WriteArrayToFile(int(testSignalExtractI,2),'testSignalExtractI.pcm')
           CALL WriteArrayToFile(int(testSignalExtractQ,2),'testSignalExtractQ.pcm')

     END SUBROUTINE  AnalyticComplexSignalTestConstructors

     SUBROUTINE AnalyticSignalTestWriteRead()
           USE analyticSignalModule
           USE complexSignalModule
           USE ModuleWriteReadArrayFromToFile
           USE WriteReadAnalyticSignalToFromFile
           IMPLICIT NONE

           TYPE(analyticSignal_t) ::signal_1
           INTEGER(1) :: intType
           CHARACTER(50) :: inputSignalFileName
           CHARACTER(50) :: outputSignalFileName

           inputSignalFileName  = 'dds_output_rest.pcm'
           outputSignalFileName = 'dds_output_writed_analytic.pcm'
           intType=2
           CALL ReadAnalyticSignalFromFile(signal_1,intType,inputSignalFileName)
           CALL WriteAnalyticSignalToFile(signal_1,intType,outputSignalFileName)
     END SUBROUTINE AnalyticSignalTestWriteRead

     SUBROUTINE ComplexSignalTestWriteRead()

           USE complexSignalModule
           USE ModuleWriteReadArrayFromToFile
           USE WriteReadComplexSignalToFromFile

           IMPLICIT NONE

           TYPE(complexSignal_t) ::signal_1
           INTEGER(1) :: intType
           CHARACTER(50) :: inputSignalFileNameI, inputSignalFileNameQ
           CHARACTER(50) :: outputSignalFileNameI,outputSignalFileNameQ

           inputSignalFileNameI  = 'sig_outi.pcm'
           inputSignalFileNameQ  = 'sig_outq.pcm'
           outputSignalFileNameI = 'sig_outi_outtest.pcm'
           outputSignalFileNameQ = 'sig_outq_outtest.pcm'

           intType=2
           CALL ReadComplexSignalFromFile(signal_1,intType,inputSignalFileNameI,inputSignalFileNameQ)
           CALL WriteComplexSignalToFile(signal_1,intType,outputSignalFileNameI,outputSignalFileNameQ)
     END SUBROUTINE ComplexSignalTestWriteRead


     SUBROUTINE AnalyticSignalMultiplyPlusShiftTest()
           USE analyticSignalModule
           USE complexSignalModule
           USE ModuleWriteReadArrayFromToFile
           USE WriteReadAnalyticSignalToFromFile
           USE ShiftMultiplexorModule
           IMPLICIT NONE

           TYPE(analyticSignal_t) ::signal_1
           TYPE(analyticSignal_t) ::signal_2
           TYPE(analyticSignal_t) ::signal_3
           TYPE(analyticSignal_t) ::signal_4
           TYPE(shiftMultiplexor_t) :: shiftPlexor1
           INTEGER(1) :: shift,intType
           CHARACTER(50) :: inputSignalFileName
           CHARACTER(50) :: inputSignalFileName2
           CHARACTER(50) :: outputSignalFileName


           shift=2
           CALL shiftPlexor1%Constructor(shift)
           inputSignalFileName  = 'dds_output_rest_4.pcm'
           inputSignalFileName2  = 'dds_output_rest_1.pcm'
           outputSignalFileName = 'dds_output_writed_multiplay.pcm'
           intType=2
           CALL ReadAnalyticSignalFromFile(signal_1,intType,inputSignalFileName)
           CALL ReadAnalyticSignalFromFile(signal_2,intType,inputSignalFileName2)
           WRITE(*,*) 'signal_3=signal_1*signal_2'
           signal_3=signal_1*signal_2
           CALL shiftPlexor1%PerformAnalyticSignalShift(signal_3,signal_4)
           CALL WriteAnalyticSignalToFile(signal_4,intType,outputSignalFileName)


     END SUBROUTINE AnalyticSignalMultiplyPlusShiftTest

       SUBROUTINE ComplexDDSTest(freq_in,sig_len)

           USE analyticSignalModule
           USE complexSignalModule
           USE ModuleWriteReadArrayFromToFile
           USE WriteReadComplexSignalToFromFile
           USE ComplexDDSModule
           USE MathConstModule
           USE PrefixModule

           INTEGER(4), INTENT(IN) :: freq_in
           INTEGER(4),INTENT(IN) :: sig_len

           TYPE(complexSignal_t)   ::signal_out
           TYPE(analyticSignal_t) ::frequencys
           TYPE(complexDDS_t)     :: ddsGeneratorComplex

           !разрядность аккамулятора фазы
            INTEGER(1) :: romLengthInBits
            !частота дискретизации
            INTEGER(4) :: samplingFrequency
            !число бит до которых усекатется таблица ПЗУ
            INTEGER(1) :: romLenthTruncedInBits
            !разрядность выходного сигнала
            INTEGER(1) :: outputSignalSampleCapacity

            INTEGER(4) :: centralFrequency
            REAL(8)                  :: phase
            INTEGER(8),allocatable :: freq(:)

            CHARACTER(50) :: outputSignalFileNameI,outputSignalFileNameQ
            INTEGER(1) :: intType

            outputSignalFileNameI = 'complex_dds_test_i.pcm'
            outputSignalFileNameQ = 'complex_dds_test_q.pcm'

            romLengthInBits=32
            romLenthTruncedInBits=14
            outputSignalSampleCapacity=12
            samplingFrequency= 2*MEGA
            centralFrequency= freq_in

            phase=0.0
            intType=2
           WRITE(*,*) 'ПОМНИ ПРО ДЛИНУ!'
           !sig_len=380001

           ALLOCATE(freq(1:sig_len))

           freq=centralFrequency

           CALL frequencys%Constructor(freq)

           CALL ddsGeneratorComplex%Constructor(romLengthInBits,romLenthTruncedInBits,samplingFrequency,&
                                                outputSignalSampleCapacity,phase)


           CALL ddsGeneratorComplex%ComputeOutput(frequencys,signal_out)

           intType=2
           CALL WriteComplexSignalToFile(signal_out,intType,outputSignalFileNameI,outputSignalFileNameQ)

       END SUBROUTINE ComplexDDSTest

      SUBROUTINE ComplexMultiplyTest(freq_in)

           USE complexSignalModule
           USE ModuleWriteReadArrayFromToFile
           USE WriteReadComplexSignalToFromFile
           USE ShiftMultiplexorModule

           TYPE(complexSignal_t) ::signal_1
           TYPE(complexSignal_t) ::signal_2
           TYPE(complexSignal_t) ::signal_3
           TYPE(complexSignal_t) ::signal_4
           TYPE(shiftMultiplexor_t) :: shiftPlexor1



           INTEGER(4), INTENT(IN) :: freq_in
           INTEGER(1) :: shift,intType



           CHARACTER(50) :: inputSignalFileNameI, inputSignalFileNameQ
           CHARACTER(50) :: inputDDSSignalFileNameI,inputDDSSignalFileNameQ
           CHARACTER(50) :: outputSignalFileNameI,outputSignalFileNameQ

           CALL ComplexDDSTest(freq_in,1624993)

           inputSignalFileNameI  = 'gfsk_mux_i_cos.pcm'
           inputSignalFileNameQ  = 'gfsk_mux_i_sin.pcm'
           outputSignalFileNameI = 'sig_outi_mult_test.pcm'
           outputSignalFileNameQ = 'sig_outq_mult_test.pcm'
           inputDDSSignalFileNameI='complex_dds_test_i.pcm'
           inputDDSSignalFileNameQ='complex_dds_test_q.pcm'


           CALL signal_1%SetName('первый И',' первый Ку')
           CALL signal_2%SetName('вторий И',' вторий Ку')
           CALL signal_3%SetName('третий И',' третий Ку')
           CALL signal_4%SetName('четвертый И',' четвертый Ку')

           intType=2
           CALL ReadComplexSignalFromFile(signal_1,intType,inputSignalFileNameI,inputSignalFileNameQ)
           CALL ReadComplexSignalFromFile(signal_2,intType,inputDDSSignalFileNameI,inputDDSSignalFileNameQ)


           shift=10
           CALL shiftPlexor1%Constructor(shift)


!
           signal_3=signal_2*signal_1

           CALL shiftPlexor1%PerformComplexSignalShift(signal_3,signal_4)



           intType=2
           CALL WriteComplexSignalToFile(signal_4,intType,outputSignalFileNameI,outputSignalFileNameQ)


      END SUBROUTINE ComplexMultiplyTest

         SUBROUTINE AnalyticSignalTestOperators()

           USE analyticSignalModule
           USE complexSignalModule
           USE ModuleWriteReadArrayFromToFile
           USE WriteReadAnalyticSignalToFromFile
           USE DDSModule
           USE ShiftMultiplexorModule
           IMPLICIT NONE

           TYPE(analyticSignal_t) ::signal_1
           TYPE(analyticSignal_t) ::signal_2
           TYPE(analyticSignal_t) ::signal_3
           TYPE(analyticSignal_t) ::signal_4
           TYPE(analyticSignal_t) ::signal_5
           TYPE(analyticSignal_t) ::signal_6
            TYPE(analyticSignal_t) ::signal_7
            TYPE(shiftMultiplexor_t) :: shiftPlexor1


            TYPE(DDS_t) ::ddsGenerator
            !разрядность аккамулятора фазы
            INTEGER(1) :: romLengthInBits
            !частота дискретизации
            INTEGER(4) :: samplingFrequency
            !число бит до которых усекатется таблица ПЗУ
            INTEGER(1) :: romLenthTruncedInBits
            !разрядность выходного сигнала
            INTEGER(1) :: outputSignalSampleCapacity




           CHARACTER(50) :: inputSignalFileName
            CHARACTER(50) :: inputSignalFileName2
           CHARACTER(50) :: outputSignalFileName
           LOGICAL       :: state=.FALSE.


           INTEGER(2),ALLOCATABLE :: testSignal(:)
           INTEGER(2),ALLOCATABLE :: testSignal2(:)
           INTEGER(8),ALLOCATABLE :: testSignalExtract(:)

            REAL(8)                  :: phase
            INTEGER(4) :: centralFrequency
            INTEGER(1) :: status
                INTEGER(1) :: shift,intType
            romLengthInBits=32
            romLenthTruncedInBits=14
            outputSignalSampleCapacity=8
            samplingFrequency= 20*MEGA

            centralFrequency= 1*MEGA

            status= ddsGenerator%Constructor(romLengthInBits,romLenthTruncedInBits,&
                                             samplingFrequency,outputSignalSampleCapacity)


             phase=0.0
             CALL ddsGenerator%SetPhase(phase)

             CALL ddsGenerator%ComputeOutput(int(centralFrequency,8),int(30000,8),signal_1)

             centralFrequency= 100*KILO
             CALL ddsGenerator%ComputeOutput(int(centralFrequency,8),int(30000,8),signal_6)

!           inputSignalFileName2='dds_output_rest_4.pcm'
!           inputSignalFileName  = 'dds_output_rest_1.pcm'
!           outputSignalFileName = 'dds_output_extracted.pcm'
!
!           CALL ReadArrayFromFile(testSignal,inputSignalFileName)
!           CALL ReadArrayFromFile(testSignal2,inputSignalFileName2)

!           CALL signal_1%Constructor(  int(testSignal,8))
!           CALL signal_6%Constructor(  int(testSignal2,8))

           CALL signal_1%SetName('Первый!')

           signal_2=signal_1

           CALL signal_2%SetName('Второй!')

           signal_3=signal_1+signal_6

           CALL signal_3%SetName('Третрий!')

           CALL signal_3%ExtractSignalData(testSignalExtract)
           CALL WriteArrayToFile(int(testSignalExtract,2),'analytic_add_test.pcm')


           DEALLOCATE(testSignalExtract)
           signal_4=signal_1-signal_6

           CALL signal_4%SetName('Чотвiртий!')
           CALL signal_4%ExtractSignalData(testSignalExtract)
           CALL WriteArrayToFile(int(testSignalExtract,2),'analytic_subctract_test.pcm')

            DEALLOCATE(testSignalExtract)

           CALL signal_5%SetName('Пятий!')

           signal_5=signal_1*signal_6
           CALL signal_5%ExtractSignalData(testSignalExtract)
           !CALL WriteArrayToFile(int(testSignalExtract,2),'analytic_multi_test.pcm')

           shift=3
          CALL shiftPlexor1%Constructor(shift)

          CALL shiftPlexor1%PerformAnalyticSignalShift(signal_5,signal_7)

intType=2
        CALL WriteAnalyticSignalToFile(signal_7,intType,'analytic_multi_test.pcm')

     END SUBROUTINE  AnalyticSignalTestOperators


END PROGRAM main
