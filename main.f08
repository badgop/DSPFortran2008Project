PROGRAM main

    IMPLICIT NONE

!    CALL InitDDSTest()
    CALL DDSOutputTest()
    CALL AnalyticSignalTest()
    WRITE(*,*) 'DONE!'


    CONTAINS


    ! процедура для тестирования функции конструктора класса DDS
    SUBROUTINE InitDDSTest()

            USE DDSModule
            USE MathConstModule

            TYPE(DDS) ::ddsGenerator
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

            TYPE(DDS) ::ddsGenerator
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
            outputSignalSampleCapacity=12
            samplingFrequency= 20*MEGA

            centralFrequency= 1*MEGA
            oscillationPeriod=samplingFrequency/ centralFrequency

            status= ddsGenerator%Constructor(romLengthInBits,romLenthTruncedInBits,&
                                             samplingFrequency,outputSignalSampleCapacity)

            WRITE(*,*) 'Тест DDS запущен - '
            WRITE(*,*) 'проверка значений полученных конструктором'
            WRITE(*,*) 'отсчетов на период', oscillationPeriod

             phase=0.0
             CALL ddsGenerator%SetPhase(phase)

            signalLengthInSamples=oscillationPeriod*300

            ALLOCATE(frequencys(1:signalLengthInSamples))


            frequencys=centralFrequency

            CALL imputFreqSignal%Constructor(frequencys)


            CALL ddsGenerator%ComputeOutput(imputFreqSignal, outputsignal)

            CALL outputsignal%ExtractSignalData(outputArray)

            ALLOCATE(outputSignal2byte(1:size(outputArray)))

            outputSignal2byte=int(outputArray,2)

            CALL WriteArrayToFile(outputSignal2byte,'dds_output_rest.pcm')



    END SUBROUTINE DDSOutputTest

     SUBROUTINE AnalyticSignalTest()

           USE analyticSignalModule
           USE complexSignalModule
           USE ModuleWriteReadArrayFromToFile
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

           CALL WriteArrayToFile(int(testSignal,2),outputSignalFileName)


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

     END SUBROUTINE AnalyticSignalTest

END PROGRAM main
