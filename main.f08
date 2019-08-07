PROGRAM main

    IMPLICIT NONE

!    CALL InitDDSTest()
!    CALL DDSOutputTest()
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
            INTEGER(8), ALLOCATABLE :: outputSignal(:)
            INTEGER(2), ALLOCATABLE :: outputSignal2byte(:)

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

            signalLengthInSamples=oscillationPeriod*2000

            ALLOCATE(frequencys(1:signalLengthInSamples))


            frequencys=centralFrequency


            CALL ddsGenerator%ComputeOutput(frequencys, outputsignal)

            ALLOCATE(outputSignal2byte(1:size(outputsignal)))

            outputSignal2byte=int(outputsignal,2)

            CALL WriteArrayToFile(outputSignal2byte,'dds_output_rest.pcm')



    END SUBROUTINE DDSOutputTest

     SUBROUTINE AnalyticSignalTest()

           USE analyticSignalModule
           USE ModuleWriteReadArrayFromToFile
           IMPLICIT NONE

           TYPE(analyticSignal_t) ::signal_1
           TYPE(analyticSignal_t), ALLOCATABLE ::signal_2

           CHARACTER(50) :: inputSignalFileName
           INTEGER(2),ALLOCATABLE :: testSignal(:)

           inputSignalFileName='dds_output_rest.pcm'

           CALL ReadArrayFromFile(testSignal,inputSignalFileName)

           CALL signal_1%Constructor(  int(testSignal,8))
           WRITE(*,*)  signal_1%signalSize
           signal_2=signal_2
           WRITE(*,*)  signal_2%signalSize

     END SUBROUTINE AnalyticSignalTest

END PROGRAM main
