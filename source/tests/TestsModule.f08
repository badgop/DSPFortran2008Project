module TestsModule

    USE PrefixModule
    implicit none


    CONTAINS

    ! тест модуля DDS.
    ! Проверка работы генератора прямого цифроовго синтеза
    ! в тесте так же используются конструкторы аналитического сигнала
    ! и процедуры записи аналитического сигнала в файл
    ! создает два файла c имиенами file1Name  и file2Name
    ! частота колебания в первом опредлеяется centralFrequency
    ! во втором centralFrequency2

    SUBROUTINE DDSOutputTest(romLengthInBits,romLenthTruncedInBits,outputSignalSampleCapacity&
                             ,samplingFrequency,phase&
                             ,periods,centralFrequency, centralFrequency2,file1Name,file2Name)

            USE analyticSignalModule
            USE DDSModule
            USE MathConstModule
            USE PrefixModule
            USE ModuleWriteReadArrayFromToFile
            USE WriteReadAnalyticSignalToFromFile

            !разрядность аккамулятора фазы
            INTEGER(1), INTENT(IN) :: romLengthInBits
            !число бит до которых усекатется таблица ПЗУ
            INTEGER(1), INTENT(IN) :: romLenthTruncedInBits
            !разрядность выходного сигнала
            INTEGER(1), INTENT(IN) :: outputSignalSampleCapacity

            !частота дискретизации
            INTEGER(4), INTENT(IN) :: samplingFrequency
            !центральная частота 1 и 2 выходных сигналов
            INTEGER(4), INTENT(IN) :: centralFrequency
            INTEGER(4), INTENT(IN) :: centralFrequency2
            !начальная фаза
            REAL(8),    INTENT(IN)    :: phase
            ! длина выходного сигнала в периодах гармонич колебания
            INTEGER(4), INTENT(IN)    :: periods


            CHARACTER(*),INTENT(IN) :: file1Name
            CHARACTER(*),INTENT(IN) :: file2Name


            INTEGER(1) :: status
            INTEGER(8) :: signalLengthInSamples
            INTEGER(4) :: oscillationPeriod


            INTEGER(8), ALLOCATABLE :: frequencys(:)

            INTEGER(8), ALLOCATABLE :: outputArray(:)

            TYPE(DDS_t) ::ddsGenerator
            TYPE(analyticSignal_t) ::imputFreqSignal
            TYPE(analyticSignal_t) ::outputSignal
            TYPE(analyticSignal_t) ::outputSignal2

            ! инициаализируем генератор
            status= ddsGenerator%Constructor(romLengthInBits,romLenthTruncedInBits,&
                                             samplingFrequency,outputSignalSampleCapacity)






            WRITE(*,*) 'Тест DDS_t запущен - '
            WRITE(*,*) 'проверка значений полученных конструктором'
            WRITE(*,*) 'отсчетов на период', oscillationPeriod

            ! пишем таблицу ПЗУ
            WRITE(*,*) 'ddsromtable.pcm - там таблица ПЗУ '
            status=ddsGenerator%DebugOutput('ddsromtable.pcm')


             CALL ddsGenerator%SetPhase(phase)

             oscillationPeriod=samplingFrequency/ centralFrequency
             signalLengthInSamples=oscillationPeriod*periods

             ! делаем массив со значениями частоты
             ALLOCATE(frequencys(1:signalLengthInSamples))
             frequencys=centralFrequency

             ! делаем из массива аналитич сигнала
             CALL imputFreqSignal%Constructor(frequencys)

             ! поулчаем гармонический сигнал
             CALL ddsGenerator%ComputeOutput(imputFreqSignal, outputsignal)


             !извлекаем массив из обьекта
             CALL outputsignal%ExtractSignalData(outputArray)
             ! пишем файл
             ! оператор int (,kind) можно применять и к массивам!!!
             CALL WriteArrayToFile(int(outputArray,2),file1Name, isBinary=.True.)

             ! второй вариант получения выходного гармонического сигнала
             CALL ddsGenerator%ComputeOutput(int(centralFrequency2,8),signalLengthInSamples,outputsignal2)
              ! оператор int (,kind) можно применять и к массивам!!!
             CALL WriteAnalyticSignalToFile(outputsignal2,int(2,1),file2Name,isBinary=.True.)
             WRITE(*,*) ''
             WRITE(*,*) 'ТЕСТ DDS ЗАКОНЧЕН!!!!!'
    END SUBROUTINE DDSOutputTest


     ! Тест аналитического и комплексного присваивающего конструктора
     ! inputSignalFileNameI,inputSignalFileNameQ  - входные сигналы, БИНАРНЫЕ,
     ! outputSignalFileNameAnaytic - выходной сигнал, содержимое объекта типа "аналитический сигнал",  БИНАРНЫЙ
     ! outputSignalFileNameComplexI,outputSignalFileNameComplexQ -выходные сигналы,
     !                                                             содержимое объекта типа "комплексный сигнал",  БИНАРНЫЙ


     SUBROUTINE AnalyticComplexSignalTestConstructors(inputSignalFileNameI,inputSignalFileNameQ&
                                                      ,outputSignalFileNameAnaytic&
                                                      ,outputSignalFileNameComplexI,outputSignalFileNameComplexQ)

           USE analyticSignalModule
           USE complexSignalModule
           USE ModuleWriteReadArrayFromToFile
           USE WriteReadAnalyticSignalToFromFile
           IMPLICIT NONE

           CHARACTER(*),INTENT(IN) :: inputSignalFileNameI
           CHARACTER(*),INTENT(IN) :: inputSignalFileNameQ

           CHARACTER(*),INTENT(IN) :: outputSignalFileNameAnaytic
           CHARACTER(*),INTENT(IN) :: outputSignalFileNameComplexI
           CHARACTER(*),INTENT(IN) :: outputSignalFileNameComplexQ



           TYPE(analyticSignal_t) ::signal_1
           TYPE(analyticSignal_t) ::signal_2


           TYPE(complexSignal_t) ::signalComplex_1
           TYPE(complexSignal_t) ::signalComplex_2
           TYPE(complexSignal_t) ::signalComplex_3

           LOGICAL       :: state=.FALSE.
           LOGICAL       :: isBinary=.True.


           INTEGER(2),ALLOCATABLE :: testSignal1(:)
           INTEGER(2),ALLOCATABLE :: testSignal2(:)
           INTEGER(8),ALLOCATABLE :: testSignalExtract(:)

           INTEGER(8),ALLOCATABLE :: testSignalExtractI(:)
           INTEGER(8),ALLOCATABLE :: testSignalExtractQ(:)


           ! читаем два сигнала из бинарных файлов и положим в массив
           CALL ReadArrayFromFile(testSignal1,inputSignalFileNameI,isBinary)
           CALL ReadArrayFromFile(testSignal2,inputSignalFileNameQ,isBinary)

           ! создаем один аналитический сигнал из массива
           CALL signal_1%Constructor(  int(testSignal1,8))

           !**********ВЫЗЫВАЕМ ОПЕРАТОР ПРИСВАИВАИЯ ДЛЯ АНАЛИТЧЕСКОГО СИГНАЛА*******
           signal_2=signal_1

           ! вытаскиваем содержимое объекта в массив
           CALL signal_2% ExtractSignalData(testSignalExtract)

           ! и пишем его в файл, что бы проверить ГЛАЗАМИ
           CALL WriteArrayToFile(int(testSignalExtract,2),outputSignalFileNameAnaytic,isBinary)



           ! проверяем выделана ли память для signalComplex_1
           state= signalComplex_1%GetAllocationStatus()
           WRITE(*,*) ' комплексный 1! - статус выделение памяти',  state

           ! вызываем конструктор класса комплексных сигналов
           CALL signalComplex_1%Constructor(int(testSignal1,8),int(testSignal2,8))

           WRITE(*,*) 'А теперь комплексный 1 после вызова конструктора!'&
           , signalComplex_1%GetSignalSize(),&
           signalComplex_1%GetAllocationStatus()


!           ! проверяем выделана ли память для signalComplex_2
!           state= signalComplex_2%GetAllocationStatus()
!           WRITE(*,*) ' комплексный 2! - статус выделение памяти',  state
!
!
!           CALL signalComplex_2%Constructor(signal_1,signal_2)
!
!           ! проверяем выделана ли память для signalComplex_2 после ВЫЗОВА конструктора
!           state= signalComplex_2%GetAllocationStatus()
!           WRITE(*,*) 'А теперь комплексный 2!', signalComplex_2%GetSignalSize(), state


           !**********ВЫЗЫВАЕМ ОПЕРАТОР ПРИСВАИВАИЯ ДЛЯ КОМПЛЕКСНОГО СИГНАЛА*******
           signalComplex_2=signalComplex_1
           WRITE(*,*) 'А теперь комплексный 3!', signalComplex_2%GetSignalSize(),&
           signalComplex_2%GetAllocationStatus()

           CALL signalComplex_2%ExtractSignalData(testSignalExtractI,testSignalExtractQ)


           CALL WriteArrayToFile(int(testSignalExtractI,2),outputSignalFileNameComplexI,isBinary)
           CALL WriteArrayToFile(int(testSignalExtractQ,2),outputSignalFileNameComplexQ,isBinary)

     END SUBROUTINE  AnalyticComplexSignalTestConstructors


     ! тест для процедур чтения записи аналитического сигнала из файла в файл
     ! сигнал БИНАРНЫЙ!!!
     SUBROUTINE AnalyticSignalTestWriteRead(inputSignalFileName, outputSignalFileName)
           USE analyticSignalModule
           USE complexSignalModule
           USE ModuleWriteReadArrayFromToFile
           USE WriteReadAnalyticSignalToFromFile
           IMPLICIT NONE

           TYPE(analyticSignal_t) ::signal_1
           INTEGER(1) :: intType
           CHARACTER(*), INTENT(IN) :: inputSignalFileName
           CHARACTER(*), INTENT(IN) :: outputSignalFileName

           intType=2
           CALL ReadAnalyticSignalFromFile(signal_1,intType,inputSignalFileName,.True.)
           CALL WriteAnalyticSignalToFile(signal_1,intType,outputSignalFileName,.True.)

     END SUBROUTINE AnalyticSignalTestWriteRead

     ! тест для процедур чтения записи Комплексного  сигнала из файла в файл
     ! сигнал БИНАРНЫЙ!!!
     SUBROUTINE ComplexSignalTestWriteRead(inputSignalFileNameI, inputSignalFileNameQ,&
                                           outputSignalFileNameI,outputSignalFileNameQ)

           USE complexSignalModule
           USE ModuleWriteReadArrayFromToFile
           USE WriteReadComplexSignalToFromFile

           IMPLICIT NONE

           TYPE(complexSignal_t) ::signal_1
           INTEGER(1) :: intType
           CHARACTER(*),INTENT(IN) :: inputSignalFileNameI, inputSignalFileNameQ
           CHARACTER(*),INTENT(IN) :: outputSignalFileNameI,outputSignalFileNameQ

           LOGICAL          ::isBinary=.True.
!           intType=2
           CALL ReadComplexSignalFromFile(signal_1,intType,inputSignalFileNameI,inputSignalFileNameQ,isBinary)
           CALL WriteComplexSignalToFile(signal_1,intType,outputSignalFileNameI,outputSignalFileNameQ,isBinary)
     END SUBROUTINE ComplexSignalTestWriteRead

     ! тест оператора умножения аналитического сигнала
     ! и сдвигового мультиплексора
     ! inputSignalFileName - входной файл 1
     ! inputSignalFileName2 - входной файл 2
     ! outputSignalFileName - выходной файл
     ! shift - на сколько разрядов нужно сдвинуть отсчеты выходного сигнала влево
     SUBROUTINE AnalyticSignalMultiplyPlusShiftTest(inputSignalFileName,inputSignalFileName2,&
                                                    outputSignalFileName,shift)
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
           INTEGER(1), intent(in) :: shift
           INTEGER(1) ::intType
           CHARACTER(*), intent(in) :: inputSignalFileName
           CHARACTER(*), intent(in) :: inputSignalFileName2
           CHARACTER(*), intent(in) :: outputSignalFileName
           LOGICAL         :: isBinary=.True.


           CALL shiftPlexor1%Constructor(shift)

           intType=2
           CALL ReadAnalyticSignalFromFile(signal_1,intType,inputSignalFileName,isBinary)
           CALL ReadAnalyticSignalFromFile(signal_2,intType,inputSignalFileName2,isBinary)
           WRITE(*,*) 'signal_3=signal_1*signal_2'
           signal_3=signal_1*signal_2
           CALL shiftPlexor1%PerformAnalyticSignalShift(signal_3,signal_4)
           CALL WriteAnalyticSignalToFile(signal_4,intType,outputSignalFileName,isBinary)


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

            LOGICAL         :: isBinary=.True.

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
                                                outputSignalSampleCapacity)


           CALL ddsGeneratorComplex%ComputeOutput(frequencys,signal_out)

           intType=2
           CALL WriteComplexSignalToFile(signal_out,intType,outputSignalFileNameI,outputSignalFileNameQ,isBinary)

       END SUBROUTINE ComplexDDSTest

      SUBROUTINE ComplexMultiplyTest(freq_in,inputSignalFileNameI,inputSignalFileNameQ)

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
           CHARACTER(*),INTENT(IN) :: inputSignalFileNameI
           CHARACTER(*),INTENT(IN) :: inputSignalFileNameQ

           INTEGER(1) :: shift,intType



           !CHARACTER(50) :: inputSignalFileNameI, inputSignalFileNameQ
           CHARACTER(50) :: inputDDSSignalFileNameI,inputDDSSignalFileNameQ
           CHARACTER(50) :: outputSignalFileNameI,outputSignalFileNameQ

           LOGICAL :: isBinary=.True.

           CALL ComplexDDSTest(freq_in,1624993)

           !inputSignalFileNameI  = 'gfsk_mux_i_cos.pcm'
           !inputSignalFileNameQ  = 'gfsk_mux_i_sin.pcm'
           outputSignalFileNameI = 'sig_outi_mult_test.pcm'
           outputSignalFileNameQ = 'sig_outq_mult_test.pcm'
           inputDDSSignalFileNameI='complex_dds_test_i.pcm'
           inputDDSSignalFileNameQ='complex_dds_test_q.pcm'


           CALL signal_1%SetName('первый И',' первый Ку')
           CALL signal_2%SetName('вторий И',' вторий Ку')
           CALL signal_3%SetName('третий И',' третий Ку')
           CALL signal_4%SetName('четвертый И',' четвертый Ку')

           intType=2
           CALL ReadComplexSignalFromFile(signal_1,intType,inputSignalFileNameI,inputSignalFileNameQ,isBinary)
           CALL ReadComplexSignalFromFile(signal_2,intType,inputDDSSignalFileNameI,inputDDSSignalFileNameQ,isBinary)


           shift=10
           CALL shiftPlexor1%Constructor(shift)


!
           signal_3=signal_2*signal_1

           CALL shiftPlexor1%PerformComplexSignalShift(signal_3,signal_4)



           intType=2
           CALL WriteComplexSignalToFile(signal_4,intType,outputSignalFileNameI,outputSignalFileNameQ,isBinary)


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


            LOGICAL :: isBinary=.True.


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
           CALL WriteArrayToFile(int(testSignalExtract,2),'analytic_add_test.pcm',isBinary)


           DEALLOCATE(testSignalExtract)
           signal_4=signal_1-signal_6

           CALL signal_4%SetName('Чотвiртий!')
           CALL signal_4%ExtractSignalData(testSignalExtract)
           CALL WriteArrayToFile(int(testSignalExtract,2),'analytic_subctract_test.pcm',isBinary)

            DEALLOCATE(testSignalExtract)

           CALL signal_5%SetName('Пятий!')

           signal_5=signal_1*signal_6
           CALL signal_5%ExtractSignalData(testSignalExtract)
           !CALL WriteArrayToFile(int(testSignalExtract,2),'analytic_multi_test.pcm')

           shift=3
          CALL shiftPlexor1%Constructor(shift)

          CALL shiftPlexor1%PerformAnalyticSignalShift(signal_5,signal_7)

intType=2
        CALL WriteAnalyticSignalToFile(signal_7,intType,'analytic_multi_test.pcm',isBinary)

     END SUBROUTINE  AnalyticSignalTestOperators

     ! Тест оператора свертки
     SUBROUTINE ConvolveTest()
         USE analyticSignalModule
         USE ShiftMultiplexorModule

          TYPE(analyticSignal_t) ::input_sig
          TYPE(analyticSignal_t) ::reference_sig



     END SUBROUTINE ConvolveTest
end module TestsModule
