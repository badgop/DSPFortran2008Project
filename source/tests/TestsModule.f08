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
                             ,signalLengthInSamples,centralFrequency, centralFrequency2,file1Name,file2Name)

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
            !INTEGER(4), INTENT(IN)    :: periods
             INTEGER(4) ,INTENT(IN) :: signalLengthInSamples


            CHARACTER(*),INTENT(IN) :: file1Name
            CHARACTER(*),INTENT(IN) :: file2Name


            INTEGER(1) :: status

           !INTEGER(4) :: oscillationPeriod


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


            ! пишем таблицу ПЗУ
            WRITE(*,*) 'ddsromtable.pcm - там таблица ПЗУ '
            status=ddsGenerator%DebugOutput('ddsromtable.pcm')


             CALL ddsGenerator%SetPhase(phase)

             !oscillationPeriod=samplingFrequency/ centralFrequency
             !signalLengthInSamples=oscillationPeriod*periods

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
             CALL ddsGenerator%ComputeOutput(int(centralFrequency2,8),int(signalLengthInSamples,8),outputsignal2)
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
                                           outputSignalFileNameI,outputSignalFileNameQ, capacity)

           USE complexSignalModule
           USE ModuleWriteReadArrayFromToFile
           USE WriteReadComplexSignalToFromFile

           IMPLICIT NONE

           TYPE(complexSignal_t) ::signal_1
           INTEGER(1),INTENT(IN) :: capacity
           CHARACTER(*),INTENT(IN) :: inputSignalFileNameI, inputSignalFileNameQ
           CHARACTER(*),INTENT(IN) :: outputSignalFileNameI,outputSignalFileNameQ

           LOGICAL          ::isBinary=.True.

           CALL ReadComplexSignalFromFile(signal_1,capacity,inputSignalFileNameI,inputSignalFileNameQ,isBinary)
           CALL WriteComplexSignalToFile(signal_1,capacity,outputSignalFileNameI,outputSignalFileNameQ,isBinary)
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


           INTEGER(1), intent(in) :: shift
           INTEGER(1) ::intType
           CHARACTER(*), intent(in) :: inputSignalFileName
           CHARACTER(*), intent(in) :: inputSignalFileName2
           CHARACTER(*), intent(in) :: outputSignalFileName
           LOGICAL         :: isBinary=.True.




           intType=2
           CALL ReadAnalyticSignalFromFile(signal_1,intType,inputSignalFileName,isBinary)
           CALL ReadAnalyticSignalFromFile(signal_2,intType,inputSignalFileName2,isBinary)
           WRITE(*,*) 'signal_3=signal_1*signal_2'
           signal_3=signal_1*signal_2

           CALL signal_3%Rshift(shift)

           CALL WriteAnalyticSignalToFile(signal_3,intType,outputSignalFileName,isBinary)


     END SUBROUTINE AnalyticSignalMultiplyPlusShiftTest

       SUBROUTINE ComplexDDSTest(centralFrequency,sig_len,romLengthInBits,samplingFrequency,&
                                 romLenthTruncedInBits,outputSignalSampleCapacity&
                                 ,outputSignalFileNameI,outputSignalFileNameQ)

           USE analyticSignalModule
           USE complexSignalModule
           USE ModuleWriteReadArrayFromToFile
           USE WriteReadComplexSignalToFromFile
           USE ComplexDDSModule
           USE MathConstModule
           USE PrefixModule


           INTEGER(4),INTENT(IN) :: sig_len

           TYPE(complexSignal_t)   ::signal_out
           TYPE(analyticSignal_t) ::frequencys
           TYPE(complexDDS_t)     :: ddsGeneratorComplex

           !разрядность аккамулятора фазы
            INTEGER(1),INTENT(IN) :: romLengthInBits
            !частота дискретизации
            INTEGER(4),INTENT(IN) :: samplingFrequency
            !число бит до которых усекатется таблица ПЗУ
            INTEGER(1),INTENT(IN) :: romLenthTruncedInBits
            !разрядность выходного сигнала
            INTEGER(1),INTENT(IN) :: outputSignalSampleCapacity

            INTEGER(4),INTENT(IN) :: centralFrequency

            INTEGER(8),allocatable :: freq(:)

            CHARACTER(*), intent(in) :: outputSignalFileNameI,outputSignalFileNameQ
            INTEGER(1) :: intType

            LOGICAL         :: isBinary=.True.


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




      SUBROUTINE AnalyticSignalTestAddSub(inputSignalFileNameI,inputSignalFileNameQ, outputSignalAddName&
                                             ,outputSignalSubName)

           USE analyticSignalModule
           USE complexSignalModule
           USE ModuleWriteReadArrayFromToFile
           USE WriteReadAnalyticSignalToFromFile
           USE DDSModule
           USE ShiftMultiplexorModule
           IMPLICIT NONE


           CHARACTER(*),INTENT(IN) :: inputSignalFileNameI
           CHARACTER(*),INTENT(IN) :: inputSignalFileNameQ
           CHARACTER(*),INTENT(IN) :: outputSignalAddName
           CHARACTER(*),INTENT(IN) :: outputSignalSubName

           TYPE(analyticSignal_t) ::signal_1
           TYPE(analyticSignal_t) ::signal_2
           TYPE(analyticSignal_t) ::signal_3
           TYPE(analyticSignal_t) ::signal_4

           INTEGER(1)    :: intType=2

           CALL ReadAnalyticSignalFromFile(signal_1,intType,inputSignalFileNameI,.True.)
           CALL ReadAnalyticSignalFromFile(signal_2,intType,inputSignalFileNameQ,.True.)


           CALL signal_1%SetName('Первый!')

           CALL signal_2%SetName('Второй!')

           signal_3=signal_1+signal_2

           CALL signal_3%SetName('Третрий!')

           CALL WriteAnalyticSignalToFile(signal_3,intType,outputSignalAddName,.True.)

           signal_4=signal_1-signal_1

           CALL signal_4%SetName('Чотвiртий!')

           CALL WriteAnalyticSignalToFile(signal_4,intType,outputSignalSubName,.True.)

     END SUBROUTINE  AnalyticSignalTestAddSub


       SUBROUTINE ComplexMultiplyTest(inputSignalFileNameI,inputSignalFileNameQ&
                                      ,inputRefI,inputRefQ&
                                      ,outputSignalFileNameI,outputSignalFileNameQ&
                                      ,shift)

           USE complexSignalModule
           USE ModuleWriteReadArrayFromToFile
           USE WriteReadComplexSignalToFromFile
           USE ShiftMultiplexorModule

           TYPE(complexSignal_t) ::signal_1
           TYPE(complexSignal_t) ::signal_2
           TYPE(complexSignal_t) ::signal_3
           TYPE(complexSignal_t) ::signal_4


           INTEGER(1), INTENT(IN) :: shift
           CHARACTER(*),INTENT(IN) :: inputSignalFileNameI
           CHARACTER(*),INTENT(IN) :: inputSignalFileNameQ
           CHARACTER(*),INTENT(IN) :: inputRefI
           CHARACTER(*),INTENT(IN) :: inputRefQ
           CHARACTER(*),INTENT(IN) :: outputSignalFileNameI
           CHARACTER(*),INTENT(IN) :: outputSignalFileNameQ

           INTEGER(1) :: intType

           LOGICAL :: isBinary=.True.

           CALL signal_1%SetName('первый И',' первый Ку')
           CALL signal_2%SetName('вторий И',' вторий Ку')
           CALL signal_3%SetName('третий И',' третий Ку')
           CALL signal_4%SetName('четвертый И',' четвертый Ку')

           intType=2
           CALL ReadComplexSignalFromFile(signal_1,intType,inputSignalFileNameI,inputSignalFileNameQ,isBinary)

           CALL ReadComplexSignalFromFile(signal_2,intType,inputRefI,inputRefQ,isBinary)

           signal_3=signal_2*signal_1

           CALL signal_3%RShift(shift)
           intType=2
           CALL WriteComplexSignalToFile(signal_3,intType,outputSignalFileNameI,outputSignalFileNameQ,isBinary)

      END SUBROUTINE ComplexMultiplyTest


     ! Тест оператора свертки (классический)
     SUBROUTINE ConvolveTest(inputSignalFileName,inputRefFileName,outputSignalFileName,shift)
         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt


         CHARACTER(*), INTENT(IN) :: inputSignalFileName
         CHARACTER(*), INTENT(IN) :: inputRefFileName
         CHARACTER(*), INTENT(IN) :: outputSignalFileName
         INTEGER(1)  , INTENT(IN) :: shift

         TYPE(analyticSignal_t) ::input_sig
         TYPE(analyticSignal_t) ::reference_sig
         TYPE(analyticSignal_t) ::conv_result


         CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputSignalFileName,.True.)
         CALL ReadAnalyticSignalFromFile(reference_sig,int(4,1),inputRefFileName,.False.)

         conv_result= input_sig.CONV.reference_sig
         CALL conv_result%Rshift(shift)
         CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileName,.True.)

     END SUBROUTINE ConvolveTest

          ! Тест оператора свертки (классический)
     SUBROUTINE AutoConvolveTest(inputSignalFileName,inputRefFileName,outputSignalFileName,shift)
         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt

         CHARACTER(*), INTENT(IN) :: inputSignalFileName
         CHARACTER(*), INTENT(IN) :: inputRefFileName
         CHARACTER(*), INTENT(IN) :: outputSignalFileName
         INTEGER(1)  , INTENT(IN) :: shift

         TYPE(analyticSignal_t) ::input_sig
         TYPE(analyticSignal_t) ::reference_sig
         TYPE(analyticSignal_t) ::conv_result

         CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputSignalFileName,.True.)
         CALL ReadAnalyticSignalFromFile(reference_sig,int(2,1),inputRefFileName,.True.)

         CALL input_sig%ZeroesStuffing(input_sig%GetSignalSize(),input_sig%GetSignalSize())
         conv_result= input_sig.CONV.reference_sig
         CALL conv_result%Rshift(shift)
         CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileName,.True.)

     END SUBROUTINE AutoConvolveTest

     SUBROUTINE OpenMPIConvolveTest(inputSignalFileName,inputRefFileName,outputSignalFileName,shift,iterationCount)

         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt

         CHARACTER(*), INTENT(IN) :: inputSignalFileName
         CHARACTER(*), INTENT(IN) :: inputRefFileName
         CHARACTER(*), INTENT(IN) :: outputSignalFileName
         INTEGER(1)  , INTENT(IN) :: shift
         INTEGER(4)  , INTENT(IN) ::  iterationCount

         TYPE(analyticSignal_t) ::input_sig
         TYPE(analyticSignal_t) ::reference_sig
         TYPE(analyticSignal_t) ::conv_result

         REAL(4) :: start, finish, mean,percents
         INTEGER(8) :: i

         CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputSignalFileName,.True.)
         CALL ReadAnalyticSignalFromFile(reference_sig,int(2,1),inputRefFileName,.True.)
         CALL input_sig%ZeroesStuffing(input_sig%GetSignalSize(),input_sig%GetSignalSize())

         mean=0
         percents=0

!$OMP PARALLEL DO
         DO I=1,iterationCount
            call cpu_time(start)
            CALL conv_result%SetName('свертка')
            conv_result = input_sig.CONV.reference_sig

            call cpu_time(finish)
             WRITE(*,*) 'count ', I

           mean=finish-start
!            WRITE(*,*) 'execution time ', mean
         END DO
         !$OMP END PARALLEL DO
         mean=mean/iterationCount
         WRITE(*,*)  'MEAN TIME ', mean
         CALL conv_result%Rshift(shift)
         CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileName,.True.)

     END SUBROUTINE OpenMPIConvolveTest

     ! тест конструкутора знакового сигнала
     ! просто проверяем как загружаются нолики или единички
     SUBROUTINE SignumSignalConstructorTest()
         USE signumSignalModule
         USE analyticSignalModule

         TYPE(signumSignal_t  ) :: signum_sig!
         INTEGER(8)             ::  array_in(1:193)
         INTEGER(8),ALLOCATABLE ::  array_out(:)
         CHARACTER(10) :: fmt="(I20.1)"
!!
         array_in=0
! !
         CALL  signum_sig%Constructor( array_in)
         CALL  signum_sig%ExtractSignalData( array_out)
         WRITE(*,fmt) array_out
     END SUBROUTINE SignumSignalConstructorTest

     ! ТЕст функции обработки регистра сдвига
     ! + еще тест функции подсчета единиц в одном регистре (целом числе)
     SUBROUTINE RegisterPushPopAnsSummTest()
         USE signumSignalModule
         USE BitOpsMod

         INTEGER(8)             ::  register
         INTEGER(1)             ::  pushBit,popBit
         INTEGER(1)             ::  popPos
         INTEGER(1)             ::  startPos

         !вариант 1
         register = -1
         pushBit  = 0
         popPos = 63
         startPos=0
         popBit   = PushPopInt_8(register,pushBit,popPos,startPos)
         WRITE(*,*) 'register was -1 '
         WRITE(*,*) 'pushBit ' , pushBit , ' popPos ' , popPos,' startPos ' , startPos
         WRITE(*,*) 'regis ' , register
         WRITE(*,*) 'summ '    , SumOnesInInt_8(register), ' popBit ',popBit

     END SUBROUTINE RegisterPushPopAnsSummTest


      ! ТЕст функции обработки знакоовго массива, в частности его сдвига
     ! + еще тест функции подсчета единиц в одном регистре (целом числе)
     SUBROUTINE RegisterArrayPushPopTest()
         USE signumSignalModule
         USE BitOpsMod

         INTEGER(8)             ::  register(1:3)
         INTEGER(1)             ::  pushBit,popBit
         INTEGER(1)             ::  popPos
         INTEGER(1)             ::  startPos
         INTEGER(8)             :: i
         CHARACTER(10) :: fmt="(I64.1)"

         !вариант 1
         register = -1
         pushBit  = 0
         popPos = 63
         startPos=0
         DO i=1,129

             popBit   = PushPopBitSignumArrayInt_8(register,pushBit,startPos)

         END DO
         WRITE(*,*) 'register all elements was -1 '
         WRITE(*,*) 'pushBit ' , pushBit , ' popPos ' , popPos,' startPos ' , startPos
         WRITE(*,*) 'registr now'
         WRITE(*,fmt)  register

     END SUBROUTINE RegisterArrayPushPopTest


           ! ТЕст функции обработки знакоовго массива, в частности его сдвига
     ! + еще тест функции подсчета единиц в одном регистре (целом числе)
     SUBROUTINE SignumCorrTest()
         USE signumSignalModule
         USE BitOpsMod
         USE analyticSignalModule

         INTEGER(8)             ::  input(1:200)
         INTEGER(8)             ::  ref(1:2)
         INTEGER(8),ALLOCATABLE ::  res(:)
         TYPE(signumSignal_t  ) :: input_sig!
         TYPE(signumSignal_t  ) :: ref_sig!

         CHARACTER(10) :: fmt="(I64.1)"

         !вариант 1
         input=1
         ref=1
         CALL  input_sig%Constructor( input)
         CALL  ref_sig%Constructor( ref)
!         write(*,*) 'len = ',size(res)
         res=input_sig.CORR.ref_sig
         WRITE(*,fmt)  res
!         write(*,*) 'len = ',size(res)
         DEALLOCATE(res)
         res=input_sig.CORR.ref_sig
         WRITE(*,fmt)  res

     END SUBROUTINE SignumCorrTest

         SUBROUTINE SignumConvolveTest(inputSignalFileName,inputRefFileName,outputSignalFileName,shift,iterationCount)

         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt

         CHARACTER(*), INTENT(IN) :: inputSignalFileName
         CHARACTER(*), INTENT(IN) :: inputRefFileName
         CHARACTER(*), INTENT(IN) :: outputSignalFileName
         INTEGER(1)  , INTENT(IN) :: shift
         INTEGER(4)  , INTENT(IN) ::  iterationCount

         TYPE(analyticSignal_t) ::input_sig
         TYPE(analyticSignal_t) ::reference_sig
         TYPE(analyticSignal_t) ::conv_result

         REAL(4) :: start, finish, mean,percents
         INTEGER(8) :: i

         CHARACTER(10) :: fmt="(I64.1)"

         CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputSignalFileName,.True.)
         CALL ReadAnalyticSignalFromFile(reference_sig,int(2,1),inputRefFileName,.True.)
         CALL input_sig%ZeroesStuffing(input_sig%GetSignalSize(),input_sig%GetSignalSize())

         mean=0
         percents=0
         call omp_set_num_threads( 4 )

         !$OMP PARALLEL DO
         DO I=1,iterationCount
             WRITE(*,*) 'Cycle ', i
            call cpu_time(start)
            CALL conv_result%SetName('свертка')

            conv_result = input_sig.CONVSIGN.reference_sig

            call cpu_time(finish)

           mean=finish-start
!            WRITE(*,*) 'execution time ', mean
         END DO
      !$OMP END PARALLEL DO
         mean=mean/iterationCount
         WRITE(*,*)  'MEAN TIME ', mean
         CALL conv_result%Rshift(shift)
         CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileName,.True.)

     END SUBROUTINE SignumConvolveTest


      SUBROUTINE ImpulseGeneratorTest(pspFileName, outPutFileName,osr)
         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt
         USE impulseGeneratorModule

         INTEGER(8), intent(in)               :: osr
         CHARACTER(*), intent(in)             :: pspFileName, outPutFileName
         INTEGER(8),ALLOCATABLE               :: prbs(:)
         INTEGER(8),ALLOCATABLE               :: prbsSignal(:)
         TYPE(analyticSignal_t)               :: sig



         CALL ReadArrayFromFile (prbs,pspFileName,.FALSE. )

         prbsSignal = GenerateImpluseSequence (tau = osr, prbs = prbs)

         prbsSignal = prbsSignal*10240

         CALL sig%Constructor(prbsSignal)

        CALL WriteAnalyticSignalToFile(sig,int(2,1),outPutFileName,.True.)



      END SUBROUTINE ImpulseGeneratorTest


end module TestsModule
