
!***********Свинка тестирует код!!!! НЕ МЕШАТЬ!
!1. .
!2.                         _
!3. _._ _..._ .-',     _.._(`))
!4.'-. `     '  /-._.-'    ',/
!5.   )         \            '.
!6.  / _    _    |             \
!7. |  a    a    /              |
!8. \   .-.                     ;
!9.  '-('' ).-'       ,'       ;
!10.     '-;           |      .'
!11.        \           \    /
!12.        | 7  .__  _.-\   \
!13.        | |  |  ``/  /`  /
!14.       /,_|  |   /,_/   /
!15.          /,_/      '`-'



module TestsModule

    USE PrefixModule
    implicit none

   INTERFACE
            REAL(8) FUNCTION omp_get_wtime()
            END FUNCTION
    END INTERFACE



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
            INTEGER(2), ALLOCATABLE :: tmp(:)

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
             ALLOCATE(tmp(1:size(outputArray)))
             tmp = int(outputArray,2)
             CALL WriteArrayToFile(tmp,file1Name)
             DEALLOCATE(tmp)

             ! второй вариант получения выходного гармонического сигнала
             CALL ddsGenerator%ComputeOutput(int(centralFrequency2,8),int(signalLengthInSamples,8),outputsignal2)
              ! оператор int (,kind) можно применять и к массивам!!!
             CALL WriteAnalyticSignalToFile(outputsignal2,int(2,1),file2Name)

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


           INTEGER(2),ALLOCATABLE :: testSignal1(:)
           INTEGER(2),ALLOCATABLE :: testSignal2(:)
           INTEGER(8),ALLOCATABLE :: testSignalExtract(:)

           INTEGER(8),ALLOCATABLE :: testSignalExtractI(:)
           INTEGER(8),ALLOCATABLE :: testSignalExtractQ(:)
           INTEGER(2),ALLOCATABLE :: tmp(:)


           ! читаем два сигнала из бинарных файлов и положим в массив
           CALL ReadArrayFromFile(testSignal1,inputSignalFileNameI)
           CALL ReadArrayFromFile(testSignal2,inputSignalFileNameQ)

           ! создаем один аналитический сигнал из массива
           CALL signal_1%Constructor(  int(testSignal1,8))

           !**********ВЫЗЫВАЕМ ОПЕРАТОР ПРИСВАИВАИЯ ДЛЯ АНАЛИТЧЕСКОГО СИГНАЛА*******
           signal_2=signal_1

           ! вытаскиваем содержимое объекта в массив
           CALL signal_2% ExtractSignalData(testSignalExtract)

           ! и пишем его в файл, что бы проверить ГЛАЗАМИ

            ALLOCATE(tmp(1:size(testSignalExtract)))
            tmp = int(testSignalExtract,2)
            CALL WriteArrayToFile(tmp,outputSignalFileNameAnaytic)
            DEALLOCATE(tmp)


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

           ALLOCATE(tmp(1:size(testSignalExtractI)))
           tmp = int(testSignalExtractI,2)

           CALL WriteArrayToFile(tmp,outputSignalFileNameComplexI)

           tmp = int(testSignalExtractQ,2)

           CALL WriteArrayToFile(tmp,outputSignalFileNameComplexQ)

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
           CALL ReadAnalyticSignalFromFile(signal_1,intType,inputSignalFileName)
           CALL WriteAnalyticSignalToFile(signal_1,intType,outputSignalFileName)

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



           CALL ReadComplexSignalFromFile(signal_1,capacity,inputSignalFileNameI,inputSignalFileNameQ)
           CALL WriteComplexSignalToFile(signal_1,capacity,outputSignalFileNameI,outputSignalFileNameQ)
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




           intType=2
           CALL ReadAnalyticSignalFromFile(signal_1,intType,inputSignalFileName)
           CALL ReadAnalyticSignalFromFile(signal_2,intType,inputSignalFileName2)

           WRITE(*,*) 'signal_1 kind' ,signal_1%GetSignalKind()
           WRITE(*,*) 'signal_2 kind' ,signal_2%GetSignalKind()
           WRITE(*,*) 'signal_3=signal_1*signal_2'
           signal_3=signal_1*signal_2
            WRITE(*,*) 'signal_3 kind' ,signal_3%GetSignalKind()
           CALL signal_3%Rshift(shift)

           CALL WriteAnalyticSignalToFile(signal_3,intType,outputSignalFileName)


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
            CALL WriteComplexSignalToFile(signal_out,intType,outputSignalFileNameI,outputSignalFileNameQ)

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

           CALL ReadAnalyticSignalFromFile(signal_1,intType,inputSignalFileNameI)
           CALL ReadAnalyticSignalFromFile(signal_2,intType,inputSignalFileNameQ)


           CALL signal_1%SetName('Первый!')

           CALL signal_2%SetName('Второй!')
           WRITE(*,*) 'signal_3=signal_1+signal_2'
           signal_3=signal_1+signal_2

           CALL signal_3%SetName('Третрий!')

           CALL WriteAnalyticSignalToFile(signal_3,intType,outputSignalAddName)

           signal_4=signal_1-signal_1
            WRITE(*,*) 'signal_4=signal_1-signal_1'
           CALL signal_4%SetName('Чотвiртий!')

           CALL WriteAnalyticSignalToFile(signal_4,intType,outputSignalSubName)

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
           !TYPE(complexSignal_t) ::signal_4


           INTEGER(1), INTENT(IN) :: shift
           CHARACTER(*),INTENT(IN) :: inputSignalFileNameI
           CHARACTER(*),INTENT(IN) :: inputSignalFileNameQ
           CHARACTER(*),INTENT(IN) :: inputRefI
           CHARACTER(*),INTENT(IN) :: inputRefQ
           CHARACTER(*),INTENT(IN) :: outputSignalFileNameI
           CHARACTER(*),INTENT(IN) :: outputSignalFileNameQ

           INTEGER(1) :: intType


           CALL signal_1%SetName('первый И',' первый Ку')
           CALL signal_2%SetName('вторий И',' вторий Ку')
           CALL signal_3%SetName('третий И',' третий Ку')
      !    CALL signal_4%SetName('четвертый И',' четвертый Ку')

           intType=2
           CALL ReadComplexSignalFromFile(signal_1,intType,inputSignalFileNameI,inputSignalFileNameQ)

           CALL ReadComplexSignalFromFile(signal_2,intType,inputRefI,inputRefQ)

           signal_3=signal_2*signal_1


           CALL signal_3%RShift(shift)
           intType=2
           CALL WriteComplexSignalToFile(signal_3,intType,outputSignalFileNameI,outputSignalFileNameQ)

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


         CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputSignalFileName)
         !!!!!!
         CALL ReadAnalyticSignalFromFile(reference_sig,int(4,1),inputRefFileName,'(I10)')

         conv_result= input_sig.CONV.reference_sig
         CALL conv_result%Rshift(shift)
         CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileName)

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

         CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputSignalFileName)
         CALL ReadAnalyticSignalFromFile(reference_sig,int(2,1),inputRefFileName)

         CALL input_sig%ZeroesStuffing(input_sig%GetSignalSize(),input_sig%GetSignalSize())

         WRITE(*,*) 'input_sig kind ', input_sig%GetSignalKind()

         conv_result= input_sig.CONV.reference_sig
         WRITE(*,*) 'kinf conv ', conv_result%GetSignalKind()
         CALL conv_result%Rshift(shift)
        CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileName)

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

         CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputSignalFileName)
         CALL ReadAnalyticSignalFromFile(reference_sig,int(2,1),inputRefFileName)
         CALL input_sig%ZeroesStuffing(input_sig%GetSignalSize(),input_sig%GetSignalSize())

         mean=0
         percents=0
call omp_set_num_threads( 4 )
!!$OMP PARALLEL DO SHARED (input_sig,reference_sig)
         DO I=1,iterationCount
            call cpu_time(start)
            CALL conv_result%SetName('свертка')
            conv_result = input_sig.CONV.reference_sig

            call cpu_time(finish)
           ! WRITE(*,*) 'count ', I

           mean=finish-start
!            WRITE(*,*) 'execution time ', mean
         END DO
  !       !$OMP END PARALLEL DO
         mean=mean/iterationCount
         WRITE(*,*)  'MEAN TIME ', mean
         CALL conv_result%Rshift(shift)
         CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileName)

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
         USE SignumArrayMod
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

             popBit   = PushPopBitSignumArray(register,pushBit,startPos)

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


         INTEGER(8)             ::  input(1:20)
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
         res=input_sig.CONV.ref_sig
         WRITE(*,fmt)  res
!         write(*,*) 'len = ',size(res)
         DEALLOCATE(res)
         res=input_sig.CONV.ref_sig
         WRITE(*,fmt)  res

     END SUBROUTINE SignumCorrTest

         SUBROUTINE SignumConvolveTest(inputSignalFileName,inputRefFileName,outputSignalFileName,shift,iterationCount)

         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt
         USE signumSignalModule
         USE ArrayFunctionsMod

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


         CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputSignalFileName)
         CALL ReadAnalyticSignalFromFile(reference_sig,int(2,1),inputRefFileName)
         CALL input_sig%ZeroesStuffing(input_sig%GetSignalSize(),input_sig%GetSignalSize())

         mean=0
         percents=0
!         call omp_set_num_threads( 4 )
        call cpu_time(start)
    ! !$OMP PARALLEL DO SHARED (input_sig,reference_sig)
         DO I=1,iterationCount
             !WRITE(*,*) 'Cycle ', i

            CALL conv_result%SetName('свертка')

            conv_result = input_sig.CONVSIGN.reference_sig




!            WRITE(*,*) 'execution time ', mean
         END DO
     !  !$OMP END PARALLEL DO
        call cpu_time(finish)
        mean=finish-start
         mean=mean/iterationCount
         WRITE(*,*)  'MEAN TIME ', mean
         CALL conv_result%Rshift(shift)
         CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileName)

     END SUBROUTINE SignumConvolveTest


      SUBROUTINE ImpulseGeneratorTest(pspFileName, outPutFileName,osr)
         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt
         USE impulseGeneratorModule

         INTEGER(8), intent(in)               :: osr
         CHARACTER(*), intent(in)             :: pspFileName, outPutFileName
         INTEGER(1),ALLOCATABLE               :: prbs(:)
         INTEGER(8),ALLOCATABLE               :: prbsSignal(:)
         TYPE(analyticSignal_t)               :: sig


          !!!!!!!
         CALL ReadArrayFromFile (prbs,pspFileName,'(I1)')

         prbsSignal = GenerateImpluseSequence (tau = osr, prbs = prbs)

         prbsSignal = prbsSignal*10240

         CALL sig%Constructor(prbsSignal)

        CALL WriteAnalyticSignalToFile(sig,int(2,1),outPutFileName)



      END SUBROUTINE ImpulseGeneratorTest

       SUBROUTINE SimplePSNGeneratorTest(pspFileName, outPutFileName,osr, lenInblocks)
         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt
         USE impulseGeneratorModule
         USE PSNSimpleMod
         INTEGER(8), intent(in)               :: osr
         INTEGER(8), intent(in)               :: lenInblocks
         CHARACTER(*), intent(in)             :: pspFileName, outPutFileName
         INTEGER(1),ALLOCATABLE               :: prbs(:)
         TYPE(analyticSignal_t)               :: sig
         TYPE(PSNSimple_t)                    :: gen1
         !!!!!!!!
         CALL ReadArrayFromFile (prbs,pspFileName,'(I1)')
         CALL gen1%Constructor(prbs,osr)
         sig = gen1%OutPutPsnAs(lenInblocks)
         CALL WriteAnalyticSignalToFile(sig,int(2,1),outPutFileName)

      END SUBROUTINE SimplePSNGeneratorTest


      SUBROUTINE BPSKGeneratorTest(pspFileName,  dataFileName, outPutFileName,filterFileName&
                                   , codedDataFileName&
                                   ,baudRateInSamples, chipRateInSamples&
                                   ,sampleRate,centralFrequency,outPutSampleCapacity,outPutShift,pauseLen)

         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt
         USE BPSKmod
         USE OctetDataModule
         USE CRC16Mod
         CHARACTER(*), intent(in)           :: pspFileName,dataFileName, outPutFileName,filterFileName,codedDataFileName
         INTEGER(8)  , intent(in)           :: baudRateInSamples
         INTEGER(8)  , intent(in)           :: SampleRate
         INTEGER(8)  , intent(in)           :: centralFrequency
         INTEGER(1)  , intent(in)           :: outPutSampleCapacity
         INTEGER(1)  , ALLOCATABLE          :: psn(:)
         INTEGER(8)  , intent(in)           :: chipRateInSamples
         INTEGER(8)  , intent(in)           :: pauseLen
         INTEGER(1), ALLOCATABLE                         :: data(:)
         INTEGER(1), ALLOCATABLE                         :: dataOctets(:)
         INTEGER(1), ALLOCATABLE                         :: dataOctetsWithCrc(:)
          INTEGER(1), ALLOCATABLE                         :: dataOctetsWithCrcBybit(:)

         INTEGER(2)                                      :: CRC16

         INTEGER(8), ALLOCATABLE                         :: codedData(:)
         INTEGER(8), ALLOCATABLE                         :: impulseResponse(:)
         INTEGER(1)  , intent(in)           :: outPutShift
         TYPE(BPSKmodulator_t)               :: modulatorBPSK
         TYPE(analyticSignal_t)  :: sig
         !!!!!!!!!!!!!!!!!!
         CALL ReadArrayFromFile (psn,pspFileName,'(I1)')
         CALL ReadArrayFromFile (data,dataFileName,'(I1)')
         CALL ReadArrayFromFile (impulseResponse,filterFileName,'(I10)')

         WRITE(*,'(I1)') data
         dataOctets = BitsToOctets(data,.TRUE.)
         WRITE(*,*) 'data Octets-----------'
         WRITE(*,'(z4)') dataOctets
         WRITE(*,*) '-----------'
         dataOctets = ReverseBitOrderINT1(dataOctets)
         WRITE(*,*) 'data Octets- reverse----------'
         WRITE(*,'(z4)') dataOctets
          WRITE(*,*) 'data Octets- reverse END----------'

         CRC16 = CRC16Compute(dataOctets, 4129,65535)

         ALLOCATE(dataOctetsWithCrc(1:(size(dataOctets)+2)))
         dataOctetsWithCrc(1:size(dataOctets))= dataOctets
         dataOctetsWithCrc(size(dataOctets)+1) = int(SHIFTR(crc16,8),1)
         dataOctetsWithCrc(size(dataOctets)+2) = int(crc16,1)
         WRITE(*,*) 'size(dataOctets)',size(dataOctets)
         WRITE(*,*) 'dataOctetsWithCrc size',size(dataOctetsWithCrc)
         WRITE(*,*) 'dataOctetsWithCrc- reverse----------'
         WRITE(*,'(z4)')  dataOctetsWithCrc

         dataOctetsWithCrcBybit = OctetsToBits(dataOctetsWithCrc,.TRUE.)


         WRITE(*,*) 'Constructor bpskmod start'
         CALL modulatorBPSK%Constructor(baudRateInSamples, SampleRate, centralFrequency, outPutSampleCapacity&
                                      , psn, chipRateInSamples,impulseResponse,outPutShift)


         sig = modulatorBPSK%Generate(dataOctetsWithCrcBybit)
         CALL sig%ZeroesStuffing(0*pauseLen,pauseLen)
         CALL WriteAnalyticSignalToFile(sig,int(2,1),outPutFileName)
         codedData = modulatorBPSK%GenerateDiffData(dataOctetsWithCrcBybit)
         CALL  WriteArrayToFileTxt(codedData,codedDataFileName,'(I1.1)')
      END SUBROUTINE BPSKGeneratorTest

      SUBROUTINE PhaseDetectorTest(inputFileName, outPutFileNameI,outPutFileNameQ,filterFileName&
                                   ,sampleRate,centralFrequency,initialPhase,outputShift)
         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt
         USE complexSignalModule
         USE PhaseDetectorModule
         USE WriteReadComplexSignalToFromFile

         CHARACTER(*), intent(in)           :: inputFileName, outPutFileNameI,outPutFileNameQ,filterFileName
         INTEGER(8)  , intent(in)           :: sampleRate
         INTEGER(8)  , intent(in)           :: centralFrequency
         REAL(8)     , intent(in)           :: initialPhase
         INTEGER(8)  , intent(in)           :: outputShift
         INTEGER(8)  , ALLOCATABLE          :: impulseResponse(:)
         TYPE(PhaseDetector_t)              :: phaseDemodulator
         TYPE(complexSignal_t)              :: outSignal
         TYPE(analyticSignal_t)             :: input_sig!
         !!!!!!!!!!
         CALL ReadArrayFromFile (impulseResponse,filterFileName,'(I10)')
         CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputFileName)
         CALL phaseDemodulator%Constructor(centralFrequency,initialPhase,sampleRate,impulseResponse,outputShift)
         outSignal = phaseDemodulator%Downconvert(input_sig)
         CALL WriteComplexSignalToFile(outSignal,int(2,1),outPutFileNameI,outPutFileNameQ)
      END SUBROUTINE PhaseDetectorTest


       SUBROUTINE AnalyticSignumConvolveTest(inputSignalFileName,inputRefFileName,outputSignalFileName,shift,iterationCount)

         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt
         USE signumSignalModule

         CHARACTER(*), INTENT(IN) :: inputSignalFileName
         CHARACTER(*), INTENT(IN) :: inputRefFileName
         CHARACTER(*), INTENT(IN) :: outputSignalFileName
         INTEGER(1)  , INTENT(IN) :: shift
         INTEGER(4)  , INTENT(IN) ::  iterationCount

         TYPE(analyticSignal_t) :: input_sig
         TYPE(analyticSignal_t) :: reference_sig
         TYPE(analyticSignal_t) :: conv_result
         TYPE(signumSignal_t  ) :: ref_sig!

         REAL(4) :: start, finish, mean,percents
         INTEGER(8) :: i
         INTEGER(2),ALLOCATABLE :: extractedSignal(:)


         CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputSignalFileName)
         CALL ReadAnalyticSignalFromFile(reference_sig,int(2,1),inputRefFileName)

         CALL input_sig%ZeroesStuffing(int(10240,8),int(10240,8))

         CALL reference_sig%ExtractSignalData(extractedSignal)

         CALL ref_sig%Constructor(extractedSignal)
         DEALLOCATE(extractedSignal)

         mean=0
         percents=0
         call omp_set_num_threads( 4 )

     !    !$OMP PARALLEL DO
         DO I=1,iterationCount
             WRITE(*,*) 'Cycle ', i
            call cpu_time(start)
            CALL conv_result%SetName('свертка')

            conv_result = input_sig.CONVSIGN.ref_sig

            call cpu_time(finish)

           mean=finish-start
!            WRITE(*,*) 'execution time ', mean
         END DO
 !    !$OMP END PARALLEL DO
        mean=mean/iterationCount
         WRITE(*,*)  'MEAN TIME ', mean
         CALL conv_result%Rshift(shift)
         CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileName)

     END SUBROUTINE AnalyticSignumConvolveTest

      SUBROUTINE BPSKDemodulatorTest(pspFileName,  dataFileName, inPutFileName,filterFileName&
                                   , deCodedDataFileName&
                                   , phaseDetectorIName&
                                   , phaseDetectorQName&
                                   ,complexModuleCorrNAme&
                                   ,baudRateInSamples, chipRateInSamples&
                                   ,sampleRate,centralFrequency&
                                   ,initialPhase&
                                   ,outPutSampleCapacity,outPutShift,decimationCoeff,ethalonCapacity&
                                   ,signumState&
                                   ,threshold&
                                   ,thresholdSumm)

         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt
         USE BPSKmod
         USE DBPSKDemod
         USE WriteReadComplexSignalToFromFile
         USE complexSignalModule
         USE OctetDataModule
         USE CRC16Mod

         CHARACTER(*), intent(in)           :: pspFileName,dataFileName, inPutFileName,filterFileName
         CHARACTER(*), intent(in)           :: phaseDetectorIName, phaseDetectorQName
         CHARACTER(*), intent(in)           :: complexModuleCorrNAme
         CHARACTER(*), intent(in)           :: deCodedDataFileName
         INTEGER(8)  , intent(in)           :: baudRateInSamples
         INTEGER(8)  , intent(in)           :: SampleRate
         INTEGER(8)  , intent(in)           :: centralFrequency
         INTEGER(1)  , intent(in)           :: outPutSampleCapacity
         INTEGER(8)  , intent(in)           :: decimationCoeff
         INTEGER(1)  , ALLOCATABLE          :: psn(:)
         INTEGER(8)  , intent(in)           :: chipRateInSamples
         INTEGER(1)  , INTENT(IN)           :: ethalonCapacity
         INTEGER(8) , INTENT(IN)            :: thresholdSumm
         INTEGER(8) , INTENT(IN)            :: threshold
         INTEGER(8), ALLOCATABLE            :: data(:)
         INTEGER(8), ALLOCATABLE            :: module(:)
         INTEGER(1), ALLOCATABLE            :: decodedData(:)
         INTEGER(8), ALLOCATABLE            :: impulseResponse(:)
         INTEGER(1)  , intent(in)           :: outPutShift
         REAL(8), intent(in)                :: initialPhase
         LOGICAL, INTENT(IN)                :: signumState
         INTEGER(1), ALLOCATABLE            :: decodedDataOctets(:)
         INTEGER(2)                         :: crc16,i,errorCount


         TYPE(BPSKDemodulator_t)               :: DemodulatorBPSK
         TYPE(analyticSignal_t)  :: sig





         !!!!!!!!!!!
         CALL ReadArrayFromFile (psn,pspFileName,'(I1)' )
         CALL ReadArrayFromFile (data,dataFileName,'(I1)'  )
         CALL ReadArrayFromFile (impulseResponse,filterFileName,'(I10)'  )

         CALL DemodulatorBPSK%Constructor( baudRate = baudRateInSamples&
                                          ,SampleRate =SampleRate&
                                          ,centralFrequency = centralFrequency &
                                          ,initialPhase = initialPhase&
                                          ,outPutSampleCapacity=outPutSampleCapacity &
                                          ,psn=psn, chipRateInSamples=chipRateInSamples&
                                          ,impulseResponseArray= impulseResponse&
                                          ,outPutShift = int(outPutShift,8)&
                                          ,decimationCoeff= decimationCoeff&
                                          ,ethalonCapacity = ethalonCapacity)

!         sig = modulatorBPSK%Generate(data)
!
         CALL ReadAnalyticSignalFromFile(sig,int(2,1),inPutFileName)
!
         CALL  DemodulatorBPSK%SetTreshold(int(threshold,8))
         CALL  DemodulatorBPSK%SetSignumComputeMode(signumState)
         CALL  DemodulatorBPSK%SetTresholdSumm(thresholdSumm)

          deCodedData = DemodulatorBPSK%GetData(sig)
          decodedDataOctets = BitsToOctets(deCodedData, .TRUE.)
          crc16 =  CRC16Compute(decodedDataOctets, 4129,65535)
          crc16=XOR(crc16,z'ffff')
           WRITE (*,*) 'А ПРИЕМЕ!'
           WRITE (*,'(Z4)') CRC16
           IF (crc16 ==z'1D0F' ) WRITE(*,*)'CRC is ok!!!!'

           decodedDataOctets= ReverseBitOrderINT1(decodedDataOctets)
           DEALLOCATE(deCodedData)
           deCodedData = OctetsToBits (decodedDataOctets,.TRUE.)

          CALL  WriteArrayToFileTxt(int(deCodedData,8),deCodedDataFileName,'(I1.1)')

          errorCount = 0
          DO i=1,size(data)
             IF(data(i).NE.deCodedData(i)) THEN
                  errorCount=errorCount+1
                   WRITE(*,*) 'i тый бит ', i
             END IF
          END DO
          WRITE(*,*) '********************** '
          WRITE(*,*) 'число ошибок ', errorCount
           WRITE(*,*) '********************** '


!           CALL ReadAnalyticSignalFromFile(inI,int(2,1),'test_signals\output\Icorr.pcm')
!           CALL ReadAnalyticSignalFromFile(inQ,int(2,1),'test_signals\output\Qcorr.pcm')
!           inI= inI*inI
!           inQ = inQ*inQ
!           summ = inI+inQ
!           CALL summ%Rshift(int(10,1))
!            CALL WriteAnalyticSignalToFile(summ,int(2,1),'test_signals\output\summ.pcm')

      END SUBROUTINE BPSKDemodulatorTest

      SUBROUTINE OctetDataMaker(inputDataFileName,outputDataFileName)
           USE OctetDataModule
           USE ModuleWriteReadArrayFromToFile
           USE ReadWriteArrayToFromTxt
           CHARACTER(*), intent(in)           :: inputDataFileName,outputDataFileName
           INTEGER(1), ALLOCATABLE            :: dataArray(:)
           INTEGER(1), ALLOCATABLE            :: dataArrayOctets(:)
           INTEGER(1), ALLOCATABLE            :: dataArray2(:)
           CALL ReadArrayFromFile (dataArray,inputDataFileName,'(I1)')
           dataArrayOctets = BitsToOctets(dataArray,.TRUE.)
           dataArray2     = OctetsToBits(dataArrayOctets, .TRUE.)
           WRITE (*,'(Z4)') dataArrayOctets
           CALL WriteArrayToFileTxt(int( dataArray2,8),outputDataFileName,'(I1.1)' )
      END SUBROUTINE OctetDataMaker

      SUBROUTINE Crc16Test(inputDataFileName,outputDataFileName)
         USE OctetDataModule
         USE ModuleWriteReadArrayFromToFile
         USE CRC16Mod
         USE BitOpsMod

         CHARACTER(*), intent(in)           :: inputDataFileName,outputDataFileName
         INTEGER(1)  , ALLOCATABLE          :: messageOctets(:)
         INTEGER(2)                          :: CRC16
         INTEGER(1)                          :: i


         CALL ReadArrayFromFile (messageOctets,inputDataFileName,'(Z2)' )


!         DO i=1,size(messageOctets)
!            messageOctets(i) = ReverseBitOrderINT1(messageOctets(i))
!         END DO

          messageOctets = ReverseBitOrderINT1(messageOctets)

         CRC16 = CRC16Compute(messageOctets, 4129,65536)
         WRITE (*,'(Z4)') 'crc16 = ', CRC16

      END SUBROUTINE Crc16Test

      SUBROUTINE PowerMeterTest(inputDataFileName,outputDataFileName,samplingFrequency,length)
      USE POWER_METER
      USE ModuleWriteReadArrayFromToFile
      USE ieee_arithmetic

          CHARACTER(*), intent(in)           :: inputDataFileName,outputDataFileName
          INTEGER(8), INTENT(IN)             :: samplingFrequency
          INTEGER(8),INTENT(IN) :: length
          REAL(8)                :: resulted
          INTEGER(2), ALLOCATABLE :: signalIn(:)

           INTEGER(8) startPos,endPos

          resulted = GetSignalRmsPowerReferenceDb(length, samplingFrequency)

          WRITE(*,*) 20*log10(resulted)

          CALL ReadArrayFromFile(signalIn,inputDataFileName)

          startPos =0
          endPos = length

          DO WHILE (endPos< size(signalIn))
          resulted =  GetSignalRmsPowerINT2(signalIn(startPos:endPos),int((endPos-startPos),8))
          IF (ieee_is_finite(resulted)) THEN
              WRITE(*,*) 'power is ', resulted

          ELSE
             WRITE(*,*) 'NO SGINAL'
          END IF

          startPos = endPos
          endPos = endPos+ length
          END DO

      END SUBROUTINE PowerMeterTest

      SUBROUTINE NoiseMakerTest(inputSignalFileName,inputRefFileName,outputSignalFileName,shift)
      USE POWER_METER
      USE ModuleWriteReadArrayFromToFile
      USE ieee_arithmetic
      USE analyticSignalModule

      USE WriteReadAnalyticSignalToFromFile
      USE ReadWriteArrayToFromTxt

      CHARACTER(*), INTENT(IN) :: inputSignalFileName
      CHARACTER(*), INTENT(IN) :: inputRefFileName
      CHARACTER(*), INTENT(IN) :: outputSignalFileName
      INTEGER(1)  , INTENT(IN) :: shift


      TYPE(analyticSignal_t) ::input_sig
      TYPE(analyticSignal_t) ::reference_sig
      TYPE(analyticSignal_t) ::conv_result


      CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputSignalFileName)
         !!!!!!
      CALL ReadAnalyticSignalFromFile(reference_sig,int(4,1),inputRefFileName,'(I10)')
      conv_result= input_sig.CONV.reference_sig
      CALL conv_result%Rshift(shift)
      CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileName)

      END SUBROUTINE NoiseMakerTest

      SUBROUTINE RandomGeneratorTest()
      USE RandomMod
         INTEGER(8)      :: i,z
         CALL RanomGeneratorInit()
         DO i=1,10
            z= GetRandomInt(int(7,1))
            WRITE(*,*) z
         END DO

      END SUBROUTINE RandomGeneratorTest

       SUBROUTINE AddNoiseTEst(inputNoiseFileName,inputSignalFileName,outputSignalFileName,amplifiedNoise,snr)
             USE RandomMod
             USE analyticSignalModule
             USE AWGNChannelMod
             USE WriteReadAnalyticSignalToFromFile
             USE ReadWriteArrayToFromTxt

            CHARACTER(*), INTENT(IN) :: inputNoiseFileName
            CHARACTER(*), INTENT(IN) :: inputSignalFileName
            CHARACTER(*), INTENT(IN) :: outputSignalFileName
            CHARACTER(*), INTENT(IN) :: amplifiedNoise
            REAL,         INTENT(IN) :: snr
            INTEGER(2)               :: testArray(1:10000)
            TYPE(AWGNChannel_t)      :: awgnChannel
            TYPE(analyticSignal_t) ::input_sig
            TYPE(analyticSignal_t) ::noise_sig
            TYPE(analyticSignal_t) ::  bold
            TYPE(analyticSignal_t) :: out_sig1
            testArray = 127
            CALL ReadAnalyticSignalFromFile(noise_sig,int(2,1),inputNoiseFileName)
            CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputSignalFileName)
            CALL bold%Constructor(testArray)
            CALL awgnChannel%LoadNoiseInt2(noise_sig)
            WRITE(*,*) 'noise power ',awgnChannel%GetPowerNoise()

            out_sig1 = awgnChannel%AddNoiseAnalytic(input_sig,snr,int(14,1))

            CALL WriteAnalyticSignalToFile(out_sig1,int(2,1),amplifiedNoise)


       END SUBROUTINE AddNoiseTEst

       SUBROUTINE ClipTest(inputSignalFileName,outPutFileName,level,outLevel)
           USE analyticSignalModule
           USE WriteReadAnalyticSignalToFromFile

           CHARACTER(*), INTENT(IN)  :: inputSignalFileName
           CHARACTER(*), INTENT(IN)  :: outPutFileName
           INTEGER(2)  , INTENT(IN)  :: level
           INTEGER(2)  , INTENT(IN)  :: outLevel

           INTEGER(2)                :: array(1:1000)

           TYPE(analyticSignal_t) ::input_sig
           TYPE(analyticSignal_t) ::output_sig

           array(1:500)    = 0
           array(501:1000) = -1

           !CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputSignalFileName)

           CALL input_sig%Constructor(array)
           output_sig = input_sig%ClipSignal(level,outLevel)
           CALL WriteAnalyticSignalToFile(output_sig,int(2,1),outPutFileName)
       END SUBROUTINE ClipTest

       SUBROUTINE ArrayReverseTest()
          USE ArrayFunctionsMod
          INTEGER(4)             ::  input(1:10)
          INTEGER(1)             :: i


          DO i=1,size(input)
             input(i)=i
          END DO

          WRITE(*,'(I2.1)') input
          CALL ReverseArrayInt(input)
          WRITE(*,'(I2.1)') input

       END SUBROUTINE ArrayReverseTest


       SUBROUTINE SignumConvolveTestReversed(inputSignalFileName,inputRefFileName,outputSignalFileName,shift,iterationCount)

         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt
         USE signumSignalModule
         USE ArrayFunctionsMod

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


         CALL ReadAnalyticSignalFromFile(input_sig,int(2,1),inputSignalFileName)
         CALL ReadAnalyticSignalFromFile(reference_sig,int(2,1),inputRefFileName)
         !CALL reference_sig%MirorReflectSignal()
         CALL input_sig%ZeroesStuffing(input_sig%GetSignalSize(),input_sig%GetSignalSize())

         mean=0
         percents=0
!         call omp_set_num_threads( 4 )
        call cpu_time(start)

         DO I=1,iterationCount
             !WRITE(*,*) 'Cycle ', i

            CALL conv_result%SetName('свертка')

            conv_result = input_sig.CONVSIGN.reference_sig




!            WRITE(*,*) 'execution time ', mean
         END DO

        call cpu_time(finish)
        mean=finish-start
         mean=mean/iterationCount
         WRITE(*,*)  'MEAN TIME ', mean
         CALL conv_result%Rshift(shift)
         CALL WriteAnalyticSignalToFile(conv_result,int(2,1),outputSignalFileName)

     END SUBROUTINE SignumConvolveTestReversed

     SUBROUTINE MakeGaussianNoiseByRandomGenerator(m,sigma,length,outputFileName)
         USE RandomMod
         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile

         REAL       ,INTENT(IN)  :: m
         REAL       ,INTENT(IN)  :: sigma
         INTEGER(8) ,INTENT(IN)  :: length
         INTEGER(8)              :: i
         INTEGER(2),dimension(:),ALLOCATABLE :: noise
         TYPE(analyticSignal_t) :: noiseSignal
         CHARACTER(*),INTENT(IN) :: outputFileName

         CALL RanomGeneratorInit()
         ALLOCATE(noise(1:length))
         DO i=1,length
             noise(i) = GetRandomGaussianInt2(m,sigma)
         END DO
         CALL noiseSignal%Constructor(noise)
         CALL WriteAnalyticSignalToFile(noiseSignal,int(2,1),outputFileName)

     END SUBROUTINE  MakeGaussianNoiseByRandomGenerator


     SUBROUTINE ImpulseGenetatorTestOOP(osr,inputPspFileName,outputfileName)

         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt
         USE ImpulseGeneratorModuleOOP

         INTEGER(2)  ,INTENT(IN)              :: osr
         CHARACTER(*),INTENT(IN)              :: inputPspFileName
         CHARACTER(*),INTENT(IN)              :: outputfileName
         INTEGER(1),ALLOCATABLE,DIMENSION(:)  :: prbs
         INTEGER(2),ALLOCATABLE,DIMENSION(:)  :: signalOut
         TYPE(analyticSignal_t)               :: sig
         INTEGER(8)                           :: i,j
         INTEGER(8)                           :: length
         LOGICAL                              :: isReady =.FALSE.

         TYPE(impulseGeretator_t) :: generator



         CALL ReadArrayFromFile (prbs,inputPspFileName,'(I1)')

         CALL generator%SetOverSampleRate(osr)

         length = size(prbs)*osr
         ALLOCATE(signalOut(1:length))

         j=1
         DO i=1,length
            IF(generator%GetReadyForData()) THEN
               WRITE(*,*) 'Данные готовы!'
               CALL generator%PutData(prbs(j))
               j = j + 1
            END IF
         signalOut(i)= generator%GetOutputSample()
         END DO

         CALL sig%Constructor(signalOut)
         CALL WriteAnalyticSignalToFile(sig,int(2,1),outPutFileName)

     END SUBROUTINE  ImpulseGenetatorTestOOP


     SUBROUTINE RandomPsnMakerTest(inputFileName,outputfileNameCrossCorr,outputfileNameAutoCorr,outputfileName)
         USE  RandomMod
         USE  PSNMakerMod
         USE ModuleWriteReadArrayFromToFile
         USE ImpulseGeneratorModuleOOP
         USE analyticSignalModule
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt

         CHARACTER(*),INTENT(IN)                     :: outputfileNameCrossCorr
         CHARACTER(*),INTENT(IN)                     :: outputfileNameAutoCorr
         CHARACTER(*),INTENT(IN)                     :: outputfileName
         CHARACTER(*),INTENT(IN)                     :: inputFileName
         INTEGER(2)                                  :: psnLength
         INTEGER(1), ALLOCATABLE, DIMENSION(:)       :: psn
         INTEGER(1), ALLOCATABLE, DIMENSION(:)       :: psnBase
         INTEGER(2)                                  :: osr
         INTEGER(8)                                  :: length,i,j,k

         INTEGER(1), ALLOCATABLE, DIMENSION(:)       :: psnSampled
         INTEGER(1), ALLOCATABLE, DIMENSION(:)       :: psnBaseSampled

         TYPE(impulseGeretator_t) :: generatorPsn0
         TYPE(impulseGeretator_t) :: generatorPsn1
         TYPE(analyticSignal_t)   :: sigBase
         TYPE(analyticSignal_t)   :: sig
         TYPE(analyticSignal_t)   :: crossCorrelation
         TYPE(analyticSignal_t)   :: autoCorrelation
          TYPE(analyticSignal_t)   :: sig2




        osr=10

         CALL RanomGeneratorInit()

         CALL ReadArrayFromFile (psnBase,inputFileName,'(I1)')

         psnLength = size(psnBase)

         psn = MakePSN(psnLength)


         CALL generatorPsn0%SetOverSampleRate(osr)
         CALL generatorPsn1%SetOverSampleRate(osr)

         length = size(psn)*osr

         ALLOCATE(psnSampled(1:length))
         ALLOCATE(psnBaseSampled(1:length))

         j=1
         k=1
         DO i=1,length
            IF(generatorPsn0%GetReadyForData()) THEN
               WRITE(*,*) 'Данные готовы!'
               CALL generatorPsn0%PutData(psnBase(j))
               j = j + 1
            END IF

            IF(generatorPsn1%GetReadyForData()) THEN
               WRITE(*,*) 'Данные готовы!'
               CALL generatorPsn1%PutData(psn(k))
               k = k + 1
            END IF

         psnBaseSampled(i) = generatorPsn0%GetOutputSample()
         psnSampled(i)     = generatorPsn1%GetOutputSample()

         END DO

         psnBaseSampled = psnBaseSampled*1
         psnSampled     = psnSampled*1

         CALL sigBase%Constructor(psnBaseSampled)
         CALL sig%Constructor    (psnSampled    )

         CALL sigBase%ZeroesStuffing(length,length)


         crossCorrelation = sigBase.CONV.sig
         sig2=sig
         CALL sig%ZeroesStuffing(length,length)
         autoCorrelation = sig.CONV.sig2

         CALL WriteAnalyticSignalToFile(crossCorrelation,int(2,1),outputfileNameCrossCorr)
         CALL WriteAnalyticSignalToFile(autoCorrelation,int(2,1),outputfileNameAutoCorr)

         CALL WriteArrayToFileTxt(psn,outputfileName,'(I1.1)' )




     END SUBROUTINE RandomPsnMakerTest


      SUBROUTINE BPSKGenerator2PSNTest(psp0FileName,psp1FileName,  dataFileName, outPutFileName,filterFileName&
                                   , codedDataFileName&
                                   ,baudRateInSamples, chipRateInSamples&
                                   ,sampleRate,centralFrequency,outPutSampleCapacity,outPutShift,pauseLen)

         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt
         USE DBPSK2PSNmod
         USE OctetDataModule
         USE CRC16Mod
         CHARACTER(*), intent(in)           :: psp0FileName,psp1FileName,dataFileName
         CHARACTER(*), intent(in)           :: outPutFileName,filterFileName,codedDataFileName
         INTEGER(8)  , intent(in)           :: baudRateInSamples
         INTEGER(8)  , intent(in)           :: SampleRate
         INTEGER(8)  , intent(in)           :: centralFrequency
         INTEGER(1)  , intent(in)           :: outPutSampleCapacity
         INTEGER(1)  , ALLOCATABLE          :: psn1(:),psn0(:)
         INTEGER(8)  , intent(in)           :: chipRateInSamples
         INTEGER(8)  , intent(in)           :: pauseLen
         INTEGER(1), ALLOCATABLE                         :: data(:)
         INTEGER(1), ALLOCATABLE                         :: dataOctets(:)
         INTEGER(1), ALLOCATABLE                         :: dataOctetsWithCrc(:)
          INTEGER(1), ALLOCATABLE                         :: dataOctetsWithCrcBybit(:)

         INTEGER(2)                                      :: CRC16

         INTEGER(8), ALLOCATABLE                         :: codedData(:)
         INTEGER(8), ALLOCATABLE                         :: impulseResponse(:)
         INTEGER(1)  , intent(in)           :: outPutShift
         TYPE(BPSK2PSNmodulator_t)               :: modulatorBPSK
         TYPE(analyticSignal_t)  :: sig
         !!!!!!!!!!!!!!!!!!
         CALL ReadArrayFromFile (psn1,psp1FileName,'(I1)')
         CALL ReadArrayFromFile (psn0,psp0FileName,'(I1)')

         CALL ReadArrayFromFile (data,dataFileName,'(I1)')
         CALL ReadArrayFromFile (impulseResponse,filterFileName,'(I10)')

         WRITE(*,'(I1)') data
         dataOctets = BitsToOctets(data,.TRUE.)
         WRITE(*,*) 'data Octets-----------'
         WRITE(*,'(z4)') dataOctets
         WRITE(*,*) '-----------'
         dataOctets = ReverseBitOrderINT1(dataOctets)
         WRITE(*,*) 'data Octets- reverse----------'
         WRITE(*,'(z4)') dataOctets
          WRITE(*,*) 'data Octets- reverse END----------'

         CRC16 = CRC16Compute(dataOctets, 4129,65535)

         ALLOCATE(dataOctetsWithCrc(1:(size(dataOctets)+2)))
         dataOctetsWithCrc(1:size(dataOctets))= dataOctets
         dataOctetsWithCrc(size(dataOctets)+1) = int(SHIFTR(crc16,8),1)
         dataOctetsWithCrc(size(dataOctets)+2) = int(crc16,1)
         WRITE(*,*) 'size(dataOctets)',size(dataOctets)
         WRITE(*,*) 'dataOctetsWithCrc size',size(dataOctetsWithCrc)
         WRITE(*,*) 'dataOctetsWithCrc- reverse----------'
         WRITE(*,'(z4)')  dataOctetsWithCrc

         dataOctetsWithCrcBybit = OctetsToBits(dataOctetsWithCrc,.TRUE.)


         WRITE(*,*) 'Constructor bpskmod start'
         CALL modulatorBPSK%Constructor(baudRateInSamples, SampleRate, centralFrequency, outPutSampleCapacity&
                                      , psn0,psn1, chipRateInSamples,impulseResponse,outPutShift)


        !sig = modulatorBPSK%Generate(data)
         sig = modulatorBPSK%Generate(dataOctetsWithCrcBybit)

         CALL sig%ZeroesStuffing(pauseLen,pauseLen)
         CALL WriteAnalyticSignalToFile(sig,int(2,1),outPutFileName)
         codedData = modulatorBPSK%GenerateDiffData(dataOctetsWithCrcBybit)
         CALL  WriteArrayToFileTxt(codedData,codedDataFileName,'(I1.1)')

      END SUBROUTINE BPSKGenerator2PSNTest

      SUBROUTINE BPSKDemodulator2PSNTest(psp0FileName,psp1FileName,  dataFileName, inPutFileName,filterFileName&
                                   , deCodedDataFileName&
                                   , phaseDetectorIName&
                                   , phaseDetectorQName&
                                   ,complexModuleCorrNAme&
                                   ,baudRateInSamples, chipRateInSamples&
                                   ,sampleRate,centralFrequency&
                                   ,initialPhase&
                                   ,outPutSampleCapacity,outPutShift,decimationCoeff,ethalonCapacity&
                                   ,signumState&
                                   ,threshold&
                                   ,thresholdSumm)

         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE ReadWriteArrayToFromTxt
         USE BPSKmod
         USE DPSK2PSnDeMod
         USE WriteReadComplexSignalToFromFile
         USE complexSignalModule
         USE OctetDataModule
         USE CRC16Mod

         CHARACTER(*), intent(in)           :: psp0FileName, psp1FileName
         CHARACTER(*), intent(in)           :: dataFileName, inPutFileName,filterFileName
         CHARACTER(*), intent(in)           :: phaseDetectorIName, phaseDetectorQName
         CHARACTER(*), intent(in)           :: complexModuleCorrNAme
         CHARACTER(*), intent(in)           :: deCodedDataFileName
         INTEGER(8)  , intent(in)           :: baudRateInSamples
         INTEGER(8)  , intent(in)           :: SampleRate
         INTEGER(8)  , intent(in)           :: centralFrequency
         INTEGER(1)  , intent(in)           :: outPutSampleCapacity
         INTEGER(8)  , intent(in)           :: decimationCoeff
         INTEGER(1)  , ALLOCATABLE          :: psn0(:)
         INTEGER(1)  , ALLOCATABLE          :: psn1(:)
         INTEGER(8)  , intent(in)           :: chipRateInSamples
         INTEGER(1)  , INTENT(IN)           :: ethalonCapacity
         INTEGER(8) , INTENT(IN)            :: thresholdSumm
         INTEGER(8) , INTENT(IN)            :: threshold
         INTEGER(8), ALLOCATABLE            :: data(:)
         INTEGER(8), ALLOCATABLE            :: module(:)
         INTEGER(1), ALLOCATABLE            :: decodedData(:)
         INTEGER(8), ALLOCATABLE            :: impulseResponse(:)
         INTEGER(1)  , intent(in)           :: outPutShift
         REAL(8), intent(in)                :: initialPhase
         LOGICAL, INTENT(IN)                :: signumState
         INTEGER(1), ALLOCATABLE            :: decodedDataOctets(:)
         INTEGER(2)                         :: crc16,i,errorCount

         REAL(8) :: start, finish
         REAL(8) :: startAll, finishAll


         TYPE(BPSK2PSNDemodulator_t)               :: DemodulatorBPSK
         TYPE(analyticSignal_t)  :: sig
         TYPE(analyticSignal_t)  :: sig2
         TYPE(complexSignal_t) ::  signal_1



         !!!!!!!!!!!
         CALL ReadArrayFromFile (psn0,psp0FileName,'(I1)' )
         CALL ReadArrayFromFile (psn1,psp1FileName,'(I1)' )
         CALL ReadArrayFromFile (data,dataFileName,'(I1)'  )
         CALL ReadArrayFromFile (impulseResponse,filterFileName,'(I10)'  )

         CALL DemodulatorBPSK%Constructor( baudRate = baudRateInSamples&
                                          ,SampleRate =SampleRate&
                                          ,centralFrequency = centralFrequency &
                                          ,initialPhase = initialPhase&
                                          ,outPutSampleCapacity=outPutSampleCapacity &
                                          ,psn0=psn0&
                                          ,psn1=psn1&
                                          ,chipRateInSamples=chipRateInSamples&
                                          ,impulseResponseArray= impulseResponse&
                                          ,outPutShift = int(outPutShift,8)&
                                          ,decimationCoeff= decimationCoeff&
                                          ,ethalonCapacity = ethalonCapacity)



!          SUBROUTINE Constructor(this,baudRate,sampleRate,centralFrequency,initialPhase&
!                               ,outPutSampleCapacity,psn0,psn1,chipRateInSamples&
!                               ,impulseResponseArray,outputShift,decimationCoeff,ethalonCapacity)

!         sig = modulatorBPSK%Generate(data)
!
         CALL ReadAnalyticSignalFromFile(sig,int(2,1),inPutFileName)
!
         CALL  DemodulatorBPSK%SetTreshold(int(threshold,8))
         CALL  DemodulatorBPSK%SetSignumComputeMode(signumState)
         CALL  DemodulatorBPSK%SetTresholdSumm(thresholdSumm)
           start=omp_get_wtime()
          deCodedData = DemodulatorBPSK%GetData(sig)
           finish=omp_get_wtime()
          WRITE(*,*)'time = ', (finish-start)
          decodedDataOctets = BitsToOctets(deCodedData, .TRUE.)
          crc16 =  CRC16Compute(decodedDataOctets, 4129,65535)
          crc16=XOR(crc16,z'ffff')
           WRITE (*,*) 'А ПРИЕМЕ!'
           WRITE (*,'(Z4)') CRC16
           IF (crc16 ==z'1D0F' ) WRITE(*,*)'CRC is ok!!!!'

           decodedDataOctets= ReverseBitOrderINT1(decodedDataOctets)
           DEALLOCATE(deCodedData)
           deCodedData = OctetsToBits (decodedDataOctets,.TRUE.)

          CALL  WriteArrayToFileTxt(int(deCodedData,8),deCodedDataFileName,'(I1.1)')

          errorCount = 0
          DO i=1,size(data)
             IF(data(i).NE.deCodedData(i)) THEN
                  errorCount=errorCount+1
                   WRITE(*,*) 'i тый бит ', i
             END IF
          END DO
          WRITE(*,*) '********************** '
          WRITE(*,*) 'число ошибок ', errorCount
           WRITE(*,*) '********************** '


!           CALL ReadAnalyticSignalFromFile(inI,int(2,1),'test_signals\output\Icorr.pcm')
!           CALL ReadAnalyticSignalFromFile(inQ,int(2,1),'test_signals\output\Qcorr.pcm')
!           inI= inI*inI
!           inQ = inQ*inQ
!           summ = inI+inQ
!           CALL summ%Rshift(int(10,1))
!            CALL WriteAnalyticSignalToFile(summ,int(2,1),'test_signals\output\summ.pcm')

      END SUBROUTINE BPSKDemodulator2PSNTest


      SUBROUTINE DDSOutputTestFromeCodes(romLengthInBits,romLenthTruncedInBits,outputSignalSampleCapacity&
                             ,samplingFrequency,phase&
                             ,signalLengthInSamples,centralFrequency,file1Name)

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

            !начальная фаза
            REAL(8),    INTENT(IN)    :: phase
            ! длина выходного сигнала в периодах гармонич колебания
            !INTEGER(4), INTENT(IN)    :: periods
             INTEGER(4) ,INTENT(IN) :: signalLengthInSamples


            CHARACTER(*),INTENT(IN) :: file1Name
            !CHARACTER(*),INTENT(IN) :: file2Name

            INTEGER(1) :: status

           !INTEGER(4) :: oscillationPeriod


            INTEGER(8), ALLOCATABLE :: codef(:)

            INTEGER(8), ALLOCATABLE :: outputArray(:)
            INTEGER(2), ALLOCATABLE :: tmp(:)

            TYPE(DDS_t) ::ddsGenerator
            TYPE(analyticSignal_t) ::imputFreqSignal
            TYPE(analyticSignal_t) ::outputSignal

            INTEGER(8)             ::code_f_int
            REAL(8)                :: delta_f

            ! инициаализируем генератор
            status= ddsGenerator%Constructor(romLengthInBits,romLenthTruncedInBits,&
                                             samplingFrequency,outputSignalSampleCapacity)






            WRITE(*,*) 'Тест DDS_t запущен - '
            WRITE(*,*) 'проверка значений полученных конструктором'


            ! пишем таблицу ПЗУ
            WRITE(*,*) 'ddsromtable.pcm - там таблица ПЗУ '
           ! status=ddsGenerator%DebugOutput('ddsromtable.pcm')


             CALL ddsGenerator%SetPhase(phase)

             !oscillationPeriod=samplingFrequency/ centralFrequency
             !signalLengthInSamples=oscillationPeriod*periods




             ! делаем массив со кодами частоты
             ALLOCATE(codef(1:signalLengthInSamples))

             delta_f = float(samplingFrequency)/float(int(2,8)**romLengthInBits)
             WRITE(*,*) 'delta_f ',delta_f


             WRITE(*,*) 'float(centralFrequency) ',float(centralFrequency)
             code_f_int =int((float(centralFrequency)/delta_f),8)
             WRITE(*,*) 'code_f_int ',code_f_int



             codef= code_f_int

             WRITE(*,*) 'code_f ',codef(10)

            WRITE(*,*) 'до конструктора'
             ! делаем из массива аналитич сигнала
             CALL imputFreqSignal%Constructor(codef)
WRITE(*,*) 'констркуткор отработал'
             ! получаем гармонический сигнал
             CALL ddsGenerator%GetOutPutFromCodeArray(imputFreqSignal, outputsignal)



 WRITE(*,*) 'до записи!!!!!'

            CALL WriteAnalyticSignalToFile(outputsignal,int(2,1),file1Name)

             WRITE(*,*) ''
             WRITE(*,*) 'ТЕСТ DDS ЗАКОНЧЕН!!!!!'





      END SUBROUTINE DDSOutputTestFromeCodes

      SUBROUTINE GAUSS_FIR_TEST(sampleRate,bt,symbolPeriod,fir_order,capacity, outputFileName)

          USE GaussFilter
          USE GFSKmod
          USE MathConstModule

          USE analyticSignalModule
          USE PrefixModule
          USE ModuleWriteReadArrayFromToFile
          USE ReadWriteArrayToFromTxt
          USE WriteReadAnalyticSignalToFromFile

          integer(4) ,intent(IN)    :: sampleRate
          real(4)    ,INTENT(IN)    :: bt
          integer(1) ,INTENT(IN)    :: fir_order
          real(8)    ,INTENT(IN)    :: symbolPeriod
          integer(1) ,INTENT(IN)    :: capacity
          CHARACTER(*), intent(in)  :: outputFileName

          integer(8) , ALLOCATABLE   :: IR_GAUSS(:)



          write(*,*) 'symbol ', symbolPeriod

          CALL  IR_GAUSS_CALCULATE_INT_2(sampleRate = sampleRate&
                 ,bt = bt&
                 ,symbolPeriod = symbolPeriod&
                 ,fir_order = fir_order&
                 ,capacity = capacity&
                 ,IR_GAUSS_int = IR_GAUSS)
         WRITE(*,'(I6)')  IR_GAUSS

         CALL WriteArrayToFileTxt(int( IR_GAUSS,8),outputFileName,'(I6.1)' )

         DEALLOCATE(IR_GAUSS)

      END SUBROUTINE GAUSS_FIR_TEST


  SUBROUTINE GAUSS_MOD_TEST(sampleRate,bt,baudRate,mIndex,centralFrequency,fir_order&
                           ,capacityFilter,outPutDDSCapacity,outputFilterShift,romLengthTruncedInBits&
                           , outPutFileNameI, outPutFileNameQ, outputFreqName,&
                           outputFreqName2, analyticName, outPutFreqShift)

          USE GaussFilter
          USE MathConstModule
          USE GFSKmod
          USE OctetDataModule
          USE CRC16Mod
          USE PayloadGeneratorMod
          USE analyticSignalModule
          USE WriteReadAnalyticSignalToFromFile
          USE complexSignalModule
          USE WriteReadComplexSignalToFromFile
          USE FreqDetectMod
          USE ComplexDDSModule

          integer(4) ,intent(IN)    :: sampleRate
          real(4)    ,INTENT(IN)    :: bt
          INTEGER(8) ,INTENT(IN)    :: baudRate
          REAL(8)   , intent(in)    :: mIndex
          INTEGER(4) ,intent(in)    :: centralFrequency
          integer(1) ,INTENT(IN)    :: fir_order
          integer(1) ,INTENT(IN)    :: capacityFilter
          integer(1) ,INTENT(IN)    :: outPutDDSCapacity
          INTEGER(1)  , intent(in)  :: outputFilterShift
          integer(1) ,INTENT(IN)    :: romLengthTruncedInBits
          CHARACTER(*), intent(in)  :: outPutFileNameI
          CHARACTER(*), intent(in)  :: outPutFileNameQ
          CHARACTER(*), intent(in)  :: outputFreqName
          CHARACTER(*), intent(in)  :: outputFreqName2
          INTEGER(1)  , intent(in)  :: outPutFreqShift
          CHARACTER(*), intent(in)  :: analyticName


          TYPE(GFSKmodulator_t)     :: gfskMod_t
          INTEGER(4)                :: messageLength
          INTEGER(1),ALLOCATABLE    :: payloadDataBitArray(:)
          INTEGER(1),ALLOCATABLE    :: payloadDataBitArrayWithCrc(:)
          TYPE(complexSignal_t)     :: outputSignalBaseBand
          TYPE(analyticSignal_t)    :: freqOutputSignal
          TYPE(analyticSignal_t)    :: freqOutputSignalInt
          TYPE(complexDDS_t)        :: iqModulator
          TYPE(ComplexSignal_t)     :: complexHeterodyne
          TYPE(ComplexSignal_t)     :: outputSignalRF
          TYPE(analyticSignal_t)    :: analyticSygnalRF


          CALL gfskMod_t%Constructor( baudRate          = baudRate&
                                    , mIndex            = mIndex&
                                    , sampleRate        = sampleRate&
                                    , bt                = bt&
                                    , centralFrequency  = centralFrequency&
                                    , fir_order         = fir_order&
                                    , outPutDDSCapacity = outPutDDSCapacity&
                                    , outputFilterShift = outputFilterShift&
                                    ,capacityFilter     = capacityFilter&
                                    ,romLengthTruncedInBits = romLengthTruncedInBits&
                                     )
           WRITE(*,*) 'GFSK constructor '

           messageLength = 200
           ! формирование пакета данных без контрольной суммы
           payloadDataBitArray =  GenerateRandomPayloadBitArray(messageLength)
          ! WRITE(*,*) 'size payloadDataBitArray ',size(payloadDataBitArray)
           ! добавляем контрольную сумму
           payloadDataBitArrayWithCrc = GeneratePayloadDataBitArrayWithCRC(payloadDataBitArray)
          ! WRITE(*,*) 'size payloadDataBitArrayWithCrc ',size(payloadDataBitArrayWithCrc)

          outputSignalBaseBand = gfskMod_t%Generate(payloadDataBitArrayWithCrc)


          DEALLOCATE(payloadDataBitArray)
          DEALLOCATE(payloadDataBitArrayWithCrc)




          CALL iqModulator%Constructor( romLengthInBits        = int(32,1)&
                                      , romLengthTruncedInBits = romLengthTruncedInBits &
                                      , samplingFrequency        = sampleRate&
                                      , outputSignalSampleCapacity = outPutDDSCapacity&
                                      )
         CALL iqModulator%ComputeOutputComplex ( frequency = int(centralFrequency,8) &
                                               , signalLength = outputSignalBaseBand%GetSignalSize()&
                                               , outputSignalComplex = complexHeterodyne&
                                               )
          WRITE(*,*)  'centralFrequency ' , centralFrequency

          !complexHeterodyne = complexHeterodyne%MakeConjugation()

         outputSignalRF  = outputSignalBaseBand*complexHeterodyne


         !outputSignalRF = outputSignalRF%MakeConjugation()

         analyticSygnalRF = outputSignalRF%SummIQ()
         CALL analyticSygnalRF%RShift(outPutDDSCapacity)

         CALL outputSignalRF%RShift(outPutDDSCapacity)
         CALL WriteComplexSignalToFile(outputSignalRF,int(2,1),outPutFileNameI,outPutFileNameQ)

          !проверка результатов сразу
          CALL FreqDetectorComplexSignalINT8(outputSignalBaseBand, freqOutputSignalInt)
          CALL freqOutputSignalInt%RShift(int(outPutFreqShift,1))
          CALL FreqDetectorComplexSignalReal(outputSignalBaseBand, freqOutputSignal,sampleRate)
          CALL WriteAnalyticSignalToFile(freqOutputSignal,int(2,1),outputFreqName)
          CALL WriteAnalyticSignalToFile(freqOutputSignalInt,int(2,1),outputFreqName2)

          CALL WriteAnalyticSignalToFile(analyticSygnalRF,int(2,1),analyticName)



      END SUBROUTINE GAUSS_MOD_TEST


      SUBROUTINE HDLCMakerTest(outputData, outputFrame,length)
         USE OctetDataModule
         USE ModuleWriteReadArrayFromToFile
         USE CRC16Mod
         USE BitOpsMod
         USE HDLCFrameMakerModule
         USE ReadWriteArrayToFromTxt

         CHARACTER(*), intent(in)            :: outputData, outputFrame
         INTEGER(1), PARAMETER,DIMENSION(24) :: preambuleByBit = [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1]

         INTEGER(2)                                  :: length
         INTEGER(1)  , ALLOCATABLE                   :: messageOctets(:)
         INTEGER(1)  , ALLOCATABLE, DIMENSION(:)     :: dataArray
         INTEGER(1)  , ALLOCATABLE, DIMENSION(:)     :: dataArrayHDLC

          dataArray = GenerateRandomPayloadBitArray(int(length,4))
          CALL WriteArrayToFileTxt(int( dataArray,8),outputData,'(I1.1)' )

          dataArrayHDLC =  MakeFrameHDLC(dataArray,preambuleByBit )
          CALL WriteArrayToFileTxt(int( dataArrayHDLC,8),outputFrame,'(I1.1)' )

      END SUBROUTINE HDLCMakerTest

      SUBROUTINE GFSKRecieverTest(inputSigFileNameI, inputSigFileNameQ , freqDetOutFileName&
                                 , impulseResponseFileName&
                                 , outPutFilterShift, decimationRate, outPutFreqShift&
                                 )
      USE analyticSignalModule
      USE ModuleWriteReadArrayFromToFile
      USE WriteReadAnalyticSignalToFromFile
      USE ReadWriteArrayToFromTxt
      USE BPSKmod
      USE DPSK2PSnDeMod
      USE WriteReadComplexSignalToFromFile
      USE complexSignalModule
      USE OctetDataModule
      USE CRC16Mod
      USE GFSKSimpleReciever

      CHARACTER(*), intent(in)             :: inputSigFileNameI
      CHARACTER(*), intent(in)             :: inputSigFileNameQ
      CHARACTER(*), intent(in)             :: freqDetOutFileName
      CHARACTER(*), intent(in)             :: impulseResponseFileName
      INTEGER(1)  , intent(in)             :: outPutFilterShift
      INTEGER(8)  , intent(in)             :: decimationRate
      INTEGER(1)  , intent(in)             :: outPutFreqShift


      INTEGER(4), DIMENSION(:),ALLOCATABLE :: impluseResponse

      TYPE(GFSKSimpleDemodulator)           :: GFSKReciever
      TYPE(analyticSignal_t)                :: freqDetOutPut
      TYPE(ComplexSignal_t)                 :: inputBaseBandSignal






      CALL ReadArrayFromFile (impluseResponse,impulseResponseFileName,'(I10)' )


      CALL GFSKReciever%Constructor( decimationRate   = decimationRate&
                                   , impulseResponse  = impluseResponse&
                                   ,outputfilterShift = outPutFilterShift&
                                   ,outPutFreqShift   = outPutFreqShift&
                                   )
      DEALLOCATE(impluseResponse)

      CALL ReadComplexSignalFromFile(inputBaseBandSignal,int(2,1),inputSigFileNameI,inputSigFileNameQ)

      freqDetOutPut = GFSKReciever%Demodulate(inputBaseBandSignal)
      CALL  freqDetOutPut%RShift(outPutFreqShift)

      CALL WriteAnalyticSignalToFile(freqDetOutPut,int(2,1),freqDetOutFileName)

      END SUBROUTINE GFSKRecieverTest


     SUBROUTINE PsnAutoCorrTest(PSNLength, phaseOSR,  corrOutPutFileName, psnFileName)

           USE RandomMod
           USE PSNMakerMod
           USE impulseGeneratorModule
           USE RawCorrOpenMPmod
           USE ModuleWriteReadArrayFromToFile
           USE ReadWriteArrayToFromTxt
           USE ClippingMode

           INTEGER(2)  , intent(in)             :: PSNLength
           INTEGER(1)  , intent(in)             :: phaseOsr
           CHARACTER(*), intent(in)             :: corrOutPutFileName
           CHARACTER(*), intent(in)             :: psnFileName

           INTEGER(1)  ,DIMENSION(:), ALLOCATABLE :: psn
           INTEGER(2)  ,DIMENSION(:), ALLOCATABLE :: psnSampled
           INTEGER(1)  ,DIMENSION(:), ALLOCATABLE :: psnSampledClipped
           INTEGER(2)  ,DIMENSION(:), ALLOCATABLE :: psn_shifted
           INTEGER(8)  ,DIMENSION(:), ALLOCATABLE :: cross_corr
           INTEGER(2)  ,DIMENSION(:), ALLOCATABLE :: cross_corr_int2

           INTEGER(1)  ,DIMENSION(:), ALLOCATABLE :: data
           INTEGER(2)  ,DIMENSION(:), ALLOCATABLE :: dataSampled
           INTEGER(2)   :: dataLength


           dataLength = 128

           CALL RanomGeneratorInit()
           psn = MakePSN(PSNLength)

           data = MakePSN(dataLength)

           psnSampled  = GenerateImpluseSequence(int(phaseOsr,8),psn )
           dataSampled = GenerateImpluseSequence(int(phaseOsr,8),data)



           ALLOCATE(psn_shifted(1:(PSNLength*3*phaseOsr+dataLength*phaseOsr)))
           WRITE(*,*) 'size  ', size (psn_shifted)
           psn_shifted=0

           psn_shifted( PSNLength*phaseOsr+1  : 2*PSNLength*phaseOsr)                       = psnSampled
           psn_shifted( 2*PSNLength*phaseOsr+1:(2*PSNLength*phaseOsr+size(dataSampled)) ) = dataSampled

           cross_corr = CorrelationRawOpemMP(psn_shifted, psnSampled)

           ALLOCATE(cross_corr_int2(1:size(cross_corr)))
           cross_corr_int2 = cross_corr


          CALL  WriteArrayToFile(cross_corr_int2,corrOutPutFileName)

          CALL WriteArrayToFileTxt( psn,psnFileName,"(I10)")



     END SUBROUTINE PsnAutoCorrTest

  SUBROUTINE TestAWGNInternalGenerator(length,inputRefFileName,shift,outputFileName)
         USE RandomMod
         USE analyticSignalModule
         USE ModuleWriteReadArrayFromToFile
         USE WriteReadAnalyticSignalToFromFile
         USE AWGNChannelMod
         USE analyticSignalModule


         INTEGER(8) ,INTENT(IN)  :: length
         CHARACTER(*), INTENT(IN) :: inputRefFileName
         CHARACTER(*),INTENT(IN) :: outputFileName
          INTEGER(1),INTENT(IN)                :: shift
         INTEGER(2), DIMENSION(:), ALLOCATABLE             :: response
         TYPE(AWGNChannel_t)      :: awgnChannel
         TYPE(analyticSignal_t) :: noiseSignal



         CALL ReadArrayFromFile (response,inputRefFileName,'(I10)' )
         CALL awgnChannel%GenerateBandNoise(length,response,shift)
         noiseSignal = awgnChannel%GetWholeNoise()



         CALL WriteAnalyticSignalToFile(noiseSignal,int(2,1),outputFileName)

     END SUBROUTINE  TestAWGNInternalGenerator

      SUBROUTINE TestDFT(romLengthInBits,romLenthTruncedInBits,outputSignalSampleCapacity&
                             ,samplingFrequency,phase&
                             ,signalLengthInSamples,centralFrequency, file1Name, file2Name,file3Name,file4Name,shift)

             USE analyticSignalModule
            USE DDSModule
            USE MathConstModule
            USE PrefixModule
            USE ModuleWriteReadArrayFromToFile
            USE WriteReadAnalyticSignalToFromFile
            USE DFTmod
            USE FastModuleMod

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

            !начальная фаза
            REAL(8),    INTENT(IN)    :: phase
            ! длина выходного сигнала в периодах гармонич колебания
            !INTEGER(4), INTENT(IN)    :: periods
            INTEGER(4) ,INTENT(IN) :: signalLengthInSamples

            CHARACTER(*),INTENT(IN) :: file1Name
            CHARACTER(*),INTENT(IN) :: file2Name
            CHARACTER(*),INTENT(IN) :: file3Name
            CHARACTER(*),INTENT(IN) :: file4Name

            INTEGER(4), INTENT(IN) :: shift

            INTEGER(8), ALLOCATABLE :: frequencys(:)


            INTEGER(8), ALLOCATABLE :: tmpRe(:)
            INTEGER(8), ALLOCATABLE :: tmpIm(:)
            INTEGER(8), ALLOCATABLE :: tmpmodule(:)


            INTEGER(2), ALLOCATABLE :: outputArray(:)
            INTEGER(2), ALLOCATABLE :: tmpRe2(:)
            INTEGER(2), ALLOCATABLE :: tmpIm2(:)

            INTEGER(2), ALLOCATABLE :: tmpmodule2(:)

            TYPE(DDS_t) ::ddsGenerator
            TYPE(analyticSignal_t) ::imputFreqSignal
            TYPE(analyticSignal_t) ::outputSignal
            TYPE(analyticSignal_t) ::outputSignal2

            INTEGER(1) :: status
            INTEGER(4) :: sizeOfArray


             sizeOfArray = size (outputArray)



                ! инициаализируем генератор
            status= ddsGenerator%Constructor(romLengthInBits,romLenthTruncedInBits,&
                                             samplingFrequency,outputSignalSampleCapacity)


             CALL ddsGenerator%ComputeOutput(int(centralFrequency,8),int(signalLengthInSamples,8),outputsignal)


             !извлекаем массив из обьекта
             CALL outputsignal%ExtractSignalData(outputArray)


             CALL  WriteArrayToFile(outputArray,file3Name)



            CALL  DFTint2ReNew(outputArray,tmpRe, tmpIm)








            tmpmodule = GetFastMouleFromComplexInt8_8 (tmpRe,tmpIm)

            ALLOCATE(tmpmodule2(1:sizeOfArray))
            tmpmodule2 = SHIFTA(tmpmodule,shift)



            CALL  WriteArrayToFile( tmpmodule2,file4Name)


            ALLOCATE(tmpRe2(1:sizeOfArray))
            ALLOCATE(tmpIm2(1:sizeOfArray))

            tmpRe2 = SHIFTA(tmpRe,shift)
            tmpIm2 = SHIFTA(tmpIM,shift)

            CALL  WriteArrayToFile(tmpRe2,file1Name)
            CALL  WriteArrayToFile( tmpIm2,file2Name)






      END SUBROUTINE TestDFT



end module TestsModule
