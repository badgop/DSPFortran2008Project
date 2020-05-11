    !======================================================
    !===== Модуль, содержащий класс цифрового синтезатора частоты (Direct Digital Synthesis)
    !
    !======
    !======
    !                    ОГРАНИЧЕНИЯ ЗНАЧЕНИЙ ПАРАМЕТРОВ ГЕНЕРАТОРА
    !======================================================
    !             Перечень обобщенных функций в модуле:
    !
    ! Авторы:
    ! Татарчук И.А
    !======================================================

MODULE DDSModule

    USE analyticSignalModule

    IMPLICIT NONE
    PRIVATE

    TYPE,  PUBLIC :: DDS_t

            PRIVATE
            ! ПЗУ таблицы с отчетами синуса
            INTEGER(2),ALLOCATABLE :: romSinusTable(:)

            !мгновенное значение аккамулятора фазы
            INTEGER(8) :: phaseAccState=0

            !разрядность аккамулятора фазы
            INTEGER(1) :: romLengthInBits

            !частота дискретизации
            INTEGER(4) :: samplingFrequency

            !число бит до которых усекатется таблица ПЗУ
            INTEGER(1) :: romLengthTruncedInBits

            !разрядность выходного сигнала с УЧЕТОМ ЗНАКА
            INTEGER(1) :: outputSignalSampleCapacity

            !размеры  ПЗУ - число
            INTEGER(8) :: romLengthInNumber
            !размеры усеченнной ПЗУ - число
            INTEGER(8) :: truncedRomLengthInNumber

            ! шаг перестройки частоты
            REAL(8)    :: frequencyStep
            ! шаг перестройки фазы
            REAL(8)    :: phaseStep



    CONTAINS
        ! оновная процедура - получение выходного гармонического сигнала типа (analyticSignal_t
        ! на входе обьект того же типа, представляющей собой значения частоты  в Гц
        PROCEDURE          :: ComputeOutputFromArray
        ! оновная процедура - получение выходного гармонического сигнала типа (analyticSignal_t
        ! на входе значение частоты в Гц, и требуемая длительность выходного сигнала

        PROCEDURE          :: ComputeOutputFromScalar
        ! Формируем обобщенный интерфейс, для удобства
        GENERIC           :: ComputeOutput =>  ComputeOutputFromArray,ComputeOutputFromScalar

        ! конструктор- для инициалзиаици параметро генератора
        PROCEDURE          :: Constructor => InitDDS
        ! отладочная функция, выводит таблицу ПЗУ при заданных параметрах генератора, и
        ! проверяет правильность пересчета фазы с вещественного в целый
        PROCEDURE          :: DebugOutput
        ! установить фазу сигнала
        ! на входе значение типа REAL(8) - фаза в радианах
        PROCEDURE          :: SetPhase

        ! функция выполняющая преобразование значения фазового аккумулятора
        ! в адрес ПЗУ
        ! использется толкьо внутри производного типа (класса)
        PROCEDURE, PRIVATE :: GetAmplitudeSample


        FINAL :: destructor

    END TYPE DDS_t

CONTAINS

    ! Процедра вырабаывает выходной гармонический сигнал в соответствии
    ! с входным сигналом, который содержит значения частоты для каждого
    ! отсчета
    SUBROUTINE ComputeOutputFromArray(this, inputSignal, outputSignal)
        USE analyticSignalModule

        CLASS(DDS_t), INTENT(INOUT)              :: this
        ! массив  со значениями частоты
        CLASS(analyticSignal_t), INTENT(IN)     :: inputSignal
        ! массив с выходом генератора
        CLASS(analyticSignal_t), INTENT(INOUT) :: outputSignal


        ! массив с кодами часоты, которые надо подавать на генератора
        INTEGER(8), ALLOCATABLE             :: frequencyCodes (:)
        INTEGER(2), ALLOCATABLE             :: tempArray (:)
        INTEGER(8)                          :: LengthInputSignal
        INTEGER(8)                          :: i

        LengthInputSignal=inputSignal%GetSignalSize()
        WRITE(*,*) 'LengthInputSignal', LengthInputSignal


        CALL   inputSignal%ExtractSignalData(frequencyCodes)


        ! из массива со значениями частоты, получаем массив с  значениями кодов частоты


        frequencyCodes= INT((REAL(frequencyCodes)/this%frequencyStep),8)

        ALLOCATE(tempArray(1:size(frequencyCodes)))

        ! цикл вычисления выходного сигнала
        DO i=1,LengthInputSignal
            ! С ПОСЛЕД ФАЗЫ!!!!!
            tempArray(i)= GetAmplitudeSample(this,this%phaseAccState)

            this%phaseAccState=this%phaseAccState+frequencyCodes(i)
!            !эмуляция переполнения аккумулятора фазы
            IF (this%phaseAccState>this%romLengthInNumber) THEN
               this%phaseAccState=this%phaseAccState - this%romLengthInNumber
            END IF
          !WRITE(*,*) 'ФАЗА', this%phaseAccState
        END DO

        CALL outputSignal%Constructor(tempArray)

        DEALLOCATE(tempArray)
        DEALLOCATE(frequencyCodes)
    END SUBROUTINE ComputeOutputFromArray
    
     ! Процедра вырабаывает выходной гармонический сигнал в соответствии
    ! с скалярным значением частоты frequency
    ! signalLength - длительность сигнала в отсчетах

     SUBROUTINE ComputeOutputFromScalar(this, frequency, signalLength, outputSignal)
        USE analyticSignalModule

        CLASS(DDS_t), INTENT(INOUT)              :: this

        INTEGER(8), INTENT(IN) :: frequency
        INTEGER(8), INTENT(IN) :: signalLength

        CLASS(analyticSignal_t), INTENT(INOUT) :: outputSignal



        INTEGER(8) :: frequencyCode
        INTEGER(8), ALLOCATABLE             :: tempArray (:)
        INTEGER(8)             :: i
        ! из массива со значениями частоты, получаем массив с  значениями кодов частоты


        frequencyCode= INT((REAL(frequency)/this%frequencyStep),8)

         ALLOCATE(tempArray(1:signalLength))
!         WRITE(*,*) 'ФАЗА ГЕНЕРАТОРА', this%phaseAccState

        ! цикл вычисления выходного сигнала
        DO i=1,signalLength
            ! С ПОСЛЕД ФАЗЫ!!!!!
            tempArray(i)= GetAmplitudeSample(this,this%phaseAccState)

            this%phaseAccState=this%phaseAccState+frequencyCode
            !эмуляция переполнения аккумулятора фазы
            IF (this%phaseAccState>this%romLengthInNumber) THEN
                this%phaseAccState=this%phaseAccState - this%romLengthInNumber
            END IF

        END DO

        CALL outputSignal%Constructor(tempArray)

        DEALLOCATE(tempArray)

    END SUBROUTINE ComputeOutputFromScalar

    ! Член функция типа КОНСТРУКТОР
    !Выполняет инициализацию генератора ПЦС (DDS_t)
    ! Вызывать после обявления переменой типа DDS_t
    FUNCTION InitDDS(this, romLengthInBits, romLengthTruncedInBits, samplingFrequency, outputSignalSampleCapacity) RESULT (ret)

       USE MathConstModule
       IMPLICIT NONE

       CLASS(DDS_t), INTENT(INOUT) :: this
       INTEGER(1), INTENT(IN)    :: romLengthInBits
       INTEGER(1), INTENT(IN)    :: romLengthTruncedInBits
       INTEGER(4), INTENT(IN)    :: samplingFrequency
       INTEGER(1), INTENT(IN)    :: outputSignalSampleCapacity
       INTEGER(1)                :: ret
       INTEGER (8)               :: i
       ! Максимальное выходное значение генератора
       INTEGER (2) :: dacMaxOutputValue
       REAL(8)     :: arg

       this%romLengthInBits               = romLengthInBits
       this%romLengthTruncedInBits        = romLengthTruncedInBits
       this%samplingFrequency             = samplingFrequency
       this%outputSignalSampleCapacity    = outputSignalSampleCapacity

       !для правильного приведения типов необходимо приводить к размеру и числа, написанные в коде
       ! т.к при умножении компилятор результат помещает в регистр, чей размер соотвествует размеру операнда с наиб размером
       this%romLengthInNumber                      = int(2,8)**this%romLengthInBits
       this%truncedRomLengthInNumber               = int(2,8)**this%romLengthTruncedInBits

       ALLOCATE(this%romSinusTable(0:this%truncedRomLengthInNumber -1))
       ! максимальная амплитуда цифрового сигнала на выходе ЦАП
       ! с учетом ЗНАКА
       dacMaxOutputValue=int(  (int(2,2)**(outputSignalSampleCapacity-1)-1),2  )

       arg = 2*PI*(1/float(this%truncedRomLengthInNumber))

       DO i=0,this%truncedRomLengthInNumber-1
            !Обрезать или округлять? Вот в чем вопрос!
            this%romSinusTable(i)= int((sin(arg*i)*dacMaxOutputValue),2)
       END DO

       ! вычисление значения шага перестройки частоты
       this%frequencyStep=real(samplingFrequency)/real(int(2,8)**romLengthInBits)
       ! вычисление значения шага перестройки фазы
       this%phaseStep=real(2*PI)/real(int(2,8)**romLengthInBits)
       ret=0
    END FUNCTION InitDDS


     ! отладочная функция
     ! Запускать после вызова конструктора если нужно проверить заданные значения и глянуть тублици ПЗУ
     FUNCTION  DebugOutput(this,romTableFileName) RESULT(ret)
        USE ModuleWriteReadArrayFromToFile
        USE MathConstModule
        IMPLICIT NONE

        CLASS(DDS_t), INTENT(INOUT)   :: this
        INTEGER(1)               :: ret

        CHARACTER(*), INTENT(IN) :: romTableFileName

        REAL(8)                  :: phase
        INTEGER(8)               :: phaseAccMax
        LOGICAL                  :: isBinary = .True.

        WRITE(*,*) 'Тест DDS_t запущен'
         WRITE(*,*) 'проверка значений полученных конструктором'
        WRITE(*,*) 'this%romLengthInBits ' ,this%romLengthInBits
        WRITE(*,*) 'this%romLengthTruncedInBits' ,this%romLengthTruncedInBits
        WRITE(*,*) 'this%samplingFrequency' ,this%samplingFrequency
        WRITE(*,*) 'this%outputSignalSampleCapacity' ,this%outputSignalSampleCapacity
        WRITE(*,*) 'this%romLengthInNumber' ,this%romLengthInNumber
        WRITE(*,*) 'truncedRomLengthInNumber',this%truncedRomLengthInNumber
        WRITE(*,*) 'frequencyStep, Hz' ,this%frequencyStep
        WRITE(*,*) 'phaseStep, radians' ,this%phaseStep

        WRITE(*,*) ''
        WRITE(*,*) 'проверка преобразования вещественного значения'
        WRITE(*,*)' фазы в целое число'
        WRITE(*,*) 'пусть сдвиг фазы будет PI/2 тогда'
        WRITE(*,*) 'значение аккумулятора фазы будет'
        WRITE(*,*) ' 1/4 от максильманого значения'
        WRITE(*,*) 'для заданной разрядности n, т.е (2**n)/4'

        ! делим на 1/4
        phaseAccMax=(int(2,8)**this%romLengthInBits)/int(4,8)
        phase=PI/2
        CALL this%SetPhase(phase)

        WRITE(*,*) 'максимальное значение/4 ',    phaseAccMax
        WRITE(*,*) 'значение аккумулятора   '  ,  this%phaseAccState

        IF(phaseAccMax==this%phaseAccState) THEN
            WRITE(*,*) 'Проверка преобразования прошла'
        ELSE
            WRITE(*,*) 'Проверка преобразования НЕ прошла'
        END IF

        !возвращаем фазу в нуль обратно
        phase=0
        CALL this%SetPhase(phase)

        WRITE(*,*) ''
        WRITE(*,*) 'Пишем в файл усеченную таблицу ПЗУ'
        isBinary=.FALSE.
        CALL WriteArrayToFile(this%romSinusTable, romTableFileName)

        ret=0
     END FUNCTION DebugOutput

     ! функция что возвращает значение амлитуды. Входной аргумет - значение фазы аккумулятора
      PURE FUNCTION GetAmplitudeSample(this,inputPhase) RESULT (amplitude)

        IMPLICIT NONE
        CLASS(DDS_t), INTENT(IN) :: this
        INTEGER(8), INTENT(IN) ::inputPhase
        INTEGER(2)             ::amplitude
        INTEGER(4)             ::resultPhase

        !значение на которое нужно сдвигать значение акумулятора фазы для взятия требуемого числа старших бит
        INTEGER(1)                          :: neededShift
        neededShift=this%romLengthInBits-this%romLengthTruncedInBits

        ! УСЕЧЕНИЕ значения аккумулятора фазы до LengthTruncedInBits значащих бит
        resultPhase=SHIFTA(inputPhase,neededShift)
        amplitude= this%romSinusTable(resultPhase)

     END FUNCTION

     SUBROUTINE SetPhase(this,phaseInRadian)
        USE MathConstModule
        IMPLICIT NONE
        CLASS(DDS_t), INTENT(INOUT) :: this
        REAL(8)   , INTENT(IN) :: phaseInRadian
        !https://habr.com/ru/company/xakep/blog/257897/
        !должно раьриаит


        IF ((phaseInRadian>=0.0).AND.(phaseInRadian<=2*PI)) THEN
             this%phaseAccState=int((phaseInRadian/this%phaseStep),8)

        ELSE
             WRITE(*,*) 'Фаза сигнала не принадлежит [0:2P*I] !=', phaseInRadian
             WRITE(*,*) 'Фаза будет установлена в нуль'
             this%phaseAccState=0
        END IF

     END SUBROUTINE SetPhase

    ! деструкторы запускаются автоматически, после того как
    ! созданный обьект выйдет из области видимости.
    SUBROUTINE destructor(this)
        TYPE(DDS_t), INTENT(INOUT) :: this
           IF (ALLOCATED(this%romSinusTable) )  DEALLOCATE(this%romSinusTable)
!           WRITE(*,*) 'DDS_t destructor завершил работу!'
    END SUBROUTINE

END MODULE DDSModule
