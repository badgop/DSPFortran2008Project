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

    USE BaseBlockModule

    IMPLICIT NONE
    PRIVATE

    TYPE, EXTENDS(baseBlock_t) , PUBLIC :: DDS

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

        PROCEDURE          :: ComputeOutput => ComputeOutDDS
        PROCEDURE          :: Constructor => InitDDS
        PROCEDURE          :: DebugOutput
        PROCEDURE, PRIVATE :: GetAmplitudeSample
        PROCEDURE          :: SetPhase

        FINAL :: destructor

    END TYPE DDS

CONTAINS


    SUBROUTINE ComputeOutDDS(this, inputSignal, outputSignal)

        CLASS(DDS), INTENT(INOUT)              :: this
        ! массив  со значениями частоты
        INTEGER(8), INTENT(IN)              :: inputSignal (:)
        ! массив с выходом генератора
        INTEGER(8), INTENT(OUT),ALLOCATABLE :: outputSignal(:)
        ! массив с кодами часоты, которые надо подавать на генератора
        INTEGER(8), ALLOCATABLE             :: frequencyCodes (:)
        INTEGER(8)                          :: LengthInputSignal
        INTEGER(8)                          :: i

        LengthInputSignal=SIZE(inputSignal)
        WRITE(*,*) 'LengthInputSignal', LengthInputSignal

        ALLOCATE(frequencyCodes(1:LengthInputSignal) )
        ALLOCATE(outputSignal  (1:LengthInputSignal) )



        ! из массива со значениями частоты, получаем массив с  значениями кодов частоты


        frequencyCodes= INT((REAL(inputSignal)/this%frequencyStep),8)



        ! цикл вычисления выходного сигнала
        DO i=1,LengthInputSignal
            ! С ПОСЛЕД ФАЗЫ!!!!!
            outputSignal(i)= GetAmplitudeSample(this,this%phaseAccState)

            this%phaseAccState=this%phaseAccState+frequencyCodes(i)
            !эмуляция переполнения аккумулятора фазы
            IF (this%phaseAccState>this%romLengthInNumber) THEN
                this%phaseAccState=this%phaseAccState-this%romLengthInNumber
            END IF
            WRITE(*,*) 'ФАЗА', this%phaseAccState




        END DO

    END SUBROUTINE ComputeOutDDS
    

    ! Член функция типа КОНСТРУКТОР
    !Выполняет инициализацию генератора ПЦС (DDS)
    ! Вызывать после обявления переменой типа DDS
    FUNCTION InitDDS(this, romLengthInBits, romLengthTruncedInBits, samplingFrequency, outputSignalSampleCapacity) RESULT (ret)

       USE MathConstModule
       IMPLICIT NONE

       CLASS(DDS), INTENT(INOUT) :: this
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
            this%romSinusTable(i)= int(FLOOR(sin(arg*i)*dacMaxOutputValue),2)
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

        CLASS(DDS), INTENT(INOUT)   :: this
        INTEGER(1)               :: ret

        CHARACTER(*), INTENT(IN) :: romTableFileName

        REAL(8)                  :: phase
        INTEGER(8)               :: phaseAccMax

        WRITE(*,*) 'Тест DDS запущен - проверка значений полученных конструктором'
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
        WRITE(*,*) 'Пишем в фал усеченную таблицу ПЗУ'
        CALL WriteArrayToFile(this%romSinusTable, romTableFileName)

        ret=0
     END FUNCTION DebugOutput

     ! функция что возвращает значение амлитуды. Входной аргумет - значение фазы аккумулятора
      PURE FUNCTION GetAmplitudeSample(this,inputPhase) RESULT (amplitude)

        IMPLICIT NONE
        CLASS(DDS), INTENT(IN) :: this
        INTEGER(8), INTENT(IN) ::inputPhase
        INTEGER(2)             ::amplitude
        INTEGER(8)             ::resultPhase

        !значение на которое нужно сдвигать значение акумулятора фазы для взятия требуемого числа старших бит
        INTEGER(1)                          :: neededShift
        neededShift=this%romLengthInBits-this%romLengthTruncedInBits

        ! УСЕЧЕНИЕ значения аккумулятора фазы до LengthTruncedInBits значащих бит
        resultPhase=SHIFTA(inputPhase,neededShift)
        amplitude= this%romSinusTable(resultPhase)

     END FUNCTION

     SUBROUTINE SetPhase(this,phaseInRadian)

        IMPLICIT NONE
        CLASS(DDS), INTENT(INOUT) :: this
        REAL(8)   , INTENT(IN) :: phaseInRadian

        !https://habr.com/ru/company/xakep/blog/257897/
        !должно раьриаит
        this%phaseAccState=int((phaseInRadian/this%phaseStep),8)

     END SUBROUTINE SetPhase

    ! деструкторы запускаются автоматически, после того как
    ! созданный обьект выйдет из области видимости.
    SUBROUTINE destructor(this)
        TYPE(DDS), INTENT(INOUT) :: this

        DEALLOCATE(this%romSinusTable)
        WRITE(*,*) 'DDS destructor завершил работу!'

    END SUBROUTINE

END MODULE DDSModule
