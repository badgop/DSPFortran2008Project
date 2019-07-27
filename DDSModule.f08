    !======================================================
    !===== Модуль, содержащий класс цифрового синтезатора частоты (Direct Digital Synthesis)
    !
    !======
    !======
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

            !размеры усеченнной ПЗУ - число
            INTEGER(8) :: romLengthInNumber

            ! шаг перестройки частоты
            REAL(8)    :: frequencyStep



    CONTAINS

        PROCEDURE :: ComputeOutput => ComputeOutDDS
        PROCEDURE :: Constructor => InitDDS
        PROCEDURE :: DebugOutput
        PROCEDURE :: GetAmplitudeSample

        FINAL :: destructor

    END TYPE DDS

CONTAINS


    SUBROUTINE ComputeOutDDS(this, inputSignal, outputSignal)

        CLASS(DDS), INTENT(INOUT)              :: this

        ! массив  со значениями частоты
        INTEGER(8), INTENT(IN)              :: inputSignal (:)

        ! массив с выходом генератора
        INTEGER(8), INTENT(OUT),ALLOCATABLE :: outputSignal(:)

        INTEGER(8), ALLOCATABLE             :: frequencyCodes (:)
        INTEGER(8)                          :: LengthInputSignal
        INTEGER(8)                          :: i


        LengthInputSignal=SIZE(inputSignal)

        ALLOCATE(frequencyCodes(1:LengthInputSignal) )
        ALLOCATE(outputSignal  (1:LengthInputSignal) )

        ! из массива со значениями частоты, получаем массив с  значениями кодов частоты
        frequencyCodes= INT((REAL(inputSignal)/this%frequencyStep),8)

        ! цикл вычисления
        DO i=1,LengthInputSignal

            this%phaseAccState=this%phaseAccState+frequencyCodes(i)

            !эмуляция переполнения аккумулятора фазы
            IF (this%phaseAccState>this%romLengthInNumber) THEN
                this%phaseAccState=this%phaseAccState-this%romLengthInNumber
            END IF

            ! ВСТАВИТЬ СЮДА НАПОЛНЕНЕИ МАССИВА

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
       INTEGER(1)  :: ret
       INTEGER (8) :: i
       ! Максимальное выходное значение генератора
       INTEGER (2) :: dacMaxOutputValue
       REAL(8)     :: arg

       this%romLengthInBits               = romLengthInBits
       this%romLengthTruncedInBits         = romLengthTruncedInBits
       this%samplingFrequency             = samplingFrequency
       this%outputSignalSampleCapacity    = outputSignalSampleCapacity

       !для правильного приведения типов необходимо приводить к размеру и числа, написанные в коде
       ! т.к при умножении компилятор результат помещает в регистр, чей размер соотвествует размеру операнда с наиб размером
       this%romLengthInNumber             = int(2,8)**this%romLengthTruncedInBits

       ALLOCATE(this%romSinusTable(0:this%romLengthInNumber-1))
       ! максимальная амплитуда цифрового сигнала на выходе ЦАП
       ! с учетом ЗНАКА
       dacMaxOutputValue=int(  (int(2,2)**(outputSignalSampleCapacity-1)-1),2  )
       arg = 2*PI*(1/float(this%romLengthInNumber))

       DO i=0,this%romLengthInNumber-1
            !Обрезать или округлять? Вот в чем вопрос!
            this%romSinusTable(i)= int(FLOOR(sin(arg*i)*dacMaxOutputValue),2)
       END DO
       ! вычисление значения шага перестройки частоты
       this%frequencyStep=real(samplingFrequency)/real(int(2,8)**romLengthInBits)

       ret=0
    END FUNCTION InitDDS


     ! отладочная функция
     ! Запускать после вызова конструктора если нужно проверить заданные значения и глянуть тублици ПЗУ
     FUNCTION  DebugOutput(this,romTableFileName) RESULT(ret)
        USE ModuleWriteReadArrayFromToFile
        IMPLICIT NONE

        CLASS(DDS), INTENT(IN) :: this
        INTEGER(1)  :: ret

        CHARACTER(*), INTENT(IN) :: romTableFileName

        WRITE(*,*) 'this%romLengthInBits ' ,this%romLengthInBits
        WRITE(*,*) 'this%romLengthTruncedInBits' ,this%romLengthTruncedInBits
        WRITE(*,*) 'this%samplingFrequency' ,this%samplingFrequency
        WRITE(*,*) 'this%outputSignalSampleCapacity' ,this%outputSignalSampleCapacity
        WRITE(*,*) 'this%romLengthInNumber' ,this%romLengthInNumber
        WRITE(*,*) 'frequencyStep' ,this%frequencyStep

        CALL WriteArrayToFile(this%romSinusTable, romTableFileName)

        ret=0
     END FUNCTION DebugOutput


     PURE FUNCTION GetAmplitudeSample(this,inputPhase) RESULT (amplitude)

        IMPLICIT NONE
        CLASS(DDS), INTENT(IN) :: this
        INTEGER(8), INTENT(IN) ::inputPhase
        INTEGER(2)             ::amplitude
        INTEGER(8),            ::resultPhase

        !значение на которое нужно сдвигать значение акумулятора фазы для взятия требуемого числа старших бит
        INTEGER(1)                          :: neededShift


        neededShift=this%romLengthInBits-this%romLengthTruncedInBits

        ! УСЕЧЕНИЕ значения аккумулятора фазы до LengthTruncedInBits значащих бит
        resultPhase=SHIFTA(inputPhase,neededShift)

        amplitude= this%romSinusTable(phase)

     END FUNCTION

    ! деструкторы запускаются автоматически, после того как
    ! созданный обьект выйдет из области видимости.
    SUBROUTINE destructor(this)
        TYPE(DDS), INTENT(INOUT) :: this

        DEALLOCATE(this%romSinusTable)
        WRITE(*,*) 'DDS destructor done!'

    END SUBROUTINE

END MODULE DDSModule
