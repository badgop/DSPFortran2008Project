    !======================================================
    !===== Модуль записи и чтения МАССИВА в/из ТЕКСТОВЫЙ файл  ======
    !======================================================
    !             Перчень обобщенных функций в модуле:
    ! ReadArrayFromTxt - чтение массива из текстового файла
    ! WriteArrayFromTxt- запись массива  в текстовый файл
    !
    !
    ! Авторы:
    ! Татарчук И.А
    !======================================================


MODULE ReadWriteArrayToFromTxt

    USE ModuleExitProg
    IMPLICIT NONE


     PRIVATE

    !из вне будут  будут доступны толкьо эти обобщенные имена
    PUBLIC :: ReadArrayFromFileTxt, WriteArrayToFileTxt

    !======================================================
    !===== Модуль записи и чтения МАССИВА в/из текстового файла ======
    !======================================================
    !             Перчень обобщенных функций в модуле:
    ! ReadFile - чтение массива из файла
    ! WriteFile- запись массива  в файл
    !
    !
    ! Авторы:
    ! Мирошниченко А.В
    ! Татарчук И.А
    !======================================================

INTERFACE ReadArrayFromFileTxt
    !======================================================
    !============= Процедура чтения из файла ==============
    !======================================================
    !                   Входные парметры:
    ! name_x - имя текстового файла из которого происходит чтение данных
    !======================================================
    !                   Выходные парметры:
    ! x - динамический массив в  который будет записанна информация из файла
    !     Массив должен не иметь формы при передаче!
    !======================================================
    !
    !  МАКС размер файла  - ‭9 223 372 036 854 775 807‬ байт
    !
    !============================================================

    MODULE PROCEDURE       ReadArrayFromTxtInt8

END INTERFACE

INTERFACE WriteArrayToFileTxt
    !======================================================
    !============== Процедура записи в файл =================
    !======================================================
    !                   Входные парметры:
    ! name_x - имя файла в который следует записать данные
    ! x - массив в  который будет записанна информация из файла
    !======================================================

    MODULE PROCEDURE       WriteArrayFromTxtInt8


END INTERFACE

    CONTAINS


    SUBROUTINE ReadArrayFromTxtInt8(x, name_x)

        IMPLICIT NONE

        INTEGER(1), PARAMETER ::INT_KIND =8

        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN OUT), ALLOCATABLE:: x(:)
        CHARACTER(*), INTENT(IN):: name_x

        INTEGER(8):: lengthFile=0
        LOGICAL(1):: existsFile=.FALSE.
        INTEGER(1):: iostat_Num=0

        INQUIRE(FILE = name_x, SIZE = lengthFile, EXIST= existsFile)

        IF (existsFile) THEN
            IF (lengthFile.EQ.0) THEN
                WRITE(*,*) 'Файл ', name_x, ' пустой'
                CALL ExitFromProgramNormal()
            ELSE
                OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "READ", FORM="FORMATTED",IOSTAT=iostat_Num)
                IF (iostat_Num.NE.0) then
                    WRITE(*,*) 'Ошибка при подключении к файлу ', name_x
                    CALL   ExitFromProgramNormal()
                END IF
                IF (ALLOCATED(x)) THEN
                    DEALLOCATE(x)
                END IF
                !Платформозависимый код
                !ВНИМАНИЕ
                ALLOCATE(x(1:lengthFile/INT_KIND))
                READ(10,*,IOSTAT=iostat_Num) x
                IF (iostat_Num.NE.0) then
                    WRITE(*,*) 'Ошибка при чтении файла ', name_x
                    CALL   ExitFromProgramNormal()
                END IF
                WRITE(*,*) 'Успешно прочитан файл ', name_x
                CLOSE(10)
           END IF
        ELSE
            WRITE(*,*) 'Файл ', name_x, ' не существует'
            CALL   ExitFromProgramNormal()
        END IF



    END SUBROUTINE ReadArrayFromTxtInt8

     SUBROUTINE WriteArrayFromTxtInt8(x, name_x,fmt)

        IMPLICIT NONE

        INTEGER(1), PARAMETER ::INT_KIND = 8

        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN) :: x(:)
        CHARACTER(*), INTENT(IN) :: name_x
        CHARACTER(*), INTENT(IN) :: fmt
        INTEGER(4):: iostat_Num=0

        OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "WRITE",ASYNCHRONOUS="YES", FORM="FORMATTED",IOSTAT=iostat_Num)
        SELECT CASE (iostat_Num)
            CASE (0)
                WRITE(*,*) 'Пишется файл  ', name_x
            CASE (1:)
                WRITE(*,*) 'ОШИБКА при записи файла ', name_x
                CALL   ExitFromProgramNormal()
            ! НУЖНО ПРОВЕРИТЬ !!!!!
            CASE (:-1)
                WRITE(*,*) 'End of file ', name_x
        END SELECT

        WRITE(10,fmt) x
        CLOSE(10)




    END SUBROUTINE WriteArrayFromTxtInt8

END MODULE ReadWriteArrayToFromTxt
