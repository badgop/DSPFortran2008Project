MODULE ModuleWriteReadArrayFromToFile

    USE ModuleExitProg

    IMPLICIT NONE


    PRIVATE

    !из вне будут  будут доступны толкьо эти обобщенные имена
    PUBLIC :: ReadArrayFromFile, WriteArrayToFile

    !======================================================
    !===== Модуль записи и чтения МАССИВА в/из файла ======
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

INTERFACE ReadArrayFromFile
    !======================================================
    !============= Процедура чтения из файла ==============
    !======================================================
    !                   Входные парметры:
    ! name_x - имя файла из которого происходит чтение данных
    !
    ! isBinary - логическая переменная - Является ли файл бинарным?
    ! если Истинно - то пишется/читается бинарный файл
    !
    !======================================================
    !                   Выходные парметры:
    ! x - динамический массив в  который будет записанна информация из файла
    !     Массив должен не иметь формы при передаче!
    !    При чтении записи целых числе максимум разряднов 21 (64 битное целоы максимальное)
    !======================================================
    !
    !  МАКС размер файла  - ‭9 223 372 036 854 775 807‬ байт
    !
    !============================================================

    MODULE PROCEDURE       ReadArrayFromFileTypeBinaryInt2
    MODULE PROCEDURE       ReadArrayFromFileTypeBinaryInt4
    MODULE PROCEDURE       ReadArrayFromFileTypeBinaryInt8
END INTERFACE

INTERFACE WriteArrayToFile
    !======================================================
    !============== Процедура записи в файл =================
    !======================================================
    !                   Входные парметры:
    ! name_x - имя файла в который следует записать данные
    ! x - массив в  который будет записанна информация из файла
    !======================================================

    MODULE PROCEDURE       WriteArrayToFileBinaryTypeInt2
    MODULE PROCEDURE       WriteArrayToFileBinaryTypeInt4
    MODULE PROCEDURE       WriteArrayToFileBinaryTypeInt8

END INTERFACE

CONTAINS

    SUBROUTINE ReadArrayFromFileTypeBinaryInt2 (x, name_x, isBinary)

        IMPLICIT NONE

        INTEGER(1), PARAMETER ::INT_KIND =2

        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN OUT), ALLOCATABLE:: x(:)
        CHARACTER(*), INTENT(IN):: name_x
        LOGICAL, INTENT(IN):: isBinary


        CHARACTER(10) :: fmt="(I21.3)"

        INTEGER(8):: lengthFile=0
        LOGICAL(1):: existsFile=.FALSE.
        INTEGER(1):: iostat_Num=0

        INQUIRE(FILE = name_x, SIZE = lengthFile, EXIST= existsFile)

        IF (existsFile) THEN
            IF (lengthFile.EQ.0) THEN
                WRITE(*,*) 'Файл ', name_x, ' пустой'
                CALL ExitFromProgramNormal()
            ELSE
                IF (isBinary) then
                   OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "READ", FORM="UNFORMATTED",IOSTAT=iostat_Num)
                ELSE
                   OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "READ", FORM="FORMATTED",IOSTAT=iostat_Num)
                END IF

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

                IF (isBinary) then
                    READ(10,IOSTAT=iostat_Num) x
                ELSE
                    READ(10,fmt,IOSTAT=iostat_Num) x
                END IF

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

    END SUBROUTINE ReadArrayFromFileTypeBinaryInt2

    !======================================================

    SUBROUTINE ReadArrayFromFileTypeBinaryInt4 (x, name_x, isBinary)

         IMPLICIT NONE

        INTEGER(1), PARAMETER ::INT_KIND =4

        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN OUT), ALLOCATABLE:: x(:)
        CHARACTER(*), INTENT(IN):: name_x
        LOGICAL, INTENT(IN):: isBinary


        CHARACTER(10) :: fmt="(I21.3)"

        INTEGER(8):: lengthFile=0
        LOGICAL(1):: existsFile=.FALSE.
        INTEGER(1):: iostat_Num=0

        INQUIRE(FILE = name_x, SIZE = lengthFile, EXIST= existsFile)

        IF (existsFile) THEN
            IF (lengthFile.EQ.0) THEN
                WRITE(*,*) 'Файл ', name_x, ' пустой'
                CALL ExitFromProgramNormal()
            ELSE
                IF (isBinary) then
                   OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "READ", FORM="UNFORMATTED",IOSTAT=iostat_Num)
                ELSE
                   OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "READ", FORM="FORMATTED",IOSTAT=iostat_Num)
                END IF

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

                IF (isBinary) then
                    READ(10,IOSTAT=iostat_Num) x
                ELSE
                    READ(10,fmt,IOSTAT=iostat_Num) x
                END IF

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


    END SUBROUTINE ReadArrayFromFileTypeBinaryInt4


     SUBROUTINE ReadArrayFromFileTypeBinaryInt8 (x, name_x, isBinary)

         IMPLICIT NONE

        INTEGER(1), PARAMETER ::INT_KIND =8

        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN OUT), ALLOCATABLE:: x(:)
        CHARACTER(*), INTENT(IN):: name_x
        LOGICAL, INTENT(IN):: isBinary


        CHARACTER(10) :: fmt="(I21.3)"

        INTEGER(8):: lengthFile=0
        LOGICAL(1):: existsFile=.FALSE.
        INTEGER(1):: iostat_Num=0

        INQUIRE(FILE = name_x, SIZE = lengthFile, EXIST= existsFile)

        IF (existsFile) THEN
            IF (lengthFile.EQ.0) THEN
                WRITE(*,*) 'Файл ', name_x, ' пустой'
                CALL ExitFromProgramNormal()
            ELSE
                IF (isBinary) then
                   OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "READ", FORM="UNFORMATTED",IOSTAT=iostat_Num)
                ELSE
                   OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "READ", FORM="FORMATTED",IOSTAT=iostat_Num)
                END IF

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

                IF (isBinary) then
                    READ(10,IOSTAT=iostat_Num) x
                ELSE
                    READ(10,fmt,IOSTAT=iostat_Num) x
                END IF

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

    END SUBROUTINE ReadArrayFromFileTypeBinaryInt8

    !======================================================

    SUBROUTINE WriteArrayToFileBinaryTypeInt2 (x, name_x, isBinary)

        IMPLICIT NONE

        INTEGER(1), PARAMETER ::INT_KIND = 2

        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN) :: x(:)
        CHARACTER(*), INTENT(IN):: name_x
        LOGICAL, INTENT(IN):: isBinary
        CHARACTER(10) :: fmt="(I21.3)"

        INTEGER(4):: iostat_Num=0
        IF (isBinary) THEN
            OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "WRITE",ASYNCHRONOUS="YES", FORM="UNFORMATTED",IOSTAT=iostat_Num)
        ELSE
            OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "WRITE",ASYNCHRONOUS="YES", FORM="FORMATTED",IOSTAT=iostat_Num)
        END IF

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
        IF (isBinary) THEN
             WRITE(10) x
        ELSE

             WRITE(10,fmt) x
        END IF

        CLOSE(10)

    END SUBROUTINE WriteArrayToFileBinaryTypeInt2

    !======================================================

    SUBROUTINE WriteArrayToFileBinaryTypeInt4 (x, name_x, isBinary)

        IMPLICIT NONE

        INTEGER(1), PARAMETER ::INT_KIND = 4

        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN) :: x(:)
        CHARACTER(*), INTENT(IN):: name_x
        LOGICAL, INTENT(IN):: isBinary

        CHARACTER(10) :: fmt="(I21.3)"

        INTEGER(4):: iostat_Num=0


        IF (isBinary) THEN
            OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "WRITE",ASYNCHRONOUS="YES", FORM="UNFORMATTED",IOSTAT=iostat_Num)
        ELSE
            OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "WRITE",ASYNCHRONOUS="YES", FORM="FORMATTED",IOSTAT=iostat_Num)
        END IF

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
        IF (isBinary) THEN
             WRITE(10) x
        ELSE

             WRITE(10,fmt) x
        END IF

        CLOSE(10)

    END SUBROUTINE WriteArrayToFileBinaryTypeInt4

    SUBROUTINE WriteArrayToFileBinaryTypeInt8 (x, name_x, isBinary)

        IMPLICIT NONE

        INTEGER(1), PARAMETER ::INT_KIND = 8

        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN) :: x(:)
        CHARACTER(*), INTENT(IN):: name_x
        LOGICAL, INTENT(IN):: isBinary
        CHARACTER(10) :: fmt="(I21.3)"

        INTEGER(4):: iostat_Num=0
        IF (isBinary) THEN
            OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "WRITE",ASYNCHRONOUS="YES", FORM="UNFORMATTED",IOSTAT=iostat_Num)
        ELSE
            OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "WRITE",ASYNCHRONOUS="YES", FORM="FORMATTED",IOSTAT=iostat_Num)
        END IF

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
        IF (isBinary) THEN
             WRITE(10) x
        ELSE

             WRITE(10,fmt) x
        END IF

        CLOSE(10)
    END SUBROUTINE WriteArrayToFileBinaryTypeInt8



END MODULE ModuleWriteReadArrayFromToFile
