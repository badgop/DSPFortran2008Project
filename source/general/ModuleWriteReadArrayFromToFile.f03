MODULE ModuleWriteReadArrayFromToFile

    USE ModuleExitProg

    IMPLICIT NONE


    PRIVATE

    !из вне будут  будут доступны толкьо эти обобщенные имена
    PUBLIC :: ReadArrayFromFile, WriteArrayToFile,IsFileExists

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
    MODULE PROCEDURE       ReadArrayFromFileTypeBinaryInt1
    MODULE PROCEDURE       ReadArrayFromFileTypeBinaryInt2
    MODULE PROCEDURE       ReadArrayFromFileTypeBinaryInt4
    MODULE PROCEDURE       ReadArrayFromFileTypeBinaryInt8
    MODULE PROCEDURE       ReadArrayFromFileTypeTxtInt1
    MODULE PROCEDURE       ReadArrayFromFileTypeTxtInt2
    MODULE PROCEDURE       ReadArrayFromFileTypeTxtInt4
    MODULE PROCEDURE       ReadArrayFromFileTypeTxtInt8
END INTERFACE

INTERFACE WriteArrayToFile
    !======================================================
    !============== Процедура записи в файл =================
    !======================================================
    !                   Входные парметры:
    ! name_x - имя файла в который следует записать данные
    ! x - массив в  который будет записанна информация из файла
    !======================================================
    MODULE PROCEDURE       WriteArrayToFileBinaryTypeInt1
    MODULE PROCEDURE       WriteArrayToFileBinaryTypeInt2
    MODULE PROCEDURE       WriteArrayToFileBinaryTypeInt4
    MODULE PROCEDURE       WriteArrayToFileBinaryTypeInt8

END INTERFACE

CONTAINS

     SUBROUTINE ReadArrayFromFileTypeBinaryInt1 (x, name_x)

        IMPLICIT NONE

        INTEGER(1), PARAMETER ::INT_KIND =1

        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN OUT), ALLOCATABLE  :: x(:)
        CHARACTER(*)     , INTENT(IN)                   :: name_x

        INTEGER(8):: lengthFile=0
        LOGICAL(1):: existsFile=.FALSE.
        INTEGER(8):: iostat_Num=0


       INQUIRE(FILE = name_x, SIZE = lengthFile, EXIST= existsFile)

            IF (existsFile) THEN
            IF (lengthFile.EQ.0) THEN
                WRITE(*,*) 'Файл ', name_x, ' пустой'
                CALL ExitFromProgramNormal()
            END IF
        ELSE
            WRITE(*,*) 'Файл ', name_x, ' не существует'
            CALL   ExitFromProgramNormal()
        END IF

!        IF (IsFileExists(name_x) .EQV. .FALSE.) THEN
!            CALL   ExitFromProgramNormal()
!        END IF

        OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "READ", FORM="UNFORMATTED",IOSTAT=iostat_Num)

        IF (iostat_Num.GT.0) then
           WRITE(*,*) 'Ошибка при подключении к файлу ', name_x
           CALL   ExitFromProgramNormal()
        END IF

        IF (ALLOCATED(x)) THEN
           DEALLOCATE(x)
        END IF

                !*********Платформозависимый код**********
                !*********ВНИМАНИЕ**************

        ALLOCATE(x(1:lengthFile/INT_KIND))
        READ(10,IOSTAT=iostat_Num) x

        !If a read statement attempts to read an endfile record, the iostat variable will be
         !set to some negative value. It will also be set to a negative value when reading beyond
        !the end of a record with a nonadvancing read statement. These conditions cannot both
        !occur at the same time.


        IF (iostat_Num.GT.0) THEN
            WRITE(*,*) 'Ошибка при чтении файла ', name_x, ' IOSTAT =',iostat_Num
            CLOSE(10)
            CALL   ExitFromProgramNormal()
        END IF
           WRITE(*,*) 'Успешно прочитан файл ', name_x
           CLOSE(10)
    END SUBROUTINE ReadArrayFromFileTypeBinaryInt1

    SUBROUTINE ReadArrayFromFileTypeBinaryInt2 (x, name_x)

        IMPLICIT NONE

        INTEGER(1), PARAMETER ::INT_KIND =2

        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN OUT), ALLOCATABLE  :: x(:)
        CHARACTER(*)     , INTENT(IN)                   :: name_x

        INTEGER(8):: lengthFile=0
        LOGICAL(1):: existsFile=.FALSE.
        INTEGER(8):: iostat_Num=0


            INQUIRE(FILE = name_x, SIZE = lengthFile, EXIST= existsFile)

            IF (existsFile) THEN
            IF (lengthFile.EQ.0) THEN
                WRITE(*,*) 'Файл ', name_x, ' пустой'
                CALL ExitFromProgramNormal()
            END IF
        ELSE
            WRITE(*,*) 'Файл ', name_x, ' не существует'
            CALL   ExitFromProgramNormal()
        END IF

!        IF (IsFileExists(name_x) .EQV. .FALSE.) THEN
!            CALL   ExitFromProgramNormal()
!        END IF

        OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "READ", FORM="UNFORMATTED",IOSTAT=iostat_Num)

        IF (iostat_Num.GT.0) then
           WRITE(*,*) 'Ошибка при подключении к файлу ', name_x
           CALL   ExitFromProgramNormal()
        END IF

        IF (ALLOCATED(x)) THEN
           DEALLOCATE(x)
        END IF

                !*********Платформозависимый код**********
                !*********ВНИМАНИЕ**************

        ALLOCATE(x(1:lengthFile/INT_KIND))
        READ(10,IOSTAT=iostat_Num) x

        !If a read statement attempts to read an endfile record, the iostat variable will be
         !set to some negative value. It will also be set to a negative value when reading beyond
        !the end of a record with a nonadvancing read statement. These conditions cannot both
        !occur at the same time.

        IF (iostat_Num.GT.0) THEN
            WRITE(*,*) 'Ошибка при чтении файла ', name_x, ' IOSTAT =',iostat_Num
            CLOSE(10)
            CALL   ExitFromProgramNormal()
        END IF
           WRITE(*,*) 'Успешно прочитан файл ', name_x
           CLOSE(10)
    END SUBROUTINE ReadArrayFromFileTypeBinaryInt2

    !======================================================

    SUBROUTINE ReadArrayFromFileTypeBinaryInt4 (x, name_x)

         IMPLICIT NONE

        INTEGER(1), PARAMETER ::INT_KIND =4

        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN OUT), ALLOCATABLE  :: x(:)
        CHARACTER(*)     , INTENT(IN)                   :: name_x

        INTEGER(8):: lengthFile=0
        LOGICAL(1):: existsFile=.FALSE.
        INTEGER(8):: iostat_Num=0


       INQUIRE(FILE = name_x, SIZE = lengthFile, EXIST= existsFile)

            IF (existsFile) THEN
            IF (lengthFile.EQ.0) THEN
                WRITE(*,*) 'Файл ', name_x, ' пустой'
                CALL ExitFromProgramNormal()
            END IF
        ELSE
            WRITE(*,*) 'Файл ', name_x, ' не существует'
            CALL   ExitFromProgramNormal()
        END IF

!        IF (IsFileExists(name_x) .EQV. .FALSE.) THEN
!            CALL   ExitFromProgramNormal()
!        END IF
        OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "READ", FORM="UNFORMATTED",IOSTAT=iostat_Num)

        IF (iostat_Num.GT.0) then
           WRITE(*,*) 'Ошибка при подключении к файлу ', name_x
           CALL   ExitFromProgramNormal()
        END IF

        IF (ALLOCATED(x)) THEN
           DEALLOCATE(x)
        END IF

                !*********Платформозависимый код**********
                !*********ВНИМАНИЕ**************

        ALLOCATE(x(1:lengthFile/INT_KIND))
        READ(10,IOSTAT=iostat_Num) x
        !If a read statement attempts to read an endfile record, the iostat variable will be
         !set to some negative value. It will also be set to a negative value when reading beyond
        !the end of a record with a nonadvancing read statement. These conditions cannot both
        !occur at the same time.

        IF (iostat_Num.GT.0) THEN
            WRITE(*,*) 'Ошибка при чтении файла ', name_x, ' IOSTAT =',iostat_Num
            CLOSE(10)
            CALL   ExitFromProgramNormal()
        END IF
           WRITE(*,*) 'Успешно прочитан файл ', name_x
           CLOSE(10)



    END SUBROUTINE ReadArrayFromFileTypeBinaryInt4


     SUBROUTINE ReadArrayFromFileTypeBinaryInt8 (x, name_x)

         IMPLICIT NONE

        INTEGER(1), PARAMETER ::INT_KIND =8

       !Входные параметры
        INTEGER(INT_KIND), INTENT(IN OUT), ALLOCATABLE  :: x(:)
        CHARACTER(*)     , INTENT(IN)                   :: name_x

        INTEGER(8):: lengthFile=0
        LOGICAL(1):: existsFile=.FALSE.
        INTEGER(8):: iostat_Num=0


       INQUIRE(FILE = name_x, SIZE = lengthFile, EXIST= existsFile)

            IF (existsFile) THEN
            IF (lengthFile.EQ.0) THEN
                WRITE(*,*) 'Файл ', name_x, ' пустой'
                CALL ExitFromProgramNormal()
            END IF
        ELSE
            WRITE(*,*) 'Файл ', name_x, ' не существует'
            CALL   ExitFromProgramNormal()
        END IF

!        IF (IsFileExists(name_x) .EQV. .FALSE.) THEN
!            CALL   ExitFromProgramNormal()
!        END IF
        OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "READ", FORM="UNFORMATTED",IOSTAT=iostat_Num)

        IF (iostat_Num.GT.0) then
           WRITE(*,*) 'Ошибка при подключении к файлу ', name_x
           CALL   ExitFromProgramNormal()
        END IF

        IF (ALLOCATED(x)) THEN
           DEALLOCATE(x)
        END IF

                !*********Платформозависимый код**********
                !*********ВНИМАНИЕ**************

        ALLOCATE(x(1:lengthFile/INT_KIND))
        READ(10,IOSTAT=iostat_Num) x

        !If a read statement attempts to read an endfile record, the iostat variable will be
         !set to some negative value. It will also be set to a negative value when reading beyond
        !the end of a record with a nonadvancing read statement. These conditions cannot both
        !occur at the same time.

        IF (iostat_Num.GT.0) THEN
            WRITE(*,*) 'Ошибка при чтении файла ', name_x, ' IOSTAT =',iostat_Num
            CLOSE(10)
            CALL   ExitFromProgramNormal()
        END IF
           WRITE(*,*) 'Успешно прочитан файл ', name_x
           CLOSE(10)


    END SUBROUTINE ReadArrayFromFileTypeBinaryInt8

    !======================================================

    SUBROUTINE ReadArrayFromFileTypeTxtInt1 (x, name_x,fmt)
        IMPLICIT NONE
        INTEGER(1), PARAMETER ::INT_KIND =1

        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN OUT), ALLOCATABLE  :: x(:)
        CHARACTER(*)     , INTENT(IN)                   :: name_x
        CHARACTER(*)     , INTENT(IN)                   :: fmt

        INTEGER(8):: lengthFile=0
        LOGICAL(1):: existsFile=.FALSE.
        INTEGER(8):: iostat_Num=0

       INQUIRE(FILE = name_x, SIZE = lengthFile, EXIST= existsFile)

            IF (existsFile) THEN
            IF (lengthFile.EQ.0) THEN
                WRITE(*,*) 'Файл ', name_x, ' пустой'
                CALL ExitFromProgramNormal()
            END IF
        ELSE
            WRITE(*,*) 'Файл ', name_x, ' не существует'
            CALL   ExitFromProgramNormal()
        END IF

!        IF (IsFileExists(name_x) .EQV. .FALSE.) THEN
!            CALL   ExitFromProgramNormal()
!        END IF

       lengthFile = GetFormattedTxtFileSize(name_x,fmt)
       IF (lengthFile < 0) THEN
            WRITE(*,*) 'не удалось определить длину файла ', name_x
            CALL   ExitFromProgramNormal()
       END IF

       OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "READ", FORM="FORMATTED", IOSTAT=iostat_Num)

       IF (ALLOCATED(x)) THEN
           DEALLOCATE(x)
       END IF

      ALLOCATE(x(1:lengthFile))
      READ(10,fmt,IOSTAT=iostat_Num) x
      CLOSE(10)

      IF (iostat_Num.GT.0) THEN
            WRITE(*,*) 'Ошибка при чтении файла ', name_x, ' IOSTAT =',iostat_Num
            CLOSE(10)
            CALL   ExitFromProgramNormal()
       END IF
           WRITE(*,*) 'Успешно прочитан файл ', name_x
           CLOSE(10)
    END SUBROUTINE ReadArrayFromFileTypeTxtInt1

     SUBROUTINE ReadArrayFromFileTypeTxtInt2 (x, name_x,fmt)
        IMPLICIT NONE
        INTEGER(1), PARAMETER ::INT_KIND =2

        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN OUT), ALLOCATABLE  :: x(:)
        CHARACTER(*)     , INTENT(IN)                   :: name_x
        CHARACTER(*)     , INTENT(IN)                   :: fmt

        INTEGER(8):: lengthFile=0
        LOGICAL(1):: existsFile=.FALSE.
        INTEGER(8):: iostat_Num=0

       INQUIRE(FILE = name_x, SIZE = lengthFile, EXIST= existsFile)

            IF (existsFile) THEN
            IF (lengthFile.EQ.0) THEN
                WRITE(*,*) 'Файл ', name_x, ' пустой'
                CALL ExitFromProgramNormal()
            END IF
        ELSE
            WRITE(*,*) 'Файл ', name_x, ' не существует'
            CALL   ExitFromProgramNormal()
        END IF

!        IF (IsFileExists(name_x) .EQV. .FALSE.) THEN
!            CALL   ExitFromProgramNormal()
!        END IF

       lengthFile = GetFormattedTxtFileSize(name_x,fmt)
       IF (lengthFile < 0) THEN
            WRITE(*,*) 'не удалось определить длину файла ', name_x
            CALL   ExitFromProgramNormal()
       END IF

       OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "READ", FORM="FORMATTED", IOSTAT=iostat_Num)

       IF (ALLOCATED(x)) THEN
           DEALLOCATE(x)
       END IF

      ALLOCATE(x(1:lengthFile))
      READ(10,fmt,IOSTAT=iostat_Num) x
      CLOSE(10)

      IF (iostat_Num.GT.0) THEN
            WRITE(*,*) 'Ошибка при чтении файла ', name_x, ' IOSTAT =',iostat_Num
            CLOSE(10)
            CALL   ExitFromProgramNormal()
       END IF
           WRITE(*,*) 'Успешно прочитан файл ', name_x
           CLOSE(10)
    END SUBROUTINE ReadArrayFromFileTypeTxtInt2

    SUBROUTINE ReadArrayFromFileTypeTxtInt4 (x, name_x,fmt)
        IMPLICIT NONE
        INTEGER(1), PARAMETER ::INT_KIND =4

        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN OUT), ALLOCATABLE  :: x(:)
        CHARACTER(*)     , INTENT(IN)                   :: name_x
        CHARACTER(*)     , INTENT(IN)                   :: fmt

        INTEGER(8):: lengthFile=0
        LOGICAL(1):: existsFile=.FALSE.
        INTEGER(8):: iostat_Num=0

       INQUIRE(FILE = name_x, SIZE = lengthFile, EXIST= existsFile)

            IF (existsFile) THEN
            IF (lengthFile.EQ.0) THEN
                WRITE(*,*) 'Файл ', name_x, ' пустой'
                CALL ExitFromProgramNormal()
            END IF
        ELSE
            WRITE(*,*) 'Файл ', name_x, ' не существует'
            CALL   ExitFromProgramNormal()
        END IF

!        IF (IsFileExists(name_x) .EQV. .FALSE.) THEN
!            CALL   ExitFromProgramNormal()
!        END IF

       lengthFile = GetFormattedTxtFileSize(name_x,fmt)
       WRITE(*,*) 'Длина!!! ', lengthFile
       IF (lengthFile < 0) THEN
            WRITE(*,*) 'не удалось определить длину файла ', name_x
            CALL   ExitFromProgramNormal()
       END IF

       OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "READ", FORM="FORMATTED", IOSTAT=iostat_Num)

       IF (ALLOCATED(x)) THEN
           DEALLOCATE(x)
       END IF

      ALLOCATE(x(1:lengthFile))
      READ(10,fmt,IOSTAT=iostat_Num) x
      CLOSE(10)

      IF (iostat_Num.GT.0) THEN
            WRITE(*,*) 'Ошибка при чтении файла ', name_x, ' IOSTAT =',iostat_Num
            CLOSE(10)
            CALL   ExitFromProgramNormal()
       END IF
           WRITE(*,*) 'Успешно прочитан файл ', name_x
           CLOSE(10)
    END SUBROUTINE ReadArrayFromFileTypeTxtInt4

    SUBROUTINE ReadArrayFromFileTypeTxtInt8 (x, name_x,fmt)
        IMPLICIT NONE
        INTEGER(1), PARAMETER ::INT_KIND =8

        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN OUT), ALLOCATABLE  :: x(:)
        CHARACTER(*)     , INTENT(IN)                   :: name_x
        CHARACTER(*)     , INTENT(IN)                   :: fmt

        INTEGER(8):: lengthFile=0
        LOGICAL(1):: existsFile=.FALSE.
        INTEGER(8):: iostat_Num=0

       INQUIRE(FILE = name_x, SIZE = lengthFile, EXIST= existsFile)

            IF (existsFile) THEN
            IF (lengthFile.EQ.0) THEN
                WRITE(*,*) 'Файл ', name_x, ' пустой'
                CALL ExitFromProgramNormal()
            END IF
        ELSE
            WRITE(*,*) 'Файл ', name_x, ' не существует'
            CALL   ExitFromProgramNormal()
        END IF

!        IF (IsFileExists(name_x) .EQV. .FALSE.) THEN
!            CALL   ExitFromProgramNormal()
!        END IF

       lengthFile = GetFormattedTxtFileSize(name_x,fmt)
       IF (lengthFile < 0) THEN
            WRITE(*,*) 'не удалось определить длину файла ', name_x
            CALL   ExitFromProgramNormal()
       END IF

       OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "READ", FORM="FORMATTED", IOSTAT=iostat_Num)

       IF (ALLOCATED(x)) THEN
           DEALLOCATE(x)
       END IF

      ALLOCATE(x(1:lengthFile))
      READ(10,fmt,IOSTAT=iostat_Num) x
      CLOSE(10)

      IF (iostat_Num.GT.0) THEN
            WRITE(*,*) 'Ошибка при чтении файла ', name_x, ' IOSTAT =',iostat_Num
            CLOSE(10)
            CALL   ExitFromProgramNormal()
       END IF
           WRITE(*,*) 'Успешно прочитан файл ', name_x
           CLOSE(10)
    END SUBROUTINE ReadArrayFromFileTypeTxtInt8

     SUBROUTINE WriteArrayToFileBinaryTypeInt1 (x, name_x)

        IMPLICIT NONE
        INTEGER(1), PARAMETER ::INT_KIND = 1
        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN OUT), ALLOCATABLE  :: x(:)
        CHARACTER(*)     , INTENT(IN)                   :: name_x

        INTEGER(4):: iostat_Num=0
        OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "WRITE",ASYNCHRONOUS="YES", FORM="UNFORMATTED",IOSTAT=iostat_Num)

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

        WRITE(10) x
        CLOSE(10)
    END SUBROUTINE WriteArrayToFileBinaryTypeInt1

    SUBROUTINE WriteArrayToFileBinaryTypeInt2 (x, name_x)

        IMPLICIT NONE
        INTEGER(1), PARAMETER ::INT_KIND = 2
        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN OUT), ALLOCATABLE  :: x(:)
        CHARACTER(*)     , INTENT(IN)                   :: name_x

        INTEGER(4):: iostat_Num=0
        OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "WRITE",ASYNCHRONOUS="YES", FORM="UNFORMATTED",IOSTAT=iostat_Num)

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

        WRITE(10) x
        CLOSE(10)
    END SUBROUTINE WriteArrayToFileBinaryTypeInt2

    !======================================================

    SUBROUTINE WriteArrayToFileBinaryTypeInt4 (x, name_x)

        IMPLICIT NONE

        INTEGER(1), PARAMETER ::INT_KIND = 4

        !Входные параметры
        INTEGER(INT_KIND), INTENT(IN OUT), ALLOCATABLE  :: x(:)
        CHARACTER(*)     , INTENT(IN)                   :: name_x

        INTEGER(4):: iostat_Num=0
        OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "WRITE",ASYNCHRONOUS="YES", FORM="UNFORMATTED",IOSTAT=iostat_Num)

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

        WRITE(10) x
        CLOSE(10)

    END SUBROUTINE WriteArrayToFileBinaryTypeInt4

    SUBROUTINE WriteArrayToFileBinaryTypeInt8 (x, name_x)

        IMPLICIT NONE

        INTEGER(1), PARAMETER ::INT_KIND = 8

       !Входные параметры
        INTEGER(INT_KIND), INTENT(IN OUT), ALLOCATABLE  :: x(:)
        CHARACTER(*)     , INTENT(IN)                   :: name_x

        INTEGER(4):: iostat_Num=0
        OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "WRITE",ASYNCHRONOUS="YES", FORM="UNFORMATTED",IOSTAT=iostat_Num)

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

        WRITE(10) x
        CLOSE(10)
    END SUBROUTINE WriteArrayToFileBinaryTypeInt8

    FUNCTION GetFormattedTxtFileSize(name_x,fmt)

        CHARACTER(*), INTENT(IN)      :: name_x
        CHARACTER(*), INTENT(IN)      :: fmt

        INTEGER(8) :: GetFormattedTxtFileSize
        INTEGER(8) :: i
        INTEGER(4) :: x = 0
        INTEGER(8) ::iostat_Num = 0

        OPEN(10, FILE = name_x, ACCESS="STREAM",ACTION= "READ", FORM="FORMATTED",IOSTAT=iostat_Num)
        i=0
        !WRITE(*,*) 'Получать длину делаю, IOSTAT ', iostat_Num ,fmt

        DO WHILE (iostat_Num>=0)
           READ(10,fmt,IOSTAT=iostat_Num) x
           i=i+1
        END DO

        SELECT CASE (iostat_Num)
           case (-1)
                 GetFormattedTxtFileSize=i
           case (:-2)
                 GetFormattedTxtFileSize = - 1
           case (0)
                 GetFormattedTxtFileSize=i
           case  (1: )
                 GetFormattedTxtFileSize = - 1

        END SELECT


!  111
!        IF(iostat_Num<0) goto 333
!        IF(iostat_Num==0)  i=i+1
!
!        goto 111
!  333   GetFormattedTxtFileSize=i

        close(10)

    END FUNCTION GetFormattedTxtFileSize

    FUNCTION IsFileExists(fileName) RESULT(isExists)
        CHARACTER(*), INTENT(IN)      :: fileName
        LOGICAL                       :: isExists
        INTEGER(8)                    :: lengthFile
        LOGICAL(1)                    :: existsFile=.FALSE.

        INQUIRE(FILE = fileName, SIZE = lengthFile, EXIST= existsFile)

        IF (existsFile) THEN
            isExists = .TRUE.
            IF (lengthFile.EQ.0) THEN
                WRITE(*,*) 'Файл ', fileName, ' пустой'
                isExists = .FALSE.
            END IF
        ELSE
            WRITE(*,*) 'Файл ', fileName, ' не существует'
               isExists = .FALSE.
        END IF
    END FUNCTION IsFileExists

END MODULE ModuleWriteReadArrayFromToFile
