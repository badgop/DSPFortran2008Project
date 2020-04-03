MODULE WriteReadAnalyticSignalToFromFile

    USE analyticSignalModule
    USE ModuleWriteReadArrayFromToFile
    USE ModuleExitProg
    IMPLICIT NONE




    !======================================================
    !===== Модуль записи и чтения МАССИВА в/из файла ======
    !======================================================
    !             Перчень обобщенных функций в модуле:
    ! ReadAnalyticSignalFromFilе - чтение массива signal(:) обьекта типа analyticSignal_t из файла
    ! WriteAnalyticSignalToFile- запись массива signal(:) обьекта типа analyticSignal_t   в файл
    !
    !
    ! Авторы:
    ! Татарчук И.А
    !======================================================


    !======================================================
    !============= Процедура чтения из файла ==============
    !======================================================
    !                   Входные парметры:
    ! fileName - имя файла из которого происходит чтение данных
    !======================================================
    !                   Выходные парметры:
    ! x - обьект типа analyticSignal_t, с не инициализированным членом  INTEGER(8) signal(:)
    !======================================================
    !
    !  МАКС размер файла  - ‭9 223 372 036 854 775 807‬ байт
    !
    !============================================================

   CONTAINS

   SUBROUTINE ReadAnalyticSignalFromFile(readedSignal,intType,fileName,fmt)
        CLASS(analyticSignal_t), INTENT(INOUT)            :: readedSignal
        INTEGER(1)             , INTENT(IN)               :: intType
        CHARACTER(*)           , INTENT(IN)               :: fileName
        CHARACTER(*)           , INTENT(IN) ,OPTIONAL     :: fmt

        INTEGER(2), ALLOCATABLE              :: arrayInt2(:)
        INTEGER(4), ALLOCATABLE              :: arrayInt4(:)
        INTEGER(8), ALLOCATABLE              :: arrayInt8(:)


        IF (.NOT.(PRESENT(fmt))) THEN
               SELECT CASE (intType)
                CASE(2)
                    CALL ReadArrayFromFile(arrayInt2,fileName)
                    CALL readedSignal%Constructor(  int(arrayInt2,8))
                    DEALLOCATE(arrayInt2)
                CASE(4)
                    CALL ReadArrayFromFile(arrayInt4,fileName)
                    CALL readedSignal%Constructor(  int(arrayInt4,8))
                    DEALLOCATE(arrayInt4)
                CASE(8)
                    CALL ReadArrayFromFile(arrayInt8,fileName)
                    CALL readedSignal%Constructor(  arrayInt8)
                    DEALLOCATE(arrayInt8)
                CASE DEFAULT
                    WRITE(*,*) 'Неправильно выбран тип целого для чтения'
            END SELECT
        ELSE
            WRITE(*,*) 'Есть формат!!!'
            SELECT CASE (intType)
            CASE(2)
                CALL ReadArrayFromFile(arrayInt2,fileName,fmt)
                CALL readedSignal%Constructor(  int(arrayInt2,8))
                DEALLOCATE(arrayInt2)
            CASE(4)
                CALL ReadArrayFromFile(arrayInt4,fileName,fmt)
                CALL readedSignal%Constructor(  int(arrayInt4,8))
                DEALLOCATE(arrayInt4)
            CASE(8)
                CALL ReadArrayFromFile(arrayInt8,fileName,fmt)
                CALL readedSignal%Constructor(  arrayInt8)
                DEALLOCATE(arrayInt8)
            CASE DEFAULT
                WRITE(*,*) 'Неправильно выбран тип целого для чтения'
        END SELECT

        END IF
   END SUBROUTINE ReadAnalyticSignalFromFile

   SUBROUTINE WriteAnalyticSignalToFile(writedSignal,intType,fileName)

        CLASS(analyticSignal_t), INTENT(IN)  :: writedSignal
        INTEGER(1), INTENT(IN)               :: intType
        CHARACTER(*), INTENT(IN)             :: fileName

        INTEGER(2), ALLOCATABLE              :: arrayInt2(:)
        INTEGER(4), ALLOCATABLE              :: arrayInt4(:)
        INTEGER(8), ALLOCATABLE              :: arrayInt8(:)
        INTEGER(1)                           :: status

        CALL writedSignal%ExtractSignalData(arrayInt8)

        SELECT CASE (intType)

            CASE(2)
                ALLOCATE(arrayInt2(1:size(arrayInt8)),STAT=status)
                IF (status/=0) THEN
                    WRITE(*,*) 'не могу выделить память, для записи ', fileName
                    CALL   ExitFromProgramNormal()
                END IF
                arrayInt2 = int(arrayInt8,2)
                CALL WriteArrayToFile(arrayInt2,fileName)
                DEALLOCATE(arrayInt8)
                DEALLOCATE(arrayInt2)
            CASE(4)
                ALLOCATE(arrayInt4(1:size(arrayInt8)),STAT=status)
                IF (status/=0) THEN
                    WRITE(*,*) 'не могу выделить память, для записи ', fileName
                    CALL   ExitFromProgramNormal()
                END IF
                arrayInt4 = int(arrayInt8,4)
                CALL WriteArrayToFile(arrayInt4,fileName)
                DEALLOCATE(arrayInt8)
                DEALLOCATE(arrayInt4)
            CASE(8)
                CALL WriteArrayToFile(arrayInt8,fileName)
                DEALLOCATE(arrayInt8)
            CASE DEFAULT
                WRITE(*,*) 'Неправильно выбран тип целого для записи'
           END SELECT

   END SUBROUTINE WriteAnalyticSignalToFile

   SUBROUTINE WriteTwoAnalyticSignalToStereoFile(writedSignalI,writedSignalQ,intType,fileName,isBinary)

        CLASS(analyticSignal_t), INTENT(IN)  :: writedSignalI
        CLASS(analyticSignal_t), INTENT(IN)  :: writedSignalQ
        INTEGER(1), INTENT(IN)               :: intType
        CHARACTER(*), INTENT(IN)             :: fileName
        LOGICAL, INTENT(IN)                  :: isBinary

!        INTEGER(2), ALLOCATABLE              :: arrayInt2(:)
!        INTEGER(4), ALLOCATABLE              :: arrayInt4(:)
         INTEGER(8), ALLOCATABLE              :: arrayInt8I(:)
         INTEGER(8), ALLOCATABLE              :: arrayInt8Q(:)
         INTEGER(8), ALLOCATABLE              :: arrayInt8Stereo(:)
         INTEGER(8)                           :: arraySize

        CALL writedSignalI%ExtractSignalData(arrayInt8I)
        CALL writedSignalQ%ExtractSignalData(arrayInt8Q)

        arraySize = size(arrayInt8I)
        ALLOCATE (arrayInt8Stereo(1:arraySize*2))


!        SELECT CASE (intType)
!
!            CASE(2)
!
!                CALL WriteArrayToFile(int(arrayInt8,2),fileName,isBinary)
!                DEALLOCATE(arrayInt8)
!
!            CASE(4)
!
!                CALL WriteArrayToFile(int(arrayInt8,4),fileName,isBinary)
!                DEALLOCATE(arrayInt8)
!
!            CASE(8)
!
!                CALL WriteArrayToFile(arrayInt8,fileName,isBinary)
!                DEALLOCATE(arrayInt8)
!
!            CASE DEFAULT
!                WRITE(*,*) 'Неправильно выбран тип целого для записи'
!           END SELECT

   END SUBROUTINE WriteTwoAnalyticSignalToStereoFile


END MODULE WriteReadAnalyticSignalToFromFile
