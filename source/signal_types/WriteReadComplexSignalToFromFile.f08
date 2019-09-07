MODULE WriteReadComplexSignalToFromFile
    USE ComplexSignalModule
    USE complexSignalModule
    USE ModuleWriteReadArrayFromToFile
    IMPLICIT NONE




    !======================================================
    !===== Модуль записи и чтения МАССИВА в/из файла ======
    !======================================================
    !             Перчень обобщенных функций в модуле:
    ! ReadComplexSignalFromFilе - чтение массива signal(:) обьекта типа ComplexSignal_t из файла
    ! WriteComplexSignalToFile- запись массива signal(:) обьекта типа ComplexSignal_t   в файл
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
    ! x - обьект типа ComplexSignal_t, с не инициализированным членом  INTEGER(8) signal(:)
    !======================================================
    !
    !  МАКС размер файла  - ‭9 223 372 036 854 775 807‬ байт
    !
    !============================================================

   CONTAINS

   SUBROUTINE ReadComplexSignalFromFile(readedSignal,intType,fileNameI,fileNameQ,isBinary)

        CLASS(ComplexSignal_t), INTENT(INOUT)  :: readedSignal
        INTEGER(1), INTENT(IN)                 :: intType
        CHARACTER(*), INTENT(IN)               :: fileNameI
        CHARACTER(*), INTENT(IN)               :: fileNameQ
        LOGICAL, INTENT(IN)                    :: isBinary

        INTEGER(2), ALLOCATABLE              :: arrayInt2I(:)
        INTEGER(2), ALLOCATABLE              :: arrayInt2Q(:)
        INTEGER(4), ALLOCATABLE              :: arrayInt4I(:)
        INTEGER(4), ALLOCATABLE              :: arrayInt4Q(:)
        INTEGER(8), ALLOCATABLE              :: arrayInt8I(:)
        INTEGER(8), ALLOCATABLE              :: arrayInt8Q(:)

        SELECT CASE (intType)

            CASE(2)
                CALL ReadArrayFromFile(arrayInt2I,fileNameI,isBinary)
                CALL ReadArrayFromFile(arrayInt2Q,fileNameQ,isBinary)
                CALL readedSignal%Constructor(  int(arrayInt2I,8),int(arrayInt2Q,8)  )
                DEALLOCATE(arrayInt2I)
                DEALLOCATE(arrayInt2Q)

            CASE(4)
                CALL ReadArrayFromFile(arrayInt4I,fileNameI,isBinary)
                CALL ReadArrayFromFile(arrayInt4Q,fileNameQ,isBinary)
                CALL readedSignal%Constructor(  int(arrayInt4I,8),int(arrayInt4Q,8)  )
                DEALLOCATE(arrayInt4I)
                DEALLOCATE(arrayInt4Q)

            CASE(8)
                CALL ReadArrayFromFile(arrayInt8I,fileNameI,isBinary)
                CALL ReadArrayFromFile(arrayInt8Q,fileNameQ,isBinary)
                CALL readedSignal%Constructor(arrayInt8I,arrayInt8Q)
                DEALLOCATE(arrayInt8I)
                DEALLOCATE(arrayInt8Q)

            CASE DEFAULT
                WRITE(*,*) 'Неправильно выбран тип целого для чтения'

        END SELECT


   END SUBROUTINE ReadComplexSignalFromFile

   SUBROUTINE WriteComplexSignalToFile(writedSignal,intType,fileNameI,fileNameQ,isBinary)

        CLASS(ComplexSignal_t), INTENT(IN)   :: writedSignal
        INTEGER(1), INTENT(IN)               :: intType
        CHARACTER(*), INTENT(IN)             :: fileNameI
        CHARACTER(*), INTENT(IN)             :: fileNameQ
        LOGICAL, INTENT(IN)                  :: isBinary

!        INTEGER(2), ALLOCATABLE              :: arrayInt2I(:)
!        INTEGER(2), ALLOCATABLE              :: arrayInt2Q(:)
!        INTEGER(4), ALLOCATABLE              :: arrayInt4I(:)
!        INTEGER(4), ALLOCATABLE              :: arrayInt4Q(:)
        INTEGER(8), ALLOCATABLE              :: arrayInt8I(:)
        INTEGER(8), ALLOCATABLE              :: arrayInt8Q(:)


        CALL writedSignal%ExtractSignalData(arrayInt8I,arrayInt8Q)

        SELECT CASE (intType)

            CASE(2)


                CALL WriteArrayToFile(int(arrayInt8I,2),fileNameI,isBinary)
                CALL WriteArrayToFile(int(arrayInt8Q,2),fileNameQ,isBinary)

                DEALLOCATE(arrayInt8I)
                DEALLOCATE(arrayInt8Q)

            CASE(4)
                CALL WriteArrayToFile(int(arrayInt8I,4),fileNameI,isBinary)
                CALL WriteArrayToFile(int(arrayInt8Q,4),fileNameQ,isBinary)

                DEALLOCATE(arrayInt8I)
                DEALLOCATE(arrayInt8Q)

            CASE(8)
                CALL WriteArrayToFile(arrayInt8I,fileNameI,isBinary)
                CALL WriteArrayToFile(arrayInt8Q,fileNameQ,isBinary)

                DEALLOCATE(arrayInt8I)
                DEALLOCATE(arrayInt8Q)

            CASE DEFAULT
                WRITE(*,*) 'Неправильно выбран тип целого для записи'
           END SELECT

   END SUBROUTINE WriteComplexSignalToFile

END MODULE WriteReadComplexSignalToFromFile