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
    ! name_x - имя файла из которого происходит чтение данных
    !======================================================
    !                   Выходные парметры:
    ! x - обьект типа ComplexSignal_t, с не инициализированным членом  INTEGER(8) signal(:)
    !======================================================
    !
    !  МАКС размер файла  - ‭9 223 372 036 854 775 807‬ байт
    !
    !============================================================

   CONTAINS

!   SUBROUTINE ReadComplexSignalFromFile(readedSignal,intType,fileName)
!
!        CLASS(ComplexSignal_t), INTENT(INOUT)  :: readedSignal
!        INTEGER(1), INTENT(IN)               :: intType
!        CHARACTER(*), INTENT(IN)             :: fileName
!
!        INTEGER(2), ALLOCATABLE              :: arrayInt2(:)
!        INTEGER(4), ALLOCATABLE              :: arrayInt4(:)
!        INTEGER(8), ALLOCATABLE              :: arrayInt8(:)
!
!        SELECT CASE (intType)
!
!            CASE(2)
!                CALL ReadArrayFromFile(arrayInt2,fileName)
!                CALL readedSignal%Constructor(  int(arrayInt2,8))
!                DEALLOCATE(arrayInt2)
!
!            CASE(4)
!                CALL ReadArrayFromFile(arrayInt4,fileName)
!                CALL readedSignal%Constructor(  int(arrayInt4,8))
!                DEALLOCATE(arrayInt4)
!
!            CASE(8)
!                CALL ReadArrayFromFile(arrayInt8,fileName)
!                CALL readedSignal%Constructor(  arrayInt8)
!                DEALLOCATE(arrayInt8)
!
!            CASE DEFAULT
!                WRITE(*,*) 'Неправильно выбран тип целого для чтения'
!
!        END SELECT
!
!
!   END SUBROUTINE ReadComplexSignalFromFile
!
!   SUBROUTINE WriteComplexSignalToFile(writedSignal,intType,fileName)
!
!        CLASS(ComplexSignal_t), INTENT(IN)  :: writedSignal
!        INTEGER(1), INTENT(IN)               :: intType
!        CHARACTER(*), INTENT(IN)             :: fileName
!
!!        INTEGER(2), ALLOCATABLE              :: arrayInt2(:)
!!        INTEGER(4), ALLOCATABLE              :: arrayInt4(:)
!        INTEGER(8), ALLOCATABLE              :: arrayInt8(:)
!
!        SELECT CASE (intType)
!
!            CASE(2)
!                CALL writedSignal%ExtractSignalData(arrayInt8)
!                CALL WriteArrayToFile(int(arrayInt8,2),fileName)
!                DEALLOCATE(arrayInt8)
!
!            CASE(4)
!                CALL writedSignal%ExtractSignalData(arrayInt8)
!                CALL WriteArrayToFile(int(arrayInt8,4),fileName)
!                DEALLOCATE(arrayInt8)
!
!            CASE(8)
!                CALL writedSignal%ExtractSignalData(arrayInt8)
!                CALL WriteArrayToFile(arrayInt8,fileName)
!                DEALLOCATE(arrayInt8)
!
!            CASE DEFAULT
!                WRITE(*,*) 'Неправильно выбран тип целого для чтения'
!           END SELECT
!
!   END SUBROUTINE WriteComplexSignalToFile

END MODULE WriteReadComplexSignalToFromFile
