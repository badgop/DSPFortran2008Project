MODULE ModuleExitProg

    IMPLICIT NONE

    !==========================================================
    !==== Модуль для обработки методов выхода из программы=====
    ! Авторы:
    ! Татарчук И.А
    !==========================================================


    CONTAINS


    !==========================================================
    !======== Штатный - нормальный выход из программмы  =======
    !==========================================================
    SUBROUTINE ExitFromProgramNormal()


        WRITE(*,*) 'Выхожу из программы....'


        !===== Платформозависимый код
        !===== заменить при необходимости
        CALL EXIT(0)


    END SUBROUTINE

END MODULE ModuleExitProg
