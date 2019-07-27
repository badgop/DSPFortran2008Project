    !======================================================
    !===== Модуль абстрактного класса - типового элемента устройства ЦОС
    !======
    !======    ВСЕ ОСТАЛЬНЫЕ КЛАССЫ создавать путем  НАСЛЕДОВАНИЯ  ОТ НЕГО
    !======================================================
    !             Перечень обобщенных функций в модуле:
    !
    ! Авторы:
    ! Татарчук И.А
    !======================================================

MODULE BaseBlockModule

    IMPLICIT NONE


    PRIVATE
    !************************  _t - метка того, что это Derived Type - произвольный тип
    TYPE, PUBLIC,  ABSTRACT :: baseBlock_t

    integer(1):: x


    CONTAINS

        PROCEDURE(WorkOut), DEFERRED  :: ComputeOutput

    END TYPE baseBlock_t



    ABSTRACT INTERFACE

    subroutine WorkOut(this, inputSignal, outputSignal)

        import baseBlock_t
        class(baseBlock_t), intent(inout) :: this

        INTEGER(8), INTENT(in)  :: inputSignal (:)
        INTEGER(8), INTENT(OUT),ALLOCATABLE :: outputSignal(:)

    end subroutine WorkOut

    END INTERFACE

end module BaseBlockModule
