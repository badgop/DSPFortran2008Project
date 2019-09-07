 MODULE complexSignalModule
    USE analyticSignalModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: complexSignal_t

        PRIVATE
        TYPE(analyticSignal_t) ::i
        TYPE(analyticSignal_t) ::q

        LOGICAL                :: isAllocated=.FALSE.
        INTEGER(8)             :: signalSize = 0

    CONTAINS
        PROCEDURE ConstructorFromArrays
        PROCEDURE ConstructorFromAnalyticSignals
        ! через 2 массива и через 2 сигнала аналитических
        GENERIC :: Constructor => ConstructorFromArrays,ConstructorFromAnalyticSignals
        PROCEDURE AssignDataComplex
        generic :: assignment(=) => AssignDataComplex

!https://stackoverflow.com/questions/19064132/nested-derived-type-with-overloaded-assignment
!https://stackoverflow.com/questions/19111471/fortran-derived-type-assignment
        PROCEDURE :: MultiplyComplexSignals
        generic :: operator(*) =>  MultiplyComplexSignals
        PROCEDURE ExtractSignalData
        PROCEDURE GetAllocationStatus
        PROCEDURE GetSignalSize
        PROCEDURE SetName
        FINAL :: destructor

    END TYPE complexSignal_t

!    interface assignment(=)
!        module procedure AssignDataComplex
!    end interface assignment(=)

CONTAINS

    SUBROUTINE AssignDataComplex(leftOp,rightOp)
        CLASS(complexSignal_t), INTENT(INOUT)  :: leftOp
        CLASS(complexSignal_t), INTENT(IN)                :: rightOp

        ! ЗАЩИТА

        leftOp%i=rightOp%i
        leftOp%q=rightOp%q

        leftOp%isAllocated=.TRUE.
        leftOp%signalSize=leftOp%i%GetSignalSize()


    END SUBROUTINE AssignDataComplex

    SUBROUTINE ConstructorFromArrays(this,componentI,componentQ)
        CLASS(complexSignal_t), INTENT(INOUT)  :: this
        INTEGER(8), INTENT(IN)                 :: componentI(:)
        INTEGER(8), INTENT(IN)                 :: componentQ(:)
        INTEGER(8) :: fileSizeI

        !что если обьект уже проинициализирован - проверить!!!


        fileSizeI=size(componentI)
        this%signalSize=fileSizeI

        CALL this%i%Constructor(componentI)
        CALL this%q%Constructor(componentQ)

        this%isAllocated=.TRUE.

    END SUBROUTINE ConstructorFromArrays

     SUBROUTINE ConstructorFromAnalyticSignals(this,iSig_t,qSig_t)
        USE analyticSignalModule

        CLASS(complexSignal_t), INTENT(INOUT)  :: this
        CLASS(analyticSignal_t), INTENT(IN)  :: iSig_t
        CLASS(analyticSignal_t), INTENT(IN)  :: qSig_t

        !что если обьект уже проинициализирован - проверить!!!

        this%i=iSig_t
        this%q=qSig_t

        this%isAllocated=.TRUE.
        this%signalSize=this%i%GetSignalSize()

    END SUBROUTINE ConstructorFromAnalyticSignals

    FUNCTION MultiplyComplexSignals(xOp,yOp)
         CLASS(complexSignal_t), INTENT(IN)  :: xOp
         CLASS(complexSignal_t), INTENT(IN)  :: yOp
         CLASS(complexSignal_t), allocatable ::MultiplyComplexSignals


            !r%signal=xOp%signal*yOp%signal
            allocate( MultiplyComplexSignals)

            ! Вот тут конструктор копирования(=) не отрабаывает - не может взять размер и посавить статус выделения - почему??
            ! Хотя деструктор вызывается спокойно с нужными парамерами
            ! требуется расследование
            !MultiplyComplexSignals%Signal=xOp%signal*yOp%signal
            CALL  MultiplyComplexSignals%SetName('комплпекс умн И','комплпекс умн Ку')
            MultiplyComplexSignals%i=(xOp%i*yOp%i)-(xOp%q*yOp%q)
            MultiplyComplexSignals%q=(xOp%i*yOp%q)+(yOp%i*xOp%q)

            MultiplyComplexSignals%isAllocated=.TRUE.



     END FUNCTION MultiplyComplexSignals


    SUBROUTINE ExtractSignalData(this,extractedI,extractedq)

        INTEGER(8),ALLOCATABLE, INTENT(INOUT) :: extractedI(:)
        INTEGER(8),ALLOCATABLE, INTENT(INOUT) :: extractedQ(:)
        CLASS(complexSignal_t), INTENT(IN)  :: this


        CALL this%i%ExtractSignalData(extractedI)
        CALL this%q%ExtractSignalData(extractedQ)

     END SUBROUTINE ExtractSignalData

     FUNCTION GetAllocationStatus(this) RESULT(stat)
        CLASS(complexSignal_t), INTENT(IN)  :: this
        LOGICAL :: stat

        stat = this%isAllocated
     END FUNCTION GetAllocationStatus

      FUNCTION GetSignalSize(this) RESULT( signalSize)
        CLASS(complexSignal_t), INTENT(IN)  :: this
        INTEGER(8) :: signalSize

         signalSize = this%signalSize
     END FUNCTION GetSignalSize

       SUBROUTINE SetName(this,signalNameI,signalNameQ)

            CLASS(complexSignal_t), INTENT(INOUT)  :: this
            CHARACTER(*),INTENT(IN)          :: signalNameI
            CHARACTER(*),INTENT(IN)          :: signalNameQ

            CALL this%i%SetName(signalNameI)
            CALL this%q%SetName(signalNameQ)

     END SUBROUTINE SetName




    SUBROUTINE destructor(this)
        TYPE(complexSignal_t), INTENT(INOUT) :: this

        !**деструкторы комноментов вызывают сами

!        DEALLOCATE(this%i%signal)
!        DEALLOCATE(this%q%signal)
        this%isAllocated=.FALSE.

    END SUBROUTINE

END MODULE complexSignalModule



