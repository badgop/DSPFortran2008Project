 MODULE complexSignalModule
    USE analyticSignalModule
    USE signumSignalModule
    USE FastModuleMod
    IMPLICIT NONE


    PRIVATE

    TYPE, PUBLIC :: complexSignal_t

        PRIVATE
        TYPE(analyticSignal_t) ::i
        TYPE(analyticSignal_t) ::q

        LOGICAL                :: isAllocated=.FALSE.
        INTEGER(8)             :: signalSize = 0

    CONTAINS
        PROCEDURE ConstructorFromArrays_int8
        PROCEDURE ConstructorFromArrays_int4
        PROCEDURE ConstructorFromArrays_int2
        PROCEDURE ConstructorFromAnalyticSignals
        ! через 2 массива и через 2 сигнала аналитических
        GENERIC :: Constructor => ConstructorFromArrays_int8,ConstructorFromArrays_int4&
                                 ,ConstructorFromArrays_int2,ConstructorFromAnalyticSignals
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
        PROCEDURE :: RShift
        PROCEDURE :: ConvolveComplexSignum
        PROCEDURE :: ConvolveComplexAnalytic
        PROCEDURE :: GetModuleFast
        PROCEDURE :: Decimate
        PROCEDURE :: ClipSignal
        PROCEDURE :: SummIQ
        PROCEDURE :: Substract
        PROCEDURE :: MakeConjugation
        generic :: operator   (.CONVSIGN.) =>  ConvolveComplexSignum
        generic :: operator   (.CONV.)     =>  ConvolveComplexAnalytic
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

    SUBROUTINE ConstructorFromArrays_int8(this,componentI,componentQ)
        CLASS(complexSignal_t), INTENT(INOUT)  :: this
        INTEGER(1),PARAMETER                   :: arrayKind=8
        INTEGER(arrayKind), DIMENSION(:), INTENT(IN)                 :: componentI
        INTEGER(arrayKind), DIMENSION(:), INTENT(IN)                 :: componentQ
        INTEGER(8) :: fileSizeI

        !что если обьект уже проинициализирован - проверить!!!


        fileSizeI=size(componentI)
        this%signalSize=fileSizeI

        CALL this%i%Constructor(componentI)
        CALL this%q%Constructor(componentQ)

        this%isAllocated=.TRUE.

    END SUBROUTINE ConstructorFromArrays_int8

    SUBROUTINE ConstructorFromArrays_int4(this,componentI,componentQ)
        CLASS(complexSignal_t), INTENT(INOUT)  :: this
        INTEGER(1),PARAMETER                   :: arrayKind=4
        INTEGER(arrayKind), DIMENSION(:), INTENT(IN)                 :: componentI
        INTEGER(arrayKind), DIMENSION(:), INTENT(IN)                 :: componentQ
        INTEGER(8) :: fileSizeI

        !что если обьект уже проинициализирован - проверить!!!


        fileSizeI=size(componentI)
        this%signalSize=fileSizeI

        CALL this%i%Constructor(componentI)
        CALL this%q%Constructor(componentQ)

        this%isAllocated=.TRUE.

    END SUBROUTINE ConstructorFromArrays_int4

        SUBROUTINE ConstructorFromArrays_int2(this,componentI,componentQ)
        CLASS(complexSignal_t), INTENT(INOUT)  :: this
        INTEGER(1),PARAMETER                   :: arrayKind=2
        INTEGER(arrayKind), DIMENSION(:), INTENT(IN)                 :: componentI
        INTEGER(arrayKind), DIMENSION(:), INTENT(IN)                 :: componentQ
        INTEGER(8) :: fileSizeI

        !что если обьект уже проинициализирован - проверить!!!


        fileSizeI=size(componentI)
        this%signalSize=fileSizeI

        CALL this%i%Constructor(componentI)
        CALL this%q%Constructor(componentQ)

        this%isAllocated=.TRUE.

    END SUBROUTINE ConstructorFromArrays_int2

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

            ! I
            WRITE(*,*) '====================== multiplay i'
            MultiplyComplexSignals%i= (xOp%i*yOp%i)  -  (xOp%q*yOp%q)
            WRITE(*,*) ' ======================max i', MultiplyComplexSignals%i%GetMaxAbs()

            WRITE(*,*) ' =====================multiplay q'
            ! Q
            MultiplyComplexSignals%q=(xOp%i*yOp%q)  +  (xOp%q*yOp%i)
             WRITE(*,*) ' ===================max q', MultiplyComplexSignals%q%GetMaxAbs()

            MultiplyComplexSignals%isAllocated=.TRUE.
     END FUNCTION MultiplyComplexSignals


    SUBROUTINE ExtractSignalData(this,extractedI,extractedq)
        INTEGER(8), DIMENSION(:),ALLOCATABLE, INTENT(INOUT) :: extractedI
        INTEGER(8), DIMENSION(:),ALLOCATABLE, INTENT(INOUT) :: extractedQ
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


    ! ВЫполняет арифметический сдвиг вправо, для выбора старших разрядов сигнала
    SUBROUTINE RShift(this,shift)

        CLASS(complexSignal_t), INTENT(INOUT)  :: this
        INTEGER(1),INTENT(IN)                :: shift

        CALL this%i%RShift(shift)
        CALL this%q%RShift(shift)


    END SUBROUTINE RShift

    FUNCTION ConvolveComplexSignum(input,reference)
       CLASS(complexSignal_t), INTENT(IN)    :: input
       CLASS(signumSignal_t) , INTENT(IN)    :: reference
       CLASS(complexSignal_t), ALLOCATABLE   :: ConvolveComplexSignum
       ALLOCATE(ConvolveComplexSignum)
        !WRITE(*,*) 'CONVSIGN'

        ConvolveComplexSignum%i= input%i.CONVSIGN.reference
        ConvolveComplexSignum%q= input%q.CONVSIGN.reference

    END FUNCTION ConvolveComplexSignum

    FUNCTION ConvolveComplexAnalytic(input,reference)
      CLASS(complexSignal_t), INTENT(IN)    :: input
      CLASS(analyticSignal_t), INTENT(IN)    :: reference
      CLASS(complexSignal_t), ALLOCATABLE   :: ConvolveComplexAnalytic
      ALLOCATE(ConvolveComplexAnalytic)

!      !$OMP parallel SECTIONS SHARED(reference) num_threads(2)
!      !$omp section
           ConvolveComplexAnalytic%i= input%i.CONV.reference
!      !$omp section
           ConvolveComplexAnalytic%q= input%q.CONV.reference
!      !$omp end parallel sections

    END FUNCTION ConvolveComplexAnalytic

    ! Возвращает МАССИВ INT8 с модулем Комплексноого числа
    FUNCTION GetModuleFast(this)
       CLASS(complexSignal_t)  , INTENT(IN)    :: this
       INTEGER(8), DIMENSION(:), ALLOCATABLE   :: GetModuleFast
       INTEGER(8), DIMENSION(:),ALLOCATABLE                  :: extractedI
       INTEGER(8), DIMENSION(:),ALLOCATABLE                  :: extractedQ


       CALL this%i%ExtractSignalData(extractedI)
       CALL this%q%ExtractSignalData(extractedQ)
       GetModuleFast = GetFastMouleFromComplexInt8(extractedI,extractedQ)
       DEALLOCATE (extractedI)
       DEALLOCATE (extractedQ)

    END FUNCTION GetModuleFast

        ! Возвращает МАССИВ INT8 с модулем Комплексноого числа
    FUNCTION Decimate(this,r)
       CLASS(complexSignal_t)  , INTENT(IN)    :: this
       INTEGER(8)              , INTENT(IN)    :: r
       CLASS(complexSignal_t)  , ALLOCATABLE   :: Decimate
       ALLOCATE(Decimate)
       Decimate%i=(this%i%Decimate(r))
       Decimate%q=(this%q%Decimate(r))
    END FUNCTION Decimate

    FUNCTION ClipSignal (this, level,outLevel)
        CLASS(complexSignal_t), INTENT(IN)  :: this
        INTEGER(2)             , INTENT(IN)  :: level
        INTEGER(2)             , INTENT(IN)  :: outLevel
        CLASS(complexSignal_t), ALLOCATABLE :: ClipSignal

        ALLOCATE(ClipSignal)
        ClipSignal%i=this%i%ClipSignal(level,outLevel)
        ClipSignal%q=this%q%ClipSignal(level,outLevel)
    END  FUNCTION ClipSignal

    FUNCTION SummIQ (this) RESULT(output)
        CLASS(complexSignal_t), INTENT(IN)  :: this
        TYPE(analyticSignal_t), ALLOCATABLE::  output

        ALLOCATE(output)
        output = this%i+this%q
    END  FUNCTION SummIQ

    FUNCTION Substract (this) RESULT(output)
        CLASS(complexSignal_t), INTENT(IN)  :: this
        TYPE(analyticSignal_t), ALLOCATABLE::  output

        ALLOCATE(output)
        output = this%i-this%q
    END  FUNCTION Substract

    FUNCTION MakeConjugation (this) RESULT(output)
        CLASS(complexSignal_t), INTENT(IN)  :: this
        TYPE(complexSignal_t), ALLOCATABLE::  output

        ALLOCATE(output)
        output = this
        WRITE(*,*) '  MakeConjugation   '
        output%q = output%q*(-int(1,8))
    END  FUNCTION MakeConjugation


    SUBROUTINE destructor(this)
        TYPE(complexSignal_t), INTENT(INOUT) :: this
        !**деструкторы комноментов вызывают сами
!        DEALLOCATE(this%i%signal)
!        DEALLOCATE(this%q%signal)
        this%isAllocated=.FALSE.
    END SUBROUTINE

END MODULE complexSignalModule



