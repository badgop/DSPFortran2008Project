MODULE PhaseDetectorModule
    USE analyticSignalModule
    USE DDSModule
    USE MathConstModule
    USE complexSignalModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: PhaseDetector_t
        PRIVATE
        INTEGER(8)                   :: centralFrequency
        INTEGER(8)                   :: sampleRate
        REAL(8)                      :: initialPhase = 0
        TYPE(DDS_t)                  :: iMixer
        TYPE(DDS_t)                  :: qMixer
        TYPE(analyticSignal_t)       :: lpf
        INTEGER(8)                   :: outputShift
    CONTAINS
        PROCEDURE :: Constructor
        PROCEDURE :: Downconvert

        FINAL :: destructor
    END TYPE PhaseDetector_t

CONTAINS

    SUBROUTINE Constructor(this,centralFrequency,initialPhase,sampleRate,impluseResponse,outputShift)
        CLASS(PhaseDetector_t), INTENT(INOUT) :: this
        INTEGER(8)            , INTENT(IN)    :: centralFrequency
        INTEGER(8)            , INTENT(IN)    :: sampleRate
        REAL(8)               , INTENT(IN)    :: initialPhase
        INTEGER(8)            , INTENT(IN)    :: impluseResponse(:)
        INTEGER(8)            , INTENT(IN)    :: outputShift
        INTEGER(8)                             :: stat


        this%centralFrequency = centralFrequency
        this%initialPhase     = initialPhase
        this%sampleRate       = sampleRate
        this%outputShift      = outputShift
        CALL this%lpf%Constructor(impluseResponse)

          ! настройка гетеродинов
        stat =  this%iMixer%Constructor(romLengthInBits = int(32,1)&
                                     , romLengthTruncedInBits =int(14,1) &
                                     , samplingFrequency =int(this%SampleRate,4) &
                                     , outputSignalSampleCapacity=int(8,1))
        CALL this%iMixer%SetPhase(PI/2+this%initialPhase)

        stat =  this%qMixer%Constructor(romLengthInBits = int(32,1)&
                                     , romLengthTruncedInBits =int(14,1) &
                                     , samplingFrequency =int(this%SampleRate,4) &
                                     , outputSignalSampleCapacity=int(8,1))
       CALL this%qMixer%SetPhase(0+this%initialPhase)

!        WRITE(*,*) 'PhaseDemod constructor works!!!!'
    END SUBROUTINE Constructor

    FUNCTION Downconvert(this, inputSignal)
        CLASS(PhaseDetector_t) , INTENT(INOUT) :: this
        CLASS(analyticSignal_t), INTENT(IN)    :: inputSignal
        CLASS(complexSignal_t) , allocatable   :: Downconvert
        CLASS(analyticSignal_t), allocatable   :: iSignal
        CLASS(analyticSignal_t), allocatable   :: qSignal
        CLASS(analyticSignal_t), allocatable   :: iHeterodyne
        CLASS(analyticSignal_t), allocatable   :: qHeterodyne

        CLASS(analyticSignal_t), allocatable   :: iSignal2
        CLASS(analyticSignal_t), allocatable   :: qSignal2

        ALLOCATE(Downconvert)
        ALLOCATE(iSignal)
        ALLOCATE(qSignal)
        ALLOCATE(iHeterodyne)
        ALLOCATE(qHeterodyne)
!
!         CALL this%iMixer%SetPhase(PI/2+this%initialPhase)
!         CALL this%qMixer%SetPhase(0+this%initialPhase)

        CALL this%iMixer%ComputeOutput(int(this%centralFrequency,8),(inputSignal%GetSignalSize()),iHeterodyne)
        CALL this%qMixer%ComputeOutput(int(this%centralFrequency,8),(inputSignal%GetSignalSize()),qHeterodyne)
        ! формирование квадратурных каналов
      !  !$omp parallel
     !     !$omp  single
     !  !$omp task
        iSignal = inputSignal*iHeterodyne

       ! !$omp end task

      !   !$omp task
        !  -sin(wt)
        qHeterodyne=qHeterodyne*(int(-1,8))

        qSignal = inputSignal*qHeterodyne
! !!$omp end task
! ! !$omp end single
!      !   !$omp END parallel
        DEALLOCATE(iHeterodyne)
        DEALLOCATE(qHeterodyne)

        !WRITE(*,*) 'FILTER BITCH!'
        ! Фильтрация каналов
!        !$omp parallel
!          !$omp  single
!       !$omp task

        iSignal = iSignal.CONV.this%lpf

!         !$omp end task
!          !$omp task

        qSignal = qSignal.CONV.this%lpf

!       !$omp end task
!       !$omp end single
!         !$omp END parallel
        ! Берем старшие разряды
              !  WRITE(*,*) 'FILTER BITCH END!'

        CALL iSignal%Rshift(int(this%outputShift,1))

        CALL qSignal%Rshift(int(this%outputShift,1))

      !  WRITE(*,*) 'фаз дем длина' , iSignal%GetSignalSize()

        ! формируеым выходной комплексный сигнал
        CALL Downconvert%Constructor(iSignal,qSignal)
        DEALLOCATE(iSignal)
        DEALLOCATE(qSignal)


    END FUNCTION Downconvert
    


    SUBROUTINE destructor(this)
        TYPE(PhaseDetector_t), INTENT(INOUT) :: this
    END SUBROUTINE

END MODULE PhaseDetectorModule
