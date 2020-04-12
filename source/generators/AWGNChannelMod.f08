MODULE AWGNChannelModMod
    USE analyticSignalModule
    USE complexSignalModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: AWGNChannel_t
        PRIVATE
        INTEGER(2),ALLOCATABLE :: noiseArray(:)
    CONTAINS
        PROCEDURE :: MakeNoise
        PROCEDURE :: LoadNoise
        FINAL     :: destructor
    END TYPE AWGNChannel_t

CONTAINS

   FUNCTION MakeNoise(this,inputSignal,impulseResponse)
        class(AWGNChannel_t), intent(in) :: this
        CLASS(analyticSignal_t), INTENT(IN)  :: inputSignal
        CLASS(analyticSignal_t), INTENT(IN)  :: impulseResponse
        CLASS (analyticSignal_t),ALLOCATABLE              :: MakeNoise

    END  FUNCTION MakeNoise

    SUBROUTINE LoadNoise(this,inputSignal)
        class(AWGNChannel_t), intent(in) :: this
        CLASS(analyticSignal_t), INTENT(IN)  :: inputSignal

    END SUBROUTINE LoadNoise
    
    FUNCTION AddNoiseAnalytic(this,inputSignal,SNRneed,outCapacity)
        class(AWGNChannel_t)    , intent(in)          :: this
        CLASS(analyticSignal_t) , INTENT(IN)          :: inputSignal
        REAL(8)                 , INTENT(IN)          :: SNRneed
        INTEGER(1)              , INTENT(IN)          :: outCapacity
        CLASS (analyticSignal_t),ALLOCATABLE          :: AddNoiseAnalytic

    END  FUNCTION AddNoiseAnalytic

      FUNCTION AddNoiseComplex(this,inputSignal,SNRneed,outCapacity)
        class(AWGNChannel_t)    , intent(in)          :: this
        CLASS(analyticSignal_t) , INTENT(IN)          :: inputSignal
        REAL(8)                 , INTENT(IN)          :: SNRneed
        INTEGER(1)              , INTENT(IN)          :: outCapacity
        CLASS (complexSignal_t) , ALLOCATABLE         :: AddNoiseComplex

    END  FUNCTION AddNoiseComplex




    SUBROUTINE destructor(this)
        type(AWGNChannel_t), intent(in) :: this
    END SUBROUTINE

END MODULE AWGNChannelModMod
