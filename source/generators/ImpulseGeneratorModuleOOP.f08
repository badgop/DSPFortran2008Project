MODULE ImpulseGeneratorModuleOOP
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: impulseGeretator_t
        PRIVATE
        ! число отсчетов на импульс
        INTEGER(2) :: overSampleRate
        ! счетчик отсчетов
        INTEGER(2) :: discreteCounter = 0
        INTEGER(1) :: currentBit      = 0
        LOGICAL    :: isDataReady     = .True.

    CONTAINS
        PROCEDURE :: GetOutputSample
        PROCEDURE :: SetOverSampleRate
        PROCEDURE :: GetReadyForData
        PROCEDURE :: PutData


        FINAL :: destructor
    END TYPE impulseGeretator_t

CONTAINS

     FUNCTION GetOutputSample(this) RESULT(outPutSample)
        CLASS(impulseGeretator_t), INTENT(INOUT) :: this
        INTEGER(1)                            :: outPutSample

        IF (this%currentBit == 0) outPutSample    = -1
        IF (this%currentBit == 1) outPutSample    =  1
        this%discreteCounter = this%discreteCounter + 1
        IF (this%discreteCounter==(this%overSampleRate)) THEN
           this%discreteCounter = 0
           this%isDataReady = .True.
        END IF
    END  FUNCTION GetOutputSample

    SUBROUTINE SetOverSampleRate(this,overSampleRate)
        CLASS(impulseGeretator_t), INTENT(INOUT) :: this
        INTEGER(2),INTENT(IN)                 :: overSampleRate
        this%overSampleRate = overSampleRate
    END SUBROUTINE
    
    FUNCTION GetReadyForData(this) RESULT(state)
        CLASS(impulseGeretator_t), INTENT(IN) :: this
        LOGICAL                                  :: state
        state = this%isDataReady
    END FUNCTION

     SUBROUTINE PutData(this,dataSymbol)
        CLASS(impulseGeretator_t), INTENT(INOUT) :: this
        INTEGER(1),INTENT(IN)                    :: dataSymbol
        IF(this%isDataReady) THEN
            this%currentBit  = dataSymbol
            this%isDataReady = .FALSE.
        ELSE
            WRITE(*,*) 'Генератор не готов принимать данные'
        END IF
    END SUBROUTINE

    SUBROUTINE destructor(this)
        TYPE(impulseGeretator_t), INTENT(INOUT) :: this
    END SUBROUTINE

END MODULE ImpulseGeneratorModuleOOP
