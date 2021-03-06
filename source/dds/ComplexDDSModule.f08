MODULE ComplexDDSModule

    USE analyticSignalModule
    USE complexSignalModule
    USE MathConstModule
    USE DDSModule
    IMPLICIT NONE
    PRIVATE

    TYPE, PUBLIC :: complexDDS_t
        PRIVATE

        TYPE(DDS_t) ::ddsGeneratorI
        TYPE(DDS_t) ::ddsGeneratorQ

        REAL(8)     :: phaseShiftI
        REAL(8)     :: phaseShiftQ
        LOGICAL     :: jSign=.FALSE.




    CONTAINS

        PROCEDURE :: Constructor
        PROCEDURE :: ComputeOutput
        PROCEDURE :: ComputeOutputFromScalar
        GENERIC   :: ComputeOutputComplex =>  ComputeOutput,ComputeOutputFromScalar

        FINAL :: destructor
    END TYPE complexDDS_t

CONTAINS

    SUBROUTINE Constructor(this,romLengthInBits, romLengthTruncedInBits, samplingFrequency, outputSignalSampleCapacity)
       CLASS(complexDDS_t), INTENT(INOUT) :: this

       INTEGER(1), INTENT(IN)    :: romLengthInBits
       INTEGER(1), INTENT(IN)    :: romLengthTruncedInBits
       INTEGER(4), INTENT(IN)    :: samplingFrequency
       INTEGER(1), INTENT(IN)    :: outputSignalSampleCapacity


       INTEGER(1) :: status


       status= this%ddsGeneratorI%Constructor(romLengthInBits,romLengthTruncedInBits,&
                                             samplingFrequency,outputSignalSampleCapacity)

       status= this%ddsGeneratorQ%Constructor(romLengthInBits,romLengthTruncedInBits,&
                                             samplingFrequency,outputSignalSampleCapacity)


       CALL  this%ddsGeneratorI%SetPhase(PI*0.5)
       CALL  this%ddsGeneratorQ%SetPhase(PI*0)


    END SUBROUTINE
    
    SUBROUTINE ComputeOutput(this,inputSignal,outputSignalComplex)
        CLASS(complexDDS_t),     INTENT(INOUT)  :: this
        CLASS(analyticSignal_t), INTENT(IN)     :: inputSignal
        CLASS(ComplexSignal_t),  INTENT(INOUT)  :: outputSignalComplex

        type(analyticSignal_t)     :: out_i
        type(analyticSignal_t)     :: out_q

        CALL this%ddsGeneratorI%ComputeOutput(inputSignal, out_i)
        CALL this%ddsGeneratorQ%ComputeOutput(inputSignal, out_q)

        CALL outputSignalComplex%Constructor(out_i,out_q)

    END SUBROUTINE ComputeOutput

    SUBROUTINE ComputeOutputFromScalar(this,frequency, signalLength, outputSignalComplex)
        CLASS(complexDDS_t),     INTENT(INOUT)  :: this
        INTEGER(8), INTENT(IN)                  :: frequency
        INTEGER(8), INTENT(IN) :: signalLength
        CLASS(ComplexSignal_t),  INTENT(INOUT)  :: outputSignalComplex

        type(analyticSignal_t)     :: out_i
        type(analyticSignal_t)     :: out_q

        CALL this%ddsGeneratorI%ComputeOutput(frequency,signalLength, out_i)
        CALL this%ddsGeneratorQ%ComputeOutput(frequency,signalLength, out_q)

        CALL outputSignalComplex%Constructor(out_i,out_q)

    END SUBROUTINE ComputeOutputFromScalar

    SUBROUTINE destructor(this)
        TYPE(complexDDS_t), INTENT(INOUT) :: this
    END SUBROUTINE

END MODULE ComplexDDSModule
