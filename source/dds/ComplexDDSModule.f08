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

        REAL(8)     :: phaseShiftI=PI/2.0
        REAL(8)     :: phaseShiftQ=0.0
        LOGICAL     :: jSign=.FALSE.




    CONTAINS

        PROCEDURE :: Constructor
        PROCEDURE :: ComputeOutput

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

    SUBROUTINE destructor(this)
        TYPE(complexDDS_t), INTENT(INOUT) :: this

    END SUBROUTINE

END MODULE ComplexDDSModule
