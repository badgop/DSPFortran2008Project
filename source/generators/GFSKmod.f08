MODULE GFSKmod
    USE GaussFilter
    USE PSNSimpleMod
    USE impulseGeneratorModule
    USE complexSignalModule
    USE DiffDataModulatorMod
    USE analyticSignalModule
    USE DDSModule
    USE MathConstModule


    IMPLICIT NONE
    PRIVATE


    TYPE, PUBLIC :: GFSKmodulator_t
        PRIVATE
        INTEGER(8)                   :: baudRateInSamples
        INTEGER(4)                   :: SampleRate
        INTEGER(8)                   :: centralFrequency
        INTEGER(1)                   :: outPutDDSCapacity
        INTEGER(1)                   :: outputFilterShift
        REAL   (8)                   :: mIndex
        REAL   (8)                   :: deviation
        INTEGER(8)                   :: deviationKoeff
        integer(1)                   :: capacityFilter
        TYPE(analyticSignal_t)       :: impluseResponse
        TYPE(DiffCodeGenerator_t)    :: coder

        TYPE(DDS_t)                  :: mixerI
        TYPE(DDS_t)                  :: mixerQ
    CONTAINS
        PROCEDURE :: Constructor
        PROCEDURE :: Generate
        PROCEDURE :: GenerateDiffData
        FINAL     :: destructor
    END TYPE GFSKmodulator_t

CONTAINS


SUBROUTINE Constructor(this,baudRate,mIndex,bt,sampleRate,centralFrequency&
                           ,fir_order,outPutDDSCapacity, capacityFilter&
                           ,outputFilterShift,romLengthTruncedInBits )
        class(GFSKmodulator_t), intent(inout) :: this
        INTEGER(8)  , intent(in)           :: baudRate
        INTEGER(4)  , intent(in)           :: SampleRate
        INTEGER(4)  , intent(in)           :: centralFrequency
        integer(1)  ,INTENT(IN)            :: fir_order
        INTEGER(1)  , intent(in)           :: outPutDDSCapacity
        integer(1) ,INTENT(IN)             :: capacityFilter
        INTEGER(1)  , intent(in)           :: outputFilterShift
        integer(1) ,INTENT(IN)             :: romLengthTruncedInBits
        REAL(8)     , intent(in)           :: mIndex
        real(4)    ,INTENT(IN)             :: bt
        INTEGER(8)                         :: stat

        integer(8) , ALLOCATABLE           :: IR_GAUSS(:)
        REAL(8)                            :: symbolPeriod
        REAL(8)                            :: devKoeff
        INTEGER(8)                         :: devKoeffInt8
        REAL(8)                            :: delta


        this%baudRateInSamples              = baudRate
        this%centralFrequency               = centralFrequency
        this%SampleRate                     = SampleRate
        this%outPutDDSCapacity              = outPutDDSCapacity
        this%outputFilterShift              = outputFilterShift
        this%mindex                         = mIndex
        this%capacityFilter                 = capacityFilter


        CALL this%coder%Constructor(int(1,8))

        stat = this%mixerI%Constructor (romLengthInBits = int(32,1)&
                                     , romLengthTruncedInBits =romLengthTruncedInBits &
                                     , samplingFrequency =int(this%SampleRate,4) &
                                     , outputSignalSampleCapacity=this%outPutDDSCapacity)
        stat = this%mixerQ%Constructor (romLengthInBits = int(32,1)&
                                     , romLengthTruncedInBits =romLengthTruncedInBits &
                                     , samplingFrequency =int(this%SampleRate,4) &
                                     , outputSignalSampleCapacity=this%outPutDDSCapacity)

       symbolPeriod = 1.0 / real(baudRate,8)
       WRITE(*,*)  symbolPeriod

       CALL  IR_GAUSS_CALCULATE_INT_2(sampleRate = this%SampleRate&
                 ,bt = bt&
                 ,symbolPeriod = symbolPeriod&
                 ,fir_order = fir_order&
                 ,capacity = this%capacityFilter&
                 ,IR_GAUSS_int = IR_GAUSS)
       WRITE(*,*)  'calculated ', sum(IR_GAUSS)
       CALL this%impluseResponse%Constructor(IR_GAUSS)
       WRITE(*,'(I6)')  IR_GAUSS


       this%deviation = (this%mIndex*real(this%baudRateInSamples))/2.0
       WRITE(*,*) 'this%deviation ',     this%deviation
       WRITE(*,*) 'deviation code ', this%mixerI%GetFreqCode(this%deviation)

       devKoeff = real(this%mixerI%GetFreqCode(this%deviation),8)/real(sum(IR_GAUSS),8)
       WRITE(*,*) 'devKoeff ', devKoeff
       WRITE(*,*) 'прибавляеем 1 к коэфф умножения девиации'
       devKoeffInt8 = int(devKoeff,8)+1

       WRITE(*,*) 'devKoeffInt8*sum(IR_GAUSS) ',devKoeffInt8*sum(IR_GAUSS)

       delta = real((devKoeffInt8*sum(IR_GAUSS) -this%mixerI%GetFreqCode(this%deviation)),8)*this%mixerI%GetFreqStep()
       WRITE(*,*) 'погрешность по частоте, Гц ' ,delta

       this%deviationKoeff = devKoeffInt8;



       DEALLOCATE(IR_GAUSS)

    END SUBROUTINE


    FUNCTION GenerateDiffData (this, data)
        CLASS(GFSKmodulator_t), intent(inout) :: this
        INTEGER(1)  , intent(in)              :: data(:)
        INTEGER(1)  , allocatable             :: GenerateDiffData(:)

        GenerateDiffData = this%coder%GenerateDBPSKData(data)

    END FUNCTION GenerateDiffData

    FUNCTION Generate (this, data)
        CLASS(GFSKmodulator_t), intent(inout) :: this
        INTEGER(1)  , intent(in)              :: data(:)

        INTEGER(1)  , allocatable             :: Diffdata(:)
        INTEGER(1)  , allocatable             :: outputDataSig(:)
        TYPE(analyticSignal_t), allocatable  :: deviation
        TYPE(analyticSignal_t), allocatable  :: i_t
        TYPE(analyticSignal_t), allocatable  :: q_t


        TYPE(complexSignal_t), allocatable   :: generate

        INTEGER(8)                            :: osr

          WRITE(*,*) 'вход в генерацию'

        ALLOCATE(deviation)
        ALLOCATE(Generate)
        ALLOCATE(i_t)
        ALLOCATE(q_t)


        diffData = this%GenerateDiffData(data)
        osr = this%SampleRate/this%baudRateInSamples
        outputDataSig = GenerateImpluseSequence(osr,diffData)
        CALL deviation%Constructor(outputDataSig)
        DEALLOCATE(outputDataSig)



        DEALLOCATE(diffData )

        deviation = deviation.CONV.this%impluseResponse
        deviation = deviation * this%deviationKoeff

        CALL this%mixerI%SetPhase(PI/2)
        CALL this%mixerQ%SetPhase(0*PI)

        CALL this%mixerI%GetOutPutFromCodeArray(deviation,i_t)
        CALL this%mixerQ%GetOutPutFromCodeArray(deviation,q_t)

        CALL generate%Constructor(i_t,q_t)

        DEALLOCATE(deviation)
        DEALLOCATE(i_t)
        DEALLOCATE(q_t)



!        CALL Generate%ZeroesStuffing(int(1000,8),int(1000,8))



    END FUNCTION Generate

    SUBROUTINE destructor(this)
        type(GFSKmodulator_t), intent(in) :: this

    END SUBROUTINE

END MODULE GFSKmod
