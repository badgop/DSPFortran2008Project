PROGRAM main



    IMPLICIT NONE








    CALL InitDDSTest()

    WRITE(*,*) 'DONE!'




    CONTAINS


    SUBROUTINE InitDDSTest()

            USE DDSModule


            TYPE(DDS) ::ddsGenerator
            !разрядность аккамулятора фазы
            INTEGER(1) :: romLengthInBits
            !частота дискретизации
            INTEGER(4) :: samplingFrequency
            !число бит до которых усекатется таблица ПЗУ
            INTEGER(1) :: romLenthTruncedInBits
            !разрядность выходного сигнала
            INTEGER(1) :: outputSignalSampleCapacity
            !размеры усеченнной ПЗУ - число

            INTEGER(1) :: status

            romLengthInBits=32
            romLenthTruncedInBits=8
            outputSignalSampleCapacity=8
            samplingFrequency= 20000000

            status= ddsGenerator%Constructor(romLengthInBits,romLenthTruncedInBits,samplingFrequency,outputSignalSampleCapacity)

            status=ddsGenerator%DebugOutput('ddsromtable.pcm')

    END SUBROUTINE



END PROGRAM main
