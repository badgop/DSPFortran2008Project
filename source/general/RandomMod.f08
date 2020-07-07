MODULE RandomMod
    USE MathConstModule
    IMPLICIT NONE


    CONTAINS


    SUBROUTINE RanomGeneratorInit()
          ! ----- variables for portable seed setting -----
           INTEGER :: i_seed
           INTEGER, DIMENSION(:), ALLOCATABLE :: a_seed
           INTEGER, DIMENSION(1:8) :: dt_seed
          ! ----- end of variables for seed setting -----

          ! ----- Set up random seed portably -----
          CALL RANDOM_SEED(size=i_seed)
          ALLOCATE(a_seed(1:i_seed))
          CALL RANDOM_SEED(get=a_seed)
          CALL DATE_AND_TIME(values=dt_seed)
          a_seed(i_seed)=dt_seed(8); a_seed(1)=dt_seed(8)*dt_seed(7)*dt_seed(6)
          CALL RANDOM_SEED(put=a_seed)
          ! ----- Done setting up random seed -----
    END SUBROUTINE RanomGeneratorInit

    FUNCTION GetRandomInt(volume) RESULT (randomInt)
          INTEGER (1),INTENT(IN) :: volume
          INTEGER (8) :: randomInt
          INTEGER (8) :: ii(1:10),i
          REAL        :: rr(1:10)
          CALL RANDOM_NUMBER(rr)
          randomInt=1
          ii = int(rr*127.0,8)
          DO i=1,volume
             randomInt = randomInt*ii(i)
          END DO
    END FUNCTION

    FUNCTION GetRandomGaussianInt1(m,sigma) RESULT (randomInt)
          REAL       ,INTENT(IN) :: m,sigma
          INTEGER (1) :: randomInt
          REAL        :: x(1:2),y

          CALL RANDOM_NUMBER(x)
          randomInt=0
          y = m +sigma*SQRT(-2*LOG(x(1)))**SIN(2*PI*x(2))
          randomInt = int((y*127.0),1)

    END FUNCTION

    FUNCTION GetRandomGaussianInt2(m,sigma) RESULT (randomInt)
          REAL       ,INTENT(IN) :: m,sigma
          INTEGER (2) :: randomInt
          REAL        :: x(1:2),y

          CALL RANDOM_NUMBER(x)
          randomInt=0
          y = m +sigma*SQRT(-2*LOG(x(1)))*SIN(2*PI*x(2))

          randomInt = int((y*32767.0),2)

    END FUNCTION






END MODULE RandomMod
