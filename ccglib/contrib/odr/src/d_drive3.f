      PROGRAM SAMPLE

C  ODRPACK ARGUMENT DEFINITIONS
C      ==> FCN      NAME OF THE USER SUPPLIED FUNCTION SUBROUTINE
C      ==> N        NUMBER OF OBSERVATIONS 
C      ==> M        COLUMNS OF DATA IN THE EXPLANATORY VARIABLE
C      ==> NP       NUMBER OF PARAMETERS
C      ==> NQ       NUMBER OF RESPONSES PER OBSERVATION
C     <==> BETA     FUNCTION PARAMETERS
C      ==> Y        RESPONSE VARIABLE
C      ==> LDY      LEADING DIMENSION OF ARRAY Y
C      ==> X        EXPLANATORY VARIABLE
C      ==> LDX      LEADING DIMENSION OF ARRAY X
C      ==> WE       "EPSILON" WEIGHTS
C      ==> LDWE     LEADING DIMENSION OF ARRAY WE
C      ==> LD2WE    SECOND DIMENSION OF ARRAY WE
C      ==> WD       "DELTA" WEIGHTS
C      ==> LDWD     LEADING DIMENSION OF ARRAY WD
C      ==> LD2WD    SECOND DIMENSION OF ARRAY WD
C      ==> IFIXB    INDICATORS FOR "FIXING" PARAMETERS (BETA)
C      ==> IFIXX    INDICATORS FOR "FIXING" EXPLANATORY VARIABLE (X)
C      ==> LDIFX    LEADING DIMENSION OF ARRAY IFIXX
C      ==> JOB      TASK TO BE PERFORMED
C      ==> NDIGIT   GOOD DIGITS IN SUBROUTINE FCN RESULTS
C      ==> TAUFAC   TRUST REGION INITIALIZATION FACTOR
C      ==> SSTOL    SUM OF SQUARES CONVERGENCE CRITERION
C      ==> PARTOL   PARAMETER CONVERGENCE CRITERION
C      ==> MAXIT    MAXIMUM NUMBER OF ITERATIONS
C      ==> IPRINT   PRINT CONTROL
C      ==> LUNERR   LOGICAL UNIT FOR ERROR REPORTS
C      ==> LUNRPT   LOGICAL UNIT FOR COMPUTATION REPORTS
C      ==> STPB     STEP SIZES FOR FINITE DIFFERENCE DERIVATIVES WRT BETA
C      ==> STPD     STEP SIZES FOR FINITE DIFFERENCE DERIVATIVES WRT DELTA
C      ==> LDSTPD   LEADING DIMENSION OF ARRAY STPD
C      ==> SCLB     SCALE VALUES FOR PARAMETERS BETA
C      ==> SCLD     SCALE VALUES FOR ERRORS DELTA IN EXPLANATORY VARIABLE
C      ==> LDSCLD   LEADING DIMENSION OF ARRAY SCLD
C     <==> WORK     DOUBLE PRECISION WORK VECTOR
C      ==> LWORK    DIMENSION OF VECTOR WORK
C     <==  IWORK    INTEGER WORK VECTOR
C      ==> LIWORK   DIMENSION OF VECTOR IWORK
C     <==  INFO     STOPPING CONDITION 
 
C  PARAMETERS SPECIFYING MAXIMUM PROBLEM SIZES HANDLED BY THIS DRIVER
C     MAXN          MAXIMUM NUMBER OF OBSERVATIONS 
C     MAXM          MAXIMUM NUMBER OF COLUMNS IN EXPLANATORY VARIABLE
C     MAXNP         MAXIMUM NUMBER OF FUNCTION PARAMETERS
C     MAXNQ         MAXIMUM NUMBER OF RESPONSES PER OBSERVATION

C  PARAMETER DECLARATIONS AND SPECIFICATIONS
      INTEGER    LDIFX,LDSCLD,LDSTPD,LDWD,LDWE,LDX,LDY,LD2WD,LD2WE,
     +           LIWORK,LWORK,MAXM,MAXN,MAXNP,MAXNQ
      PARAMETER (MAXM=5,MAXN=100,MAXNP=25,MAXNQ=5,
     +           LDY=MAXN,LDX=MAXN,
     +           LDWE=MAXN,LD2WE=MAXNQ,LDWD=MAXN,LD2WD=1,
     +           LDIFX=MAXN,LDSCLD=1,LDSTPD=1,
     +           LWORK=18 + 11*MAXNP + MAXNP**2 + MAXM + MAXM**2 + 
     +                 4*MAXN*MAXNQ + 6*MAXN*MAXM + 2*MAXN*MAXNQ*MAXNP +  
     +                 2*MAXN*MAXNQ*MAXM + MAXNQ**2 + 
     +                 5*MAXNQ + MAXNQ*(MAXNP+MAXM) + LDWE*LD2WE*MAXNQ,
     +           LIWORK=20+MAXNP+MAXNQ*(MAXNP+MAXM))

C  VARIABLE DECLARATIONS 
      INTEGER          I,INFO,IPRINT,J,JOB,L,LUNERR,LUNRPT,M,MAXIT,N,
     +                 NDIGIT,NP,NQ
      INTEGER          IFIXB(MAXNP),IFIXX(LDIFX,MAXM),IWORK(LIWORK)   
      DOUBLE PRECISION PARTOL,SSTOL,TAUFAC
      DOUBLE PRECISION BETA(MAXNP),SCLB(MAXNP),SCLD(LDSCLD,MAXM),
     +                 STPB(MAXNP),STPD(LDSTPD,MAXM),
     +                 WD(LDWD,LD2WD,MAXM),WE(LDWE,LD2WE,MAXNQ),
     +                 WORK(LWORK),X(LDX,MAXM),Y(LDY,MAXNQ)
      EXTERNAL         FCN


C  SPECIFY DEFAULT VALUES FOR DODRC ARGUMENTS
      WE(1,1,1)  = -1.0D0
      WD(1,1,1)  = -1.0D0
      IFIXB(1)   = -1
      IFIXX(1,1) = -1
      JOB        = -1
      NDIGIT     = -1
      TAUFAC     = -1.0D0
      SSTOL      = -1.0D0
      PARTOL     = -1.0D0
      MAXIT      = -1
      IPRINT     = -1
      LUNERR     = -1
      LUNRPT     = -1
      STPB(1)    = -1.0D0
      STPD(1,1)  = -1.0D0
      SCLB(1)    = -1.0D0
      SCLD(1,1)  = -1.0D0
 
C  SET UP ODRPACK REPORT FILES
      LUNERR  =   9
      LUNRPT  =   9
      OPEN (UNIT=9,FILE='REPORT3')

C  READ PROBLEM DATA
      OPEN (UNIT=5,FILE='DATA3')
      READ (5,FMT=*) N,M,NP,NQ
      READ (5,FMT=*) (BETA(I),I=1,NP)
      DO 10 I=1,N
         READ (5,FMT=*) (X(I,J),J=1,M),(Y(I,L),L=1,NQ)
   10 CONTINUE

C  SPECIFY TASK AS EXPLICIT ORTHOGONAL DISTANCE REGRESSION
C                  WITH CENTRAL DIFFERENCE DERIVATIVES
C                  COVARIANCE MATRIX CONSTRUCTED WITH RECOMPUTED DERIVATIVES
C                  DELTA INITIALIZED BY USER
C                  NOT A RESTART
C  AND INDICATE LONG INITIAL REPORT
C               NO ITERATION REPORTS
C               LONG FINAL REPORT
      JOB     = 01010
      IPRINT  = 2002

C  INITIALIZE DELTA, AND SPECIFY FIRST DECADE OF FREQUENCIES AS FIXED
      DO 20 I=1,N
         IF (X(I,1).LT.100.0D0) THEN
            WORK(I)    = 0.0D0
            IFIXX(I,1) = 0
         ELSE IF (X(I,1).LE.150.0D0) THEN
            WORK(I)    = 0.0D0
            IFIXX(I,1) = 1
         ELSE IF (X(I,1).LE.1000.0D0) THEN
            WORK(I)    = 25.0D0
            IFIXX(I,1) = 1
         ELSE IF (X(I,1).LE.10000.0D0) THEN
            WORK(I)    = 560.0D0
            IFIXX(I,1) = 1
         ELSE IF (X(I,1).LE.100000.0D0) THEN
            WORK(I)    = 9500.0D0
            IFIXX(I,1) = 1
         ELSE 
            WORK(I) = 144000.0D0
            IFIXX(I,1) = 1
         END IF
   20 CONTINUE

C  SET WEIGHTS
      DO 30 I=1,N
         IF (X(I,1).EQ.100.0D0 .OR. X(I,1).EQ.150.0D0) THEN
            WE(I,1,1) = 0.0D0
            WE(I,1,2) = 0.0D0
            WE(I,2,1) = 0.0D0
            WE(I,2,2) = 0.0D0
         ELSE
            WE(I,1,1) =   559.6D0
            WE(I,1,2) = -1634.0D0
            WE(I,2,1) = -1634.0D0
            WE(I,2,2) =  8397.0D0
         END IF
         WD(I,1,1)    =  (1.0D-4)/(X(I,1)**2)
   30 CONTINUE

C  COMPUTE SOLUTION
      CALL DODRC(FCN,
     +           N,M,NP,NQ,
     +           BETA,
     +           Y,LDY,X,LDX,
     +           WE,LDWE,LD2WE,WD,LDWD,LD2WD,
     +           IFIXB,IFIXX,LDIFX,
     +           JOB,NDIGIT,TAUFAC,
     +           SSTOL,PARTOL,MAXIT,
     +           IPRINT,LUNERR,LUNRPT,
     +           STPB,STPD,LDSTPD,
     +           SCLB,SCLD,LDSCLD,
     +           WORK,LWORK,IWORK,LIWORK,
     +           INFO)
      END


      SUBROUTINE FCN(N,M,NP,NQ,
     +               LDN,LDM,LDNP,
     +               BETA,XPLUSD,
     +               IFIXB,IFIXX,LDIFX,
     +               IDEVAL,F,FJACB,FJACD,
     +               ISTOP)

C  SUBROUTINE ARGUMENTS
C      ==> N        NUMBER OF OBSERVATIONS
C      ==> M        NUMBER OF COLUMNS IN EXPLANATORY VARIABLE
C      ==> NP       NUMBER OF PARAMETERS
C      ==> NQ       NUMBER OF RESPONSES PER OBSERVATION
C      ==> LDN      LEADING DIMENSION DECLARATOR EQUAL OR EXCEEDING N
C      ==> LDM      LEADING DIMENSION DECLARATOR EQUAL OR EXCEEDING M
C      ==> LDNP     LEADING DIMENSION DECLARATOR EQUAL OR EXCEEDING NP
C      ==> BETA     CURRENT VALUES OF PARAMETERS
C      ==> XPLUSD   CURRENT VALUE OF EXPLANATORY VARIABLE, I.E., X + DELTA
C      ==> IFIXB    INDICATORS FOR "FIXING" PARAMETERS (BETA)
C      ==> IFIXX    INDICATORS FOR "FIXING" EXPLANATORY VARIABLE (X)
C      ==> LDIFX    LEADING DIMENSION OF ARRAY IFIXX
C      ==> IDEVAL   INDICATOR FOR SELECTING COMPUTATION TO BE PERFORMED
C     <==  F        PREDICTED FUNCTION VALUES
C     <==  FJACB    JACOBIAN WITH RESPECT TO BETA
C     <==  FJACD    JACOBIAN WITH RESPECT TO ERRORS DELTA
C     <==  ISTOP    STOPPING CONDITION, WHERE
C                     0 MEANS CURRENT BETA AND X+DELTA WERE
C                       ACCEPTABLE AND VALUES WERE COMPUTED SUCCESSFULLY
C                     1 MEANS CURRENT BETA AND X+DELTA ARE
C                       NOT ACCEPTABLE;  ODRPACK SHOULD SELECT VALUES  
C                       CLOSER TO MOST RECENTLY USED VALUES IF POSSIBLE 
C                    -1 MEANS CURRENT BETA AND X+DELTA ARE
C                       NOT ACCEPTABLE;  ODRPACK SHOULD STOP

C  INPUT ARGUMENTS, NOT TO BE CHANGED BY THIS ROUTINE:
      INTEGER          I,IDEVAL,ISTOP,LDIFX,LDM,LDN,LDNP,M,N,NP,NQ
      DOUBLE PRECISION BETA(NP),XPLUSD(LDN,M)
      INTEGER          IFIXB(NP),IFIXX(LDIFX,M)
C  OUTPUT ARGUMENTS:
      DOUBLE PRECISION F(LDN,NQ),FJACB(LDN,LDNP,NQ),FJACD(LDN,LDM,NQ)
C  LOCAL VARIABLES
      DOUBLE PRECISION FREQ,PI,OMEGA,CTHETA,STHETA,THETA,PHI,R
      INTRINSIC        ATAN2,EXP,SQRT


C  CHECK FOR UNACCEPTABLE VALUES FOR THIS PROBLEM
      DO 10 I=1,N
         IF (XPLUSD(I,1).LT.0.0D0) THEN
            ISTOP = 1
            RETURN
         END IF
   10 CONTINUE
      ISTOP = 0

      PI = 3.141592653589793238462643383279D0

      THETA = PI*BETA(4)*0.5D0
      CTHETA = COS(THETA)
      STHETA = SIN(THETA)

C  COMPUTE PREDICTED VALUES
      IF (MOD(IDEVAL,10).GE.1) THEN
         DO 100 I = 1,N
            FREQ  = XPLUSD(I,1)
            OMEGA = (2.0D0*PI*FREQ*EXP(-BETA(3)))**BETA(4)
            PHI   = ATAN2((OMEGA*STHETA),(1+OMEGA*CTHETA))
            R     = (BETA(1)-BETA(2)) * 
     +              SQRT((1+OMEGA*CTHETA)**2+
     +                       (OMEGA*STHETA)**2)**(-BETA(5))
            F(I,1) = BETA(2) + R*COS(BETA(5)*PHI)
            F(I,2) =           R*SIN(BETA(5)*PHI)
  100    CONTINUE
      END IF

      RETURN
      END
