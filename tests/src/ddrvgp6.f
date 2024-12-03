      PROGRAM DDRVGP6
C     ===============
C
C***********************************************************************
C*                                                                     *
C*    DDRVGP6 is a driver model for the generalized eigenvalue         *
C*    problem                                                          *
C*                                                                     *
C*           (A)*(x) - eig*(B)*(x) = (0)                               *
C*                                                                     *
C*    where (A) is a diagonal matrix of dimension N and (B) is a       *
C*    symmetric matrix. It is assumed that (B) may be indefinite       *
C*    so that the driver is actually prepared to solve                 *
C*                                                                     *
C*           (B)*(x) - mu*(A)*(x) = (0)                                *
C*                                                                     *
C*    where eig = 1/mu.                                                *
C*                                                                     *
C*    Data for DDRVGP6 is read from the standard input (see below).    *
C*    The diagonal of (A) is read from the file MATRXA and the upper   *
C*    triangle of (B) is read from the file MATRXB (in coordinate      *
C*    format).                                                         *
C*                                                                     *
C***********************************************************************
C
C.... parameters .......................................................
C
      INTEGER          LEIG
      PARAMETER        (LEIG   =  600)
      INTEGER          LISTOR
      PARAMETER        (LISTOR =  10000)
      INTEGER          LN
      PARAMETER        (LN     =  3200)
      INTEGER          LRSTOR
      PARAMETER        (LRSTOR =  1000000)
      INTEGER          MAXB
      PARAMETER        (MAXB   =  3000000)
      INTEGER          MAXIW1
      PARAMETER        (MAXIW1 =  3000000)
      INTEGER          MAXNE
      PARAMETER        (MAXNE  =  1050000)
      INTEGER          NCUV
      PARAMETER        (NCUV   =  3)
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO   =  0.0D0)
C
C.... work variables ...................................................
C
      INTEGER          I,J,K
      DOUBLE PRECISION AJK,BJK 
C
C.... DBLZDRV variables ................................................
C
      INTEGER          ISTOR(LISTOR),LFLAG,NNEIG,NVOPU
      DOUBLE PRECISION EIG(LEIG,2),RSTOR(LRSTOR),SIGMA,
     &                 U(LN,NCUV),V(LN,NCUV),X(LN,LEIG)
C
C.... matrices (A) and (B) .............................................
C
      INTEGER          ICNTL(7),INFO(24),IRN(MAXNE),IW1(MAXIW1),
     &                 IW2(LN*2+2),JCN(MAXNE),KEEP(MAXNE+LN*5+2),N,NE
      DOUBLE PRECISION A(LN),B(MAXB),C(MAXB),CNTL(2),RINFO(4),W(LN)
      CHARACTER        MATRXA*16,MATRXB*16
C
C=======================================================================
C
C.... read data ........................................................
C
      READ (*,ERR=1,FMT=*) ISTOR( 3)
      READ (*,ERR=1,FMT=*) ISTOR( 5)
      READ (*,ERR=1,FMT=*) ISTOR( 6)
      READ (*,ERR=1,FMT=*) ISTOR( 7)
      READ (*,ERR=1,FMT=*) ISTOR( 8)
      READ (*,ERR=1,FMT=*) ISTOR( 9)
      READ (*,ERR=1,FMT=*) ISTOR(10)
      READ (*,ERR=1,FMT=*) ISTOR(11)
      READ (*,ERR=1,FMT=*) ISTOR(12)
      READ (*,ERR=1,FMT=*) ISTOR(13)
      READ (*,ERR=1,FMT=*) RSTOR( 1)
      READ (*,ERR=1,FMT=*) RSTOR( 2)
      READ (*,ERR=1,FMT=*) RSTOR( 3)
      READ (*,ERR=1,FMT=*) MATRXA
      READ (*,ERR=1,FMT=*) MATRXB
C
C.... read (A) .........................................................
C
      N = 0
      NE = 0
      OPEN (UNIT=10,ERR=2,STATUS='OLD',FILE=MATRXB)
      DO 10 I = 1,LN*LN
         READ (UNIT=10,ERR=2,END=20,FMT=*) J,K,BJK
         NE = NE + 1
         IF ( NE .GT. MAXB ) STOP '* Error: NE > MAXB *'
         IF ( NE .GT. MAXNE ) STOP '* Error: NE > MAXNE *'
         N = MAX(J,K,N)
         IRN(NE) = J
         JCN(NE) = K
         B(NE) = BJK
   10 CONTINUE
   20 CLOSE (UNIT=10,ERR=2)
C
C.... check the dimensions .............................................
C
      IF ( N .GT. LN ) STOP '* Error: N > LN *'
      IF ( ISTOR(5) .GT. NCUV ) STOP '* Error: NVB > NCUV *'
C
      ISTOR( 1) = N
      ISTOR( 2) = LN
      ISTOR( 4) = LEIG
      ISTOR(14) = 0     
      ISTOR(15) = LISTOR    
      RSTOR( 4) = LRSTOR    
C
C.... read (B), diagonal ...............................................
C
      OPEN (UNIT=10,ERR=3,STATUS='OLD',FILE=MATRXA)
      DO 30 I = 1,N
         READ (UNIT=10,ERR=3,END=40,FMT=*) AJK
         A(I) = AJK
   30 CONTINUE
   40 CLOSE (UNIT=10,ERR=3)
C
C.... set default parameters for MA47 and analyse sparsity pattern .....
C
      CALL MA47ID (CNTL,ICNTL)
C
      CALL MA47AD (N,NE,IRN,JCN,IW1,MAXIW1,KEEP,ICNTL,RINFO,INFO)
C
      IF ( INFO( 1).NE.0 ) STOP '* Error: MA47AD, INFO(1)>0 *'
C
C.... reverse communication strategy ...................................
C
      LFLAG = 0
C
   50 CONTINUE
C
C     ************************************************************
      CALL DBLZDRV (ISTOR,RSTOR,SIGMA,NNEIG,U,V,LFLAG,NVOPU,EIG,X)
C     ************************************************************
C
      IF      ( LFLAG .LT. 0 ) THEN
C
C............ early  termination .......................................
C
              WRITE (*,'(/A)') 'execution finished: abnormal exit' 
C
      ELSE IF ( LFLAG .EQ. 0 ) THEN
C
C............ normal termination .......................................
C
              WRITE (*,'(/A)') 'execution finished: standard exit'
C
      ELSE IF ( LFLAG .EQ. 1 ) THEN
C
C............ given (U), solve (C)*(V)=(U) .............................
C
              DO 60 I = 1,NVOPU
                 CALL DCOPY  (N,U(1,I),1,V(1,I),1)
                 CALL MA47CD (N,C,MAXB,IW1,MAXIW1,W,V(1,I),IW2,ICNTL)
   60         CONTINUE
C
              GO TO 50
C
      ELSE IF ( LFLAG .EQ. 2 ) THEN
C
C............ given (U), compute (V)=(A)*(U) ...........................
C
              DO 80 I = 1,NVOPU
                 DO 70 J = 1,N
                    V(J,I) = A(J)*U(J,I)
   70            CONTINUE
   80         CONTINUE
C
              GO TO 50
C
      ELSE IF ( LFLAG .EQ. 3 ) THEN
C
C............ given SIGMA, form (C)=(B)-SIGMA*(A) ......................
C
              CALL DCOPY (NE,B,1,C,1)
              DO 90 I = 1,NE
                 IF ( IRN(I).EQ.JCN(I) ) C(I) = C(I) - SIGMA*A(IRN(I))
   90         CONTINUE
C
C............ factor (C)=(L)*(D)*(L') ..................................
C
              CALL MA47BD (N,NE,JCN,C,MAXB,IW1,MAXIW1,KEEP,
     &                     CNTL,ICNTL,IW2,RINFO,INFO)
C
              IF ( INFO( 1).NE.0 ) STOP '* Error: MA47BD, INFO( 1)>0 *'
              IF ( INFO(24).GT.0 ) STOP '* Error: MA47BD, INFO(24)>0 *'
C
              NNEIG = INFO(23)
C
              GO TO 50
C
      END IF
C
      STOP
    1 STOP '* IO error: standard input *'
    2 STOP '* IO error: file MATRXB *'
    3 STOP '* IO error: file MATRXA *'
C
C**** end of DDRVGP6 **************************************************
C
      END