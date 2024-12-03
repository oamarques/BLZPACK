      PROGRAM DDRVSP1
!     ===============
!
!***********************************************************************
!*                                                                     *
!*    DDRVSP1 is a driver model for the standard eigenvalue problem    *
!*                                                                     *
!*           (A)*(x)-eig*(x)=(0)                                       *
!*                                                                     *
!*    where (A) is a symmetric matrix of dimension N.                  *
!*                                                                     *
!*    Data for DDRVSP1 is read from the standard input (see below).    *
!*    The upper triangle of (A) is read from the file MATRXA (in       *
!*    coordinate format).                                              *
!*                                                                     *
!*    Examples: files 'DRVSP1.dat' and 'A.dat'                         *
!*    BLAS kernel used: DSYMM                                          *
!*                                                                     *
!***********************************************************************
!
!.... parameters .......................................................
!
      INTEGER, PARAMETER          :: MAXN = 200
      DOUBLE PRECISION, PARAMETER :: ONE  = 1.0D0
      DOUBLE PRECISION, PARAMETER :: ZERO = 0.0D0
!
!.... work variables ...................................................
!
      INTEGER          :: I,IERROR,ITEMP(14),J,K,K1,K2,K3, &
                          LEIG,LISTOR,LN,LRSTOR,N 
      DOUBLE PRECISION :: RTEMP(3)
!
!.... DBLZDRV variables ................................................
!
      INTEGER                       :: LFLAG,NNEIG,NVOPU
      DOUBLE PRECISION              :: SIGMA
      INTEGER, ALLOCATABLE          :: ISTOR(:)
      DOUBLE PRECISION, ALLOCATABLE :: EIG(:,:),RSTOR(:),U(:,:), &
                                       V(:,:),X(:,:)
!
!.... matrix (A) .......................................................
!
      INTEGER          :: N
      DOUBLE PRECISION :: A(MAXN,MAXN)
      CHARACTER        :: MATRXA*16
!
!=======================================================================
!
!.... read data ........................................................
!
      ITEMP = 0
      RTEMP = ZERO
      READ (*,ERR=10,FMT=*) ITEMP( 3)
      READ (*,ERR=10,FMT=*) ITEMP( 5)
      READ (*,ERR=10,FMT=*) ITEMP( 6)
      READ (*,ERR=10,FMT=*) ITEMP( 7)
      READ (*,ERR=10,FMT=*) ITEMP( 8)
      READ (*,ERR=10,FMT=*) ITEMP( 9)
      READ (*,ERR=10,FMT=*) ITEMP(10)
      READ (*,ERR=10,FMT=*) ITEMP(11)
      READ (*,ERR=10,FMT=*) ITEMP(12)
      READ (*,ERR=10,FMT=*) ITEMP(13)
      READ (*,ERR=10,FMT=*) RTEMP( 1)
      READ (*,ERR=10,FMT=*) RTEMP( 2)
      READ (*,ERR=10,FMT=*) RTEMP( 3)
      READ (*,ERR=10,FMT=*) MATRXA
!
!.... read (A) .........................................................
!
      N = 0
      A = ZERO
      OPEN (UNIT=10,IOSTAT=IERROR,STATUS='OLD',FILE=MATRXA)
      IF ( IERROR /= 0 ) STOP '* Failed to open matrix input file'
      DO 
         READ (UNIT=10,IOSTAT=IERROR,FMT=*) J,K,A(J,K)
         IF      ( IERROR < 0 ) THEN  ! End of file.
                 EXIT
         ELSE IF ( IERROR > 0 ) THEN  ! IO error, quit.
                 STOP '* IO error in matrix input file'
         END IF
         N = MAX(J,K,N)
      END DO 
!
!.... set dimensions and allocate arrays ...............................
!
      LN = N
      LEIG = ITEMP(3)+10
!
      IF ( ITEMP(5)*ITEMP(6) > 0 ) THEN
         K1 = ITEMP(5)*ITEMP(6)
      ELSE
         K1 = MIN(N,180)
      END IF
      IF ( ITEMP(5) > 0 ) THEN
         K2 = ITEMP(5)
      ELSE
         K2 = 3
      END IF
      K3 = 484 + K1*(13+K1*2+K2+MAX(18,K2+2)) + K2*K2*3 + MIN(LEIG,N)*2
      LRSTOR = N*(K2*4+K1) + K3
      LISTOR = 123 + K1*12
!
      ITEMP( 1) = N
      ITEMP( 2) = LN
      ITEMP( 4) = LEIG
      ITEMP(14) = 0     
!
      ALLOCATE (ISTOR(17+LISTOR),STAT=IERROR)
      IF ( IERROR /= 0 ) STOP '* Failed to allocate ISTOR'
      ALLOCATE (RSTOR(4+LRSTOR),STAT=IERROR)
      IF ( IERROR /= 0 ) STOP '* Failed to allocate RSTOR'
      ALLOCATE (U(LN,K2),STAT=IERROR)
      IF ( IERROR /= 0 ) STOP '* Failed to allocate U'
      ALLOCATE (V(LN,K2),STAT=IERROR)
      IF ( IERROR /= 0 ) STOP '* Failed to allocate V'
      ALLOCATE (EIG(LEIG,2),STAT=IERROR)
      IF ( IERROR /= 0 ) STOP '* Failed to allocate EIG'
      ALLOCATE (X(LN,LEIG),STAT=IERROR)
      IF ( IERROR /= 0 ) STOP '* Failed to allocate X'
!
      ISTOR(1:14) = ITEMP; ISTOR(15) = LISTOR
      RSTOR(1: 3) = RTEMP; RSTOR(4 ) = LRSTOR
!
!.... reverse communication strategy ...................................
!
      LFLAG = 0
!
      REVERSE: DO
!
!        ************************************************************
         CALL DBLZDRV (ISTOR,RSTOR,SIGMA,NNEIG,U,V,LFLAG,NVOPU,EIG,X)
!        ************************************************************
!
         SELECT CASE ( LFLAG )
!
         CASE ( :-1 )
!
!............ early  finalization ......................................
!
              WRITE (*,'(/A)') 'execution finished: abnormal exit' 
              EXIT REVERSE
!
         CASE ( 0 )
!
!............ normal finalization ......................................
!
              WRITE (*,'(/A)') 'execution finished: standard exit'
              EXIT REVERSE
!
         CASE ( 1 )
!
!............ given (U), compute (V)=(A)*(U) ...........................
!
              CALL DSYMM ('L','U',N,NVOPU,ONE,A,MAXN,U,LN,ZERO,V,LN)
!
         CASE DEFAULT
!
!............ other flags should not be used here ......................
!
              STOP '* Error: LFLAG does not apply in this case *'
!
         END SELECT
!
      END DO REVERSE
!
      STOP
   10 STOP '* IO error: standard input *'
!
!**** end of DDRVSP1 ***************************************************
!
      END
