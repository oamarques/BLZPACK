      PROGRAM DDRVGP1_MUMPS
!     =====================
!
!***********************************************************************
!*                                                                     *
!*    DDRVGP1_MUMPS is a driver model for the standard eigenvalue      *
!*    problem                                                          *
!*                                                                     *
!*             (A)*(x)-eig*(x)=(0)                                     *
!*                                                                     *
!*    where (A) is a symmetric matrix of dimension N. DDRVGP1_MUMPS    *
!*    is the parallel, MPI based, version of DRVGP1 calling MUMPS.     *
!*                                                                     *
!*    Data for DDRVGP1_MUMPS is read from 'drvgp1_MUMPS.dat'.          *
!*    The upper triangle of (A) is read from the file MATRXA           *
!*    in coordinate format.                                            *
!*                                                                     *
!*    Examples: files 'drvgp1_MUMPS.dat' and 'A.dat'                   *
!*    Factorization and solver used: DMUMPS (requires LAPACK,          *
!*    ScaLAPACK and METIS).                                            *
!*                                                                     *
!***********************************************************************
!
      INCLUDE 'mpif.h'
      INCLUDE 'dmumps_struc.h'
!
!.... parameters .......................................................
!
      INTEGER   LISTOR
      PARAMETER (LISTOR = 10000)
      INTEGER   LRSTOR
      PARAMETER (LRSTOR = 10000)
!
!.... work variables ...................................................
!
      INTEGER :: I,INFO,IOERROR,ITEMP(17),J,K,MYPE,NI,NJ,NPE,RMNDR
      DOUBLE PRECISION :: RTEMP(4)
!
!.... DBLZDRV variables .................................................
!
      INTEGER :: LEIG,LFLAG,NVOPU
      INTEGER, ALLOCATABLE :: ISTOR(:)
      DOUBLE PRECISION :: SIGMA
      DOUBLE PRECISION, ALLOCATABLE :: EIG(:,:),RSTOR(:),
     &                                 U(:,:),V(:,:),X(:,:)
      DOUBLE PRECISION, ALLOCATABLE, TARGET :: T(:) 
!
!.... matrix (A) .......................................................
!
      CHARACTER :: MATRXA*16
      INTEGER :: ICOLR,N,NCOLP,NNEIG,NNZ,NNZMAX 
      INTEGER, ALLOCATABLE :: TCOUNT(:),TDISP(:) 
      INTEGER, ALLOCATABLE, TARGET :: ICOL(:),IROW(:)
      DOUBLE PRECISION, ALLOCATABLE ::  A(:)
      DOUBLE PRECISION, ALLOCATABLE, TARGET :: C(:)
!
!.... MUMPS data structure .............................................
!
      TYPE (DMUMPS_STRUC) MUMPS_DATA
!
!=======================================================================
!
      CALL MPI_INIT      (INFO)
      CALL MPI_COMM_SIZE (MPI_COMM_WORLD,NPE ,INFO)
      CALL MPI_COMM_RANK (MPI_COMM_WORLD,MYPE,INFO)
!
      ALLOCATE( TCOUNT(NPE) )
      ALLOCATE( TDISP(NPE) )
!
      IF ( MYPE .EQ. 0 ) THEN
! 
!....... read data .....................................................
!
         OPEN  (UNIT=10,STATUS='OLD',FILE='DDRVGP1_MUMPS.dat')
         READ  (UNIT=10,FMT=*) ITEMP(1)    ! = NREIG
         READ  (UNIT=10,FMT=*) ITEMP(2)    ! = NVBSET
         READ  (UNIT=10,FMT=*) ITEMP(3)    ! = NSTEPS
         READ  (UNIT=10,FMT=*) ITEMP(4)    ! = GNRZD
         READ  (UNIT=10,FMT=*) ITEMP(5)    ! = SLICE
         READ  (UNIT=10,FMT=*) ITEMP(6)    ! = PURIFY
         READ  (UNIT=10,FMT=*) ITEMP(7)    ! = LPRNT
         READ  (UNIT=10,FMT=*) ITEMP(8)    ! = LFILE
         READ  (UNIT=10,FMT=*) RTEMP(1)    ! = EIGL
         READ  (UNIT=10,FMT=*) RTEMP(2)    ! = EIGR
         READ  (UNIT=10,FMT=*) RTEMP(3)    ! = THRSH
         READ  (UNIT=10,FMT=*) MATRXA
         READ  (UNIT=10,FMT=*) NNZMAX
         CLOSE (UNIT=10)
!
!....... read (A) ......................................................
!
         N = 0
         NNZ = 0
         ALLOCATE( A(NNZMAX) )
         ALLOCATE( C(NNZMAX) )
         ALLOCATE( IROW(NNZMAX) )
         ALLOCATE( ICOL(NNZMAX) )
         OPEN (UNIT=11,IOSTAT=IOERROR,STATUS='OLD',FILE=MATRXA)
         DO I = 1,NNZMAX
            READ (UNIT=11,IOSTAT=IOERROR,FMT=*) IROW(I),ICOL(I),A(I)
            IF ( IOERROR /= 0 ) EXIT
            N = MAX(N,IROW(I),ICOL(I))
            NNZ = NNZ + 1
         END DO
         CLOSE (UNIT=11)
         ALLOCATE( T(N) )
!
      END IF
!
!.... broadcast data ...................................................
!
      CALL MPI_BCAST (N    ,1,MPI_INTEGER         ,0,
     &                MPI_COMM_WORLD,INFO)
      CALL MPI_BCAST (ITEMP,8,MPI_INTEGER         ,0,
     &                MPI_COMM_WORLD,INFO)
      CALL MPI_BCAST (RTEMP,3,MPI_DOUBLE_PRECISION,0,
     &                MPI_COMM_WORLD,INFO)
!
      TDISP = 0
      NI = N/NPE
      RMNDR = MOD(N,NPE)
      DO I = 1,NPE
         IF ( RMNDR > I-1 ) THEN
            TCOUNT(I) = NI + 1
         ELSE
            TCOUNT(I) = NI
         END IF
      END DO
      DO I = 2,NPE
         TDISP(I) = TDISP(I-1) + TCOUNT(I-1)
      END DO
      IF ( RMNDR > MYPE ) NI = NI + 1
!
!.... instantiate MUMPS ................................................
!
      MUMPS_DATA%COMM = MPI_COMM_WORLD
      MUMPS_DATA%SYM  =  2 
      MUMPS_DATA%PAR  =  1
      MUMPS_DATA%JOB  = -1
      CALL DMUMPS (MUMPS_DATA)
      IF ( MYPE .EQ. 0 ) THEN
         MUMPS_DATA%N  = N 
         MUMPS_DATA%NZ = NNZ
         MUMPS_DATA%IRN => IROW(1:NNZ)
         MUMPS_DATA%JCN => ICOL(1:NNZ)
         MUMPS_DATA%A   => C(1:NNZ)
         MUMPS_DATA%RHS => T(1:N)
      END IF
      MUMPS_DATA%ICNTL(1) = 0 
      MUMPS_DATA%ICNTL(2) = 0 
      MUMPS_DATA%ICNTL(3) = 0 
      MUMPS_DATA%ICNTL(4) = 0 
!
!.... allocate arrays for BLZPACK ......................................
!
      LEIG = MIN(ITEMP(1)+10,ITEMP(1)*2)
      ALLOCATE( ISTOR(LISTOR) )
      ALLOCATE( RSTOR(LRSTOR) )
      ALLOCATE( EIG(LEIG,2) )
      ALLOCATE( U(NI,ITEMP(2)) )
      ALLOCATE( V(NI,ITEMP(2)) )
      ALLOCATE( X(NI,LEIG) )
!
      ISTOR( 1) = NI
      ISTOR( 2) = NI
      ISTOR( 3) = ITEMP(1)
      ISTOR( 4) = LEIG
      ISTOR( 5) = ITEMP(2)
      ISTOR( 6) = ITEMP(3)
      ISTOR( 7) = 0 
      ISTOR( 8) = 0
      ISTOR( 9) = ITEMP(4)
      ISTOR(10) = ITEMP(5)
      ISTOR(11) = ITEMP(6)
      ISTOR(12) = ITEMP(7)
      ISTOR(13) = ITEMP(8)
      ISTOR(14) = MPI_COMM_WORLD
      ISTOR(15) = LISTOR
      RSTOR( 1) = RTEMP(1)
      RSTOR( 2) = RTEMP(2)
      RSTOR( 3) = RTEMP(3)
      RSTOR( 4) = LRSTOR
!
!.... reverse communication strategy ...................................
!
      LFLAG = 0
!
      DO 
!
!        ============================================================
         CALL DBLZDRV (ISTOR,RSTOR,SIGMA,NNEIG,U,V,LFLAG,NVOPU,EIG,X)
!        ============================================================
!
         IF      ( LFLAG < 0 ) THEN
!
!
!............... early  termination ....................................
!
                 IF ( MYPE==0 ) WRITE (*,'(/A)') 'DBLZDRV: abnormal exit'
                 EXIT
!
         ELSE IF ( LFLAG .EQ. 0 ) THEN
!
!............... normal termination ....................................
!
                 IF ( MYPE==0 ) WRITE (*,'(/A)') 'DBLZDRV: standard exit'
                 EXIT
!
         ELSE IF ( LFLAG .EQ. 1 ) THEN
!
!............... given (U), solve (C)*(V)=(U) ..........................
!
                 MUMPS_DATA%JOB = 3
                 DO I = 1,NVOPU
                    CALL MPI_GATHERV  (
     &                   U(:,I),NI,MPI_DOUBLE_PRECISION, 
     &                   T,TCOUNT,TDISP,MPI_DOUBLE_PRECISION,  
     &                   0,MPI_COMM_WORLD,INFO )
                    CALL DMUMPS (MUMPS_DATA)
                    CALL MPI_SCATTERV (
     &                   T,TCOUNT,TDISP,MPI_DOUBLE_PRECISION, 
     &                   V(:,I),NI,MPI_DOUBLE_PRECISION, 
     &                   0,MPI_COMM_WORLD,INFO)
                 END DO
!
         ELSE IF ( LFLAG .EQ. 2 ) THEN
!
!............... given (U), compute (V)=(B)*(U), (B)=(I) ...............
!
                 V = U 
!
         ELSE IF ( LFLAG .EQ. 3 ) THEN
!
!............... given SIGMA, form (C)=(A)-SIGMA*(B) ...................
!
                 IF ( MYPE .EQ. 0 ) THEN
                    DO I = 1,NNZ
                       IF ( IROW(I).EQ.ICOL(I) ) THEN
                          C(I) = A(I) - SIGMA
                       ELSE
                          C(I) = A(I)
                       END IF
                    END DO
                 END IF
!
!............... factor (C)=(L)*(D)*(L') ...............................
!
                 MUMPS_DATA%JOB = 4
                 CALL DMUMPS (MUMPS_DATA)
                 NNEIG = MUMPS_DATA%INFOG(12)
                 CALL MPI_BCAST (NNEIG,1,MPI_INTEGER,0,
     &                           MPI_COMM_WORLD,INFO)
!
         END IF
!
      END DO
!
      DEALLOCATE (TCOUNT,TDISP)
      DEALLOCATE (A,C,ICOL,IROW,T)
      DEALLOCATE (ISTOR,RSTOR,EIG,U,V,X)
!
!.... destroy MUMPS ....................................................
!
      MUMPS_DATA%JOB = -2
      CALL DMUMPS (MUMPS_DATA)
!
      CALL MPI_BARRIER  (MPI_COMM_WORLD,INFO)
      CALL MPI_FINALIZE (INFO)
!
      STOP
!
!**** end of DDRVGP1_MUMPS *********************************************
!
      END PROGRAM DDRVGP1_MUMPS

