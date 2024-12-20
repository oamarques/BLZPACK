      SUBROUTINE DLZSTP3 (IR,JL,JLMAX,JT,NVB,LEIG,LTAU,LNI,NI,N,FHNDL,
     &                    LBLAS,LCOMM,LRERR,LRWRN,NPORTH,NSORTH,NTEIG,
     &                    NULLDQ,NULLDR,NQMAX,NXMAX,NBXMAX,EIG,X,ALPHA,
     &                    ANORM,BETAQ,BETAR,BNORM,ETA,TAU,R,BASIS,BX,
     &                    TIME,WORK,ENDL,ENDR,EPS,EPS1,REPS,SIGMA,
     &                    THETA0,IDXETA,IDXTAU,ABSETA,
     &                    ABSTAU,GNRZD)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DLZSTP3 factors (R) and controls orthogonality level             *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    IR     (aib) : pointers for (R)                                  *
C*    JL     (sib) : number of steps                                   *
C*    JLMAX  (sii) : maximum number of steps                           *
C*    JT     (sib) : dimension of the block tridiagonal matrix         *
C*    NVB    (sii) : number of vectors in a block                      *
C*    LEIG   (sii) : leading dimension of (EIG)                        *
C*    LTAU   (sii) : leading dimension of (TAU)                        *
C*    LNI    (sii) : leading dimension of (U), (V) and (X)             *
C*    NI     (sii) : dimension of the vectors in (U), (V) and (X)      *
C*    N      (sii) : dimension of the eigenvalue problem               *
C*    FHNDL  (aii) : file handle                                       *
C*    LBLAS  (aii) : BLAS level setting                                *
C*    LCOMM  (sii) : communicator for the parallel version             *
C*    LRERR  (sio) : code for error messages                           *
C*    LRWRN  (sio) : code for warning messages                         *
C*    NPORTH (sio) : number of partial reorthogonalizations performed  *
C*    NSORTH (sio) : number of selective orthogonalizations performed  *
C*    NTEIG  (sii) : number of computed eigenpairs                     *
C*    NULLDQ (sio) : number of zero diagonal entries in BETAQ          *
C*    NULLDR (sio) : number of zero diagonal entries in BETAR          *
C*    NQMAX  (sii) : maximum number of vectors in (BASIS)              *
C*    NXMAX  (aii) : maximum number of vectors in (BX) and (X)         *
C*    NBXMAX (aii) : maximum number of vectors in (BX)                 *
C*    EIG    (ari) : eigenvalue approximations and estimated residuals *
C*    X      (ari) : eigenvector approximations                        *
C*    ALPHA  (arb) : (Q')*(B)*(R)                                      *
C*    ANORM  (arb) : extreme singular values of (ALPHA)                *
C*    BETAQ  (arb) : matrix (BETA) in (R)*(Q)*(BETA) at previous step  *
C*    BETAR  (arb) : matrix (BETA) in (R)*(Q)*(BETA) at current  step  *
C*    BNORM  (arb) : extreme singular values of (BETA)                 *
C*    ETA    (arb) : orthog. bounds among (R) and Lanczos vectors      *
C*    TAU    (arb) : orthog. bounds among (R) and Ritz    vectors      *
C*    R      (arb) : work array for Lanczos vectors                    *
C*    BASIS  (arb) : Lanczos vectors array                             *
C*    BX     (ari) : (B)*(X)                                           *
C*    TIME   (arb) : time table                                        *
C*    WORK   (arw) : workspace                                         *
C*    ENDL   (sri) : inferior bound for Ritz values                    *
C*    ENDR   (sri) : superior bound for Ritz values                    *
C*    EPS    (sri) : roundoff unit                                     *
C*    EPS1   (sri) : EPS*NVB*sqrt(N)                                   *
C*    REPS   (sri) : sqrt(EPS)                                         *
C*    SIGMA  (sri) : origin translation                                *
C*    THETA0 (sri) : reference point for THETA                         *
C*    IDXETA (sio) : index of the maximum entry in (ETA)               *
C*    IDXTAU (sio) : index of the maximum entry in (TAU)               *
C*    ABSETA (sro) : maximum entry in (ETA)                            *
C*    ABSTAU (sro) : maximum entry in (TAU)                            *
C*    GNRZD  (sli) : problem type flag                                 *
C*                                                                     *
C*  - Subprograms:                                                     *
C*                                                                     *
C*    DPURGE,DSETLRM,DSETTO0,DSITIME,DUPETA,DUPTAU                     *
C*                                                                     *
C*  - Intrinsic Functions:                                             *
C*                                                                     *
C*    MAX,MIN                                                          *
C*                                                                     *
C***********************************************************************
C
C==== parameter ========================================================
C
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO=0.0D0)
C
C==== arguments ========================================================
C
      INTEGER          IDXETA,IDXTAU,JL,JLMAX,JT,LCOMM,LEIG,LNI,
     &                 LRERR,LRWRN,LTAU,N,NBXMAX,NI,NPORTH,NQMAX,
     &                 NSORTH,NTEIG,NULLDQ,NULLDR,NVB,NXMAX
      DOUBLE PRECISION ABSETA,ABSTAU,ENDL,ENDR,EPS,EPS1,
     &                 REPS,SIGMA,THETA0
      LOGICAL          GNRZD
C
      INTEGER          FHNDL(*),IR(4),LBLAS(*)
      DOUBLE PRECISION ALPHA(NVB,NVB),ANORM(JLMAX,2),BASIS(*),
     &                 BETAQ(NVB,NVB),BETAR(NVB,NVB),BNORM(JLMAX,2),
     &                 BX(*),EIG(*),ETA(JLMAX,2),R(*),TAU(LTAU,2),
     &                 TIME(*),WORK(*),X(*)
C
C==== local variables ==================================================
C
      INTEGER          IWORK1,IWORK2,JLP1
      DOUBLE PRECISION TIME0,TDPURGE
C
C==== subprogram =======================================================
C
      DOUBLE PRECISION DSITIME
C
C==== intrinsic functions ==============================================
C
      INTRINSIC        MAX,MIN
C
C**** executable statements ********************************************
C
      IWORK1 = 1
      IWORK2 = IWORK1 + NVB*NVB
      JLP1   = MAX(1,JL)
C
      IF ( JL .EQ. 0 ) THEN
C
C....... starting vectors only .........................................
C
         IF ( NULLDR .GT. 0 ) CALL DSETLRM (3,LRWRN)
         CALL DSETTO0 (JLMAX*2,ETA,1) 
         ABSETA = ZERO
         ABSTAU = ZERO
         IDXETA = 0
         IDXTAU = 0
C
      ELSE
C
         TIME0 = DSITIME(ZERO)
C
C....... update bounds for orthogonality control .......................
C
         CALL DUPETA (JL,ANORM(1,2),BNORM(1,2),BNORM(JLP1,1),
     &                EPS1,ETA(1,1),ETA(1,2))
C
         CALL DUPTAU (LEIG,NTEIG,NVB,ALPHA,BNORM(JLP1,2),
     &                BNORM(JLP1,1),WORK(IWORK1),EIG,ENDL,ENDR,
     &                EPS,EPS1,SIGMA,TAU(1,1),TAU(1,2),THETA0,
     &                WORK(IWORK2),GNRZD)
C
C....... perform orthogonalizations ....................................
C
         CALL DPURGE (IDXETA,IDXTAU,JL,JLMAX,JT,NVB,LTAU,LNI,NI,N,FHNDL,
     &                LBLAS,LCOMM,LRERR,LRWRN,NPORTH,NSORTH,NTEIG,NQMAX,
     &                NXMAX,NBXMAX,R(IR(2)),R(IR(3)),BX,R(IR(4)),
     &                R(IR(1)),X,BASIS,BETAQ,BETAR,ANORM,BNORM,
     &                WORK,EPS,EPS1,REPS,ETA,TAU,ABSETA,
     &                ABSTAU,NULLDQ,NULLDR,GNRZD)
C
         TDPURGE = DSITIME(TIME0)
C
         TIME(5) = TIME(5) + TDPURGE
         TIME(9) = TIME(9) + TDPURGE
C
      END IF
C
      RETURN 
C
C**** end of DLZSTP3 ***************************************************
C
      END
