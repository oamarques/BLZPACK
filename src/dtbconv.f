      SUBROUTINE DTBCONV (JT,NVB,NSLS,NSRS,NSRLS,NSRRS,NULLD,BIGNUM,EPS,
     &                    REPS,BETA,RNORM,S,THETA,THETA0,THETAL,THETAR,
     &                    SIGMA,THRES,THRSH,GNRZD,SLICE,OUTER)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DTBCONV looks for converged THETA's, the eigenvalues of (TB)     *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    JT     (sii) : dimension of the basis                            *
C*    NVB    (sii) : number of vectors in a block                      *
C*    NSLS   (sio) : number of negative eigenvalues converged          *
C*    NSRS   (sio) : number of positive eigenvalues converged          *
C*    NSRLS  (sii) : number of negative eigenvalues required           *
C*    NSRRS  (sii) : number of positive eigenvalues required           *
C*    NULLD  (sii) : number of eigenvalues equal to zero               *
C*    BIGNUM (sri) : big number                                        *
C*    EPS    (sri) : roundoff unit                                     *
C*    REPS   (sri) : sqrt(EPS)                                         *
C*    BETA   (ari) : matrix (BETA) in (R)=(Q)*(BETA) at the JL-th step *
C*    RNORM  (aro) : estimated residuals of (TB)*(s)-(theta)*(s)       *
C*    S      (ari) : eigenvectors of the tridiagonal matrix            *
C*    THETA  (aro) : eigenvalues  of the tridiagonal matrix            *
C*    THETA0 (sri) : reference point for THETA                         *
C*    THETAL (sri) : inferior limit to converged eigenvalues           *
C*    THETAR (sri) : superior limit to converged eigenvalues           *
C*    SIGMA  (sri) : origin translation                                *
C*    THRES  (sro) : threshold for convergence                         *
C*    THRSH  (sri) : threshold for convergence (default)               *
C*    GNRZD  (sli) : problem type flag                                 *
C*    SLICE  (sli) : spectrum slicing flag                             *
C*    OUTER  (slo) : convergence flag                                  *
C*                                                                     *
C*  - Subprograms:                                                     *
C*                                                                     *
C*    DRESNRM                                                          *
C*                                                                     *
C*  - Intrinsic Functions:                                             *
C*                                                                     *
C*    ABS,MAX,MIN                                                      *
C*                                                                     *
C***********************************************************************
C*                                                                     *
C*            theta x               .                                  *
C*                  x               . *                                *
C*                  x               .                                  *
C*                  x               .                                  *
C*                  x               .                                  *
C*                  x               .  *                               *
C*                  x               .                                  *
C*                  x               .                                  *
C*                  x               .   *                              *
C*                  x               .                                  *
C*                  x               .    *                             *
C*                  x               .                                  *
C*            zetar x......................*                           *
C*                  |               .      . *                         *
C*                  |               .      .   *                       *
C*                  |               .      .      *                    *
C*                  |	            .	   .          *                *
C*                  |	            .      .		   *           *
C*           theta0 |      lower    .    upper		          *    *
C*    --------------+--------[======+======]-----------------------    *
C*    *             |        .    sigma                      ritz      *
C*           *      |        .      .                      (lambda)    *
C*                * |        .      .                                  *
C*                  | *      .      .                                  *
C*                  |    *   .      .                                  *
C*                  |      * .      .                                  *
C*            zetal x........*      .                                  *
C*                  x               .                                  *
C*                  x          *    .                                  *
C*                  x               .                                  *
C*                  x           *   .                                  *
C*                  x               .                                  *
C*                  x               .                                  *
C*                  x            *  .                                  *
C*                  x               .                                  *
C*                  x               .                                  *
C*                  x               .                                  *
C*                  x             * .                                  *
C*                                                                     *
C*    Note that if nu is an approximation for an eigenvalue lambda     *
C*    close to sigma, then                                             *
C*                                                                     *
C*    | lambda - nu | <= min( bound_1,bound_2 ), where                 *
C*                                                                     *
C*    for the generalized problem:                                     *
C*                                                                     *
C*       nu = sigma + 1/theta                                          *
C*       bound_1 = betj/theta**2                                       *
C*       bound_2 = betj**2/(gamma*theta**2)                            *
C*       gamma = min[1/(lambda_i-sigma)-1/(lambda-sigma)],             *
C*               lambda_i .ne. lambda                                  *
C*                                                                     *
C*    for the buckling problem:                                        *
C*                                                                     *
C*       nu = sigma*theta/(theta-1)                                    *
C*       bound_1 = |sigma|*betj/(theta-1)**2                           *
C*       bound_2 = |sigma|*gamma*betj**2/(theta-1)**2                  *
C*       gamma = min[lambda/(lambda_i-sigma)-lambda/(lambda-sigma)],   *
C*               lambda_i .ne. lambda                                  *
C*                                                                     *
C*    therefore betj does not need to be very small for nu to be a     *
C*    good approximation for a lambda that is close to sigma. For      *
C*    details, see "A Shifted Block Lanczos Algorithm for Solving      *
C*    Sparse Symmetric Eigenvalue Problems", R. G. Grimes, J. G.       *
C*    Lewis and H. D. Simon, SIAM J. Matrix Anal. Appl., 15:228-       *
C*    272, 1994.                                                       *
C*                                                                     *
C***********************************************************************
C
C==== parameters =======================================================
C
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO=0.0D0)
C
C==== arguments ========================================================
C
      INTEGER          JT,NSLS,NSRS,NSRLS,NSRRS,NULLD,NVB
      DOUBLE PRECISION BIGNUM,EPS,REPS,SIGMA,THETA0,
     &                 THETAL,THETAR,THRES,THRSH
      LOGICAL          GNRZD,OUTER,SLICE
C
      DOUBLE PRECISION BETA(NVB,NVB),RNORM(JT),S(JT,JT),THETA(JT)
C
C==== local variables ==================================================
C
      INTEGER          I,JEND,K,NRLZL,NRRZR,NSLZL,NSRZR
      DOUBLE PRECISION BETJ,RSBND,TBNORM,ZETAL,ZETAR
      LOGICAL          IGNORE,OUTERL,OUTERU
C
C==== subprogram =======================================================
C
      DOUBLE PRECISION DRESNRM
C
C==== intrinsic functions ==============================================
C
      INTRINSIC        ABS,MAX,MIN
C
C**** executable statements ********************************************
C
C.... set convergence parameters and the threshold .....................
C
      NSLZL = 0
      NSRZR = 0
      JEND = JT - NVB + 1
C
      TBNORM = MAX(ABS(THETA(1)),ABS(THETA(JT)))
C
      IF ( THRSH .GT. ZERO ) THEN
         THRES = THRSH
      ELSE 
         THRES = REPS
      END IF
C
C.... compute the residual norms and check for convergence .............
C
      DO 10 I = 1,JT
C
	 IF ( NULLD.GT.0 .AND. ABS(THETA(I)).LE.TBNORM*EPS**2 ) THEN
C
C.......... zero eigenvalue corresponding to an invariant subspace .....
C
            THETA(I) = ZERO 
            RNORM(I) = -BIGNUM 
C
         ELSE
C
C.......... check for convergence ......................................
C
            BETJ = DRESNRM(NVB,BETA,BIGNUM,S(JEND,I))
            RSBND = BETJ/MAX(EPS,ABS(THETA(I)))
C
            IF ( RSBND .LE. THRES ) THEN
               RNORM(I) = MAX(EPS,RSBND)
            ELSE 
               RNORM(I) = -RSBND
            END IF
C
	 END IF
C
   10 CONTINUE
C
C.... set boundaries ...................................................
C
      IF ( THETAR .GE. THETAL ) THEN
         ZETAL = THETAL
         ZETAR = THETAR
         NRLZL = NSRLS
         NRRZR = NSRRS
      ELSE
         ZETAL = THETAR
         ZETAR = THETAL
         NRLZL = NSRRS
         NRRZR = NSRLS
      END IF
C
C.... convergence check for THETA < THETA0 .............................
C
      K = 0
      OUTERL = NRLZL.EQ.0
      IGNORE = OUTERL .AND. GNRZD
C
      DO 20 I = 1,JT,+1
         IF ( THETA(I) .LT. THETA0 ) THEN
            K = K + 1
            IF ( RNORM(I) .GT. ZERO ) THEN
               IF      ( IGNORE ) THEN
                       RNORM(I) = -RNORM(I)
               ELSE IF ( THETA(I) .LT. ZETAL ) THEN
                       NSLZL = NSLZL + 1
               ELSE 
                       IGNORE = .TRUE.
                       IF ( K.EQ.NSLZL+1 ) THEN
                          RNORM(I) = -RNORM(I)
                          OUTERL = .TRUE.      
                       END IF
               END IF
            END IF
         END IF
   20 CONTINUE
C
C.... convergence check for THETA > THETA0 .............................
C
      K = 0
      OUTERU = NRRZR.EQ.0
      IGNORE = OUTERU .AND. GNRZD
C
      DO 30 I = JT,1,-1
         IF ( THETA(I) .GE. THETA0 ) THEN
            K = K + 1
            IF ( RNORM(I) .GT. ZERO ) THEN
               IF      ( IGNORE ) THEN
                       RNORM(I) = -RNORM(I)
               ELSE IF ( THETA(I) .GT. ZETAR ) THEN
                       NSRZR = NSRZR + 1
               ELSE 
                       IGNORE = .TRUE.
                       IF ( K.EQ.NSRZR+1 ) THEN
                          RNORM(I) = -RNORM(I) 
                          OUTERU = .TRUE.   
                       END IF
               END IF
            END IF
         END IF
   30 CONTINUE
C
C.... set counters .....................................................
C
      IF ( THETAR .GE. THETAL ) THEN
         NSLS = NSLZL
         NSRS = NSRZR
      ELSE
         NSLS = NSRZR
         NSRS = NSLZL
      END IF
C
C.... eigenvalues out of boundaries ....................................
C
      OUTER = OUTERL .AND. OUTERU
C
      IF ( OUTER .AND. .NOT.SLICE ) THEN
         NSRLS = NSLS
         NSRRS = NSRS
      END IF
C
      RETURN 
C
C**** end of DTBCONV ***************************************************
C
      END
