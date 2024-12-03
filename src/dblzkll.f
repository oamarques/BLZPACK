      SUBROUTINE DBLZKLL (ISTOR,RSTOR,SIGMA,EIG,X,U)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DBLZKLL deals with a premature termination                       *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    ISTOR (aib) : array for integer variables                        *
C*    RSTOR (arb) : array for real variables                           *
C*    SIGMA (sri) : origin translation                                 *
C*    EIG   (arb) : eigenvalue approximations and estimated residuals  *
C*    X     (arb) : Ritz vectors                                       *
C*    U     (ari) : Lanczos vectors at step JL+1                       *
C*                                                                     *
C*  - Subprograms:                                                     *
C*                                                                     *
C*    DLZPRT4,DRVMNGR,DSETLRM,DSSTRSF,DTBEIGP                          *
C*                                                                     *
C*  - BLAS kernels:                                                    *
C*                                                                     *
C*    DCOPY,IDAMAX                                                     *
C*                                                                     *
C***********************************************************************
C
C==== parameters =======================================================
C
      INCLUDE          'blz_address.h'
C
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO=0.0D0)
C
C==== arguments ========================================================
C
      DOUBLE PRECISION SIGMA
C
      INTEGER          ISTOR(*)
      DOUBLE PRECISION EIG(*),RSTOR(*),U(*),X(*)
C
C==== local variables ==================================================
C
      INTEGER          BASIS,BETAR,IDXRES,IWORK,NSLS,NSRS,
     &                 RITZ,RNORM,RWORK,S,TB,THETA
      DOUBLE PRECISION THRES
      LOGICAL          GNRZD,OUTER
C
C==== BLAS kernel ======================================================
C
      INTEGER          IDAMAX
C
C**** executable statements ********************************************
C
C.... set pointers .....................................................
C
      RITZ  = ISTOR(IRITZ)
      TB    = ISTOR(ITB)
      BETAR = ISTOR(IBETAR)
      RWORK = ISTOR(IRWORK)
      THETA = ISTOR(ITHETA)
      S     = ISTOR(IS)
      BASIS = ISTOR(IBASIS)
      IWORK = ISTOR(IIWORK)
C
      RNORM = RITZ + ISTOR(JT)
C
      GNRZD = ISTOR(LOPTS).GT.0
C
C.... solve the reduced eigenproblem ...................................
C
      CALL DTBEIGP (ISTOR(JT)    ,ISTOR(JTMAX) ,ISTOR(NVB)   , 
     &              ISTOR(LRWRN) ,NSLS         ,NSRS         ,
     &              ISTOR(NSRLS) ,ISTOR(NSRRS) ,ISTOR(NULLDR),
     &              RSTOR(BIGNUM),RSTOR(EPS)   ,RSTOR(REPS)  ,
     &              RSTOR(BETAR) ,RSTOR(TB)    ,RSTOR(S)     ,
     &              RSTOR(THETA) ,RSTOR(THETA0),RSTOR(THETAL),
     &              RSTOR(THETAR),SIGMA        ,THRES        ,
     &              RSTOR(THRSH) ,RSTOR(RWORK) ,ISTOR(IWORK) ,
     &              GNRZD        ,.FALSE.      ,OUTER        ,
     &              .TRUE.       )
C
      IF ( GNRZD ) CALL DSSTRSF (ISTOR(JT)    ,ISTOR(JT)    ,
     &                           RSTOR(THETA) ,RSTOR(S)     ,
     &                           SIGMA        ,RSTOR(THETA0))
C
      IDXRES = IDAMAX(ISTOR(JT),RSTOR(RNORM),1)
C
      IF ( RSTOR(RNORM+IDXRES-1) .EQ. ZERO ) CALL DCOPY (ISTOR(JT)*2 ,
     &                                                   RSTOR(THETA),1,
     &                                                   RSTOR(RITZ) ,1)
C
C.... compute the eigenvectors .........................................
C
C*OAM This needs to be tested for buckling mode
      CALL DRVMNGR (ISTOR(JL)    ,ISTOR(JT)    ,ISTOR(NVB)   ,
     &              ISTOR(LEIG)  ,ISTOR(LNI)   ,ISTOR(NI)    ,
     &              ISTOR(NQMAX) ,ISTOR(NXMAX) ,ISTOR(FHNDL) ,
     &              ISTOR(LCOMM) ,ISTOR(LRERR) ,ISTOR(LRWRN) ,
     &              RSTOR(RITZ)  ,RSTOR(S)     ,SIGMA        ,
     &              ZERO         ,RSTOR(BETAR) ,RSTOR(BASIS) ,
     &              U            ,ISTOR(NTEIG) ,EIG          ,
     &              X            ,RSTOR(RWORK) ,GNRZD        ,
     &              .FALSE.      )
C
C.... print Ritz values ................................................
C
      CALL DLZPRT4 (0            ,ISTOR(JT)    ,ISTOR(LFILE) ,
     &              ISTOR(LPRNT) ,0            ,0            ,
     &              0            ,0            ,RSTOR(RITZ)  ,
     &              .FALSE.      )
C
      CALL DSETLRM (17,ISTOR(LRWRN))
C
      RETURN 
C
C**** end of DBLZKLL ***************************************************
C
      END
