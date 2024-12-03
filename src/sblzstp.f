      SUBROUTINE SBLZSTP (ISTOR,RSTOR,SIGMA,NNEIG,LFLAG,NVOPU,
     &                    EIG,X,U,V,AGAIN,EIGON,ENDON)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    SBLZSTP performs a Lanczos algorithm step                        *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    ISTOR (aib) : array for integer variables                        *
C*    RSTOR (aib) : array for real variables                           *
C*    SIGMA (sri) : origin translation                                 *
C*    NNEIG (sii) : number of eigenvalues less than SIGMA              *
C*    LFLAG (sib) : reverse communication flag                         *
C*    NVOPU (sio) : number of vectors for reverse communication        *
C*    EIG   (ari) : eigenvalue approximations and estimated residuals  *
C*    X     (ari) : eigenvector approximations                         *
C*    U     (arb) : array for reverse communication, U(LNI,NVB)        *
C*    V     (arb) : array for reverse communication, V(LNI,NVB)        *
C*    AGAIN (slo) : loop control flag                                  *
C*    EIGON (slo) : eigenvectors computation flag                      *
C*    ENDON (slo) : termination flag                                   *
C*                                                                     *
C*  - Subprograms:                                                     *
C*                                                                     *
C*    SLZHIST,SLZSTP1,SLZSTP2,SLZSTP3,SLZSTP4,                         *
C*    SLZSTP5,SLZSTP6,SSETSSL,SIBTST                                   *
C*                                                                     *
C***********************************************************************
C
C==== parameters =======================================================
C
      INCLUDE          'blz_address.h'
C
C==== arguments ========================================================
C
      INTEGER          LFLAG,NNEIG,NVOPU
      REAL             SIGMA
      LOGICAL          AGAIN,EIGON,ENDON
C
      INTEGER          ISTOR(*)
      REAL             EIG(*),RSTOR(*),U(*),V(*),X(*)
C
C==== local variables ==================================================
C
      INTEGER          ALPHA,ANORM,BASIS,BETAQ,BETAR,BNORM,BX,ETA,
     &                 IDXETA,IDXTAU,IR1,IR3,IWORK,NSLS,NSRS,R,
     &                 RITZ,RWORK,S,SSLOG,TAU,TB,THETA,TIME
      REAL             ABSETA,ABSTAU
      LOGICAL          GNRZD,SLICE
C
C==== subprogram =======================================================
C
      LOGICAL          SIBTST
C
C**** executable statements ********************************************
C
C.... set pointers .....................................................
C
      TIME  = ISTOR(ITIME)
      SSLOG = ISTOR(ISSLOG)
      RITZ  = ISTOR(IRITZ)
      TB    = ISTOR(ITB)
      ALPHA = ISTOR(IALPHA)
      BETAQ = ISTOR(IBETAQ)
      BETAR = ISTOR(IBETAR)
      ANORM = ISTOR(IANORM)
      BNORM = ISTOR(IBNORM)
      ETA   = ISTOR(IETA)
      TAU   = ISTOR(ITAU)
      R     = ISTOR(IR)
      THETA = ISTOR(ITHETA)
      S     = ISTOR(IS)
      BASIS = ISTOR(IBASIS)
      BX    = ISTOR(IBX)
      RWORK = ISTOR(IRWORK)
      IWORK = ISTOR(IIWORK)
C
      GNRZD = ISTOR(LOPTS  ).GT.0
      SLICE = ISTOR(LOPTS+1).GT.0
C
C.... LFLAG = 1 requires (V) = op(A)*op(B)*(U) .........................
C
      IF ( LFLAG.EQ.1 ) THEN
C
C....... (R) = (V) - (Q_{j})*(ALPHA_{j}) - (Q_{j-1})*(BETA_{j}) ........
C
         CALL SLZSTP1 (ISTOR(INDR)  ,ISTOR(JL)    ,ISTOR(JLMAX) ,
     &                 ISTOR(JT)    ,ISTOR(NVB)   ,ISTOR(LNI)   ,
     &                 ISTOR(NI)    ,ISTOR(LBLAS) ,ISTOR(LCOMM) ,
     &                 ISTOR(LRERR) ,ISTOR(NMOPA) ,ISTOR(NMOPB) ,
     &                 RSTOR(EPS)   ,RSTOR(TIME)  ,RSTOR(ALPHA) ,
     &                 RSTOR(ANORM) ,RSTOR(BETAR) ,RSTOR(R)     ,
     &                 U            ,V            ,RSTOR(RWORK) ,
     &                 GNRZD        )
C
      END IF
C
C.... LFLAG = 2 requires (V) = op(B)*(U) ...............................
C
      IF ( LFLAG.EQ.1 .AND. GNRZD ) THEN
C
         LFLAG = 2
C
      ELSE
C
C....... (R) = (Q_{j+1})*(BETA_{j+1}) ..................................
C
         CALL SLZSTP2 (ISTOR(INDR)  ,ISTOR(JL)    ,ISTOR(JLMAX) ,
     &                 ISTOR(NVB)   ,ISTOR(LNI)   ,ISTOR(NI)    ,
     &                 ISTOR(LBLAS) ,ISTOR(LCOMM) ,ISTOR(LRERR) ,
     &                 ISTOR(LRWRN) ,ISTOR(NULLDQ),ISTOR(NULLDR),
     &                 V            ,RSTOR(ANORM) ,RSTOR(BETAR) ,
     &                 RSTOR(BNORM) ,RSTOR(R)     ,RSTOR(TIME)  ,
     &                 RSTOR(RWORK) ,RSTOR(EPS)   ,GNRZD        )
C
C
C....... basis orthogonality control ...................................
C
         CALL SLZSTP3 (ISTOR(INDR)  ,ISTOR(JL)    ,ISTOR(JLMAX) ,
     &                 ISTOR(JT)    ,ISTOR(NVB)   ,ISTOR(LEIG)  ,
     &                 ISTOR(LTAU)  ,ISTOR(LNI)   ,ISTOR(NI)    ,
     &                 ISTOR(N)     ,ISTOR(FHNDL) ,ISTOR(LBLAS) ,
     &                 ISTOR(LCOMM) ,ISTOR(LRERR) ,ISTOR(LRWRN) ,
     &                 ISTOR(NPORTH),ISTOR(NSORTH),ISTOR(NTEIG) ,
     &                 ISTOR(NULLDQ),ISTOR(NULLDR),ISTOR(NQMAX) ,
     &                 ISTOR(NXMAX) ,ISTOR(NBXMAX),EIG          ,
     &                 X            ,RSTOR(ALPHA) ,RSTOR(ANORM) ,
     &                 RSTOR(BETAQ) ,RSTOR(BETAR) ,RSTOR(BNORM) ,
     &                 RSTOR(ETA)   ,RSTOR(TAU)   ,RSTOR(R)     ,
     &                 RSTOR(BASIS) ,RSTOR(BX)    ,RSTOR(TIME)  ,
     &                 RSTOR(RWORK) ,RSTOR(ENDL)  ,RSTOR(ENDR)  ,
     &                 RSTOR(EPS)   ,RSTOR(EPS1)  ,RSTOR(REPS)  ,
     &                 SIGMA        ,RSTOR(THETA0),IDXETA       ,
     &                 IDXTAU       ,ABSETA       ,ABSTAU       ,
     &                 GNRZD        )
C
C....... store (ALPHA), (BETA) and Lanczos vectors .....................
C
         CALL SLZSTP4 (ISTOR(INDR)  ,ISTOR(JL)    ,ISTOR(JTMAX) ,
     &                 ISTOR(NVB)   ,ISTOR(NI)    ,ISTOR(FHNDL) ,
     &                 ISTOR(LCOMM) ,ISTOR(LRERR) ,ISTOR(NQMAX) ,
     &                 RSTOR(TB)    ,RSTOR(ALPHA) ,RSTOR(BETAQ) ,
     &                 RSTOR(BETAR) ,RSTOR(R)     ,RSTOR(BASIS) ,
     &                 GNRZD        )
C
C....... check convergence .............................................
C
         CALL SLZSTP5 (ISTOR(JL)    ,ISTOR(JT)    ,ISTOR(JTMAX) ,
     &                 ISTOR(JTMIN) ,ISTOR(NVB)   ,ISTOR(N)     ,
     &                 ISTOR(LCOMM) ,ISTOR(NPE)   ,ISTOR(LFILE) ,
     &                 ISTOR(LPRNT) ,ISTOR(LRMDE) ,ISTOR(LRWRN) ,
     &                 ISTOR(NEWSIG),ISTOR(NDEIG) ,NSLS         ,
     &                 NSRS         ,ISTOR(NSRLS) ,ISTOR(NSRRS) ,
     &                 ISTOR(NRITZ) ,ISTOR(NULLDQ),ISTOR(NULLDR),
     &                 RSTOR(TIME)  ,RSTOR(RITZ)  ,RSTOR(BETAR) ,
     &                 RSTOR(TB)    ,RSTOR(S)     ,RSTOR(THETA) ,
     &                 ISTOR(IWORK) ,RSTOR(RWORK) ,RSTOR(BIGNUM),
     &                 RSTOR(EPS)   ,RSTOR(REPS)  ,SIGMA        ,
     &                 RSTOR(THETA0),RSTOR(THETAL),RSTOR(THETAR),
     &                 RSTOR(THRSH) ,IDXETA       ,IDXTAU       ,
     &                 ABSETA       ,ABSTAU       ,EIGON        ,
     &                 GNRZD        ,SLICE        )
C
C....... prepare for (R)=op(A)*op(B)*(Q) ...............................
C
         CALL SLZSTP6 (ISTOR(INDR)  ,ISTOR(NVB)   ,ISTOR(LNI)   ,
     &                 ISTOR(NI)    ,RSTOR(R)     ,U            ,
     &                 EIGON        ,GNRZD        )
C
C....... check the run history .........................................
C
         CALL SLZHIST (ISTOR(JL)    ,ISTOR(JT)    ,ISTOR(NVB)   ,
     &                 ISTOR(NI)    ,ISTOR(NPE)   ,ISTOR(LFILE) ,
     &                 ISTOR(LPRNT) ,ISTOR(LRERR) ,ISTOR(LRWRN) ,
     &                 ISTOR(NQMAX) ,ISTOR(NEWSIG),ISTOR(NRUN)  ,
     &                 ISTOR(NONEWS),NSLS         ,NSRS         ,
     &                 ISTOR(NSRLS) ,ISTOR(NSRRS) ,RSTOR(RITZ)  ,
     &                 RSTOR(BASIS) ,RSTOR(RWORK) ,EIGON        ,
     &                 ENDON        ,GNRZD        ,SLICE        )
C
C....... swap addresses of the Lanczos vectors .........................
C
C        INDR(1) : (B)*(Q), index j-1
C        INDR(2) : (B)*(Q), index j
C        INDR(3) : (Q), index j-1
C        INDR(4) : (Q), index j
C
         IF ( ISTOR(LOPTS).GT.0 ) THEN
            IR1 = ISTOR(INDR)
            ISTOR(INDR  ) = ISTOR(INDR+1)
            ISTOR(INDR+1) = ISTOR(INDR+2)
            ISTOR(INDR+2) = ISTOR(INDR+3)
            ISTOR(INDR+3) = IR1
         ELSE
            IR3 = ISTOR(INDR+2)
            ISTOR(INDR+2) = ISTOR(INDR+1)
            ISTOR(INDR+1) = ISTOR(INDR  )
            ISTOR(INDR  ) = IR3
            ISTOR(INDR+3) = ISTOR(INDR+1)
         END IF
C
         LFLAG = 1
C
      END IF
C
C.... set flags accordingly ............................................
C
      ENDON = ISTOR(LRERR).NE.0 .OR. SIBTST(15,ISTOR(LRWRN))
      AGAIN = EIGON .OR. ENDON
      NVOPU = ISTOR(NVB)
C
      IF ( SLICE .AND. ENDON ) 
     &   CALL SSETSSL (ISTOR(JL)    ,ISTOR(JT)    ,NNEIG        ,
     &                 0            ,RSTOR(ENDL)  ,RSTOR(ENDR)  ,
     &                 SIGMA        ,ISTOR(NSLOG) ,RSTOR(SSLOG) ,
     &                 RSTOR(TIME)  )
C
      RETURN 
C
C**** end of SBLZSTP ***************************************************
C
      END