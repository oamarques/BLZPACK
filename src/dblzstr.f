      SUBROUTINE DBLZSTR (ISTOR,RSTOR,SIGMA,LFLAG,NVOPU,
     &                    U,V,EIG,X,AGAIN,ENDON,STPON)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DBLZSTR prepares to start a Lanczos run                          *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    ISTOR (aib) : array for integer variables                        *
C*    RSTOR (aib) : array for real variables                           *
C*    SIGMA (sri) : origin translation                                 *
C*    LFLAG (sib) : reverse communication flag                         *
C*    NVOPU (sib) : number of vectors for reverse communication        *
C*    U     (arb) : array for reverse communication, U(LNI,NVB)        *
C*    V     (arb) : array for reverse communication, V(LNI,NVB)        *
C*    EIG   (ari) : eigenvalue approximations and estimated residuals  *
C*    X     (ari) : eigenvector approximations                         *
C*    AGAIN (slo) : loop control flag                                  *
C*    ENDON (slo) : termination flag                                   *
C*    STPON (slo) : algorithm step flag                                *
C*                                                                     *
C*  - Subprograms:                                                     *
C*                                                                     *
C*    DLZCOPY,DLZPRT1,DRANDNR,DRVMNGR,DRVRSTR,DSETTO0,                 *
C*    DSITIME,DSORTH0,DSTARTR,DSTARTX                                 *
C*                                                                     *
C*  - Intrinsic Function:                                              *
C*                                                                     *
C*    MIN                                                              *
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
      INTEGER          LFLAG,NVOPU
      DOUBLE PRECISION SIGMA
      LOGICAL          AGAIN,ENDON,STPON
C
      INTEGER          ISTOR(*)
      DOUBLE PRECISION EIG(*),RSTOR(*),U(*),V(*),X(*)
C
C==== local variables ==================================================
C
      INTEGER          BASIS,BETAR,BX,IDUMMY,IWORK,LBLAS4,NV,R,
     &                 RITZ,RNORM,S,TAUQ,TAUR,RWORK1,RWORK2,TIME
      DOUBLE PRECISION RDUMMY,TIME0
      LOGICAL          GNRZD,SLICE
C
C==== subprogram =======================================================
C
      DOUBLE PRECISION DSITIME
C
C==== intrinsic function ===============================================
C
      INTRINSIC        MIN
C
C**** executable statements ********************************************
C
C.... set pointers .....................................................
C
      IDUMMY = 0
      RDUMMY = 0
C
      NVOPU = ISTOR(NVB)
      GNRZD = ISTOR(LOPTS  ).GT.0
      SLICE = ISTOR(LOPTS+1).GT.0
C
      TIME   = ISTOR(ITIME)
      RITZ   = ISTOR(IRITZ)
      BETAR  = ISTOR(IBETAR)
      TAUQ   = ISTOR(ITAU)
      R      = ISTOR(IR)
      S      = ISTOR(IS)
      BASIS  = ISTOR(IBASIS)
      BX     = ISTOR(IBX)
      RWORK1 = ISTOR(IRWORK)
      IWORK  = ISTOR(IIWORK)
C
      RNORM  = RITZ + ISTOR(JT)
      TAUR   = TAUQ + ISTOR(LTAU)
      RWORK2 = RWORK1 + ISTOR(JT)*ISTOR(NVB)
C
      TIME0 = DSITIME(ZERO)
      RSTOR(TIME+8 ) = ZERO
      RSTOR(TIME+9 ) = ZERO
      RSTOR(TIME+10) = TIME0
      IF ( RSTOR(TIME+11) .EQ. ZERO ) RSTOR(TIME+11) = TIME0
C
C.... starting block ...................................................
C
      IF      ( ISTOR(NSVIN) .NE. 0 ) THEN
C
C............ the starting vectors are defined by the user .............
C
              IF ( LFLAG .EQ. 4 ) THEN
                 NV = MIN(ISTOR(NVB),ISTOR(NSVIN))
                 CALL DSTARTR (ISTOR(LCOMM),ISTOR(LRERR),ISTOR(LNI)  ,
     &                         ISTOR(NI)   ,NV          ,RSTOR(R)    ,
     &                         V           )
                 IF ( ISTOR(LRERR) .NE. 0 ) GO TO 10
              ELSE
                 AGAIN = .FALSE.
                 LFLAG = 4
                 RETURN
              END IF
C
      ELSE IF ( ISTOR(NRUN) .GT. 0 ) THEN
C
C............ initialize (R) ...........................................
C
              CALL DSETTO0 (ISTOR(NI)*ISTOR(NVB),RSTOR(R),1)
C
C............ choose Ritz vectors for restarting .......................
C
              CALL DRVRSTR (ISTOR(JT)    ,NV           ,ISTOR(NVB)   ,
     &                      RSTOR(BIGNUM),RSTOR(REPS)  ,RSTOR(ENDL)  ,
     &                      RSTOR(ENDR)  ,RSTOR(RITZ)  ,RSTOR(RNORM) ,
     &                      RSTOR(S)     ,RSTOR(RWORK1),SIGMA        ,
     &                      ISTOR(IWORK) ,RSTOR(RWORK2),GNRZD        ,
     &                      SLICE        )
C
C............ compute Ritz vectors for restarting ......................
C
              CALL DRVMNGR (ISTOR(JL)    ,ISTOR(JT)    ,ISTOR(NVB)   ,
     &                      IDUMMY       ,ISTOR(NI)    ,ISTOR(NI)    ,
     &                      ISTOR(NQMAX) ,ISTOR(NXMAX) ,ISTOR(FHNDL) ,
     &                      ISTOR(LCOMM) ,ISTOR(LRERR) ,ISTOR(LRWRN) ,
     &                      RSTOR(RITZ)  ,RSTOR(RWORK1),RDUMMY       ,
     &                      RDUMMY       ,RSTOR(BETAR) ,RSTOR(BASIS) ,
     &                      RSTOR(R)     ,ISTOR(NTEIG) ,EIG          ,
     &                      RSTOR(R)     ,RSTOR(S)     ,GNRZD        ,
     &                      .FALSE.      )
C
      ELSE
C
C............ prepare to generate NVB random vectors ...................
C
              NV = 0
C
      END IF
C
C.... check the integrity of the eigenvectors provided by the user .....
C
      IF ( ISTOR(NRUN).EQ.0 .AND. ISTOR(NTEIG).NE.0 ) THEN
         CALL DSTARTX (ISTOR(LCOMM),ISTOR(LRERR),ISTOR(LNI)  ,
     &                 ISTOR(NI)   ,ISTOR(NTEIG),X           )
         IF ( ISTOR(LRERR) .NE. 0 ) GO TO 10
      END IF
C
      ISTOR(NRUN) = ISTOR(NRUN) + 1
C
C.... print the run mode ...............................................
C
      CALL DLZPRT1 (ISTOR(LFILE) ,ISTOR(LPRNT) ,ISTOR(LRMDE) ,
     &              ISTOR(NRUN)  )
C
C.... if there are not enough Ritz vectors use random entries ..........
C
      CALL DRANDNR (ISTOR(LCOMM) ,ISTOR(LRERR) ,ISTOR(NI)    ,
     &              NV           ,ISTOR(NRUN)  ,ISTOR(NVB)   ,
     &              RSTOR(R)     )
C
C.... purge the starting vectors .......................................
C
      LBLAS4 = ISTOR(LBLAS+3)
C
      CALL DSORTH0 (LBLAS4       ,ISTOR(LCOMM) ,ISTOR(LRERR) ,
     &              ISTOR(LEIG)  ,ISTOR(LNI)   ,ISTOR(NI)    ,
     &              ISTOR(NSORTH),ISTOR(NTEIG) ,ISTOR(NVB)   ,
     &              ISTOR(NXMAX) ,ISTOR(NBXMAX),RSTOR(BX)    ,
     &              RSTOR(R)     ,X            ,EIG          ,
     &              RSTOR(ENDL)  ,RSTOR(ENDR)  ,RSTOR(EPS1)  ,
     &              RSTOR(TAUQ)  ,RSTOR(TAUR)  ,RSTOR(RWORK1),
     &              RSTOR(RWORK2),ISTOR(FHNDL) ,GNRZD        )
C
C.....set pointers for (R) and other variables .........................
C
      ISTOR(INDR  ) = 1
      ISTOR(INDR+1) = ISTOR(INDR  ) + ISTOR(NI)*ISTOR(NVB)
      ISTOR(INDR+2) = ISTOR(INDR+1) + ISTOR(NI)*ISTOR(NVB)
      ISTOR(INDR+3) = ISTOR(INDR+2) + ISTOR(NI)*ISTOR(NVB)
      ISTOR(JL) = 0
      ISTOR(JT) = 0
C
C.....set flags accordingly ............................................
C
   10 CONTINUE
C
      ENDON = ISTOR(LRERR).NE.0 .OR. ISTOR(LRWRN).NE.0
      STPON = .NOT.ENDON
      LFLAG = 2
C
      IF ( GNRZD ) THEN
         CALL DLZCOPY (ISTOR(NI) ,ISTOR(LNI),ISTOR(NI),
     &                 ISTOR(NVB),RSTOR(R)  ,U        )
         AGAIN = .FALSE.
      ELSE
         AGAIN = .TRUE.
      END IF
C
      RSTOR(TIME+3) = DSITIME(TIME0)
C
      RETURN
C
C**** end of DBLZSTR ***************************************************
C
      END
