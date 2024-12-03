      SUBROUTINE DLZHIST (JL,JT,NVB,NI,NPE,LFILE,LPRNT,LRERR,
     &                    LRWRN,NQMAX,NEWSIG,NRUN,NONEWS,NSLS,
     &                    NSRS,NSRLS,NSRRS,RITZ,BASIS,WORK,
     &                    EIGON,ENDON,GNRZD,SLICE)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DLZHIST checks the run history                                   *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    JL     (sii) : number of steps                                   *
C*    JT     (sii) : dimension of the block tridiagonal matrix         *
C*    NVB    (sii) : number of vectors in a block                      *
C*    NI     (sii) : dimension of the vectors in (BASIS)               *
C*    NPE    (sii) : number of processes                               *
C*    LFILE  (sii) : file unit for output                              *
C*    LPRNT  (sii) : level of printing                                 *
C*    LRERR  (sio) : code for error messages                           *
C*    LRWRN  (sii) : code for warning messages                         *
C*    NQMAX  (sii) : maximum number of vectors in (BASIS)              *
C*    NEWSIG (sib) : flag for a new starting point                     *
C*    NRUN   (sii) : number of runs                                    *
C*    NONEWS (sii) : number of runs without any converged eingenvalue  *
C*    NSLS   (sii) : number of eigenvalues converged < than SIGMA      *
C*    NSRS   (sii) : number of eigenvalues converged > than SIGMA      *
C*    NSRLS  (sii) : number of eigenvalues required  < than SIGMA      *
C*    NSRRS  (sii) : number of eigenvalues required  > than SIGMA      *
C*    RITZ   (ari) : Ritz values and estimated residuals               *
C*    BASIS  (ari) : Lanczos vectors array                             *
C*    WORK   (arw) : workspace                                         *
C*    EIGON  (slo) : eigenvectors computation flag                     *
C*    ENDON  (slo) : termination flag                                  *
C*    GNRZD  (slo) : problem type flag                                 *
C*    SLICE  (sli) : spectrum slicing flag                             *
C*                                                                     *
C*  - Subprograms:                                                     *
C*                                                                     *
C*    DLZPRT4,DQTBQ,DSETLRM,SIBTST                                     *
C*                                                                     *
C***********************************************************************
C
C==== arguments ========================================================
C
      INTEGER          JL,JT,LFILE,LPRNT,LRERR,LRWRN,NI,NEWSIG,NONEWS,
     &                 NPE,NQMAX,NRUN,NSLS,NSRS,NSRLS,NSRRS,NVB
      LOGICAL          ENDON,EIGON,GNRZD,SLICE
C
      DOUBLE PRECISION BASIS(*),RITZ(JT,2),WORK(*)
C
C==== subprogram =======================================================
C
      LOGICAL          SIBTST
C
C**** executable statements ********************************************
C
C.... check the number of converged eigenpairs .........................
C
      IF      ( .NOT.EIGON ) THEN
C
              RETURN
C
      ELSE IF ( (NSLS+NSRS) .NE. 0 ) THEN
C
C............ some eigenvalues found ...................................
C
              NONEWS = 0
C
      ELSE
C
C............ no eigenvalue found ......................................
C
              NONEWS = NONEWS + 1
              IF ( NONEWS.EQ.3 ) CALL DSETLRM (6,LRWRN)
              IF ( NONEWS.EQ.1 .AND. NEWSIG.EQ.1 ) NEWSIG = 2
C
      END IF
C
C.... compute orthogonality level ......................................
C
C     this feature should be used only for debugging purposes
C     =======================================================
C
      IF      ( JT.EQ.0 .OR. NPE.GT.1 .OR. .NOT.SIBTST(7,LPRNT) ) THEN
              CONTINUE
      ELSE IF ( JT.GT.50 .OR. JL.GT.NQMAX ) THEN
              CALL DSETLRM (16,LRWRN)
      ELSE
              CALL DQTBQ (JL,JT,NVB,NI,NRUN,LFILE,BASIS,WORK,GNRZD)
      END IF
C
C.... print general information ........................................
C
      CALL DLZPRT4 (JL,JT,LFILE,LPRNT,NSLS,NSRS,NSRLS,NSRRS,RITZ,SLICE)
C
C.... set flags accordingly ............................................
C
      ENDON = LRERR.NE.0 
      EIGON = LRERR.EQ.0 
C
      RETURN 
C
C**** end of DLZHIST ***************************************************
C
      END
