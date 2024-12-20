      SUBROUTINE SEIGCHK (INDXP,INDXS,LEIG,LNI,LRWRN,NNSPNT,NPEIG,NREIG,
     &                    NTEIG,BIGNUM,EIG,X,EIGL,EIGR,ORIGIN,TRUSTL,
     &                    TRUSTR,WORK,GNRZD,SLICE)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    SEIGCHK checks the computed eigenpairs                           *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    INDXP  (sio) : index for the eigenpairs (printing)               *
C*    INDXS  (sio) : index for the eigenpairs (algebraic)              *
C*    LEIG   (sii) : leading dimension of (EIG)                        *
C*    LNI    (sii) : leading dimension of (X)                          *
C*    LRWRN  (sio) : code for warning messages                         *
C*    NNSPNT (sii) : number of eigenvalues less than ORIGIN            *
C*    NPEIG  (sio) : number of eigenpairs to be printed                *
C*    NREIG  (sii) : number of required eigenpairs                     *
C*    NTEIG  (sii) : number of computed eigenpairs                     *
C*    BIGNUM (sri) : big number                                        *
C*    EIG    (ari) : eigenvalue approximations and estimated residuals *
C*    X      (ari) : eigenvector approximations                        *
C*    EIGL   (sri) : inferior bound for eigenvalues                    *
C*    EIGR   (sri) : superior bound for eigenvalues                    *
C*    ORIGIN (sri) : starting point (first SIGMA)                      *
C*    TRUSTL (sri) : inferior trust bound                              *
C*    TRUSTR (sri) : superior trust bound                              *
C*    WORK   (arw) : work array                                        *
C*    GNRZD  (sli) : problem type flag                                 *
C*    SLICE  (sli) : spectrum slicing flag                             *
C*                                                                     *
C*  - Subprograms:                                                     *
C*                                                                     *
C*    SEIGIDX,SNEIGAB,SEIGSRT,SSETLRM                                  *
C*                                                                     *
C*  - BLAS kernel:                                                     *
C*                                                                     *
C*    SCOPY                                                            *
C*                                                                     *
C*  - Intrinsic Functions:                                             *
C*                                                                     *
C*    MAX,MIN                                                          *
C*                                                                     *
C***********************************************************************
C
C==== arguments ========================================================
C
      INTEGER          INDXP,INDXS,LEIG,LNI,LRWRN,
     &                 NNSPNT,NPEIG,NREIG,NTEIG
      REAL             BIGNUM,EIGL,EIGR,ORIGIN,TRUSTL,TRUSTR
      LOGICAL          GNRZD,SLICE
C
      REAL             EIG(LEIG,2),WORK(*),X(LNI,LEIG)
C
C==== local variables ==================================================
C
      INTEGER          NSLXI,NSRXI
C
C==== subprogram =======================================================
C
      INTEGER          SNEIGAB
C
C==== intrinsic functions ==============================================
C
      INTRINSIC        MAX,MIN
C
C**** executable statements ********************************************
C
C.... check the number of computed eigenpairs ..........................
C
      IF      ( NTEIG .EQ. 0     ) THEN
              CALL SSETLRM (11,LRWRN)
      ELSE IF ( NTEIG .LT. NREIG ) THEN
              CALL SSETLRM (13,LRWRN)
      END IF
C
C.... reorder the eigenpairs ...........................................
C 
      CALL SEIGSRT (LEIG,LNI,NTEIG,EIG,X)
C
C.... identify the interesting eigenpairs ..............................
C
      IF ( GNRZD ) THEN
         CALL SCOPY  (NTEIG,EIG,1,WORK,1)
         CALL SEIGIDX (INDXP,INDXS,NNSPNT,NPEIG,NREIG,NTEIG,
     &                 BIGNUM,WORK,EIGL,EIGR,ORIGIN)
         IF ( NPEIG .EQ. 0 ) NPEIG = MIN(NREIG,NTEIG)
         INDXS = MAX(1,INDXS)
      ELSE
         NPEIG = MIN(NREIG,NTEIG)
         INDXP = NTEIG - NPEIG + 1
         INDXS = 1
      END IF
C
C.... validate eigenpairs ..............................................
C
      IF ( SLICE .AND. NTEIG.GT.0 ) THEN
C
         NSLXI = SNEIGAB(NTEIG,TRUSTL,ORIGIN,EIG)
         NSRXI = SNEIGAB(NTEIG,ORIGIN,TRUSTR,EIG)
C
         IF      ( (NSLXI.EQ.0) .AND. (NSRXI.EQ.0) ) THEN 
                 CALL SSETLRM (12,LRWRN)
         ELSE IF ( (NSLXI+NSRXI).LT.NPEIG ) THEN
                 CALL SSETLRM (14,LRWRN)
         END IF
C
      END IF
C 
      RETURN 
C
C**** end of SEIGCHK ***************************************************
C
      END
