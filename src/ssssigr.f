      SUBROUTINE SSSSIGR (JT,NRVL,NSRS,NSRRS,RITZ,SIGMAR,TGAP)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    SSSSIGR defines an origin translation to the right of SIGMA      *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    JT     (sii) : dimension of the block tridiagonal matrix         *
C*    NRVL   (sii) : number of Ritz values < SIGMA                     *
C*    NSRS   (sii) : number of eigenvalues converged > SIGMA           *
C*    NSRRS  (sii) : number of eigenvalues required  > SIGMA           *
C*    RITZ   (ari) : Ritz values                                       *
C*    SIGMAR (sro) : new origin translation > SIGMA                    *
C*    TGAP   (sri) : relative separation between Ritz values           *
C*                                                                     *
C*  - Intrinsic Functions:                                             *
C*                                                                     *
C*    ABS,MAX,MIN                                                      *
C*                                                                     *
C***********************************************************************
C
C==== parameter ========================================================
C 
      REAL             HALF
      PARAMETER        (HALF=0.50E0)
C
C==== arguments ========================================================
C
      INTEGER          JT,NRVL,NSRS,NSRRS
      REAL             SIGMAR,TGAP
C
      REAL             RITZ(JT)
C
C==== local variables ==================================================
C
      INTEGER          J,MINJ
      REAL             DELTA,RZMAX,RZMIN
C
C==== intrinsic functions ==============================================
C
      INTRINSIC        ABS,MAX,MIN
C
C**** executable statements ********************************************
C
      MINJ = NRVL + MIN(NSRS*2,NSRRS)
      IF ( MINJ .GE. JT ) MINJ = NRVL + NSRS
C
C.... new origin splits Ritz values ....................................
C
      DO 10 J = MINJ,JT-1
C
         RZMIN = MIN(ABS(RITZ(J)),ABS(RITZ(J+1)))
         RZMAX = MAX(ABS(RITZ(J)),ABS(RITZ(J+1)))
C
         DELTA = RZMAX - RZMIN
C
         IF ( DELTA. GT. RZMAX*TGAP ) THEN
            SIGMAR = (RZMIN+RZMAX)*HALF
            RETURN
         END IF
C
   10 CONTINUE
C
      RETURN 
C
C**** end of SSSSIGR ***************************************************
C
      END
