      SUBROUTINE DSSSIGL (JT,NRVL,NSLS,NSRLS,RITZ,SIGMAL,TGAP)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DSSSIGL defines an origin translation to the left  of SIGMA      *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    JT     (sii) : dimension of the block tridiagonal matrix         *
C*    NRVL   (sii) : number of Ritz values < SIGMA                     *
C*    NSLS   (sii) : number of eigenvalues converged < SIGMA           *
C*    NSRLS  (sii) : number of eigenvalues required  < SIGMA           *
C*    RITZ   (ari) : Ritz values                                       *
C*    SIGMAL (sro) : new origin translation < SIGMA                    *
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
      DOUBLE PRECISION HALF
      PARAMETER        (HALF=0.50D0)
C
C==== arguments ========================================================
C
      INTEGER          JT,NRVL,NSLS,NSRLS
      DOUBLE PRECISION SIGMAL,TGAP
C
      DOUBLE PRECISION RITZ(JT)
C
C==== local variables ==================================================
C
      INTEGER          J,MAXJ
      DOUBLE PRECISION DELTA,RZMAX,RZMIN
C
C==== intrinsic functions ==============================================
C
      INTRINSIC        ABS,MAX,MIN
C
C**** executable statements ********************************************
C
      MAXJ = NRVL - MIN(NSLS*2,NSRLS)
      IF ( MAXJ .LE. 0 ) MAXJ = NRVL - NSLS
C
C.... new origin splits Ritz values ....................................
C
      DO 10 J = MAXJ,2,-1
C
         RZMIN = MIN(ABS(RITZ(J)),ABS(RITZ(J-1)))
         RZMAX = MAX(ABS(RITZ(J)),ABS(RITZ(J-1)))
C
         DELTA = RZMAX - RZMIN
C
         IF ( DELTA .GT. RZMAX*TGAP ) THEN
            SIGMAL = (RZMIN+RZMAX)*HALF
            RETURN
         END IF
C
   10 CONTINUE
C
      RETURN 
C
C**** end of DSSSIGL ***************************************************
C
      END
