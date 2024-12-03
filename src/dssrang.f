      DOUBLE PRECISION FUNCTION DSSRANG (N,THETA,WHICH)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DSSRANG finds the extreme eigenvalues of the reduced problem     *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    N     (sii) : number of eigenvalues                              *
C*    THETA (ari) : eigenvalues of the tridiagonal matrix              *
C*    WHICH (sci) : specifies the search                               *
C*                  - if "MAX", searches for the maximum (non zero)    *
C*                  - if "MIN", searches for the minimum (non zero)    *
C*                                                                     *
C*  - Intrinsic Function:                                              *
C*                                                                     *
C*    ABS                                                              *
C*                                                                     *
C***********************************************************************
C
C==== parameters =======================================================
C
      DOUBLE PRECISION BIGNMM,BIGNMP,T
      PARAMETER        (BIGNMM=-1.0D+32,BIGNMP=+1.0D+32)
C
C==== arguments ========================================================
C
      INTEGER          N
      CHARACTER        WHICH*3
C
      DOUBLE PRECISION THETA(N)
C
C==== local variables ==================================================
C
      INTEGER          I
C
C==== intrinsic function ===============================================
C
      INTRINSIC        ABS
C
C**** executable statements ********************************************
C
      IF      ( WHICH .EQ. 'MAX' ) THEN
              T = BIGNMM
              DO 10 I = 1,N
                 IF ( ABS(THETA(I)).GT.T ) T = ABS(THETA(I))
   10         CONTINUE
      ELSE IF ( WHICH .EQ. 'MIN' ) THEN
              T = BIGNMP
              DO 20 I = 1,N
                 IF ( ABS(THETA(I)).LT.T ) T = ABS(THETA(I))
   20         CONTINUE
      END IF
C
      DSSRANG = T
C
      RETURN 
C
C**** end of DSSRANG ***************************************************
C
      END
