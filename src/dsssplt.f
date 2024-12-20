      SUBROUTINE DSSSPLT (INDSI,SIGMA,RSINT)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DSSSPLT sets the origin translation for a subinterval            *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    INDSI (sii) : index of the subinterval to be closed              *
C*    SIGMA (sro) : origin translation                                 *
C*    RSINT (ari) : lower and upper limits of each subinterval         *
C*                                                                     *
C*  - Intrinsic Function:                                              *
C*                                                                     *
C*    SQRT                                                             *
C*                                                                     *
C***********************************************************************
C
C==== parameters =======================================================
C
      DOUBLE PRECISION FUDGE,HALF,ZERO
      PARAMETER        (FUDGE=0.0150D0,HALF=0.50D0,ZERO=0.0D0)
C
C==== arguments ========================================================
C
      INTEGER          INDSI
      DOUBLE PRECISION SIGMA
C
      DOUBLE PRECISION RSINT(6,*)
C
C==== local variables ==================================================
C
      DOUBLE PRECISION A,B
C
C==== intrinsic function ===============================================
C
      INTRINSIC        SQRT
C
C**** executable statements ********************************************
C
C.... define bounds ....................................................
C
      IF ( RSINT(2,INDSI) .GT. RSINT(5,INDSI) ) THEN
         A = RSINT(1,INDSI)
         B = RSINT(6,INDSI)
      ELSE
         A = RSINT(2,INDSI)
         B = RSINT(5,INDSI)
      END IF
C
C.... split the subinterval ............................................
C
      IF      ( (ZERO.LT.A*2) .AND. (B.GT.A*2) ) THEN
              SIGMA = +SQRT(A*B)
      ELSE IF ( (ZERO.GT.B*2) .AND. (A.LT.B*2) ) THEN
              SIGMA = -SQRT(A*B)
      ELSE IF ( (A.EQ.ZERO) .OR. (B.EQ.ZERO) ) THEN
              SIGMA = (A+B)*(HALF+FUDGE)
      ELSE
              SIGMA = (A+B)*HALF
      END IF
C
      RETURN
C
C**** end of DSSSPLT ***************************************************
C
      END
