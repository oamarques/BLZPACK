      SUBROUTINE SIDENTY (N,A)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    SIDENTY initializes a matrix as identity                         *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    N (sii) : dimension of (A)                                       *
C*    A (aro) : identity matrix on output                              *
C*                                                                     *
C***********************************************************************
C
C==== parameters =======================================================
C
      REAL             ONE,ZERO
      PARAMETER        (ONE=1.0E0,ZERO=0.0E0)
C
C==== arguments ========================================================
C
      INTEGER          N
      REAL             A(N,N)
C
C==== local variables ==================================================
C
      INTEGER          I,J
C
C**** executable statements ********************************************
C
      DO 20 I = 1,N
         DO 10 J = 1,N
            A(J,I) = ZERO
   10    CONTINUE
         A(I,I) = ONE
   20 CONTINUE
C
      RETURN
C
C**** end of SIDENTY ***************************************************
C
      END
