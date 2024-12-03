      SUBROUTINE SNORM2A (N,A,B,EPS,SVMAX,SVMIN)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    SNORM2A computes the extreme singular values of a square matrix  *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    N     (sii) : dimension of (A)                                   *
C*    A     (ari) : square matrix of dimension N                       *
C*    B     (arw) : augmented matrix                                   *
C*    EPS   (sri) : roundoff unit                                      *
C*    SVMAX (sro) : maximum singular value of (A)                      *
C*    SVMIN (sro) : minimum singular value of (A)                      *
C*                                                                     *
C*  - Subprograms:                                                     *
C*                                                                     *
C*    STBBRED,STBIMQL,SSETTO0,SSSRANG                                  *
C*                                                                     *
C*  - Intrinsic Function:                                              *
C*                                                                     *
C*    MAX                                                              *
C*                                                                     *
C***********************************************************************
C     
C==== arguments ========================================================
C
      INTEGER          N
      REAL             EPS,SVMAX,SVMIN
C
      REAL             A(N,N),B(N*2,N*2+2)
C
C==== local variables ==================================================
C
      INTEGER          I,INFO,J
C
      REAL             DUMMY(1)
C
C==== subprogram =======================================================
C
      REAL             SSSRANG
C
C==== intrinsic function ===============================================
C
      INTRINSIC        MAX 
C
C**** executable statements ********************************************
C
C.... initialize (B) ...................................................
C
      CALL SSETTO0 (N*4*(N+1),B,1)
C
C.... build up the augmented matrix ....................................
C
C     STBBRED requires the lower triangle of B as an N*2-by-N array
C
C         |  0   A' |
C     B = |         |
C         |  A   0  |
C
      DO 20 I = 1,N
         DO 10 J = 1,N
            B(J+N,N+I-J+2) = A(J,I)
   10    CONTINUE
   20 CONTINUE
C
C.... compute the eigenvalues using STBBRED and STBIMQL ................
C
      CALL STBBRED (N*2,N*2,B(1,3),B(1,1),B(1,2),DUMMY,N*4)
C
      CALL STBIMQL (N*2,B(1,1),B(1,2),DUMMY,N*4,INFO)
C
      SVMIN = MAX(EPS**2,SSSRANG(N*2,B(1,1),'MIN'))
      SVMAX = MAX(EPS**2,SSSRANG(N*2,B(1,1),'MAX'))
C
      RETURN
C
C**** end of SNORM2A ***************************************************
C
      END
