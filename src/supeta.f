      SUBROUTINE SUPETA (J,AMAXN,BMAXN,BMINN,EPS1,ETAQ,ETAR) 
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    SUPETA updates bounds for the partial reorthog. strategy         *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    J     (sii) : number of steps                                    *
C*    AMAXN (ari) : Norms of (ALPHA)                                   *
C*    BMAXN (ari) : Norms of (BETA)                                    *
C*    BMINN (sri) : minimum singular value of current (BETA)           *
C*    EPS1  (sri) : EPS*NVB*sqrt(N)                                    *
C*    ETAQ  (arb) : orthog. bounds among (Q) and Lanczos vectors       *
C*    ETAR  (arb) : orthog. bounds among (R) and Lanczos vectors       *
C*                                                                     *
C***********************************************************************
C*                                                                     *
C*    Recurrence implemented:                                          *
C*                                                                     *
C*    eta_(j+1,i) = { [||alpha_(i)||+||alpha_(j)||]*eta_(j,i) +        *
C*                    ||beta_(i+1)||*eta_(j  ,i+1) +                   *
C*                    ||beta_(  i)||*eta_(j  ,i-1) +                   *
C*                    ||beta_(  j)||*eta_(j-1,i  ) } / ||beta_(j+1)||  *
C*                                                                     *
C*    Note: in the current implementation the indexes for ||beta_()||  *
C*          are shifted by -1 in order to conform with the storage of  *
C*          those norms in the array BMAXN.                            *
C*                                                                     *
C*    For details, see:                                                *
C*                                                                     *
C*  . "The Lanczos Algorithm with Partial Reorthogonalization", H. D.  *
C*    Simon, Mathematics of Computation, 42:115-142, 1984.             *
C*  . "A Shifted Block Lanczos Algorithm for Solving Sparse Symmetric  *
C*    Eigenvalue Problems", R. G. Grimes, J. G. Lewis and H. D. Simon, *
C*    SIAM J. Matrix Anal. Appl., 15:228-272, 1994.                    *
C*                                                                     *
C***********************************************************************
C
C==== parameters =======================================================
C 
      REAL             ONE,TWO
      PARAMETER        (ONE=1.0E0,TWO=2.0E0)
C
C==== arguments ========================================================
C
      INTEGER          J
      REAL             BMINN,EPS1
C
      REAL             AMAXN(*),BMAXN(*),ETAQ(*),ETAR(*)
C
C
C==== local variables ==================================================
C
      INTEGER          I
      REAL             T
C
C**** executable statements ********************************************
C
      T = ONE/BMINN
      ETAQ(J) = EPS1
      ETAR(J) = EPS1
C
      IF      ( J .EQ. 2 ) THEN
C
              ETAQ(J-1) = T * ( (AMAXN(J)+AMAXN(J-1))*EPS1 + 
     &                           BMAXN(J-1)*EPS1*TWO )
C
      ELSE IF ( J .GT. 2 ) THEN
C
              ETAQ(  1) = T * ( (AMAXN(1)+AMAXN(J))*ETAR(1) +
     &                          BMAXN(1)*ETAR(2) + BMAXN(J-1)*ETAQ(1) )
C
              ETAQ(J-1) = T * ( (AMAXN(J)+AMAXN(J-1))*EPS1 + 
     &                          BMAXN(J-2)*ETAQ(J-2) +
     &                          BMAXN(J-1)*EPS1*TWO )
C
              DO 10 I = 2,J-2
                 ETAQ(I) = T * ( (AMAXN(I)+AMAXN(J))*ETAR(I) +
     &                           BMAXN(I-1)*ETAR(I-1) + 
     &                           BMAXN(I)*ETAR(I+1) +
     &                           BMAXN(J-1)*ETAQ(I) )
   10         CONTINUE
C
      END IF
C
      DO 20 I = 1,J-1
         T       = ETAQ(I)
         ETAQ(I) = ETAR(I)
         ETAR(I) = T
   20 CONTINUE
C
      RETURN
C
C**** end of SUPETA ****************************************************
C
      END
