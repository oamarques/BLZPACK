      SUBROUTINE SEIGSRT (L,M,N,EIG,X)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    SEIGSRT sorts the eigenpairs in ascending order                  *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    L   (sii) : number of rows in (EIG) and columns in (X)           *
C*    M   (sii) : dimension of each vector                             *
C*    N   (sii) : number of eigenvalues                                *
C*    EIG (arb) : eigenvalue approximations and estimated residuals    *
C*    X   (arb) : eigenvector approximations                           *
C*                                                                     *
C*  - BLAS kernel:                                                     *
C*                                                                     *
C*    SSWAP                                                            *
C*                                                                     *
C***********************************************************************
C
C==== arguments ========================================================
C
      INTEGER          L,M,N
C
      REAL             EIG(L,2),X(M,L)
C
C==== local variables ==================================================
C
      INTEGER          I,J,K
C
      REAL             TEMP(2)
C
C**** executable statements ********************************************
C
      DO 20 I = 1,N
         K = I
         TEMP(1) = EIG(I,1)
         TEMP(2) = EIG(I,2)
         DO 10 J = I+1,N
            IF ( EIG(J,1) .LT. TEMP(1) ) THEN
               K = J
               TEMP(1) = EIG(J,1)
               TEMP(2) = EIG(J,2)
            END IF
   10    CONTINUE
         IF ( K .NE. I ) THEN
            EIG(K,1) = EIG(I,1)
            EIG(K,2) = EIG(I,2)
            EIG(I,1) = TEMP(1) 
            EIG(I,2) = TEMP(2)
            CALL SSWAP (M,X(1,I),1,X(1,K),1)
         END IF
   20 CONTINUE
C
      RETURN 
C
C**** end of SEIGSRT ***************************************************
C
      END
