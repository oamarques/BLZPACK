      INTEGER FUNCTION SNEIGAB (NTEIG,A,B,EIG)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    SNEIGAB counts the number of eigenvalues computed in an interval *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    NTEIG (sii) : number of computed eigenpairs                      *
C*    A     (sri) : lower/upper bound of the interval                  *
C*    B     (sri) : lower/upper bound of the interval                  *
C*    EIG   (ari) : eigenvalues                                        *
C*                                                                     *
C***********************************************************************
C
C==== arguments ========================================================
C
      INTEGER          NTEIG
      REAL             A,B
C
      REAL             EIG(*)
C
C==== local variables ==================================================
C
      INTEGER          I
      REAL             LOWER,UPPER
C
C**** executable statements ********************************************
C
      SNEIGAB = 0
C
C.... set LOWER and UPPER ..............................................
C
      IF ( B .GT. A ) THEN
         LOWER = A
         UPPER = B
      ELSE 
         LOWER = B 
         UPPER = A
      END IF
C
C.... compute SNEIGAB ..................................................
C
      DO 10 I = 1,NTEIG 
         IF ((LOWER.LT.EIG(I)).AND.(EIG(I).LT.UPPER)) SNEIGAB=SNEIGAB+1
   10 CONTINUE
C
      RETURN 
C
C**** end of SNEIGAB ***************************************************
C
      END
