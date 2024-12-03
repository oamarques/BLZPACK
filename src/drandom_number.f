      DOUBLE PRECISION FUNCTION DSIRAND (ISEED)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DSIRAND is an interface for a random number generator            *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    ISEED (sii) : flag for the random-number generator               *
C*                 - if not zero, it is used as a new seed             *
C*                 - if zero, a random number is returned              *
C*                                                                     *
C*  - Intrinsic Function:                                              *
C*                                                                     *
C*    DBLE                                                             *
C*                                                                     *
C*  - External Functions:                                              *
C*                                                                     *
C*    RANDOM_NUMBER,RANDOM_SEED                                        *
C*                                                                     *
C***********************************************************************
C
C==== argument =========================================================
C
      INTEGER   ISEED
C
C==== intrinsic function ===============================================
C
      INTRINSIC DBLE
C
C==== local variable ===================================================
C 
      REAL      RN
C
C**** executable statements ********************************************
C
      IF ( ISEED.NE.0 ) THEN
         CALL RANDOM_SEED(ISEED)
      ELSE
         CALL RANDOM_NUMBER(RN)
         DSIRAND = DBLE(RN)
      END IF
C
      RETURN
C
C**** end of DSIRAND ***************************************************
C
      END
