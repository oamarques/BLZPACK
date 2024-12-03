      REAL             FUNCTION SSIRAND (SEED)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    SSIRAND is an interface for a random number generator            *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    SEED (sii) : flag for the random-number generator                *
C*                 - if not zero, it is used as a new seed             *
C*                 - if zero, a random number is returned              *
C*                                                                     *
C*  - Intrinsic Function:                                              *
C*                                                                     *
C*    REAL                                                             *
C*                                                                     *
C*  - External Function:                                               *
C*                                                                     *
C*    RAND                                                             *
C*                                                                     *
C***********************************************************************
C
C==== argument =========================================================
C
      INTEGER   SEED
C
C==== intrinsic function ===============================================
C
      INTRINSIC REAL
C
C==== external function ================================================
C 
      REAL      RAND
C
C**** executable statements ********************************************
C
      SSIRAND = REAL(RAND(SEED))
C
      RETURN
C
C**** end of SSIRAND ***************************************************
C
      END
