      LOGICAL FUNCTION SIBTST (INDEX,INTGR)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    SIBTST is an interface for a a bit test function                 *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    INDEX (sii) : the bit index                                      *
C*    INTGR (sii) : the integer number                                 *
C*                                                                     *
C*  - Intrinsic Function:                                              *
C*                                                                     *
C*    BTEST                                                            *
C*                                                                     *
C***********************************************************************
C
C==== arguments ========================================================
C
      INTEGER   INDEX,INTGR
C
C==== intrinsic function ===============================================
C 
      INTRINSIC BTEST
C
C**** executable statements ********************************************
C
      SIBTST = BTEST(INTGR,INDEX)
C
      RETURN
C
C**** end of SIBTST ****************************************************
C
      END
