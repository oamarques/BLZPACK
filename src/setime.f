      REAL             FUNCTION SSITIME (TIME)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    SSITIME is an interface for a CPU time function                  *
C*                                                                     *
C*  - Argument:                                                        *
C*                                                                     *
C*    TIME (sri) : reference time                                      *
C*                                                                     *
C*  - Intrinsic Function:                                              *
C*                                                                     *
C*    REAL                                                             *
C*                                                                     *
C*  - External Function:                                               *
C*                                                                     *
C*    ETIME                                                            *
C*                                                                     *
C***********************************************************************
C
C==== argument =========================================================
C
      REAL             TIME
C
C==== local variables ==================================================
C 
      REAL             TTOTAL
      REAL             TARRAY(2)
C
C==== intrinsic function ===============================================
C
      INTRINSIC        REAL
C
C==== external function ================================================
C 
      REAL             ETIME
C
C**** executable statements ********************************************
C
      TTOTAL  = REAL(ETIME(TARRAY))
      SSITIME = REAL(TARRAY(1))
C
      SSITIME = SSITIME - TIME
C
      RETURN
C
C**** end of SSITIME ****************************************************
C
      END
