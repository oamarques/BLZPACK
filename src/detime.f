      DOUBLE PRECISION FUNCTION DSITIME (TIME)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DSITIME is an interface for a CPU time function                  *
C*                                                                     *
C*  - Argument:                                                        *
C*                                                                     *
C*    TIME (sri) : reference time                                      *
C*                                                                     *
C*  - Intrinsic Function:                                              *
C*                                                                     *
C*    DBLE                                                             *
C*                                                                     *
C*  - External Function:                                               *
C*                                                                     *
C*    ETIME                                                            *
C*                                                                     *
C***********************************************************************
C
C==== argument =========================================================
C
      DOUBLE PRECISION TIME
C
C==== local variables ==================================================
C 
      DOUBLE PRECISION TTOTAL
C 
      REAL             TARRAY(2)
C
C==== intrinsic function ===============================================
C
      INTRINSIC        DBLE
C
C==== external function ================================================
C 
      REAL             ETIME
C
C**** executable statements ********************************************
C
      TTOTAL  = DBLE(ETIME(TARRAY))
      DSITIME = DBLE(TARRAY(1))
C
      DSITIME = DSITIME - TIME
C
      RETURN
C
C**** end of DSITIME ****************************************************
C
      END
