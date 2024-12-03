      SUBROUTINE SSETEPS (EPS)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    SSETEPS sets the roundoff unit                                   *
C*                                                                     *
C*  - Argument:                                                        *
C*                                                                     *
C*    EPS (sro) : roundoff unit                                        *
C*                                                                     *
C***********************************************************************
C
C==== argument =========================================================
C
      REAL             EPS
C
C==== LAPACK function ==================================================
C
      REAL             SLAMCH
      EXTERNAL         SLAMCH 
C
C**** executable statements ********************************************
C
      EPS = SLAMCH( 'Precision' )
C
      RETURN
C
C**** end of SSETEPS ***************************************************
C
      END
