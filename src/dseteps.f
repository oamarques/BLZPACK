      SUBROUTINE DSETEPS (EPS)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DSETEPS sets the roundoff unit                                   *
C*                                                                     *
C*  - Argument:                                                        *
C*                                                                     *
C*    EPS (sro) : roundoff unit                                        *
C*                                                                     *
C***********************************************************************
C
C==== argument =========================================================
C
      DOUBLE PRECISION EPS
C
C==== LAPACK function ==================================================
C
      DOUBLE PRECISION DLAMCH
      EXTERNAL         DLAMCH 
C
C**** executable statements ********************************************
C
      EPS = DLAMCH( 'Precision' )
C
      RETURN
C
C**** end of DSETEPS ***************************************************
C
      END
