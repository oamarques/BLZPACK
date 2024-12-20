      SUBROUTINE SUPBETA (NVB,BETA,DELTA)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    SUPBETA updates (BETA) as (BETA)=(DELTA)*(BETA)                  *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    NVB   (sii) : number of vectors in a block                       *
C*    BETA  (ari) : matrix (BETA) in (R)=(Q)*(BETA) at j-th step       *
C*    DELTA (ari) : residual (BETA)                                    *
C*                                                                     *
C*  - BLAS kernel:                                                     *
C*                                                                     *
C*    STRMM                                                            *
C*                                                                     *
C***********************************************************************
C
C==== parameter ========================================================
C 
      REAL             ONE
      PARAMETER        (ONE=1.0E0)
C
C==== arguments ========================================================
C
      INTEGER          NVB
C
      REAL             BETA(NVB,NVB),DELTA(NVB,NVB)
C
C**** executable statements ********************************************
C
      CALL STRMM ('L','U','N','N',NVB,NVB,ONE,DELTA,NVB,BETA,NVB)
C
      RETURN
C
C**** end of SUPBETA ***************************************************
C
      END
