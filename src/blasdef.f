      SUBROUTINE BLASDEF (LBLAS,N,NVB)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    BLASDEF sets the level of the basic linear algebra subroutines   *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    LBLAS (aio) : BLAS level setting                                 *
C*    N     (sii) : dimension of the problem                           *
C*    NVB   (sii) : number of vectors in a block                       *
C*                                                                     *
C*  - Intrinsic Function:                                              *
C*                                                                     *
C*    MIN                                                              *
C*                                                                     *
C***********************************************************************
C
C==== arguments ========================================================
C
      INTEGER   N,NVB
      INTEGER   LBLAS(*)
C
C==== intrinsic function ===============================================
C
      INTRINSIC MIN
C
C**** executable statements ********************************************
C
C.... subroutine RQBETA ................................................
C
      LBLAS(1) = MIN(2,NVB)
C
C.... subroutine QTBR ..................................................
C
      LBLAS(2) = MIN(3,NVB)
C
C.... subroutine RQALPH ................................................
C
      LBLAS(3) = MIN(3,NVB)
C
C.... subroutine MGRAMS ................................................
C
      LBLAS(4) = MIN(2,NVB)
C
      RETURN 
C
C**** end of BLASDEF ****************************************************
C
      END
