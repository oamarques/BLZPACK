C***********************************************************************
C*                                                                     *
C*    This file contains:                                              *
C*                                                                     *
C*    DPIGTR : interface for a parallel gather operation               *
C*    DPIRED : interface for a parallel reduce operation               *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE DPIGTR (N,X,Y,LCOMM,INFO)
C
      INTEGER          INFO,LCOMM,N
      DOUBLE PRECISION X(*),Y(*)
C
C     DPIGTR is an interface for a parallel gather operation 
C     ====== 
C
      INTEGER I
C
      INFO = 0
C
      DO 10 I = 1,N
         Y(I) = X(I)
   10 CONTINUE
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE DPIRED (FUNC,N,X,Y,LCOMM,INFO)
C
      INTEGER          INFO,LCOMM,N
      DOUBLE PRECISION X(N),Y(N)
      CHARACTER        FUNC*3
C
C     DPIRED is an interface for a parallel reduce operation
C     ====== 
C
      INTEGER I
C
      INFO = 0
C
      DO 10 I = 1,N
         Y(I) = X(I)
   10 CONTINUE
C
      RETURN
      END
