C***********************************************************************
C*                                                                     *
C*    This file contains:                                              *
C*                                                                     *
C*    IPINPE : interface for NPE                                       *
C*    IPIPID : interface for PID                                       *
C*    IPIRED : interface for a parallel reduce operation               *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE IPINPE (NPE,LCOMM,INFO)
C
      INTEGER INFO,LCOMM,NPE
C
C     IPINPE is an interface for obtaining the number of processes
C     ====== 
C
      NPE  = 1
      INFO = 0
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE IPIPID (PID,LCOMM,INFO)
C
      INTEGER INFO,LCOMM,PID
C
C     IPIPID is an interface for obtaining the calling process ID
C     ====== 
C
      PID  = 0
      INFO = 0
C
      RETURN
      END
C
C-----------------------------------------------------------------------
C
      SUBROUTINE IPIRED (FUNC,N,X,Y,LCOMM,INFO)
C
      INTEGER   LCOMM,INFO,N
      INTEGER   X(N),Y(N)
      CHARACTER FUNC*3
C
C     IPIRED is an interface for a parallel reduce operation
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
