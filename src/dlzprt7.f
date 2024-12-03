      SUBROUTINE DLZPRT7 (LFILE,RITZL,RITZR,SIGMAL,SIGMAR)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DLZPRT7 prints the shifts and farthest Ritz values               *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    RITZL  (sro) : farthest Ritz value < SIGMA                       *
C*    RITZR  (sro) : farthest Ritz value > SIGMA                       *
C*    SIGMAL (sro) : origin translation  < SIGMA                       *
C*    SIGMAR (sro) : origin translation  > SIGMA                       *
C*                                                                     *
C***********************************************************************
C
C==== arguments ========================================================
C
      INTEGER          LFILE
      DOUBLE PRECISION RITZL,RITZR,SIGMAL,SIGMAR
C
C**** executable statements ********************************************
C
      WRITE(LFILE,1000) SIGMAL,RITZL,SIGMAR,RITZR
C
      RETURN 
C
 1000 FORMAT (/,'new shift to the left :',1P,SP,E12.4,4X,
     &          'farthest Ritz value:',E12.4,
     &        /,'new shift to the right:',1P,SP,E12.4,4X,
     &          'farthest Ritz value:',E12.4)
C
C**** end of DLZPRT7 ***************************************************
C
      END
