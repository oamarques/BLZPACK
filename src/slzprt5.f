      SUBROUTINE SLZPRT5 (JL,JT,NVB,NRUN,LFILE,ORTH)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    SLZPRT5 prints the basis orthogonality level                     *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    JL    (sii) : number of steps                                    *
C*    JT    (sii) : dimension of the block tridiagonal matrix          *
C*    NVB   (sii) : number of vectors in a block                       *
C*    NRUN  (sii) : number of runs                                     *
C*    LFILE (sii) : file unit for output                               *
C*    ORTH  (arw) : stores (Q')*(B)*(Q)                                *
C*                                                                     *
C*  - Intrinsic Function:                                              *
C*                                                                     *
C*    NINT                                                             *
C*                                                                     *
C***********************************************************************
C
C==== arguments ========================================================
C
      INTEGER          JL,JT,NRUN,LFILE,NVB
C
      REAL             ORTH(JT,JT)
C
C==== local variables ==================================================
C
      INTEGER          I,J,K
      CHARACTER        FRMT1*136,FRMT2*18,FRMT3*18,FRMT4*38,FRMT5*44
C
C==== intrinsic function ===============================================
C
      INTRINSIC        NINT
C
C**** executable statements ********************************************
C
C.... set formats ......................................................
C
      WRITE (FRMT1,'(54X,I3,16X,I3,50X,I3)') NRUN,JT*3,JT*3
C
      FRMT1(  1: 30) = '(/,''basis orthogonality check '
      FRMT1( 31: 54) = '(i=block, j=vector), run'
      FRMT1( 58: 73) = ':'',/,7(''-''),''+'','
      FRMT1( 77:113) = '(''-''),/,''  i  j |  |log10(Q''''*B*Q)|'','
      FRMT1(114:126) = '/,7(''-''),''+'','
      FRMT1(130:136) = '(''-'')) '
C
      WRITE (FRMT2,'(12X,I3)') JT
      WRITE (FRMT3,'(12X,I3)') JT
C
      FRMT2(  1: 12) = '(I3,''  1 |'','
      FRMT2( 16: 18) = 'I3)'  
      FRMT3(  1: 12) = '(3X,I3,'' |'','
      FRMT3( 16: 18) = 'I3)'  
C
      WRITE (FRMT4,'(12X,I3,17X,I3)') JT*3,JT
      WRITE (FRMT5,'(10X,I2,4X,I2,16X,I3)') JL,(NVB-1)*3,JT*3
C
      FRMT4(  1: 12) = '(7(''-''),''+'','
      FRMT4( 16: 32) = '(''-''),/,5X,''j |'','
      FRMT4( 36: 38) = 'I3)'
      FRMT5(  1: 10) = '(5X,''i |'','
      FRMT5( 13: 16) = '(I3,'
      FRMT5( 19: 34) = 'X),/,7(''-''),''+'','
      FRMT5( 38: 44) = '(''-'')) '
C
      IF ( NVB .EQ. 1 ) FRMT5(16:19) = '   '
C
C.... print the orthogonality level ....................................
C
      WRITE (LFILE,FRMT1)
C
      DO 20 I = 0,JL-1
         WRITE (LFILE,FRMT2) I+1,(NINT(ORTH(I*NVB+1,K)),K=1,JT)
         DO 10 J = 2,NVB
            WRITE (LFILE,FRMT3) J,(NINT(ORTH(I*NVB+J,K)),K=1,JT)
   10    CONTINUE
   20 CONTINUE
C
      WRITE (LFILE,FRMT4) ((I,I=1,NVB),J=1,JL)
      WRITE (LFILE,FRMT5) (J,J=1,JL)
C
      RETURN 
C
C**** end of SLZPRT5 ***************************************************
C
      END
