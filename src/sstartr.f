      SUBROUTINE SSTARTR (LCOMM,LRERR,LNI,NI,NR,R,V)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    SSTARTR checks the starting vectors defined by the user          *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    LCOMM (sii) : communicator for the parallel version              *
C*    LRERR (sio) : code for error messages                            *
C*    LNI   (sii) : leading dimension of (V)                           *
C*    NI    (sii) : dimension of the vectors in (R)                    *
C*    NR    (sii) : number of starting vectors                         *
C*    R     (aro) : starting vectors                                   *
C*    V     (ari) : starting vectors                                   *
C*                                                                     *
C*  - Subprograms:                                                     *
C*                                                                     *
C*    SLZCOPY,SPIRED,IPIRED,SSETLRM                                    *
C*                                                                     *
C*  - BLAS kernel:                                                     *
C*                                                                     *
C*    SASUM                                                            *
C*                                                                     *
C***********************************************************************
C
C==== arguments ========================================================
C
      INTEGER          LCOMM,LNI,LRERR,NI,NR
C
      REAL             R(NI,NR),V(LNI,NR)
C
C==== local variables ==================================================
C
      REAL             GSUM,SUMV
      INTEGER          GERR,I,INFO
C
C==== BLAS kernel ======================================================
C
      REAL             SASUM
C
C**** executable statements ********************************************
C
C.... loop on the columns of V .........................................
C
      DO 10 I = 1,NR
C
C....... sum up all entries of V .......................................
C
         SUMV = SASUM(NI,V(1,I),1)
C
C....... check for NaNs ................................................
C
         IF ( SUMV .NE. SUMV ) CALL SSETLRM (25,LRERR)
         CALL IPIRED ('MAX',1,LRERR,GERR,LCOMM,INFO)
         IF ( INFO .NE. 0 ) CALL SSETLRM (32,LRERR)
         IF ( GERR .NE. 0 ) CALL SSETLRM (25,LRERR)
C
C....... check for a zero V ............................................
C
         IF ( LRERR .EQ. 0 ) THEN
            CALL SPIRED ('MAX',1,SUMV,GSUM,LCOMM,INFO)
            IF ( INFO .NE. 0 ) CALL SSETLRM (32,LRERR)
            IF ( GSUM .EQ. 0 ) CALL SSETLRM (25,LRERR)
         END IF
C
   10 CONTINUE
C
C.... copy V into R ....................................................
C
      IF ( LRERR .EQ. 0 ) CALL SLZCOPY (LNI,NI,NI,NR,V,R)
C
      RETURN
C
C**** end of SSTARTR ***************************************************
C
      END
