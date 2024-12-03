      SUBROUTINE DDATCHK (IPSET,RPSET,LRERR,N,MYPE,NPE)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DDATCHK checks the input data                                    *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    IPSET (aii) : integer input data                                 *
C*    RPSET (ari) : real input data                                    *
C*    LRERR (sio) : code for error messages                            *
C*    N     (sio) : dimension of the eigenvalue problem                *
C*    MYPE  (sio) : process rank                                       *
C*    NPE   (sio) : number of processes                                *
C*                                                                     *
C*  - Subprograms:                                                     *
C*                                                                     *
C*    DSETLRM,DPIRED,IPINPE,IPIPID,IPIRED                              *
C*                                                                     *
C*  - Intrinsic Function:                                              *
C*                                                                     *
C*    MIN                                                              *
C*                                                                     *
C***********************************************************************
C
C==== arguments ========================================================
C
      INTEGER          LRERR,MYPE,N,NPE
C
      INTEGER          IPSET(*)
      DOUBLE PRECISION RPSET(*)
C
C==== local variables ==================================================
C
      INTEGER          I,INFO,LCOMM,LRMAX
C
      INTEGER          IPMAX(12)
      DOUBLE PRECISION RPMAX(3)
C
C==== intrinsic function ===============================================
C
      INTRINSIC        MIN        
C
C**** executable statements ********************************************
C
      LRERR = 0
C
C.... check input data .................................................
C
      IF ( IPSET( 1) .LE. 0 ) CALL DSETLRM ( 2,LRERR)
      IF ( IPSET( 2) .LE. 0 ) CALL DSETLRM ( 3,LRERR)
      IF ( IPSET( 3) .LE. 0 ) CALL DSETLRM ( 4,LRERR)
      IF ( IPSET( 4) .LE. 0 ) CALL DSETLRM ( 5,LRERR)
      IF ( IPSET( 5) .LT. 0 ) CALL DSETLRM ( 6,LRERR)
      IF ( IPSET( 6) .LT. 0 ) CALL DSETLRM ( 7,LRERR)
      IF ( IPSET( 7) .LT. 0 ) CALL DSETLRM ( 8,LRERR)
      IF ( IPSET( 8) .LT. 0 ) CALL DSETLRM ( 9,LRERR)
      IF ( IPSET( 9) .LT. 0 ) CALL DSETLRM (10,LRERR)
      IF ( IPSET(10) .LT. 0 ) CALL DSETLRM (11,LRERR)
      IF ( IPSET(11) .LT. 0 ) CALL DSETLRM (12,LRERR)
      IF ( IPSET(12) .LT. 0 ) CALL DSETLRM (13,LRERR)
      IF ( IPSET(13) .LT. 0 ) CALL DSETLRM (14,LRERR)
      IF ( IPSET(14) .LT. 0 ) CALL DSETLRM (15,LRERR)
      IF ( IPSET(15) .LT. 0 ) CALL DSETLRM (16,LRERR)
C
      IF ( IPSET( 9) .GT. 2 ) CALL DSETLRM (10,LRERR)
      IF ( IPSET(10) .GT. 1 ) CALL DSETLRM (11,LRERR)
      IF ( IPSET(11) .GT. 1 ) CALL DSETLRM (12,LRERR)
C
      IF ( RPSET( 3) .LT. 0 ) CALL DSETLRM (17,LRERR)
      IF ( RPSET( 4) .LT. 0 ) CALL DSETLRM (18,LRERR)
C
      LCOMM = IPSET(14)
C
C.... cross-check error flag ...........................................
C
      CALL IPIRED ('MAX',1,LRERR,LRMAX,LCOMM,INFO)
C
      IF ( INFO .NE. 0 ) CALL DSETLRM (32,LRERR)
      IF ( LRMAX.NE.0 .AND. NPE.GT.1 ) CALL DSETLRM (19,LRERR)
C
      IF ( LRERR .GT. 0 ) RETURN
C
C.... further checking .................................................
C
      CALL IPIRED ('SUM',1,IPSET(1),N,LCOMM,INFO)
C
      IF ( INFO .NE. 0  ) CALL DSETLRM (32,LRERR)
C
      IF ( IPSET(3) .GT. MIN(N,N-IPSET(8)) ) CALL DSETLRM (5,LRERR)
C
      IF ( IPSET(5) .GT. N ) CALL DSETLRM (6,LRERR)
      IF ( IPSET(6) .GT. N ) CALL DSETLRM (7,LRERR)
      IF ( IPSET(7) .GT. N ) CALL DSETLRM (8,LRERR)
      IF ( IPSET(8) .GT. N ) CALL DSETLRM (9,LRERR)
      IF ( IPSET(1) .GT. IPSET(2) ) CALL DSETLRM (3,LRERR)
      IF ( IPSET(3) .GT. IPSET(4) ) CALL DSETLRM (4,LRERR)
C
C.... number of processes in group LCOMM and process rank ..............
C
      CALL IPINPE (NPE,LCOMM,INFO)
      IF ( INFO .NE. 0 ) CALL DSETLRM (32,LRERR)
      CALL IPIPID (MYPE,LCOMM,INFO)
      IF ( INFO .NE. 0 ) CALL DSETLRM (32,LRERR)
C
C.... cross-check error flag ...........................................
C
      CALL IPIRED ('MAX',1,LRERR,LRMAX,LCOMM,INFO)
C
      IF ( INFO .NE. 0 ) CALL DSETLRM (32,LRERR)
      IF ( LRMAX.NE.0 .AND. NPE.GT.1 ) CALL DSETLRM (19,LRERR)
C
C.... cross-check integer input data ...................................
C
      CALL IPIRED ('MAX',12,IPSET(3),IPMAX,LCOMM,INFO)
C
      IF ( INFO .NE. 0 ) CALL DSETLRM (32,LRERR)
C
      DO 10 I = 1,12
         IF ( IPMAX(I) .NE. IPSET(I+2) ) CALL DSETLRM (20,LRERR)
   10 CONTINUE
C
C.... cross-check real input data ......................................
C
      CALL DPIRED ('MAX',3,RPSET,RPMAX,LCOMM,INFO)
C
      IF ( INFO .NE. 0 ) CALL DSETLRM (32,LRERR)
C
      DO 20 I = 1,3
         IF ( RPMAX(I) .NE. RPSET(I) ) CALL DSETLRM (21,LRERR)
   20 CONTINUE
C
      RETURN 
C
C**** end of DDATCHK ***************************************************
C
      END
