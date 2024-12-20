      SUBROUTINE DLZNRMX (NVB,LNI,NI,FHNDL,LCOMM,LRERR,LRWRN,NMOPB,NBX,
     &                    NTEIG,NVOPU,NXMAX,NBXMAX,BX,X,U,V,TIME,PURFY)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DLZNRMX deals with the product (B)*(X)                           *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    NVB    (sii) : number of vectors in a block                      *
C*    LNI    (sii) : leading dimension of (X)                          *
C*    NI     (sii) : dimension of the vectors in (X)                   *
C*    FHNDL  (aii) : file handle                                       *
C*    LCOMM  (sii) : communicator for the parallel version             *
C*    LRERR  (sio) : code for error messages                           *
C*    LRWRN  (sio) : code for warning messages                         *
C*    NMOPB  (sib) : number of op(B)*vector performed                  *
C*    NBX    (sib) : number of vectors stored in (BX)                  *
C*    NTEIG  (sii) : number of computed eigenpairs                     *
C*    NVOPU  (sib) : number of vectors for reverse communication       *
C*    NXMAX  (aii) : maximum number of vectors in (X)                  *
C*    NBXMAX (aii) : maximum number of vectors in (BX)                 *
C*    BX     (arb) : (B)*(X)                                           *
C*    X      (arb) : eigenvector approximations                        *
C*    U      (arb) : array for reverse communication, U(LNI,NVB)       *
C*    V      (arb) : array for reverse communication, V(LNI,NVB)       *
C*    TIME   (arb) : time table                                        *
C*    PURFY  (sli) : eigenvectors purification flag                    *
C*                                                                     *
C*  - Subprograms:                                                     *
C*                                                                     *
C*    DLZCOPY,DLZIOOP,DPIRED,DSETLRM,DSITIME                           *
C*                                                                     *
C*  - BLAS kernels:                                                    *
C*                                                                     *
C*    DDOT,DSCAL                                                       *
C*                                                                     *
C*  - Intrinsic Functions:                                             *
C*                                                                     *
C*    MIN,SQRT                                                         *
C*                                                                     *
C***********************************************************************
C
C==== parameters =======================================================
C
      DOUBLE PRECISION ONE,ZERO
      PARAMETER        (ONE=1.0D0,ZERO=0.0D0)
C
C==== arguments ========================================================
C
      INTEGER          LCOMM,LNI,LRERR,LRWRN,NBX,NBXMAX,
     &                 NI,NMOPB,NTEIG,NVB,NVOPU,NXMAX
      LOGICAL          PURFY
C
      INTEGER          FHNDL(*)
      DOUBLE PRECISION BX(NI,*),TIME(*),U(LNI,*),V(LNI,*),X(LNI,*)
C
C==== local variables ==================================================
C
      INTEGER          I,INFO,J,NXOUT
      DOUBLE PRECISION TEMP,TIME0,ZETA
C
C==== subprogram =======================================================
C
      DOUBLE PRECISION DSITIME
C
C==== BLAS kernel ======================================================
C
      DOUBLE PRECISION DDOT
C
C==== intrinsic functions ==============================================
C
      INTRINSIC        MIN,SQRT
C
C**** executable statements ********************************************
C
C.... loop on the NVOPU vectors ........................................
C
      DO 10 I = 1,NVOPU
C
         NBX = NBX + 1
C
         TIME(2) = TIME(2) + DSITIME(TIME(11))
C
         IF ( PURFY ) THEN
C
            TIME0 = DSITIME(ZERO)
C
            TEMP = DDOT(NI,U(1,I),1,V(1,I),1)
            CALL DPIRED ('SUM',1,TEMP,ZETA,LCOMM,INFO)
            IF ( INFO .NE. 0 ) CALL DSETLRM (32,LRERR)
C
            IF ( ZETA .LT. ZERO ) THEN
               CALL DSETLRM (15,LRWRN)
               RETURN
            ELSE
               ZETA = ONE/SQRT(ZETA)                    
            END IF
C
            IF ( ZETA .NE. ONE ) THEN
C
C............. normalize (X) ...........................................
C
               CALL DSCAL (NI,ZETA,U(1,I),1)
               CALL DSCAL (NI,ZETA,V(1,I),1)
C
C............. copy (X) somewhere else .................................
C
               IF ( NXMAX .EQ. 0 ) THEN
                  CALL DLZIOOP (FHNDL,LCOMM,LRERR,NBX,NI,
     &                          U(1,I),'X ','PUT')
                  IF ( LRERR .NE. 0 ) RETURN
               ELSE
                  CALL DLZCOPY (LNI,NI,NI,1,U(1,I),X(1,NBX))
               END IF
C
            END IF
C
            TIME(7) = TIME(7) + DSITIME(TIME0)
C
         END IF
C
C....... copy (B)*(X) somewhere else ...................................
C
         IF ( NBXMAX .LT. NBX ) THEN
            J = NBX - NBXMAX
            CALL DLZIOOP (FHNDL,LCOMM,LRERR,J,NI,V(1,I),'BX','PUT')
            IF ( LRERR .NE. 0 ) RETURN
         ELSE
            CALL DLZCOPY (LNI,NI,NI,1,V(1,I),BX(1,NBX))
         END IF
C
   10 CONTINUE
C
C.... retrieve vectors to be multiplied by (B) .........................
C
      NXOUT = MIN(NVB,NTEIG-NBX) 
C
      IF ( NXOUT .GT. 0 ) THEN
         IF ( NXMAX .GT. 0 ) THEN
            CALL DLZCOPY (LNI,LNI,NI,NXOUT,X(1,NBX+1),U)
         ELSE
            DO 20 I = 1,NXOUT
               J = NBX + I
               CALL DLZIOOP (FHNDL,LCOMM,LRERR,J,NI,U(1,I),'X ','GET')
               IF ( LRERR .NE. 0 ) RETURN
   20       CONTINUE
         END IF
      END IF
C
      NMOPB = NMOPB + NXOUT
      NVOPU = NXOUT
C
      RETURN 
C
C**** end of DLZNRMX ***************************************************
C
      END
