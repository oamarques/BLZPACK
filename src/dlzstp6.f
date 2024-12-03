      SUBROUTINE DLZSTP6 (IR,NVB,LNI,NI,R,U,EIGON,GNRZD)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DLZSTP6 sets (U) for reverse communication                       *
C*                                                                     *
C*           (U) = op(B)*(Q) if GNRZD = .TRUE.                         *
C*           (U) = (Q)       if GNRZD = .FALSE.                        *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    IR    (aii) : pointers for (R)                                   *
C*    NVB   (sii) : number of vectors in a block                       *
C*    LNI   (sii) : leading dimension of (U), (V) and (X)              *
C*    NI    (sii) : dimension of the vectors in (U), (V) and (X)       *
C*    R     (arb) : work array for Lanczos vectors                     *
C*    U     (arb) : array for reverse communication, U(LNI,NVB)        *
C*    EIGON (sli) : eigenvectors computation flag                      *
C*    GNRZD (sli) : problem type flag                                  *
C*                                                                     *
C*  - Subprogram:                                                      *
C*                                                                     *
C*    DLZCOPY                                                          *
C*                                                                     *
C***********************************************************************
C
C==== arguments ========================================================
C
      INTEGER          LNI,NI,NVB
      LOGICAL          EIGON,GNRZD
C
      INTEGER          IR(4)
      DOUBLE PRECISION R(*),U(*)
C
C**** executable statements ********************************************
C
      IF ( GNRZD ) THEN
C
         IF ( EIGON ) THEN
C
C.......... (U) <--- (Q), refinement of eigenvectors in DRVMNGR .........
C
            CALL DLZCOPY (NI, NI,NI,NVB,R(IR(1)),U)
C
         ELSE
C
C.......... (U) <--- op(B)*(Q), reverse communication ..................
C
            CALL DLZCOPY (NI,LNI,NI,NVB,R(IR(3)),U)
C
         END IF
C
      ELSE
C
C....... (U) <--- (Q), reverse communication ...........................
C
         CALL DLZCOPY (NI,LNI,NI,NVB,R(IR(1)),U)
C
      END IF
C
      RETURN 
C
C**** end of DLZSTP6 ***************************************************
C
      END
