      SUBROUTINE DLZRANG (LRMDE,NDEIG,NPEIG,NREIG,NREIGL,NREIGR,
     &                    BIGNUM,EIGL,EIGR,ENDL,ENDR,RANGE,
     &                    SIGMA,THETA0,GNRZD)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DLZRANG checks the computational interval set by the user        *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    LRMDE  (sio) : run mode                                          *
C*                   = 0, done                                         *
C*                   = 1, ENDL=ENDR                                    *
C*                   = 2, ENDL<ENDR                                    *
C*                   = 3, ENDL>ENDR                                    *
C*                   = 4, split subinterval (ssorgn.f)                 *
C*                   = 5, reinitialization with fixed shift (ssorgn.f) *
C*                   = 6, extend bound or split subinterval (ssorgn.f) *
C*                   = 7, extend bound, SIGMA<=EIGL (ssorgn.f)         *
C*                   = 8, extend bound, SIGMA>=EIGR (ssorgn.f)         *
C*                   = 9, standard problem                             *
C*    NDEIG  (sio) : number of required eigenpairs                     *
C*    NPEIG  (sii) : number of eigenpairs given as input               *
C*    NREIG  (sii) : number of required eigenpairs                     *
C*    NREIGL (sio) : number of required eigenvalues less    than EIGR  *
C*    NREIGR (sio) : number of required eigenvalues greater than EIGL  *
C*    BIGNUM (sri) : big number                                        *
C*    EIGL   (sro) : inferior bound for eigenvalues                    *
C*    EIGR   (sro) : superior bound for eigenvalues                    *
C*    ENDL   (sri) : inferior bound for Ritz values                    *
C*    ENDR   (sri) : superior bound for Ritz values                    *
C*    RANGE  (ari) : user computational interval                       *
C*    SIGMA  (sro) : origin translation                                *
C*    THETA0 (sri) : reference point for THETA                         *
C*    GNRZD  (sli) : problem type flag                                 *
C*                                                                     *
C***********************************************************************
C
C==== parameters =======================================================
C
      DOUBLE PRECISION FUDGE,ZERO
      PARAMETER        (FUDGE=0.0010D0,ZERO=0.0D0)
C
C==== arguments ========================================================
C
      INTEGER          LRMDE,NDEIG,NPEIG,NREIG,NREIGL,NREIGR
      DOUBLE PRECISION BIGNUM,EIGL,EIGR,ENDL,ENDR,SIGMA,THETA0
      LOGICAL          GNRZD
C
      DOUBLE PRECISION RANGE(2)
C
C**** executable statements ********************************************
C
C.... eigenvalue range .................................................
C
      ENDL  = RANGE(1)
      ENDR  = RANGE(2)
      EIGL  = BIGNUM*(-1)
      EIGR  = BIGNUM*(+1)
C
C.... check the range for buckling mode ................................
C
      IF ( THETA0 .NE. ZERO ) THEN
         IF ( ENDL .EQ. ZERO ) ENDL = -FUDGE
         IF ( ENDR .EQ. ZERO ) ENDR = -FUDGE
      END IF
C
C.... first SIGMA ......................................................
C
      SIGMA = ENDL
C
      IF      ( .NOT.GNRZD ) THEN
C
C............ standard problem .........................................
C
              SIGMA  = ZERO
              LRMDE = 9
              NREIGL = 0
              NREIGR = 0
C
      ELSE IF ( ENDL .GT. ENDR ) THEN
C
C............ Lanczos run from right to left, interval [ENDR,ENDL] .....
C
              LRMDE = 3
              NREIGL = NREIG + NPEIG
              NREIGR = 0
C
              EIGL = ENDR
              EIGR = SIGMA   
C
      ELSE IF ( ENDL .LT. ENDR ) THEN
C
C............ Lanczos run from left to right, interval [ENDL,ENDR] .....
C
              LRMDE = 2
              NREIGL = 0
              NREIGR = NREIG + NPEIG
C
              EIGL = SIGMA   
              EIGR = ENDR
C
      ELSE 
C
C............ Lanczos run around ENDL (=ENDR) ..........................
C
              LRMDE = 1
              NREIGL = NREIG + NPEIG
              NREIGR = NREIG + NPEIG
C
      END IF
C
      NDEIG = NREIG
      ENDL  = EIGL
      ENDR  = EIGR
C
      RETURN 
C
C**** end of DLZRANG ***************************************************
C
      END
