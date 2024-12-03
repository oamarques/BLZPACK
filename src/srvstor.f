      REAL             FUNCTION SRVSTOR (RSTOR,VNAME)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    SRVSTOR is a function for retrieving internal variables          *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    RSTOR (ari) : array for real variables                           *
C*    VNAME (sci) : variable name (case sensitive)                     *
C*                                                                     *
C***********************************************************************
C
C==== parameters =======================================================
C
      INCLUDE          'blz_address.h'
C
C==== arguments ========================================================
C
      REAL             RSTOR(*)
      CHARACTER        VNAME*(*)
C
C==== local variable ===================================================
C
      INTEGER          RSTART
C
C**** executable statements ********************************************
C
      RSTART = RINIT - 1
C
      IF      ( VNAME .EQ. 'BIGNUM' ) THEN
              SRVSTOR = RSTOR(RSTART+BIGNUM)
      ELSE IF ( VNAME .EQ. 'EIGL'   ) THEN
              SRVSTOR = RSTOR(RSTART+EIGL)
      ELSE IF ( VNAME .EQ. 'EIGR'   ) THEN
              SRVSTOR = RSTOR(RSTART+EIGR)
      ELSE IF ( VNAME .EQ. 'ENDL'   ) THEN
              SRVSTOR = RSTOR(RSTART+ENDL)
      ELSE IF ( VNAME .EQ. 'ENDR'   ) THEN
              SRVSTOR = RSTOR(RSTART+ENDR)
      ELSE IF ( VNAME .EQ. 'EPS'    ) THEN
              SRVSTOR = RSTOR(RSTART+EPS)
      ELSE IF ( VNAME .EQ. 'EPS1'   ) THEN
              SRVSTOR = RSTOR(RSTART+EPS1)
      ELSE IF ( VNAME .EQ. 'GRNRM'  ) THEN
              SRVSTOR = RSTOR(RSTART+GRNRM)
      ELSE IF ( VNAME .EQ. 'ORIGIN' ) THEN
              SRVSTOR = RSTOR(RSTART+ORIGIN)
      ELSE IF ( VNAME .EQ. 'RADIUS' ) THEN
              SRVSTOR = RSTOR(RSTART+RADIUS)
      ELSE IF ( VNAME .EQ. 'REPS'   ) THEN
              SRVSTOR = RSTOR(RSTART+REPS)
      ELSE IF ( VNAME .EQ. 'SFARL'  ) THEN
              SRVSTOR = RSTOR(RSTART+SFARL)
      ELSE IF ( VNAME .EQ. 'SFARR'  ) THEN
              SRVSTOR = RSTOR(RSTART+SFARR)
      ELSE IF ( VNAME .EQ. 'THETA0' ) THEN
              SRVSTOR = RSTOR(RSTART+THETA0)
      ELSE IF ( VNAME .EQ. 'THETAL' ) THEN
              SRVSTOR = RSTOR(RSTART+THETAL)
      ELSE IF ( VNAME .EQ. 'THETAR' ) THEN
              SRVSTOR = RSTOR(RSTART+THETAR)
      ELSE IF ( VNAME .EQ. 'THRSH'  ) THEN
              SRVSTOR = RSTOR(RSTART+THRSH)
      ELSE IF ( VNAME .EQ. 'TRUSTL' ) THEN
              SRVSTOR = RSTOR(RSTART+TRUSTL)
      ELSE IF ( VNAME .EQ. 'TRUSTR' ) THEN
              SRVSTOR = RSTOR(RSTART+TRUSTR)
      ELSE
              WRITE (*,'(2A)') '* SRVSTOR, variable not defined: ',VNAME
              SRVSTOR = 0
      END IF
C
      RETURN
C
C**** end of SRVSTOR ****************************************************
C
      END
