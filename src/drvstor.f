      DOUBLE PRECISION FUNCTION DRVSTOR (RSTOR,VNAME)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DRVSTOR is a function for retrieving internal variables          *
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
      DOUBLE PRECISION RSTOR(*)
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
              DRVSTOR = RSTOR(RSTART+BIGNUM)
      ELSE IF ( VNAME .EQ. 'EIGL'   ) THEN
              DRVSTOR = RSTOR(RSTART+EIGL)
      ELSE IF ( VNAME .EQ. 'EIGR'   ) THEN
              DRVSTOR = RSTOR(RSTART+EIGR)
      ELSE IF ( VNAME .EQ. 'ENDL'   ) THEN
              DRVSTOR = RSTOR(RSTART+ENDL)
      ELSE IF ( VNAME .EQ. 'ENDR'   ) THEN
              DRVSTOR = RSTOR(RSTART+ENDR)
      ELSE IF ( VNAME .EQ. 'EPS'    ) THEN
              DRVSTOR = RSTOR(RSTART+EPS)
      ELSE IF ( VNAME .EQ. 'EPS1'   ) THEN
              DRVSTOR = RSTOR(RSTART+EPS1)
      ELSE IF ( VNAME .EQ. 'GRNRM'  ) THEN
              DRVSTOR = RSTOR(RSTART+GRNRM)
      ELSE IF ( VNAME .EQ. 'ORIGIN' ) THEN
              DRVSTOR = RSTOR(RSTART+ORIGIN)
      ELSE IF ( VNAME .EQ. 'RADIUS' ) THEN
              DRVSTOR = RSTOR(RSTART+RADIUS)
      ELSE IF ( VNAME .EQ. 'REPS'   ) THEN
              DRVSTOR = RSTOR(RSTART+REPS)
      ELSE IF ( VNAME .EQ. 'SFARL'  ) THEN
              DRVSTOR = RSTOR(RSTART+SFARL)
      ELSE IF ( VNAME .EQ. 'SFARR'  ) THEN
              DRVSTOR = RSTOR(RSTART+SFARR)
      ELSE IF ( VNAME .EQ. 'THETA0' ) THEN
              DRVSTOR = RSTOR(RSTART+THETA0)
      ELSE IF ( VNAME .EQ. 'THETAL' ) THEN
              DRVSTOR = RSTOR(RSTART+THETAL)
      ELSE IF ( VNAME .EQ. 'THETAR' ) THEN
              DRVSTOR = RSTOR(RSTART+THETAR)
      ELSE IF ( VNAME .EQ. 'THRSH'  ) THEN
              DRVSTOR = RSTOR(RSTART+THRSH)
      ELSE IF ( VNAME .EQ. 'TRUSTL' ) THEN
              DRVSTOR = RSTOR(RSTART+TRUSTL)
      ELSE IF ( VNAME .EQ. 'TRUSTR' ) THEN
              DRVSTOR = RSTOR(RSTART+TRUSTR)
      ELSE
              WRITE (*,'(2A)') '* DRVSTOR, variable not defined: ',VNAME
              DRVSTOR = 0
      END IF
C
      RETURN
C
C**** end of DRVSTOR ****************************************************
C
      END
