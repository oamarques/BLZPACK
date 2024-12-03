      INTEGER FUNCTION IRVSTOR (ISTOR,VNAME)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    IRVSTOR is a function for retrieving internal variables          *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    ISTOR (aii) : array for integer variables                        *
C*    VNAME (sci) : variable name (case sensitive)                     *
C*                                                                     *
C***********************************************************************
C
C==== parameters =======================================================
C
      INCLUDE   'blz_address.h'
C
C==== arguments ========================================================
C
      INTEGER   ISTOR(*)
      CHARACTER VNAME*(*)
C
C==== local variable ===================================================
C
      INTEGER   ISTART,RSTART
C
C**** executable statements ********************************************
C
      ISTART = IINIT - 1
      RSTART = RINIT - 1
C
      IF      ( VNAME .EQ. 'INDSI'  ) THEN
              IRVSTOR = ISTOR(ISTART+INDSI)
      ELSE IF ( VNAME .EQ. 'JL'     ) THEN
              IRVSTOR = ISTOR(ISTART+JL)
      ELSE IF ( VNAME .EQ. 'JLMAX'  ) THEN
              IRVSTOR = ISTOR(ISTART+JLMAX)
      ELSE IF ( VNAME .EQ. 'JT'     ) THEN
              IRVSTOR = ISTOR(ISTART+JT)
      ELSE IF ( VNAME .EQ. 'JTMAX'  ) THEN
              IRVSTOR = ISTOR(ISTART+JTMAX)
      ELSE IF ( VNAME .EQ. 'JTMIN'  ) THEN
              IRVSTOR = ISTOR(ISTART+JTMIN)
      ELSE IF ( VNAME .EQ. 'LCOMM'  ) THEN
              IRVSTOR = ISTOR(ISTART+LCOMM)
      ELSE IF ( VNAME .EQ. 'LEIG'   ) THEN
              IRVSTOR = ISTOR(ISTART+LEIG)
      ELSE IF ( VNAME .EQ. 'LFILE'  ) THEN
              IRVSTOR = ISTOR(ISTART+LFILE)
      ELSE IF ( VNAME .EQ. 'LNI'    ) THEN
              IRVSTOR = ISTOR(ISTART+LNI)
      ELSE IF ( VNAME .EQ. 'LPRNT'  ) THEN
              IRVSTOR = ISTOR(ISTART+LPRNT)
      ELSE IF ( VNAME .EQ. 'LRERR'  ) THEN
              IRVSTOR = ISTOR(ISTART+LRERR)
      ELSE IF ( VNAME .EQ. 'LRMDE'  ) THEN
              IRVSTOR = ISTOR(ISTART+LRMDE)
      ELSE IF ( VNAME .EQ. 'LRWRN'  ) THEN
              IRVSTOR = ISTOR(ISTART+LRWRN)
      ELSE IF ( VNAME .EQ. 'LTAU'   ) THEN
              IRVSTOR = ISTOR(ISTART+LTAU)
      ELSE IF ( VNAME .EQ. 'MYPE'   ) THEN
              IRVSTOR = ISTOR(ISTART+MYPE)
      ELSE IF ( VNAME .EQ. 'N'      ) THEN
              IRVSTOR = ISTOR(ISTART+N)
      ELSE IF ( VNAME .EQ. 'NBX'    ) THEN
              IRVSTOR = ISTOR(ISTART+NBX)
      ELSE IF ( VNAME .EQ. 'NBXMAX' ) THEN
              IRVSTOR = ISTOR(ISTART+NBXMAX)
      ELSE IF ( VNAME .EQ. 'NDEIG'  ) THEN
              IRVSTOR = ISTOR(ISTART+NDEIG)
      ELSE IF ( VNAME .EQ. 'NEPIN'  ) THEN
              IRVSTOR = ISTOR(ISTART+NEPIN)
      ELSE IF ( VNAME .EQ. 'NEWSIG' ) THEN
              IRVSTOR = ISTOR(ISTART+NEWSIG)
      ELSE IF ( VNAME .EQ. 'NFARL'  ) THEN
              IRVSTOR = ISTOR(ISTART+NFARL)
      ELSE IF ( VNAME .EQ. 'NFARR'  ) THEN
              IRVSTOR = ISTOR(ISTART+NFARR)
      ELSE IF ( VNAME .EQ. 'NI'     ) THEN
              IRVSTOR = ISTOR(ISTART+NI)
      ELSE IF ( VNAME .EQ. 'NMOPA'  ) THEN
              IRVSTOR = ISTOR(ISTART+NMOPA)
      ELSE IF ( VNAME .EQ. 'NMOPB'  ) THEN
              IRVSTOR = ISTOR(ISTART+NMOPB)
      ELSE IF ( VNAME .EQ. 'NNSPNT' ) THEN
              IRVSTOR = ISTOR(ISTART+NNSPNT)
      ELSE IF ( VNAME .EQ. 'NNTRTL' ) THEN
              IRVSTOR = ISTOR(ISTART+NNTRTL)
      ELSE IF ( VNAME .EQ. 'NNTRTR' ) THEN
              IRVSTOR = ISTOR(ISTART+NNTRTR)
      ELSE IF ( VNAME .EQ. 'NONEWS' ) THEN
              IRVSTOR = ISTOR(ISTART+NONEWS)
      ELSE IF ( VNAME .EQ. 'NPE'    ) THEN
              IRVSTOR = ISTOR(ISTART+NPE)
      ELSE IF ( VNAME .EQ. 'NPORTH' ) THEN
              IRVSTOR = ISTOR(ISTART+NPORTH)
      ELSE IF ( VNAME .EQ. 'NQMAX'  ) THEN
              IRVSTOR = ISTOR(ISTART+NQMAX)
      ELSE IF ( VNAME .EQ. 'NREIG'  ) THEN
              IRVSTOR = ISTOR(ISTART+NREIG)
      ELSE IF ( VNAME .EQ. 'NREIGL' ) THEN
              IRVSTOR = ISTOR(ISTART+NREIGL)
      ELSE IF ( VNAME .EQ. 'NREIGR' ) THEN
              IRVSTOR = ISTOR(ISTART+NREIGR)
      ELSE IF ( VNAME .EQ. 'NRITZ'  ) THEN
              IRVSTOR = ISTOR(ISTART+NRITZ)
      ELSE IF ( VNAME .EQ. 'NRUN'   ) THEN
              IRVSTOR = ISTOR(ISTART+NRUN)
      ELSE IF ( VNAME .EQ. 'NRUNMX' ) THEN
              IRVSTOR = ISTOR(ISTART+NRUNMX)
      ELSE IF ( VNAME .EQ. 'NSFAIL' ) THEN
              IRVSTOR = ISTOR(ISTART+NSFAIL)
      ELSE IF ( VNAME .EQ. 'NSIGMA' ) THEN
              IRVSTOR = ISTOR(ISTART+NSIGMA)
      ELSE IF ( VNAME .EQ. 'NSIMAX' ) THEN
              IRVSTOR = ISTOR(ISTART+NSIMAX)
      ELSE IF ( VNAME .EQ. 'NSINT'  ) THEN
              IRVSTOR = ISTOR(ISTART+NSINT)
      ELSE IF ( VNAME .EQ. 'NSORTH' ) THEN
              IRVSTOR = ISTOR(ISTART+NSORTH)
      ELSE IF ( VNAME .EQ. 'NSRLS'  ) THEN
              IRVSTOR = ISTOR(ISTART+NSRLS)
      ELSE IF ( VNAME .EQ. 'NSRRS'  ) THEN
              IRVSTOR = ISTOR(ISTART+NSRRS)
      ELSE IF ( VNAME .EQ. 'NSVIN'  ) THEN
              IRVSTOR = ISTOR(ISTART+NSVIN)
      ELSE IF ( VNAME .EQ. 'NTEIG'  ) THEN
              IRVSTOR = ISTOR(ISTART+NTEIG)
      ELSE IF ( VNAME .EQ. 'NULLDQ' ) THEN
              IRVSTOR = ISTOR(ISTART+NULLDQ)
      ELSE IF ( VNAME .EQ. 'NULLDR' ) THEN
              IRVSTOR = ISTOR(ISTART+NULLDR)
      ELSE IF ( VNAME .EQ. 'NVB'    ) THEN
              IRVSTOR = ISTOR(ISTART+NVB)
      ELSE IF ( VNAME .EQ. 'NWBSY'  ) THEN
              IRVSTOR = ISTOR(ISTART+NWBSY)
      ELSE IF ( VNAME .EQ. 'NWMAX'  ) THEN
              IRVSTOR = ISTOR(ISTART+NWMAX)
      ELSE IF ( VNAME .EQ. 'NWMIN'  ) THEN
              IRVSTOR = ISTOR(ISTART+NWMIN)
      ELSE IF ( VNAME .EQ. 'NXMAX'  ) THEN
              IRVSTOR = ISTOR(ISTART+NXMAX)
      ELSE IF ( VNAME .EQ. 'ITIME'  ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+ITIME)
      ELSE IF ( VNAME .EQ. 'IRSINT' ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+IRSINT)
      ELSE IF ( VNAME .EQ. 'ISSLOG' ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+ISSLOG)
      ELSE IF ( VNAME .EQ. 'IRITZ'  ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+IRITZ)
      ELSE IF ( VNAME .EQ. 'ITB'    ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+ITB)
      ELSE IF ( VNAME .EQ. 'IALPHA' ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+IALPHA)
      ELSE IF ( VNAME .EQ. 'IBETAQ' ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+IBETAQ)
      ELSE IF ( VNAME .EQ. 'IBETAR' ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+IBETAR)
      ELSE IF ( VNAME .EQ. 'IANORM' ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+IANORM)
      ELSE IF ( VNAME .EQ. 'IBNORM' ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+IBNORM)
      ELSE IF ( VNAME .EQ. 'IETA'   ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+IETA)
      ELSE IF ( VNAME .EQ. 'ITAU'   ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+ITAU)
      ELSE IF ( VNAME .EQ. 'IR'     ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+IR)
      ELSE IF ( VNAME .EQ. 'ITHETA' ) THEN
              IRVSTOR = RSTART + ISTOR(ITHETA)
      ELSE IF ( VNAME .EQ. 'IS'     ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+IS)
      ELSE IF ( VNAME .EQ. 'IBASIS' ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+IBASIS)
      ELSE IF ( VNAME .EQ. 'IBX'    ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+IBX)
      ELSE IF ( VNAME .EQ. 'IRWORK' ) THEN
              IRVSTOR = RSTART + ISTOR(ISTART+IRWORK)
      ELSE IF ( VNAME .EQ. 'IIWORK' ) THEN
              IRVSTOR = ISTOR(ISTART+IIWORK)
      ELSE IF ( VNAME .EQ. 'INDR'   ) THEN
              IRVSTOR = ISTOR(ISTART+INDR)
      ELSE IF ( VNAME .EQ. 'LOPTS'  ) THEN
              IRVSTOR = ISTOR(ISTART+LOPTS)
      ELSE IF ( VNAME .EQ. 'LBLAS'  ) THEN
              IRVSTOR = ISTOR(ISTART+LBLAS)
      ELSE IF ( VNAME .EQ. 'FHNDL'  ) THEN
              IRVSTOR = ISTOR(ISTART+FHNDL)
      ELSE IF ( VNAME .EQ. 'ISINT'  ) THEN
              IRVSTOR = ISTOR(ISTART+ISINT)
      ELSE
              WRITE (*,'(2A)') 
     &        '* IRVSTOR, variable not defined: ',VNAME
              IRVSTOR = 0
      END IF
C
      RETURN
C
C**** end of IRVSTOR **************************************************
C
      END
