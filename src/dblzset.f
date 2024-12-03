      SUBROUTINE DBLZSET (IPSET,RPSET,ISTOR,RSTOR,SIGMA,
     &                    LFLAG,AGAIN,STRON)
C
C***********************************************************************
C*                                                                     *
C*  - Purpose:                                                         *
C*                                                                     *
C*    DBLZSET initializes basic variables                              *
C*                                                                     *
C*  - Arguments:                                                       *
C*                                                                     *
C*    IPSET (aii) : integer input data                                 *
C*    RPSET (aii) : real input data                                    *
C*    ISTOR (aio) : array for integer variables                        *
C*    RSTOR (aro) : array for real variables                           *
C*    SIGMA (sro) : origin translation                                 *
C*    LFLAG (sio) : reverse communication flag                         *
C*    AGAIN (slo) : loop control flag                                  *
C*    STRON (slo) : run starting flag                                  *
C*                                                                     *
C*  - Subprograms:                                                     *
C*                                                                     *
C*    BLASDEF,DDATCHK,DDATSET,DLZERRS,DLZPNTR,DSETDFT,DSITIME          *
C*                                                                     *
C***********************************************************************
C
C==== parameters =======================================================
C
      INCLUDE          'blz_address.h'
C
      DOUBLE PRECISION ZERO
      PARAMETER        (ZERO=0.0D0)
C
C==== arguments ========================================================
C
      INTEGER          LFLAG
      DOUBLE PRECISION SIGMA
      LOGICAL          AGAIN,STRON
C
      INTEGER          IPSET(*),ISTOR(*)
      DOUBLE PRECISION RPSET(*),RSTOR(*)
C
C==== local variables ==================================================
C
      INTEGER          I,IBUSY,OFFSET,RBUSY
      DOUBLE PRECISION TIME0
C
      INTEGER          ITEMP(IUSED)
      DOUBLE PRECISION RTEMP(RUSED)
C
C==== subprogram =======================================================
C
      DOUBLE PRECISION DSITIME
C
C**** executable statements ********************************************
C
      TIME0 = DSITIME(ZERO)
C
C.... set all integer variables to zero ................................
C
      DO 10 I = 1,IUSED
         ITEMP(I) = 0
   10 CONTINUE
C
C.... set all real variables to zero ...................................
C
      DO 20 I = 1,RUSED
         RTEMP(I) = 0
   20 CONTINUE
C
C.... set defaults .....................................................
C
      CALL DSETDFT (ITEMP(JTMIN) ,ITEMP(NVB)   ,ITEMP(NRUNMX),
     &              ITEMP(NSIMAX),ITEMP(NXMAX) ,ITEMP(FHNDL) ,
     &              RTEMP(BIGNUM),RTEMP(EPS)   )
C
C.... check input data .................................................
C
      CALL DDATCHK (IPSET   ,RPSET      ,ITEMP(LRERR),
     &              ITEMP(N),ITEMP(MYPE),ITEMP(NPE)  )
C
C.... initialize variables .............................................
C
      IBUSY = IUSED
      RBUSY = RUSED
C
      CALL DDATSET (IPSET        ,RPSET        ,ITEMP(JLMAX) ,
     &              ITEMP(JTMAX) ,ITEMP(JTMIN) ,ITEMP(NVB)   ,
     &              ITEMP(LEIG)  ,ITEMP(LTAU)  ,ITEMP(LNI)   ,
     &              ITEMP(NI)    ,ITEMP(N)     ,ITEMP(LCOMM) ,
     &              ITEMP(MYPE)  ,ITEMP(NPE)   ,ITEMP(LFILE) ,
     &              ITEMP(LPRNT) ,ITEMP(LOPTS) ,ITEMP(LRERR) ,
     &              ITEMP(LRMDE) ,ITEMP(NQMAX) ,ITEMP(NBXMAX),
     &              ITEMP(NRUNMX),ITEMP(NSIMAX),ITEMP(NSVIN) ,
     &              RTEMP(BIGNUM),RTEMP(EPS)   ,RTEMP(EPS1)  ,
     &              RTEMP(REPS)  ,ITEMP(NDEIG) ,ITEMP(NEPIN) ,
     &              ITEMP(NREIG) ,ITEMP(NREIGL),ITEMP(NREIGR),
     &              ITEMP(NTEIG) ,RTEMP(EIGL)  ,RTEMP(EIGR)  ,
     &              RTEMP(ENDL)  ,RTEMP(ENDR)  ,SIGMA        ,
     &              RTEMP(THETA0),RTEMP(THRSH) ,IBUSY        ,
     &              RBUSY        ,AGAIN        ,STRON        )
C
C.... reset input flag .................................................
C
      IF      ( ITEMP(LRERR) .GT. 0 ) THEN
C
C             error in input data
C
              CALL DLZERRS (ITEMP(LFILE),ITEMP(LRERR))
              LFLAG = -ITEMP(LRERR)  
              RETURN
C
      ELSE IF ( IPSET(15).LE.0 .OR. RPSET(4).LE.0 ) THEN
C
C             workspace query
C
              IPSET(15) = IBUSY
              RPSET( 4) = RBUSY
              RETURN
C
      ELSE IF ( ITEMP(LOPTS) .GT. 0 ) THEN
C
C             generalized eigenproblem
C
              LFLAG = 3  
C
      END IF
C
C.... copy ITEMP into ISTOR and RTEMP into RSTOR .......................
C
      DO 30 I = 1,IUSED
         ISTOR(I) = ITEMP(I)
   30 CONTINUE
      DO 40 I = 1,RUSED 
         RSTOR(I) = RTEMP(I)
   40 CONTINUE
C
C.... set pointers for the work array ..................................
C
      OFFSET = RUSED + 1
C
C     Note that DLZPNTR defines 18 addresses from ISTOR(ITIME)
C
      CALL DLZPNTR (OFFSET       ,ISTOR(ITIME) ,ISTOR(JLMAX) ,
     &              ISTOR(JTMAX) ,ISTOR(NVB)   ,ISTOR(LTAU)  ,
     &              ISTOR(NI)    ,ISTOR(NQMAX) ,ISTOR(NBXMAX),
     &              ISTOR(NRUNMX),ISTOR(NSIMAX),ISTOR(LOPTS) ,
     &              ISTOR(NWBSY) ,ISTOR(NWMAX) ,ISTOR(NWMIN) )
C
      ISTOR(IIWORK) = ISINT + ISTOR(NSIMAX)*2
C
      DO 50 I = ISINT,ISTOR(IIWORK)
         ISTOR(I) = 0
   50 CONTINUE
      DO 60 I = ISTOR(ITIME),ISTOR(IRWORK)
         RSTOR(I) = 0
   60 CONTINUE
C
C.... set the BLAS level ...............................................
C
      CALL BLASDEF (ISTOR(LBLAS),ISTOR(NI),ISTOR(NVB))
C
C.... set the starting time ............................................
C
      RSTOR(ISTOR(ITIME)+12) = TIME0
C
      RETURN 
C
C**** end of DBLZSET ***************************************************
C
      END
