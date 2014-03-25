      SUBROUTINE RDSET( SETREQ, IUNIT, UOPEN, ISETF )
C
C     Routine for SCHED that reads the setup files.
C     Also sets defaults.
C
C     Routine called by SCHIN for in-stream setup files (IUNIT=5) 
C     and by GETSET for external files (IUNIT = IUSET).
C
C     UOPEN tells whether the file needs to be opened (external)
C     of not (imbedded in input file).
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER           KS, ICHAN, IER, IUNIT, IIF, IP, ISETF
      INTEGER           VLBOPE, I, LEN1
      INTEGER           I1, I2, I3, I4, I5, I6, I7, I8
      DOUBLE PRECISION  FREQSET(MCHAN), FREQOFF(MCHAN), ZZZZ
      LOGICAL           UOPEN, ALLOMIT, JUSTVLA
      CHARACTER         RESULT*256, SETREQ*(*), SETRQ*80
      CHARACTER         LFE(4)*6
      DOUBLE PRECISION  BLANK, ASTRO
C
C     For KEYIN.
C
      INTEGER           MSETV, MODE, KEYPTR
      PARAMETER         (MSETV=150 + MANT + 21*MCHAN)
      INTEGER           KI(MSETV)
      DOUBLE PRECISION  KD(2*MSETV), ENDMARK
      CHARACTER         KC(MSETV)*8, KCHAR*256
      SAVE              KI, KD, KC, ENDMARK, BLANK
      SAVE              ZZZZ, ASTRO
C ---------------------------------------------------------------
C     Facilitate debugging of routines that don't have sched.inc,
C     or, by setting SDEBUG true, debugging the frequency setting.
C
      SDEBUG = DEBUG
C      SDEBUG = .TRUE.
      IF( SDEBUG ) WRITE(*,*) 'RDSET SDEBUG=', SDEBUG
C
C     Write some debugging output.
C
      IF( DEBUG ) THEN
         SETMSG = ' '
         WRITE( SETMSG, '( A, I4, 1X, L1, 2X, A )' )
     1       'RDSET: Reading setup', ISETF, UOPEN, 
     2       SETREQ(1:LEN1(SETREQ))
         CALL WLOG( 0, SETMSG )
      END IF
C
C     Following needed for portability:
C
      SETRQ = SETREQ
C
C     Set the input variables if needed.  Trigger on NSET = 0
C     so that this is done on a restart to reset all inputs.
C
      IF( NSET .EQ. 0 ) THEN
         CALL KPACK( '/       ', ENDMARK )
         CALL KPACK( '        ', BLANK )
         CALL KPACK( 'ZZZZ    ', ZZZZ )
         CALL KPACK( 'ASTRO   ', ASTRO )
         KI(1) = MSETV
         KI(2) = 0
         KI(3) = 3
         CALL INSET( KD, KC, KI )
      END IF
C
C     Complain if this is the second time this file is requested.
C
      IF( NSET .GT. 0 ) THEN
         DO I = 1, NSET
            IF( SETNAME(I) .EQ. SETREQ ) THEN
               CALL WLOG( 1, 'RDSET: Duplicate setup file name? ' )
               CALL WLOG( 1, 'RDSET:' // SETRQ )
               CALL ERRLOG( 'Check setup file names' )
            END IF
         END DO
      END IF
C
C     Open setup file.  Put output string first so error messages
C     make more sense.
C
      CALL WLOG( 0, 'RDSET:   Reading setup file:      '//
     1              SETRQ(1:LEN1(SETRQ) ) )
      IF( UOPEN ) THEN
         IER = VLBOPE( IUNIT, SETREQ, 'TEXT', 'OLD', RESULT )
         IF( IER .NE. 1 ) THEN
            CALL WLOG( 1, RESULT )
            CALL ERRLOG( 'RDSET: Problem opening setup file' )
         END IF
      END IF
C
C     Set defaults of some individual parameters. First set all to 0.D0,
C     then set a few to reasonable defaults or to non-zero values for
C     use as defaults.  Set strings to blank.  Some defaults will
C     be established after the reads, especially those that depend
C     on the number of channels.  For some items, 0 is a legit value
C     so the default is set to UNSET so it is possible to detect change.
C
      DO I = 1, KI(2)
         KD(I) = 0.D0
      END DO
      KD( KEYPTR( 'ENDSET', KC, KI ) ) = UNSET
      KD( KEYPTR( 'PERIOD', KC, KI ) ) = UNSET
      KD( KEYPTR( 'LEVEL', KC, KI ) ) = UNSET
      KD( KEYPTR( 'DUALX', KC, KI ) ) = UNSET
      KD( KEYPTR( 'MODETEST', KC, KI ) ) = UNSET
      KD( KEYPTR( 'FRSWITCH', KC, KI ) ) = UNSET
      KD( KEYPTR( 'FORMAT', KC, KI ) ) = BLANK
      KD( KEYPTR( 'DBE', KC, KI ) ) = BLANK
      KD( KEYPTR( 'SWTCHDUR', KC, KI ) ) = 15.0
      KD( KEYPTR( 'BAND', KC, KI ) ) = BLANK
      I1 = KEYPTR( 'FIRSTLO', KC, KI ) - 1
      I2 = KEYPTR( 'IFCHAN', KC, KI ) - 1
      I3 = KEYPTR( 'POL', KC, KI ) - 1
      DO I = 1, MCHAN
         KD(I1+I) = UNSET
         KD(I2+I) = BLANK
         KD(I3+I) = BLANK
      END DO
      I1 = KEYPTR( 'STRING1', KC, KI ) - 1
      I2 = KEYPTR( 'STRING2', KC, KI ) - 1
      I3 = KEYPTR( 'STRING3', KC, KI ) - 1
      I4 = KEYPTR( 'STRING4', KC, KI ) - 1
      I5 = KEYPTR( 'FIRMFILE', KC, KI ) - 1
      DO I = 1, 10
         KD(I1+I) = BLANK
         KD(I2+I) = BLANK
         KD(I3+I) = BLANK
         KD(I4+I) = BLANK
         KD(I5+I) = BLANK
      END DO
      KD( KEYPTR( 'FEFILTER', KC, KI ) ) = ZZZZ
      KD( KEYPTR( 'VLABW', KC, KI ) ) = ZZZZ
      KD( KEYPTR( 'VLABAND', KC, KI ) ) = ZZZZ
      I1 = KEYPTR( 'VLAIF', KC, KI ) - 1
      I2 = KEYPTR( 'VLAROT', KC, KI ) - 1
      DO I = 1, 2
         KD(I1+I) = BLANK
         KD(I2+I) = BLANK
      END DO
      DO I = 1, 4
         LFE(I) = 'omit'
      END DO
      KD( KEYPTR( 'M4PATCH', KC, KI ) ) = ASTRO
C
C     Read the groups from the setup file.
C
  100 CONTINUE
         MODE = 0
C
C        Defaulting on FE is a bit different.  A FE(1),FE(3) 
C        combination can replace a FE(2),FE(4) combination.
C
         I1 = KEYPTR( 'FE', KC, KI ) - 1
         DO I = 1, 4
            KD(I+I1) = 0.D0
         END DO
C
C        Don't default the station names!
C
         I1 = KEYPTR( 'STATION', KC, KI ) - 1
         DO I = 1, MANT
            KD(I+I1) = 0.D0
         END DO
C
         CALL KEYIN( KD(MSETV+1), KD, KI(2), ENDMARK, MODE, IUNIT, 6 )
C
C        Assume last input was last if EOF found or ENDSET specified.
C
         IF( MODE .EQ. 1 ) GO TO 999
         IF( KD( KEYPTR( 'ENDSET', KC, KI ) ) .EQ. 0.D0 ) GO TO 999
C
C        Have a new setup group.  Increment NSET.  KS is for consistency
C        with other routines that use setup information.
C
         NSET = NSET + 1
         KS = NSET
         IF( NSET .GT. MSET ) THEN
            CALL ERRLOG( 'RDSET: Too many setup groups ' )
         END IF
C
C        Store setup name and number.  Get stations.  Note that later,
C        in SETEXPND, a new setup group will be created for each
C        station to allow station dependent defaults to be installed.
C
         SETNAME(KS) = SETREQ
         ISETNUM(KS) = ISETF
         I1 = KEYPTR( 'STATION', KC, KI ) - 1
         DO I = 1, MANT
            IF( KD(I1+I) .EQ. 0.D0 ) THEN
               SETSTA(I,KS) = ' '
            ELSE
               WRITE( SETSTA(I,KS), '( A8 ) ' ) KD(I1+I)
            END IF
            CALL UPCASE( SETSTA(I,KS) )
         END DO
C
C        Logging type - default to 'STANDARD'.  Case sensitive if not
C        STANDARD, NONE, or POINTING!
C
         I1 = KEYPTR( 'LOGGING', KC, KI )
         IF( KD(I1) .EQ. 0.D0 ) THEN
            LOGGING(KS) = 'STANDARD'
         ELSE
            WRITE( LOGGING(KS), '(A8)' ) KD(I1)
         END IF
C
C        Noise cal switching time for Pie Town - VLA link observations.
C 
         IF( KD( KEYPTR( 'NOISEFRQ', KC, KI ) ) .NE. 0.D0 ) THEN
            NOISEFRQ(KS) = KCHAR( 'NOISEFRQ', 4, .TRUE., KD, KC, KI )
         ELSE
            NOISEFRQ(KS) = 'VLBA'
         END IF
C
C        Get specifications related to the IFs from the antenna
C        (on the VLBA, this is the 500-1000 MHz IF A, B, C, or D)
C        These are the FE, NOISE, and IFDIST inputs.
C        For the FE, the defaults are a bit tricky as mentioned
C        earlier.  I need to sense if there was a change from
C        FE(1)(3) to FE(2)(4) or visa versa.
C
         ALLOMIT = .TRUE.
         I1 = KEYPTR( 'FE', KC, KI ) - 1
         DO IIF = 1, 4
            IF( KD(I1+IIF) .NE. 0.D0 ) THEN
               WRITE( FE(IIF,KS), '(A5)' ) KD(I1+IIF)
               CALL DWCASE( FE(IIF,KS) )
               ALLOMIT = .FALSE.
            ELSE
               FE(IIF,KS) = 'omit'
            END IF
         END DO
         IF( ALLOMIT ) THEN
            DO IIF = 1, 4
               FE(IIF,KS) = LFE(IIF)
            END DO
         END IF
         DO IIF = 1, 4
            LFE(IIF) = FE(IIF,KS)
         END DO
C
C        Get noise cal state and IF distributer attenuation/input.
C        Default noise to low switching.
C
         I2 = KEYPTR( 'NOISE', KC, KI ) - 1
         I3 = KEYPTR( 'IFDIST', KC, KI ) - 1
         DO IIF = 1, 4
            IF( KD(I2+IIF) .NE. 0.D0 ) THEN
               WRITE( NOISE(IIF,KS), '(A6)' ) KD(I2+IIF)
               CALL DWCASE( NOISE(IIF,KS) )
            ELSE
               NOISE(IIF,KS) = 'low-s'
            END IF
C
C           ifdist command can now be 0, 20, A, or 20A to allow
C           for alternate inputs - eg OVRO mm experiments.  To
C           avoid having to change all old setup files, allow
C           integer input for 0 and 20 and accept character inputs
C           for all allowed values.  Values will be checked later.
C
            IF( KD(I3+IIF) .EQ. 0.D0 ) THEN
               IFDIST(IIF,KS) = '0  '
            ELSE IF( KD(I3+IIF) .EQ. 20.D0 ) THEN
               IFDIST(IIF,KS) = '20 '
            ELSE 
               WRITE( IFDIST(IIF,KS), '(A3)' ) KD(I3+IIF)
               CALL UPCASE( IFDIST(IIF,KS) )
            END IF
         END DO
C
C        Pointing.
C
         AZCOLIM(KS)  = KD( KEYPTR( 'AZCOLIM', KC, KI ) )
         ELCOLIM(KS)  = KD( KEYPTR( 'ELCOLIM', KC, KI ) )
         PTINCR(KS)   = KD( KEYPTR( 'PTINCR', KC, KI ) )
         PTOFF(KS)    = KD( KEYPTR( 'PTOFF', KC, KI ) )
         IF( PTOFF(KS) .LE. 0.D0 ) PTOFF(KS) = 6.0 * PTINCR(KS)
C
C        Verious obvious parameters.
C
         PERIOD(KS)   = KD( KEYPTR( 'PERIOD', KC, KI ) )
         LEVEL(KS)    = KD( KEYPTR( 'LEVEL', KC, KI ) )
         SAMPRATE(KS) = KD( KEYPTR( 'SAMPRATE', KC, KI ) )
         DUALX(KS)    = KD( KEYPTR( 'DUALX', KC, KI ) ) .EQ. 0.D0
         I1 = KEYPTR( 'SYNTH', KC, KI ) - 1
         SYNTH(1,KS)  = KD(I1+1)
         SYNTH(2,KS)  = KD(I1+2)
         SYNTH(3,KS)  = KD(I1+3)
         FRSWITCH(KS) = KD( KEYPTR( 'FRSWITCH', KC, KI ) ) .EQ. 0.D0
         SWTCHDUR(KS) = KD( KEYPTR( 'SWTCHDUR', KC, KI ) )
         TPMODE(KS)   = KD( KEYPTR( 'TPMODE', KC, KI ) )
         FORMAT(KS)   = KCHAR( 'FORMAT', 8, .TRUE., KD, KC, KI )
         MODETEST(KS) = KD( KEYPTR( 'MODETEST', KC, KI ) ) .EQ. 0.D0
C
C        DBE is the digital backend type, perhaps combined with a
C        personality description.  FIRMFILE is the specific firmware 
C        file that should be loaded at the station for use in tests.
C        If you don't know about it, ignore it.
C
         DBE(KS)      = KCHAR( 'DBE', 8, .TRUE., KD, KC, KI )
         FIRMFILE(KS) = KCHAR( 'FIRMFILE', 80, .FALSE., KD, KC, KI )
C
         I1 = KEYPTR( 'BARREL', KC, KI )
         IF( KD(I1) .EQ. 0.D0 ) THEN
            BARREL(KS) = 'not_set'
         ELSE
            BARREL(KS)   = KCHAR( 'BARREL', 9, .FALSE., KD, KC, KI )
         END IF
         CALL DWCASE( BARREL(KS) )
C
C        Tape speed.  This is obsolete stuff and is not used in
C        SCHED.  Only keep in case of encountering files where
C        the parameters get set.
C
         SPEEDH(KS)    = KD( KEYPTR( 'TPSPEEDH', KC, KI ) )
         SPEEDL(KS)    = KD( KEYPTR( 'TPSPEEDL', KC, KI ) )
         I1 = KEYPTR( 'TPSPEED', KC, KI )
         IF( KD(I1) .NE. 0.D0 ) THEN
C             be silent            CALL WRTMSG( 0, 'RDSET', 'oldspeed' )
            IF( SPEEDL(KS) .EQ. 0.0 ) THEN
C             be silent       CALL WLOG( 0, 'RDSET: TPSPEED used for TPSPEEDL' )
               SPEEDL(KS) = KD(I1)
            END IF
         END IF
C
C        50 cm filter.  Set actual default in SETFCAT.
C
         I1 = KEYPTR( 'LCP50CM', KC, KI )
         IF( KD(I1) .EQ. 0.D0 ) THEN
            LCP50CM(KS) = 'DEF'
         ELSE
            WRITE( LCP50CM(KS), '(A6)' ) KD(I1)
            CALL UPCASE( LCP50CM(KS) )
         END IF
         I1 = KEYPTR( 'RCP50CM', KC, KI )
         IF( KD(I1) .EQ. 0.D0 ) THEN
            RCP50CM(KS) = 'DEF'
         ELSE
            WRITE( RCP50CM(KS), '(A6)' ) KD(I1)
            CALL UPCASE( RCP50CM(KS) )
         END IF
C
C        Set some defaults that require the input KD.
C
         IF( KD( KEYPTR( 'PERIOD', KC, KI ) ) .EQ. UNSET ) 
     1          PERIOD(KS) = 1
         IF( KD( KEYPTR( 'LEVEL', KC, KI ) ) .EQ. UNSET ) 
     1          LEVEL(KS) = -1
C
C        RCP and LCP channels
C
         I1 = KEYPTR( 'RCHAN', KC, KI )
         IF( KD(I1) .NE. 0.D0 ) THEN
            WRITE( RCHAN(KS), '(A2)' ) KD(I1)
            CALL UPCASE( RCHAN(KS) )
         ELSE
            RCHAN(KS) = ' '
         END IF
C
         I1 = KEYPTR( 'LCHAN', KC, KI )
         IF( KD(I1) .NE. 0.D0 ) THEN
            WRITE( LCHAN(KS), '(A2)' ) KD(I1)
            CALL UPCASE( LCHAN(KS) )
         ELSE
            LCHAN(KS) = ' '
         END IF
C
C        Get the number of channels.  Allow 0 for VLA only projects.
C
         NCHAN(KS) = KD( KEYPTR( 'NCHAN', KC, KI ) )
         IF( NCHAN(KS) .GT. MCHAN ) THEN
            CALL ERRLOG( 'RDSET: Too many ' //
     1                   ' channels in ' // SETRQ(1:LEN1(SETRQ)) )
         END IF
C
C        For in-line setups, we don't know the value of VLAONLY
C        yet, so look for FORMAT=NONE and SETSTA = 'VLA' and
C        NCHAN = 0  
C
         JUSTVLA = .TRUE.
         DO I = 1, MANT
            IF( SETSTA(I,KS)(1:3) .NE. 'VLA' .AND. 
     1          SETSTA(I,KS)(1:3) .NE. ' ' ) JUSTVLA = .FALSE.
         END DO
         JUSTVLA = JUSTVLA .AND. FORMAT(KS) .EQ. 'NONE' .AND.
     1             NCHAN(KS) .EQ. 0
C
C        Channel parameters.  
C
C        Some will be set to the first value if the
C        users does not set them.  Note that this just sets them all to 
C        zero if the user does not set any.  To test for this, test
C        KD, not the parameter.  Then the user can change all
C        elements by only changing the first.
C        The parameters set this way are FIRSTLO, BBFILT, BITS,
C        FREQREF, SIDEBAND, and NETSIDE
C
         IF( .NOT. JUSTVLA ) THEN
            WRITE( BAND(KS), '(A5)' ) KD( KEYPTR( 'BAND', KC, KI ) )
            CALL DWCASE( BAND(KS) )

            IF( NCHAN(KS) .LT. 1 ) THEN
               CALL ERRLOG( 'RDSET: NCHAN must be greater than 0.' )
            END IF
            I1 = KEYPTR( 'FIRSTLO', KC, KI ) - 1
            I2 = KEYPTR( 'BBSYN', KC, KI ) - 1
            I3 = KEYPTR( 'BBSYN2', KC, KI ) - 1
            I4 = KEYPTR( 'BBFILTER', KC, KI ) - 1
            DO ICHAN = 1,NCHAN(KS)
               FIRSTLO(ICHAN,KS)  = KD(I1+ICHAN)
               IF( KD(I1+ICHAN) .EQ. UNSET ) 
     1              FIRSTLO(ICHAN,KS) = FIRSTLO(1,KS)
               BBSYN(ICHAN,KS)    = KD(I2+ICHAN)
               BBSYN2(ICHAN,KS)   = KD(I3+ICHAN)
               BBFILT(ICHAN,KS)   = KD(I4+ICHAN)
               IF( KD(I4+ICHAN) .EQ. 0.D0 ) 
     1              BBFILT(ICHAN,KS) = BBFILT(1,KS)
            END DO
C
            I1 = KEYPTR( 'SIDEBAND', KC, KI ) - 1
            I2 = KEYPTR( 'NETSIDE', KC, KI ) - 1
            I3 = KEYPTR( 'IFCHAN', KC, KI ) - 1
            I4 = KEYPTR( 'BITS', KC, KI ) - 1
            I5 = KEYPTR( 'BBC', KC, KI ) - 1
            I6 = KEYPTR( 'FREQOFF', KC, KI ) - 1
            I7 = KEYPTR( 'FREQREF', KC, KI ) - 1
            I8 = KEYPTR( 'POL', KC, KI ) - 1
            DO ICHAN = 1, NCHAN(KS)
               WRITE( SIDEBD(ICHAN,KS), '(A1)' ) KD(ICHAN+I1)
               CALL UPCASE( SIDEBD(ICHAN,KS) )
               IF( KD(ICHAN+I1) .EQ. 0.D0 ) 
     1              SIDEBD(ICHAN,KS) = SIDEBD(1,KS)
               WRITE( NETSIDE(ICHAN,KS),'(A1)' ) KD(ICHAN+I2)
               CALL UPCASE( NETSIDE(ICHAN,KS) )
               IF( KD(ICHAN+I2) .EQ. 0.D0 ) 
     1              NETSIDE(ICHAN,KS) = NETSIDE(1,KS)
C
               WRITE( IFCHAN(ICHAN,KS), '(A2)' ) KD(ICHAN+I3)
               CALL UPCASE( IFCHAN(ICHAN,KS) )
C
               BITS(ICHAN,KS)     = KD(ICHAN+I4)
               IF( ICHAN.EQ.1 .AND. BITS(ICHAN,KS).EQ.0 ) BITS(1,KS) = 1
               IF( KD(ICHAN+I4) .EQ. 0.D0 ) 
     1                 BITS(ICHAN,KS) = BITS(1,KS)
C
               BBC(ICHAN,KS)      = KD(ICHAN+I5)
C
               FREQOFF(ICHAN)     = KD(ICHAN+I6)
               FREQSET(ICHAN)     = KD(ICHAN+I7)
               IF( KD(ICHAN+I7) .EQ. 0.D0 ) 
     1                 FREQSET(ICHAN) = FREQSET(1)
               FREQREF(ICHAN,KS) = FREQSET(ICHAN) + FREQOFF(ICHAN)
C
C              Polarization stuff.
C
               WRITE( POL(ICHAN,KS), '(A4)' ) KD(ICHAN+I8)
               IF( POL(ICHAN,KS) .EQ. ' ' )
     1                 POL(ICHAN,KS) = POL(1,KS)
               CALL UPCASE( POL(ICHAN,KS) )
            END DO
C
C           Work on the polarization defaulting.
C
            DO ICHAN = 1, NCHAN(KS)
C
C              Allow dodos like me to forget that POL should be RCP
C              or LCP and just specify R or L.
C
               IF( POL(ICHAN,KS) .EQ. 'R' .AND. 
     1             LEN1( POL(ICHAN,KS) ) .EQ. 1 ) POL(ICHAN,KS) = 'RCP'
               IF( POL(ICHAN,KS) .EQ. 'L' .AND. 
     1             LEN1( POL(ICHAN,KS) ) .EQ. 1 ) POL(ICHAN,KS) = 'LCP'
C
C              Try to allow IFCHAN of R or L to establish POL if POL
C              was not already set.
C
               IF( IFCHAN(ICHAN,KS) .EQ. 'R' .AND. 
     1             POL(ICHAN,KS) .EQ. ' ' )
     2                 POL(ICHAN,KS) = 'RCP'
               IF( IFCHAN(ICHAN,KS) .EQ. 'L' .AND. 
     1             POL(ICHAN,KS) .EQ. ' ' )
     2                 POL(ICHAN,KS) = 'LCP'
C
C              If R or L were specified, set to RCHAN and LCHAN.  If
C              those are blank, that is ok - that is what we want to
C              trigger getting the values from the frequency catalog
C              now that POL is set.
C
               IF( IFCHAN(ICHAN,KS) .EQ. 'R' )
     1                 IFCHAN(ICHAN,KS) = RCHAN(KS)
               IF( IFCHAN(ICHAN,KS) .EQ. 'L' )
     1                 IFCHAN(ICHAN,KS) = LCHAN(KS)
            END DO
C
            I1 = KEYPTR( 'TRACK1', KC, KI ) - 1
            I2 = KEYPTR( 'TRACK2', KC, KI ) - 1
            I3 = KEYPTR( 'TRACK3', KC, KI ) - 1
            I4 = KEYPTR( 'TRACK4', KC, KI ) - 1
            I5 = KEYPTR( 'TRACK5', KC, KI ) - 1
            I6 = KEYPTR( 'TRACK6', KC, KI ) - 1
            I7 = KEYPTR( 'TRACK7', KC, KI ) - 1
            I8 = KEYPTR( 'TRACK8', KC, KI ) - 1
            DO ICHAN = 1, NCHAN(KS)
               TRACK(ICHAN,1,KS)  = KD(ICHAN+I1)
               TRACK(ICHAN,2,KS)  = KD(ICHAN+I2)
               TRACK(ICHAN,3,KS)  = KD(ICHAN+I3)
               TRACK(ICHAN,4,KS)  = KD(ICHAN+I4)
               TRACK(ICHAN,5,KS)  = KD(ICHAN+I5)
               TRACK(ICHAN,6,KS)  = KD(ICHAN+I6)
               TRACK(ICHAN,7,KS)  = KD(ICHAN+I7)
               TRACK(ICHAN,8,KS)  = KD(ICHAN+I8)
C
            END DO
C
         END IF ! NCHAN greater than zero.
C
C        Four arbitrary strings can be passed to the control file
C        This covers cases where a new observing parameter is 
C        introduced and SCHED does not yet understand it.
C
         STRING(1,KS) = KCHAR( 'STRING1', 80, .FALSE., KD, KC, KI )
         STRING(2,KS) = KCHAR( 'STRING2', 80, .FALSE., KD, KC, KI )
         STRING(3,KS) = KCHAR( 'STRING3', 80, .FALSE., KD, KC, KI )
         STRING(4,KS) = KCHAR( 'STRING4', 80, .FALSE., KD, KC, KI )
C
C        Get the VLA specific parameters.
C        These are now all obsolete as of 2012 or earlier.
C
         FLUKESET(KS) = KD( KEYPTR( 'FLUKESET', KC, KI ) )
         FLUKEA(KS)   = KD( KEYPTR( 'FLUKEA', KC, KI ) )
         FLUKEB(KS)   = KD( KEYPTR( 'FLUKEB', KC, KI ) )
         VLAFEAB(KS)  = KD( KEYPTR( 'VLAFEAB', KC, KI ) )
         VLAFECD(KS)  = KD( KEYPTR( 'VLAFECD', KC, KI ) )
         VLASYNA(KS)  = KD( KEYPTR( 'VLASYNA', KC, KI ) )
         VLASYNB(KS)  = KD( KEYPTR( 'VLASYNB', KC, KI ) )
         FEFILTER(KS) = KCHAR( 'FEFILTER', 4, .FALSE., KD, KC, KI )
         VLAIF(KS)    = KCHAR( 'VLAIF', 10, .FALSE., KD, KC, KI )
         VLAROT(KS)   = KCHAR( 'VLAROT', 10, .FALSE., KD, KC, KI )
         VLABAND(KS)  = KCHAR( 'VLABAND', 2, .TRUE., KD, KC, KI )
         VLABW(KS)    = KCHAR( 'VLABW', 4, .TRUE., KD, KC, KI )
C
C        Warn of use of obsolete parameters.
C
         IF( VLABAND(KS) .NE. 'ZZ' .OR.
     1       VLABW(KS) .NE. 'ZZZZ' .OR.
     2       FLUKESET(KS) .NE. 0.D0 .OR.
     3       FLUKEA(KS) .NE. 0.D0 .OR.
     4       FLUKEB(KS) .NE. 0.D0 .OR.
     5       VLAFEAB(KS) .NE. 0.D0 .OR. 
     6       VLAFECD(KS) .NE. 0.D0 .OR. 
     7       VLASYNA(KS) .NE. 0.D0 .OR. 
     8       VLASYNB(KS) .NE. 0.D0 .OR. 
     9       FEFILTER(KS) .NE. 'ZZZZ' .OR.
     A       VLAIF(KS) .NE. ' ' .OR.
     B       VLAROT(KS) .NE. ' ' ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( 2A, I4 )' )
     1         'RDSET - WARNING: Old VLA parameters used in ', 
     2         'input setup: ', KS
            CALL WLOG( 1, MSGTXT )
            MSGTXT = ' '
            WRITE( MSGTXT, '( 2A )' )
     1         '                 They are no longer used and ',
     2         'will be removed soon. '
            CALL WLOG( 1, MSGTXT )
         END IF
C
C        Get the PCAL information.  Convert SPCAL to the traditional
C        capitalization.  Recall KCHAR will return upcased version.
C
         I1 = KEYPTR( 'PCAL', KC, KI )
         IF( KD(I1) .EQ. 0.D0 ) THEN
            SPCAL(KS) = '1MHZ'
         ELSE
            SPCAL(KS) = KCHAR( 'PCAL', 4, .TRUE., KD, KC, KI )
         END IF
         IF( SPCAL(KS) .EQ. 'OFF' ) THEN
            SPCAL(KS) = 'off'
         ELSE IF( SPCAL(KS) .EQ. '1MHZ' ) THEN
            SPCAL(KS) = '1MHz'
         ELSE IF( SPCAL(KS) .EQ. '5MHZ' ) THEN
            SPCAL(KS) = '5MHz'
         ELSE
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A )' ) 
     1          'CHKSET: Invalid PCAL specification: ', SPCAL(KS)
            CALL ERRLOG( MSGTXT )
         END IF
C
C        Get the VLBA legacy system PCAL detector information.
C
         I1 = KEYPTR( 'PCALXB1', KC, KI ) - 1
         I2 = KEYPTR( 'PCALXB2', KC, KI ) - 1
         I3 = KEYPTR( 'PCALFR1', KC, KI ) - 1
         I4 = KEYPTR( 'PCALFR2', KC, KI ) - 1
         DO IP = 1, MAXPC
            IF( KD(I1+IP) .EQ. 0.D0 ) THEN
               PCALX1(IP,KS) = ' '
            ELSE
               WRITE( PCALX1(IP,KS), '(A3)' ) KD(I1+IP)
               CALL UPCASE( PCALX1(IP,KS) )
            END IF
C
            IF( KD(I2+IP) .EQ. 0.D0 ) THEN
               PCALX2(IP,KS) = ' '
            ELSE
               WRITE( PCALX2(IP,KS), '(A3)' ) KD(I2+IP)
               CALL UPCASE( PCALX2(IP,KS) )
            END IF
C
            PCALFR1(IP,KS) = KD(I3+IP)
            PCALFR2(IP,KS) = KD(I4+IP)
         END DO
C
C        Get the Mark4 patch specifier.
C
         M4PATCH(KS) = KCHAR( 'M4PATCH', 8, .TRUE., KD, KC, KI )
C
C     Go back for next group.
C
      GO TO 100
C
C     Close up for all cases.
C
  999 CONTINUE
C
C     Close setup file
C
      IF( UOPEN ) CLOSE( UNIT=IUNIT )
C
C     That's all folks!
C
      RETURN
      END
