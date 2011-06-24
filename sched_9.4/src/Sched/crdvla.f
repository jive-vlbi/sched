      SUBROUTINE CRDVLA( ISCN, ISTA, FIRSTS )
C
C     Writes Observe Deck card images for control of the VLA
C
C     This subroutine must be called after VLBA.  The SETUP file
C     is read from within VLBA.  The parameters for the LO and FI
C     cards are read there.  They will be checked here.
C     We now have the VLBA DAR at the VLA.  Now any small frequency
C     adjustments will be made there.  (29jun93)
C
C     Removed tape change comment July 20, 2010  RCW.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER          PMY, PMD, ISYNA, ISYNB, NEXP, I, KSTA, ISRC
      INTEGER          ISCN, ISTA, LEN1, SIDDAY, DAY, YEAR, USERN
      INTEGER          NCHAR, MCHAR, ISDAY, ISTDAY, IER, KS, ICH
      INTEGER          IY, IM, ID, JSTAT, IC1, IC2, JC1, JC2
      INTEGER          LVINTEG
      REAL             PDRA, PDDEC
      DOUBLE PRECISION PRA, PDEC, PPMTIME, DDRA, DDDEC, DIST, HPARAL
      DOUBLE PRECISION LLST, LSTOP, STOP, PMT, PMIAT, FD
      DOUBLE PRECISION JULMST, LSTDAY, LSTTIM
      DOUBLE PRECISION PLST, ONEMIN, ONEMINJ
      DOUBLE PRECISION LSTOPJ, PSTOPJ1, PSTOPJ2
      LOGICAL          FIRSTS, GOTLO, GOTFI ! WARN, GOTFQ (dar)
      LOGICAL          PADDED, WPS, WLO, WFI, WDS, PWARN
      LOGICAL          GOTLCP, GOTRCP
      CHARACTER        TFORM*15, PRCODE*6, UANNOT*64, TLST*15
      CHARACTER        VLASTR*50, RASCH*15, DECSCH*15, EPOCH*1
      CHARACTER        PMFT*9, FLKSETC*1
      CHARACTER        PRTSRC*13, CC*8, TMST*15
      CHARACTER        SSLINE*80, PSLINE*80, LOLINE*80, FILINE*80
      CHARACTER        DSLINE*80
      CHARACTER        PTLO*9, RPK*1, PHMODE*2
      SAVE             PADDED, PWARN, LLST, LSTOP, LSTOPJ, LVINTEG
C
      PARAMETER        (ONEMIN=RADHR/60.D0)            ! Rad/min
      PARAMETER        (ONEMINJ=1.D0/1440.D0)          ! Day/min
C
      DATA             CC / '//* *** ' /
      DATA             LVINTEG / -999 /
C
Cdar      DATA             WARN / .TRUE. /
C ---------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. 3 ) CALL WLOG( 0, 'CRDVLA: Starting.' )
C
C     Initialize.
C
      WPS = .FALSE.
      WLO = .FALSE.
      WFI = .FALSE.
      WDS = .FALSE.
C
C     Save some long subscripts later
C
      IF( ISCN .GE. 1 ) THEN
         ISRC = SRCNUM(ISCN)
         KSTA = STANUM(ISTA)
      END IF
C
      IF  (FIRSTS .AND. ISCN .NE. -999 ) THEN
C
C        First get lst start day and time, including sidereal day.
C
         CALL SIDTIM( STARTJ(ISCN), LONG(KSTA), TWOPI, SIDDAY, 
     1                LSTTIM, LSTDAY )
C
C        Put result in character variable for later printing.
C
         TLST = TFORM( LSTTIM, 'T', 0, 2, 2, '  @' )
C
C        Get the thousands and hundreds so a comma can be inserted
C        in the file.
C
         ISTDAY = SIDDAY / 1000
         ISDAY  = SIDDAY - ISTDAY * 1000
C
C        Get year, month, day, and MST of start time.
C        Use a Mountain Standard Julian Time (how's that for a new
C        concept?).  This allows SLA_DJCL to get the year, day etc
C        right, not to mention the fractional time.
C
         JULMST = STARTJ(ISCN) - ( 7.0D0 / 24.0D0 )
         CALL SLA_DJCL( JULMST, IY, IM, ID, FD, JSTAT )
         FD = FD * TWOPI
         TMST = TFORM( FD, 'T', 0, 2, 2, '::@' )
C
C        Now write the string with the times for the header.
C
         WRITE (VLASTR, '( I2.2, A, I3.3, A, A8, A, I4.4, A, I2.2, A,'//
     1     'I2.2, A, A8, A )' ) 
     2     ISTDAY, ',', ISDAY, ' at ', TLST, ' LST, ', IY, '.', IM, 
     3     '.', ID, ' ', TMST, ' MST.'
C
C        Get the VLA project code.
C
         NEXP = MIN( LEN1(EXPCODE), 6 )
         PRCODE = EXPCODE(1:NEXP)
         CALL UPCASE( PRCODE )
C
C        Find user number.
C
         USERN = VLAUSERN
         IF( USERN .EQ. 0 ) USERN = 600
C
C        Write the header lines.
C
         WRITE( IULOC, '( ''/.'', A, T9, I5 )' ) 
     1        PRCODE, USERN 
C
C        Write cover information.  This needs a very precise format
C        to keep OBSERVE happy when the files are remade by the
C        analysts.
C
         WRITE( IULOC, '( A,/, 2A, F6.2, 2X, A,/, A,/, 3A,/,A,/,2A )' )
     1      CC,
     2      CC, 'SCHED version: ', VERNUM, VERSION(1:LEN1(VERSION)),
     3      CC,
     4      CC, 'Observation day ', VLASTR,
     5      CC,
     6      CC, 'Observer'
C
         CALL CHARS( PINAME, 1, 50, IC1, IC2 )
         WRITE( IULOC, '( 2A, T65, A )' )  
     1      CC, PINAME(IC1:IC2),'Phone'
C
         CALL CHARS( PHONE, 1, 20, JC1, JC2 )
         WRITE( IULOC, '( 2A, T53, 2A )' )
     1      CC, ADDRESS(1)(1:24),'Office: ', PHONE(JC1:JC2)
C
         CALL CHARS( OBSPHONE, 1, 20, IC1, IC2 )
         WRITE( IULOC, '( 2A, T41, 2A )' )
     1      CC, ADDRESS(2)(1:24), 'During observation: ', 
     2      OBSPHONE(IC1:IC2)
C
         WRITE( IULOC, '( 2A, /, 2A, /, A, /, 2A, /, 2A, /, A )' )
     1      CC, ADDRESS(3)(1:24),
     2      CC, ADDRESS(4)(1:24),
     3      CC, 
     4      CC, 'E-Mail address',
     5      CC, EMAIL,
     6      CC
C
         WRITE( IULOC, '( 3A, /, A, /, 2A, /, 4( 2A, / ), A )' )
     1      CC, 'Observing mode(s): ', VLATYPE(1:LEN1(VLATYPE)),
     2      CC,
     3      CC, 'Special Instructions',
     4      CC, NOTE(1)(1:LEN1(NOTE(1))),
     5      CC, NOTE(2)(1:LEN1(NOTE(2))),
     6      CC, NOTE(3)(1:LEN1(NOTE(3))),
     7      CC, NOTE(4)(1:LEN1(NOTE(4))),
     8      CC
C
         IF( .NOT. NOTAPE ) WRITE( IULOC, '( A )' )
     1         '//* ------ MOUNT VLBI MEDIA BEFORE RUN STARTS  -------'
C
C        Write an escaper (EVLA control system) that sets the reference
C        antenna.  Note that this will be turned off locally for 
C        pointing scans.  See toward the end of this routine.
C
         IF( VLARFANT .LE. 9 ) THEN
            WRITE( IULOC, '( A, I1, A )' ) '//* & Grefant = [', 
     1         VLARFANT, ']'
         ELSE
            WRITE( IULOC, '( A, I2, A )' ) '//* & Grefant = [', 
     1         VLARFANT, ']'
         END IF
C
C        Initialize
C
         PADDED = .FALSE.
         PWARN = .FALSE.
         LLST = 0.D0
         LSTOP = 0.D0
         LSTOPJ = 0.D0
C
      END IF
C
C     Deal with end of schedule.
C 
      IF( ISCN .EQ. -999 ) THEN
C
C        Warn of phasing scan problems.
C
         IF( PWARN ) THEN
            WRITE( MSGTXT, '( A )' )
     1       ' *****  WARNING  *****  There were problems fitting '//
     2       'in VLA phasing scans.'
            CALL WLOG( 1, MSGTXT )
            WRITE( MSGTXT, '( A )' )
     1       '                        See the OBS. file for details.'
            CALL WLOG( 1, MSGTXT )
         END IF
C
C        Warn user to check slew times etc.
C
         IF( PADDED ) THEN
            WRITE( MSGTXT, '( A )' )
     1       ' VLA phasing scans have been added.'
            CALL WLOG( 0, MSGTXT )
            WRITE( MSGTXT, '( A )' )
     1      ' Use OBSERVE to check slew times and on-source durations.'
            CALL WLOG( 0, MSGTXT )
         END IF
C
      ELSE
C
C        Be sure that a mode has been specified.  Band was checked
C        in CHKVLA.
C
         IF( VLAMODE(ISCN) .EQ. 'ZZ' .AND. DOPEAK(ISCN) .LE. 0 ) THEN
            WRITE( MSGTXT, '( A, I4, A, A, A, I4 )' )
     1            ' Scan ', ISCN, '  VLAMODE ', 
     2            VLAMODE(ISCN),'  PEAK ', DOPEAK(ISCN)
            CALL WLOG( 1, 'CRDVLA: '//MSGTXT )
            CALL WLOG( 1, '        Setup file: '//SETNAME(LS) )
            CALL ERRLOG(  '        VLAMODE must be specified.' )
         END IF
C
C        Prepare for phasing scan if the mode is VX and a source not the
C        same as the program source was specified.
C
C        The stop time will be set to one minute before the start of the
C        VLBI scan if possible.  However a minimum time of 3 minutes 
C        since the last scan stop time will be required.  If necessary,
C        the phasing scan will eat into the start of the VLBI scan.  If
C        this causes the VLBI scan to be too short, no phasing scan will
C        be written.
C
C        Some hoop jumping is needed because LST drops at end of day.
C
         IF( ISRC .NE. IVLAPHS(ISCN) .AND. 
     1       VLAMODE(ISCN) .EQ. 'VX' ) THEN
            WPS = .TRUE.
            PSTOPJ1 = LSTOPJ + 3.D0 * ONEMINJ
            PSTOPJ2 = STARTJ(ISCN) - ONEMINJ
            IF( PSTOPJ2 .GT. PSTOPJ1 ) THEN
               PLST = LST1(ISCN,ISTA) - ONEMIN
               IF( PLST .LT. 0.D0 ) PLST = PLST + TWOPI 
            ELSE
               PLST = LLST + 3.0D0 * ONEMIN
               IF( PLST .GT. TWOPI ) PLST = PLST - TWOPI
               PWARN = .TRUE.
               IF( PSTOPJ1 .LT. STOPJ(ISCN) - 2.D0 * ONEMINJ ) THEN
                  WRITE( IULOC, '(A)' )
     1              '//*  Phasing scan shortens VLBI scan.'
               ELSE
                  WRITE( IULOC, '(''//*  No time for phasing scan.'')')
                  WPS = .FALSE.
               END IF
            END IF
         END IF
         IF( WPS ) PADDED = .TRUE.
C
C        Store previous stop times.
C
         LLST = LST2(ISCN,ISTA)
         CALL TIMEJ( STOPJ(ISCN), YEAR, DAY, STOP )
         LSTOP = STOP
         LSTOPJ = STOPJ(ISCN)
C
C        Put in comment, if required. Do I need to make this upper case?
C
         IF( ANNOT(ISCN) .NE. ' ' ) THEN
            UANNOT = ANNOT(ISCN)
            CALL UPCASE( UANNOT )
            WRITE(IULOC,'(''//* ---- '',A,'' ----'')')
     1           UANNOT(1:LEN1(UANNOT))
         END IF
C
C        Get RA, DEC, and epoch for schedule.  If the source is
C        a planet, get the current position.  Need double precision
C        in call for some items that should be single precision later.
C        The distance is set to effectively infinity if the motions
C        came from external input.  Someday, that should be user input.
C
         IF( PLANET(ISRC) .OR. SATEL(ISRC) ) THEN
            PPMTIME = STARTJ(ISCN)
            IF( PLANET(ISRC) ) THEN
               CALL JPLEP2( EPHFILE, SCNSRC(ISCN), PPMTIME, 
     1              (-1.D0)*LONG(KSTA), LAT(KSTA), ELEV(KSTA), 'VLA',
     2                   PRA, PDEC, DDRA, DDDEC, DIST, IER )
               IF( IER .NE. 0 ) THEN
                  CALL ERRLOG( 'VLA: Problem with ephemeris for '//
     1                SCNSRC(ISCN) )
               END IF
            ELSE IF( SATEL(ISRC) ) THEN
               CALL ERRLOG( 'CRDVLA: Don''t do satellites yet' )
            END IF
            RASCH  = TFORM( PRA, 'T', 1, 2, 7, '  @' )
            DECSCH = TFORM( PDEC, ' ',-1, 2, 6, '  @' )
            EPOCH  = 'D'
            PDRA   = DDRA
            PDDEC  = DDDEC
         ELSE
            PPMTIME = PMTIME(ISRC)
            PDRA    = DRA(ISRC)
            PDDEC   = DDEC(ISRC)
            DIST    = 1.D10
C
            IF( C2000(ISRC).EQ.'*' ) THEN
               RASCH = TFORM( RA2000(ISRC), 'T', 1, 2, 7, '  @' )
               DECSCH = TFORM( D2000(ISRC), ' ',-1, 2, 6, '  @' )
               EPOCH = 'C'
            ELSE IF( C1950(ISRC).EQ.'*' ) THEN
               RASCH = TFORM( RA1950(ISRC), 'T', 1, 2, 7, '  @' )
               DECSCH = TFORM( D1950(ISRC), ' ',-1, 2, 6, '  @' )
               EPOCH = ' '
            ELSE
               RASCH = TFORM( RAP(ISRC), 'T', 1, 2, 7, '  @' )
               DECSCH = TFORM( DECP(ISRC), ' ',-1, 2, 6, '  @' )
               EPOCH = 'D'
            END IF
         END IF
C
C        Get the source name to use.  The source name plus qualifier 
C        must be less than 13 characters and there must be a blank 
C        between them.  A qualifier of 0 is assumed if none is given.
C        If the primary source name is too long, use the 8 character
C        alias, if available.  Otherwise complain.
C
         IF( QUAL(ISCN) .EQ. 0 ) THEN
            PRTSRC = ' '
         ELSE
            WRITE( PRTSRC, '( 8X, I5 )' ) QUAL(ISCN)
         END IF
         MCHAR = 8
         DO I = 9, 13
            IF( PRTSRC(I:I) .EQ. ' ' ) MCHAR = I
         END DO
         IF( MCHAR .LT. 13 ) MCHAR = MCHAR - 1
         NCHAR = LEN1( SCNSRC(ISCN) )
         IF( NCHAR .LE. MCHAR ) THEN
            PRTSRC(1:NCHAR) = SCNSRC(ISCN)(1:NCHAR)
         ELSE 
            IF( SOUR8(ISRC) .NE. ' ' ) THEN
               WRITE( PRTSRC, '(A8, 1X, I4 )' ) 
     1             SOUR8(ISRC), QUAL(ISCN)
            ELSE
               CALL ERRLOG( ' Too many characters in source name '//
     1                'and qualifier for VLA: '//SCNSRC(ISCN) )
            END IF
         END IF
C
C        Write main control card into character string.
C        Make VLAPEAK = D equivalent to VLAPEAK = ' '.  VLAPEAK = D
C        is a flag to the automatic pointing routines to return
C        to automatic setting of VLAPEAK if AUTOPEAK is on.  It is
C        also the default VLAPEAK.
C
         TLST = TFORM( LST2(ISCN,ISTA), 'T', 0, 2, 2, '  @' )
         RPK = VLAPEAK(ISCN)
         IF( RPK .EQ. 'D' ) RPK = ' '
         WRITE( SSLINE, '( A13, A1, A8, A14, 1X, A13, A1, 4X,' //
     1        'A2, 1X, A2, A1, 3X, A4, A1, A1)') 
     2        PRTSRC, ' ', TLST, RASCH, 
     3        DECSCH, EPOCH, VLABAND(LS), VLAMODE(ISCN), 
     4        CALCODE(ISRC), VLABW(LS), RPK, VLATSYS(ISCN)
C
C        Generate the line for the phasing scan.
C
         IF( WPS ) THEN
            IF( C2000(IVLAPHS(ISCN)).EQ.'*' ) THEN
               RASCH = TFORM( RA2000(IVLAPHS(ISCN)),'T', 1,2,7, '  @' )
               DECSCH= TFORM( D2000(IVLAPHS(ISCN)), ' ',-1,2,6, '  @' )
               EPOCH = 'C'
            ELSE IF( C1950(IVLAPHS(ISCN)).EQ.'*' ) THEN
               RASCH = TFORM( RA1950(IVLAPHS(ISCN)),'T', 1,2,7, '  @' )
               DECSCH= TFORM( D1950(IVLAPHS(ISCN)), ' ',-1,2,6, '  @' )
               EPOCH = ' '
            ELSE
               RASCH = TFORM( RAP(IVLAPHS(ISCN)),  'T', 1,2,7, '  @' )
               DECSCH= TFORM( DECP(IVLAPHS(ISCN)), ' ',-1,2,6, '  @' )
               EPOCH = 'D'
            END IF
C
C           Get which mode to use for the phasing scan.
C
            KS = NSETUP( ISCN, ISTA )
            GOTLCP = .FALSE.
            GOTRCP = .FALSE.
            DO ICH = 1, NCHAN(KS)
               IF( POL(ICH,KS) .EQ. 'LCP' ) GOTLCP = .TRUE.
               IF( POL(ICH,KS) .EQ. 'RCP' ) GOTRCP = .TRUE.
            END DO
            IF( GOTLCP .AND. GOTRCP ) THEN
               PHMODE = 'VA'
            ELSE IF( GOTLCP ) THEN
               PHMODE = 'VL'
            ELSE IF( GOTRCP ) THEN
               PHMODE = 'VR' 
            ELSE
               PHMODE = 'VA'
            END IF
C
C           For the phasing scan, assume qualifier zero.  This allows
C           source names up to the maximum length of 12 characters.
C
            TLST = TFORM( PLST, 'T', 0, 2, 2, '  @' )
            WRITE( PSLINE, '( A12, A2, A8, A14, 1X, A13, A1, 4X,' //
     1        'A2, 1X, A2, A1, 3X, A4, 1X, A1 ) ' ) 
     2        VLAPHS(ISCN), '  ', TLST, RASCH, 
     3        DECSCH, EPOCH, VLABAND(LS), PHMODE,
     4        CALCODE(IVLAPHS(ISCN)), VLABW(LS), VLATSYS(ISCN)
C
         END IF
C
C        Now deal with LO and FI cards if they are needed.
C        For a while, write them always for VQ because we are 
C        changing the standard.
C
         IF( VLALOFI(LS) .OR. PTLINK .OR. VLABAND(LS) .EQ. 'VQ' ) THEN
C
C           Test what we have.  Note VLAFEAB and B can be 0.D0 for 6cm.
C
            GOTLO = VLASYNA(LS) .NE. 0.D0 .AND. VLASYNB(LS) .NE. 0.D0
C     1       .AND. VLAFEAB(LS) .NE. 0.D0 .AND. VLAFECD(LS) .NE. 0.D0 
            GOTFI = FLUKEA(LS) .NE. 0.D0 .AND. FLUKEB(LS) .NE. 0.D0 
C
            IF( .NOT. ( GOTLO .AND. GOTFI ) ) CALL ERRLOG(
     1         'Insufficient information provided for LO and FI cards' )
C
C           Get character form for flukeset so it can be blank.
C
            IF( FLUKESET(LS) .GT. 0 ) THEN
               WRITE( FLKSETC, '(I1)' ) FLUKESET(LS)
            ELSE
               FLKSETC = ' '
            END IF
C
C           For the Pie Town link observations, get the PT first LO.
C
            CALL GETPTLO( PTLO, ISCN )
C
C           Write the cards.  Round off the synthesizer settings.
C           Note frequencies were checked in CHKVLA
C
            IF( FIRSTS .AND. ( WLO .OR. WFI ) ) WRITE( IULOC, '( A )' ) 
     1        '//* ====  CHECK THE FLUKESET.'
            ISYNA = IDNINT( VLASYNA(LS) )
            ISYNB = IDNINT( VLASYNB(LS) )
            WRITE( LOLINE, '( A, T7, 2F7.1, T26, I5, T36, I5,
     1            T46, A9, T55, A4, T61, A, T71, A )' )
     2            '//LO', VLAFEAB(LS), VLAFECD(LS), ISYNA, ISYNB,
     3            PTLO, FEFILTER(LS), VLAIF(LS), VLAROT(LS)
            WLO = .TRUE.
            WRITE( FILINE, '( A, T10, A1, T17, F14.5, T37, F14.5 )')
     1            '//FIS', FLKSETC, FLUKEA(LS), FLUKEB(LS)
            WFI = .TRUE.
C
         END IF
C
C        Make a //DS line for correlator integration time.
C        Recall it must be 10 seconds for pointing scans so enforce
C        that (VLAMODE='IR')
C
         IF( LVINTEG .NE. -999 .OR. VLAINTEG(ISCN) .NE. 0 ) THEN
            IF( VLAINTEG(ISCN) .EQ. 0 .OR. VLAMODE(ISCN) .EQ. 'IR' ) 
     1         VLAINTEG(ISCN) = 10
            LVINTEG = VLAINTEG(ISCN)
            DSLINE = ' '
            WRITE( DSLINE, '( A4, 11X, I3 )' ) '//DS', VLAINTEG(ISCN)
            WDS = .TRUE.
         END IF
C
C        Now actually write out the lines.  This delay was required
C        in order to do the phasing scans.
C
         IF( WPS ) THEN
            WRITE( IULOC, '(A)' ) PSLINE
            IF( WLO ) WRITE( IULOC, '(A)' ) LOLINE
            IF( WFI ) WRITE( IULOC, '(A)' ) FILINE
         END IF
         WRITE( IULOC, '(A)' ) SSLINE
         IF( WLO ) WRITE( IULOC, '(A)' ) LOLINE
         IF( WFI ) WRITE( IULOC, '(A)' ) FILINE
         IF( WDS ) WRITE( IULOC, '(A)' ) DSLINE
C
C        Add proper motion card if parameters were provided.
C        Note that the time is supposed to be IAT, not UT.
C        Proper motion cards should not be used on phasing scans.
C
         IF( PDRA .NE. 0.0 .OR. 
     1       PDDEC .NE. 0.0 ) THEN
            IF( IATUTC .EQ. 0.0 ) CALL ERRLOG( 
     1          'CRDVLA: IATUTC required for VLA PM reference time' )
            PMIAT = PPMTIME + IATUTC/86400.D0
            CALL TIMEJ( PMIAT, PMY, PMD, PMT )
            IF( PMY .NE. YEAR .OR. PMD .NE. DAY ) CALL ERRLOG( 
     1        'CRDVLA: VLA PM reference time must be on day '//
     2        'of observation' )
C
C           Get the horizontal paralax and reference time for motion.
C
            HPARAL = 8.794148D0/DIST      
            PMFT = TFORM( PMT, 'T', 0, 2, 2, '  @' )
            WRITE( IULOC, '( A, T11, 2F10.3, T32, A, T41, F10.4 )' )
     1        '//PM', PDRA, PDDEC, PMFT, HPARAL
C
         END IF
C
C        Turn off the reference antenna for peaking scans.
C        Otherwise a specified reference antenna will not be pointed.
C
         IF( VLAMODE(ISCN) .EQ. 'IR' ) THEN
            WRITE( IULOC, '( A )' ) '//* & Refant = []'
         END IF
C        
C
      END IF   !  Not scan -999
C
      RETURN
      END
