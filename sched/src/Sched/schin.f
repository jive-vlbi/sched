      SUBROUTINE SCHIN
C
C     Subroutine for SCHED.  Reads main KEYIN schedule input data 
C     and fills schedule parameters which are mainly in the include
C     file.
C
      INCLUDE 'sched.inc'
C
      INTEGER           MODE, I, LEN1, ISCN, ISTA, IREP, INAME
      INTEGER           I1, I2, I3, I4, I5, KEYPTR
      LOGICAL           GOTINI, GOTSAT, DOINIT, DOSTWARN, GOTVEX
      CHARACTER         TPFILE*80
      CHARACTER         CSFILE*80
      INTEGER           YEAR(MAXSCN), DAY(MAXSCN)
      DOUBLE PRECISION  STOP(MAXSCN), START(MAXSCN)
C
C     Keyin input parameters.  Key names are in second half of KD.
C     KI(2) contains number of parameters.
C
      INTEGER           MK, INSCH
      PARAMETER         (MK=650 + 7*MAXSTA + 2*MAXCHN)
      INTEGER           KI(MK)
      CHARACTER         KC(MK)*8, KCHAR*80
      DOUBLE PRECISION  KD(2*MK), ENDMARK, BLANK
      LOGICAL           SETKEYS, AWARN
      SAVE              KI, KD, KC, ENDMARK, BLANK, INSCH, SETKEYS
C
      DATA   (KI(I),I=1,3)  / MK, 0, 3 /
      DATA   SETKEYS        / .TRUE. /
C ----------------------------------------------------------------------
C     Set up the arrays of keyin variables.  SETKEYS avoids doing
C     it again after a RESTART from the plotting routines.  If on
C     a restart, the input file needs to be opened right now. 
C     Otherwise, it will be opened in SCHFILES.
C
      IF( SETKEYS ) THEN
         CALL KPACK( '/       ', ENDMARK )
         CALL KPACK( '        ', BLANK )
         INSCH = 5   !  Schedule input unit.
         CALL INMAIN( KD, KC, KI )
         SETKEYS = .FALSE.
      ELSE
         CALL SCHOPEN( INSCH, IUSCH, KD, KC, KI )
      END IF
C
C     Initialize various program variables. 
C
      ISCN = 0    !  Number of scans.
      NSTA = 0    !  Number of stations.
      NSAT = 0    !  Number of satellites in SATINI group.
      IREP = 0    !  First scan of repeat group.
      MSTA = 0    !  Total entries in Station catalog.
      MSRC = 0    !  Total entries in main Source catalog.
      NSETF = 0   !  Number of setup files.
      SCAN1 = 1   !  First scan actually used.  SCHOPT may change.
      GOTINI = .FALSE.  !  Need tape initialization information.
      GOTSAT = .FALSE.  !  Need satellite info.
      GOTFREQ = .FALSE. !  Frequencies, Bandwidths, or Dopcals set.
      DOINIT = .TRUE.   !  Do initializations before next read.
      DWELLS = .FALSE.  !  Got any dwell requests.
      DOSTWARN = .TRUE. !  Warn if DOSTA specified.
      DOVEX = .TRUE.    !  Will a VEX file be needed?
      GOTVEX = .FALSE.  !  Found a VEX station.
      COVERLET = .FALSE. ! Is there a cover letter?
      ALLVLBA = .TRUE.  !  All stations have VLBA control systems.
      DO I = 1, MAXSCN
          SRCNUM(I) = 0
          IDOPSRC(I) = 0
          IVLAPHS(I) = 0
          VLAINTEG(I) = 0
      END DO
      DO I = 1, MAXSRC
         SUSED(I) = .FALSE.
         SRCATN(I) = 0
         SRLSTN(I) = 0
         DIDNDOP(I) = 0
         DOPPED(I) = .FALSE.
         DO INAME = 1, 5
            CSUSED(INAME,I) = ' '
         END DO
         PLANET(I) = .FALSE.
         SATEL(I) = .FALSE.
         SATN(I) = 0
      END DO
C
C  ---------  Jump to here to read next scan data.   ---------------
C
  1   CONTINUE
C
C        Reset input parameters that need it.  DOINIT allows avoiding
C        this section when the scan wasn't finished, but was interrupted
C        by some instream catalog, tapeinit or whatever.
C     
         IF( DOINIT ) THEN
            KD( KEYPTR( 'START', KC, KI ) ) = UNSET
            KD( KEYPTR( 'STOP', KC, KI ) ) = UNSET
            KD( KEYPTR( 'REPeat', KC, KI ) ) = 1.D0
            KD( KEYPTR( 'GROUP', KC, KI ) ) = 1.D0
            KD( KEYPTR( 'DURation', KC, KI ) ) = UNSET
            KD( KEYPTR( 'DWELL', KC, KI ) ) = UNSET
            KD( KEYPTR( 'NOPEAK', KC, KI ) ) = UNSET
            KD( KEYPTR( 'POINT', KC, KI ) ) = UNSET
            KD( KEYPTR( 'SCANTAG', KC, KI ) ) = BLANK
C
C           For toggle pairs.
C
            KD( KEYPTR( 'DOPPLER', KC, KI ) ) = UNSET
            KD( KEYPTR( 'NODOP', KC, KI ) ) = UNSET
            KD( KEYPTR( 'RECord', KC, KI ) ) = UNSET
            KD( KEYPTR( 'NORECord', KC, KI ) ) = UNSET
            KD( KEYPTR( 'PTVLBA', KC, KI ) ) = UNSET
            KD( KEYPTR( 'NOPTVLBA', KC, KI ) ) = UNSET
            KD( KEYPTR( 'TAVLBA', KC, KI ) ) = UNSET
            KD( KEYPTR( 'NOTAVLBA', KC, KI ) ) = UNSET
            KD( KEYPTR( 'TSYS', KC, KI ) ) = UNSET
            KD( KEYPTR( 'NOTSYS', KC, KI ) ) = UNSET
            KD( KEYPTR( 'VLATSYS', KC, KI ) ) = UNSET
            KD( KEYPTR( 'VLANTSYS', KC, KI ) ) = UNSET
C
C           For some arrays.
C
            I1 = KEYPTR( 'STATions', KC, KI ) - 1
            I2 = KEYPTR( 'TAPE', KC, KI ) - 1
            I3 = KEYPTR( 'REWIND', KC, KI ) - 1
            I4 = KEYPTR( 'FASTFOR', KC, KI ) - 1
            I5 = KEYPTR( 'REVERSE', KC, KI ) - 1
            DO I = 1, MAXSTA
               KD(I1+I) = 0.D0
               KD(I2+I) = UNSET
               KD(I3+I) = UNSET
               KD(I4+I) = UNSET
               KD(I5+I) = UNSET
            END DO
            I1 = KEYPTR( 'COMMENT', KC, KI ) - 1
            DO I = 1, 8
               KD(I1+I) = BLANK
            END DO
         END IF
C
C        Get scan information with KEYIN.
C
         MODE = 0
         CALL KEYIN( KD(MK+1), KD, KI(2), ENDMARK, MODE, INSCH, 6 )
         I1 = KEYPTR( 'EXIT', KC, KI )
         IF( MODE.EQ.1 .OR. KD(I1) .EQ. 0.D0 ) GO TO 990
C
C        Some program controls that will be needed quickly.
C
         DEBUG = KD( KEYPTR( 'DEBUG', KC, KI ) ) .EQ. 0.D0
         OVERWRIT = KD( KEYPTR( 'OVERwrit', KC, KI ) ) .EQ. 0.D0
         OVERRIDE = KD( KEYPTR( 'OVERRIDE', KC, KI ) ) .EQ. 0.D0
C
C        Now have DEBUG, write out sizes of input arrays.
C
         IF( DEBUG .AND. ISCN .EQ. 0) THEN
            WRITE( MSGTXT, '( A, I4, A, I4, A, I4 )' )
     1        'SCHIN: Maxpars:', KI(1), ' Npars:', KI(2), 
     2        ' Nkey:', KI(3)
            CALL WLOG( 0, MSGTXT )
         END IF
C
C        Get the message file early so it stands a better chance
C        of being available when needed.
C
         MSGFILE = KCHAR( 'MSGFILE', 80, .FALSE., KD, KC, KI )
         CALL ENVIR( MSGFILE )
C
C        There are some possible dead end paths where SCHED does
C        something with minimal information and then quits.  Do
C        those here.
C
         CALL DIVERT( KD, KC, KI, ISCN )
C
C        Check quickly if this is a NOSETUP run.
C
         NOSET = KD( KEYPTR( 'NOSETUP', KC, KI ) ) .EQ. 0.D0
C
C        Get any instream information (catalog files, setups, tape
C        initialization etc.).  Also detect if the rest of the main
C        schedule input should come from IUSCH - an external file
C        specified with SCHEDULE.  Also detect a cover letter and
C        read past it.
C
         CALL SCHFILES( DOINIT, GOTINI, GOTSAT, 
     1                  KD, KC, KI, INSCH, BLANK )
         IF( .NOT. DOINIT ) GO TO 1
C
C        Got new scan (not the start of an in-stream catalog).
C
         ISCN = ISCN + 1
         IF( ISCN .GT. MAXSCN ) THEN
            WRITE( MSGTXT, '(A,I5)')  
     1         'SCHIN: Too many scans, maximum ',MAXSCN
            CALL ERRLOG( MSGTXT )
         END IF
C
C        Allow user to only process one station.
C        Note that only as many letters as are given for DOSTA will be
C        compared with the station name so, for example, all VLA and 
C        VLBA sites can be done by specifying DOSTA='VL'.
C        This is put here rather than at the end so that stations
C        can be omitted as they are read.
C
         DOSTA = KCHAR( 'DOSTA', 8, .TRUE., KD, KC, KI )
         IF( DOSTA .NE. 'ALL' .AND. DOSTWARN ) THEN
            CALL WLOG( 0, 'SCHIN:  DOSTA specified as '//DOSTA )
            CALL WLOG( 0, '        Some stations may be skipped.' )
            DOSTWARN = .FALSE.
         END IF
C
C        Process the station request for this scan. 
C        Read the station catalog if haven't already.
C        Also get station dependent tape motion requests in GETSTA.
C
         CALL GETSTA( ISCN, KD, KC, KI, GOTVEX )
C
C        Get the eVLBI input parameters.
C
         DATAPATH(ISCN) = KCHAR( 'DATAPATH', 8, .TRUE., KD, KC, KI )
         GRABTO(ISCN) = KCHAR( 'GRABTO', 4, .TRUE., KD, KC, KI )
         GRABTIME(1,ISCN) = KD( KEYPTR( 'GRABTIME', KC, KI ) )
         GRABTIME(2,ISCN) = KD( KEYPTR( 'GRABTIME', KC, KI ) + 1 )
         GRABGAP(ISCN) = KD( KEYPTR( 'GRABGAP', KC, KI ) )
C
C        Get source.
C
         SCNSRC(ISCN) = KCHAR( 'SOURCE', 12, .TRUE., KD, KC, KI )
         QUAL(ISCN) = KD( KEYPTR( 'QUAL', KC, KI ) )
         IF( SCNSRC(ISCN) .EQ. ' ' ) CALL ERRLOG( 'SCHIN: Need source'//
     1         ' name - blank specified.' )
C
C        Set scan times etc.
C
         CALL GETTIM( ISCN, KD, KC, KI, START, STOP, DAY, YEAR )
C
C        Some observing instructions.
C
         ANNOT(ISCN) = KCHAR( 'COMMENT', 64, .FALSE., KD, KC, KI )
         TANT1(ISCN)  = KD( KEYPTR( 'TANT1', KC, KI ) ) .EQ. 0.D0
         TANT2(ISCN)  = KD( KEYPTR( 'TANT2', KC, KI ) ) .EQ. 0.D0
         CALTIME(ISCN) = KD( KEYPTR( 'CALTIME', KC, KI ) )
         PTSLEW(ISCN) = KD( KEYPTR( 'PTSLEW', KC, KI ) )
         PCAL(ISCN) = KCHAR( 'PCAL', 4, .FALSE., KD, KC, KI )
         FOCUS(ISCN) = KD( KEYPTR( 'FOCUS', KC, KI ) )
         ROTATION(ISCN) = KD( KEYPTR( 'ROTATION', KC, KI ) )
         SAZCOL(ISCN) = KD( KEYPTR( 'AZCOLIM', KC, KI ) )
         SELCOL(ISCN) = KD( KEYPTR( 'ELCOLIM', KC, KI ) )
         OPMISS(ISCN) = KD( KEYPTR( 'OPMISS', KC, KI ) )
         SCANTAG(ISCN) = KCHAR( 'SCANTAG', 4, .FALSE., KD, KC, KI )
         CRDLINE(ISCN) = KCHAR( 'CRDLINE', 80, .FALSE., KD, KC, KI )
C
C        The minimum tape pause time and the tape prestart time.
C
         PRESTART(ISCN) = KD( KEYPTR( 'PRESTART', KC, KI ) ) / 86400.D0
         MINPAUSE(ISCN) = KD( KEYPTR( 'MINPAUSE', KC, KI ) ) / 86400.D0
C
C        Some toggled logicals.
C
         CALL TOGGLE( NOTSYS, ISCN, 'NOTSYS', 'TSYS', UNSET,
     1                KD, KC, KI )
         CALL TOGGLE( NOREC, ISCN, 'NORECord', 'RECord', UNSET, 
     1                KD, KC, KI )
         CALL TOGGLE( PNTVLBA, ISCN, 'PTVLBA', 'NOPTVLBA', UNSET,
     1                KD, KC, KI )
         CALL TOGGLE( TANVLBA, ISCN, 'TAVLBA', 'NOTAVLBA', UNSET,
     1                KD, KC, KI )
         CALL TOGGLE( DOPN3DB, ISCN, 'PN3DB', 'NOPN3DB', UNSET,
     1                KD, KC, KI )
C
C        Peak up pointing command.  Value is channel to use.  NOPEAK
C        is equivalent to setting PEAK to -1.
C        Also get POINT which will convert a file to a reference
C        pointing file (like switching setup, setting PEAK etc).
C
         I1 = KEYPTR( 'PEAK', KC, KI )
         I2 = KEYPTR( 'NOPEAK', KC, KI )
         IF( KD(I2) .NE. UNSET ) KD(I1) = -1.D0
         DOPEAK(ISCN) = KD(I1)
         IF( KD( KEYPTR( 'POINT', KC, KI ) ) .EQ. UNSET ) THEN
            POINT(ISCN) = -999
         ELSE
            POINT(ISCN) = KD( KEYPTR( 'POINT', KC, KI ) )
         END IF
         AUTOPEAK = KD( KEYPTR( 'AUTOPEAK', KC, KI ) ) .EQ. 0.D0
         PKWATCH = KD( KEYPTR( 'PKWATCH', KC, KI ) ) .EQ. 0.D0
         PEAKFILE = KCHAR( 'PEAKFILE', 80, .FALSE., KD, KC, KI ) 
         CALL ENVIR( PEAKFILE )
C     
C        Get setup file name.
C
C        See if this setup has been requested before.  If not, add it
C        to SETFILE.
C
         SETNUM(ISCN) = 0
         CSFILE = KCHAR( 'SETUP', 80, .FALSE., KD, KC, KI )
         CALL ENVIR( CSFILE )
         IF( NSETF .GT. 0 ) THEN
            DO I = 1, NSETF
               IF( CSFILE .EQ. SETFILE(I) ) THEN
                  SETNUM(ISCN) = I
               END IF
            END DO
         END IF
C
         IF( SETNUM(ISCN) .EQ. 0 ) THEN
            IF( NSETF .LT. MAXSET ) THEN
               NSETF = NSETF + 1
               SETFILE(NSETF) = CSFILE
               SETNUM(ISCN) = NSETF
            ELSE
               CALL ERRLOG( ' SCHIN: Too many setup files. ' )
            END IF
         END IF
C
C        Freq and BW and Doppler requests.
C
         CALL INFDB( ISCN, KD, KC, KI )
C
C        VLA Parameters.  Assumes SETNUM(ISCN) is known.
C
         CALL INVLA( 1, ISCN, KD, KC, KI )
C
C        Optimization parameters that are scan dependent.
C
         OPMINEL(ISCN)  = KD( KEYPTR( 'OPMINEL', KC, KI ) )
         OPMIAN(ISCN)   = KD( KEYPTR( 'OPMINANT', KC, KI ) )
         OPMINSEP(ISCN) = KD( KEYPTR( 'OPMINSEP', KC, KI ) )
         OPSLEWWT(ISCN) = KD( KEYPTR( 'OPSLEWWT', KC, KI ) )
         OPSLEWTI(ISCN) = KD( KEYPTR( 'OPSLEWTI', KC, KI ) )
         OPHLIMWT(ISCN) = KD( KEYPTR( 'OPHLIMWT', KC, KI ) )
         OPHLIMTI(ISCN) = KD( KEYPTR( 'OPHLIMTI', KC, KI ) )
         OPHA(ISCN)     = KD( KEYPTR( 'OPHA', KC, KI ) )
         OPHAWID(ISCN)  = KD( KEYPTR( 'OPHAWID', KC, KI ) )
         OPHAWT(ISCN)   = KD( KEYPTR( 'OPHAWT', KC, KI ) )
         OPHMAXDT(ISCN) = KD( KEYPTR( 'OPHMAXDT', KC, KI ) )
C
C        *****  End of scan dependent input.  
C             All such inputs must preceed the call to SCHREP for loops.
C
C        Decode looping request.  Nesting of loops is not allowed.
C        ISCN is set to value for last of scan of loop.  
C
         CALL SCHREP( ISCN, IREP, KD, KC, KI, START, STOP, 
     1                DAY, YEAR )
C
C        Warn about some retired parameters.  Only ones that were used
C        before will go here.  VLABAND and VLABW warned of elsewhere.
C
         IF( KD( KEYPTR( 'NCHAN', KC, KI ) ) .NE. UNSET ) 
     1      CALL WLOG( 1, 'SCHIN: NCHAN in main schedule now ignored.' )
C
C
         IF( DEBUG ) THEN
            WRITE( MSGTXT, '( A, I4 )' ) 
     1          'SCHIN: Finished reading scan: ', ISCN
            CALL WLOG( 0, MSGTXT )
         END IF
C
         GO TO 1      ! Read next Scan
C
C    -----------------   End of scan loop  -------------------
C
  990 CONTINUE
C
C     Close the input unit if necessary.
C
      IF( INSCH .EQ. IUSCH ) THEN 
         CLOSE( UNIT=INSCH )
      END IF
C
C     Get the number of scans.  Keep updating SCANL, although
C     it will eventually be set in SCHOPT.
C
      NSCANS = ISCN
      SCANL = NSCANS
      IF( ISCN .LE. 0 ) THEN
         CALL ERRLOG('SCHIN: No input scans')
      ELSE
         WRITE( MSGTXT, '( A, I5, A )' ) 'SCHIN:   Found ', NSCANS, 
     1   ' input scans. '
         CALL WLOG( 0, MSGTXT )
      END IF
C 
C     Get items for which only the last input given is used.
C
      EXPT = KCHAR( 'EXPT', 72, .FALSE., KD, KC, KI ) 
      IF( LEN1(EXPT) .EQ. 0 ) EXPT = 'No description given.'
      EXPCODE = KCHAR( 'EXPCODE', 8, .FALSE., KD, KC, KI )
      LINEPG = KD( KEYPTR( 'LINEPG', KC, KI ) )
      TPREF = KD( KEYPTR( 'TPREF', KC, KI ) )
      PTDUR = KD( KEYPTR( 'PTDUR', KC, KI ) )
      PRECDATE = KD( KEYPTR( 'PRECDATE', KC, KI ) )
      PTLINK = KD( KEYPTR( 'PTLINK', KC, KI ) ) .EQ. 0.D0
C
C     The default of DOVEX was set to true on Oct. 14, 2008 in preparation
C     for the use of VEX for the VLBA software correlator.  Now setting
C     DOVEX to a non-zero value will turn it off.  Later it will be set
C     back to true if a VEX station is found.
C
C     The original claimed that we could not allow bandwidths to be set
C     when writing a VEX or VSOP (DRUDG ) file.  I'm not sure this is
C     still true - check.  Note that BW input and Station 
C     catalog both have been read by this point.
C     Also set the VEXTEST flag, which allows testing of features
C     that have not been publically released.
C
      IF( KD( KEYPTR( 'DOVEX', KC, KI ) ) .NE. 0.D0 ) DOVEX = .FALSE.
      IF( GOTVEX ) DOVEX = .TRUE.
      VEXTEST =  KD( KEYPTR( 'VEXTEST', KC, KI ) ) .EQ. 0.D0
C
C     Get ephemeris file name.
C
      EPHFILE = KCHAR( 'EPHFILE', 80, .FALSE., KD, KC, KI )
      CALL ENVIR( EPHFILE )
C
C     Check the validity of the experiment code.
C
      CALL CHKCODE( EXPCODE )
C
C     Get the observation type.  Let Markxx=Mkxx.
C     Also allow PTVLBA for historical reasons.
C
      OBSTYP = KCHAR( 'OBSTYPE', 8, .TRUE., KD, KC, KI )
      IF( OBSTYP(1:4) .EQ. 'MARK' ) 
     1        OBSTYP = 'MK'//OBSTYP(5:LEN1(OBSTYP))
      MARK2   = OBSTYP .EQ. 'MKII' .AND. LEN1(OBSTYP) .EQ. 4
      VLBITP  = OBSTYP .EQ. 'VLBA' .OR. OBSTYP .EQ. 'MKIII' .OR. 
     1          OBSTYP .EQ. 'VLBI' .OR. OBSTYP .EQ. 'MKIV'
      VLAONLY = OBSTYP .EQ. 'VLA'
      NOTAPE  = OBSTYP .EQ. 'VLA' .OR. OBSTYP .EQ. 'NONE' .OR.
     1          OBSTYP .EQ. 'PTVLBA' .OR. OBSTYP .EQ. 'CONFIG'
      CONFIG  = OBSTYP .EQ. 'CONFIG'
      IF( .NOT. ( MARK2 .OR. VLBITP .OR. VLAONLY .OR. 
     1    OBSTYP .EQ. 'NONE' .OR. OBSTYP .EQ. 'PTVLBA' .OR.
     2    OBSTYP .EQ. 'CONFIG' ) ) THEN
         CALL WLOG( 1, ' SCHIN: Invalid OBSTYPE: '//OBSTYP )
         CALL ERRLOG( ' SCHIN: OBSTYPE must be MKII, MKIII, VLBA, '//
     1         'MKIV, VLBI, VLA, NONE, or CONFIG' )
      END IF
C
C     For the schedule optimization mode and some plotting stuff.
C
      OPTMODE = KCHAR( 'OPTMODE', 8, .TRUE., KD, KC, KI )
      OPDUR   = KD( KEYPTR( 'OPDUR', KC, KI ) ) / 86400.D0
      OPNOSUB = KD( KEYPTR( 'OPNOSUB', KC, KI ) ) .EQ. 0.D0
      OPSKIP  = KD( KEYPTR( 'OPSKIP', KC, KI ) )
      OPTSLEW = KD( KEYPTR( 'OPTSLEW', KC, KI ) )
      OPTLOWT = KD( KEYPTR( 'OPTLOWT', KC, KI ) )
      OPHASTA = KCHAR( 'OPHASTA', 8, .TRUE., KD, KC, KI )
      TAPESYNC = KD( KEYPTR( 'TAPESYNC', KC, KI ) ) .EQ. 0.D0
      OPPRTLEV = KD( KEYPTR( 'OPPRTLEV', KC, KI ) )
      I1 = KEYPTR( 'OPELPRIO', KC, KI ) - 1
      I2 = KEYPTR( 'MAPLIM', KC, KI ) - 1
      DO I = 1, 4
         OPELPRIO(I) = KD(I1+I)
         MAPLIM(I) = KD(I2+I)
      END DO
      GRIDNR   = KD( KEYPTR( 'GRIDNR', KC, KI ) )
      GRIDNT   = KD( KEYPTR( 'GRIDNT', KC, KI ) )
      GRIDMIN  = KD( KEYPTR( 'GRIDMIN', KC, KI ) )
      GRIDMAX  = KD( KEYPTR( 'GRIDMAX', KC, KI ) )
      GRIDW0   = KD( KEYPTR( 'GRIDW0', KC, KI ) )
      GRIDSTEP = KD( KEYPTR( 'GRIDSTEP', KC, KI ) )
      GRIDMEAS = KCHAR( 'GRIDMEAS', 8, .TRUE., KD, KC, KI )
      GRIDVLA  = KD( KEYPTR( 'GRIDVLA', KC, KI ) ) .EQ. 0.D0
      GRIDUSED = .FALSE.
      I1 = KEYPTR( 'UVMFS', KC, KI )
      NMFS     = KD(I1)
      MFSRAT   = KD(I1+1)
C
C     Process Cover Letter and Correlator input.
C
      CALL GETCOV( KD, KC, KI )
      CALL GETCOR( KD, KC, KI )
C
C     Focus/rotation pointing patterns.
C
      ROTPAT = KD( KEYPTR( 'ROTPAT', KC, KI ) )
      I1 = KEYPTR( 'FOCOFF', KC, KI ) - 1
      I2 = KEYPTR( 'ROTOFF', KC, KI ) - 1
      DO I = 1, 20
         FOC(I) = KD(I1+I)
         ROT(I) = KD(I2+I)
      END DO
C
C     Get the tape initialization information if don't have it already.
C     The station catalog must have been read before TPTPNS is called.
C
      IF( .NOT. GOTINI ) THEN
         TPFILE = KCHAR( 'TAPEFILE', 80, .FALSE., KD, KC, KI )
         CALL ENVIR( TPFILE )
         CALL TPINI( IUTAP, TPFILE )
      END IF
      CALL TPTPNS
C
C     Check the tape initialization OBSCODE for default of match.
C     Done here because don't have EXPCODE at time of in-stream
C     tapeini input.
C
      IF( TPOBS .NE. 'DEFAULT' .AND. TPOBS .NE. EXPCODE ) THEN
         CALL ERRLOG( 'SCHIN: Tape initialization OBSCODE ('//
     1        TPOBS//') does not match EXPCODE ('//EXPCODE//')' )
      END IF
C
C     Set up any automatic tape handling.
C
C     TPLEN is only used for Mark2.
C     Actually, the sequence below requires Mark2 users to set a
C     time to get automatic tape handling.  I'd fix this if there really
C     were any Mark II's any more (actually there seem to be based in 
C     Russia with Italy involved - some narrow band application).
C
C     For Mark III and VLBA formats, AUTOTAPE triggers the automatic
C     tape handling.  It also triggers leaving stations out of scans when
C     when the source is down.  
C        AUTOTAPE=0 is the default, which will be to not use the 
C           automatic handling.  
C        AUTOTAPE=1 will set AUTOALLOCATE.  It doesn't make much sense
C           and is not recommended.
C        AUTOTAPE=2 will set AUTOALLOCATE and AUTOREVERSE.
C
C     Must have stations catalog by here.
C     Must also have USETAPE and USEDISK from TPTPNS.
C     Automatic tape allocation, if requested, will be used at VLBA
C     stations and at stations using Mark5 recording systems.
C
      I1 = KEYPTR( 'AUTOTAPE', KC, KI )
      AUTOTAPE = KD(I1) .GT. 0.1D0
      IF( AUTOTAPE ) THEN
         IF( KD(I1).EQ.0.D0 ) TPLEN = 4.D0 / 24.D0
         IF( KD(I1).GT.0.D0 ) TPLEN = KD(I1) / 86400.D0
      END IF
      AWARN = .FALSE.
      MSGTXT = 'SCHIN:   * Automatic tape allocation requested at:'
      DO ISTA = 1, NSTA
         AUTOALOC(ISTA) =  KD(I1) .GT. 0.5 
     1         .AND. ( CONTROL(STANUM(ISTA)) .EQ. 'VLBA' .OR. 
     2                 VLBADAR(STANUM(ISTA)) )
     3         .AND. STNDRIV(STANUM(ISTA)) .GE. 2
         AUTOREV(ISTA) = ( KD(I1) .GT. 1.5D0 ) .AND. AUTOALOC(ISTA)
         IF( VLBITP .AND. AUTOALOC(ISTA) ) THEN
            MSGTXT = MSGTXT(1:LEN1(MSGTXT)) // ' ' // 
     1               STCODE(STANUM(ISTA))
            AWARN = .TRUE.
         END IF
      END DO
      IF( AWARN ) CALL WLOG( 0, MSGTXT )
C
C     Get command to observe scans even if the source is down.
C
      DODOWN =  KD( KEYPTR( 'DODOWN', KC, KI ) ) .EQ. 0.D0 
C
C     Items wanted in summary file.  Need NOTAPE by here.
C
      I1 = KEYPTR( 'SUMITEM', KC, KI ) - 1
      DO I = 1, 10
         IF( KD(I1+I) .EQ. UNSET ) THEN
            SUMITEM(I) = ' '
         ELSE
            WRITE( SUMITEM(I), '(A8)' ) KD(I1+I)
            CALL UPCASE( SUMITEM(I) )
         END IF
      END DO
      IF( SUMITEM(1) .EQ. ' ' ) THEN
         SUMITEM(1) = 'ELA'
         SUMITEM(2) = 'DWELL'
C         IF( .NOT. NOTAPE .AND. .NOT. NOSET ) THEN
C            SUMITEM(3) = 'TAPE1'
C            SUMITEM(4) = 'TAPE2'
C         END IF
      END IF
C
C     Some schedule wide VLA items (called with MODE=2):  
C     VLBITP should be set by here.
C
      CALL INVLA( 2, ISCN, KD, KC, KI )
C
      IF( DEBUG ) CALL WLOG( 0, 'SCHIN: About to read catalogs.' )
C
C     Get scan timing.
C
      CALL TIMES( KD, KC, KI, START, STOP, DAY, YEAR )
C
C     Get the source catalog file name.  The catalog will be read later.
C
      SRCFILE = KCHAR( 'SRCFILE', 80, .FALSE., KD, KC, KI )
      CALL ENVIR( SRCFILE )
C
C     Decode TANT stations lists.  Must have station catalog by here.
C
      CALL STTANT( KD, KC, KI )
C
C     Get the frequency file name and listing instructions.
C
      FREQFILE = KCHAR( 'FREQFILE', 80, .FALSE., KD, KC, KI )
      CALL ENVIR( FREQFILE )
      I1 = KEYPTR( 'FREQLIST', KC, KI )
      FREQLIST(1) = KD(I1)
      FREQLIST(2) = KD(I1+1)
C
      IF( DEBUG ) THEN
         WRITE( MSGTXT, '( A, I5, A, I5, A, I5 ) ' )
     1        'SCHIN: MC: ', KI(1), '  NPARS: ', KI(2),
     2        '  NKEYS: ', KI(3)
         CALL WLOG( 0, MSGTXT )
      END IF
C
      RETURN
      END
