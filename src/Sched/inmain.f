      SUBROUTINE INMAIN( KD, KC, KI )
C
c     Routine for SCHED subroutine SCHIN that sets up the input
C     variables for the main sched input.
C
C     The include file is needed to get the parameters.
C
C     Putting more often read parameters early to speed parsing.
C
      INCLUDE 'sched.inc'
C
      DOUBLE PRECISION  KD(*), BLANK
      CHARACTER         KC(*)*(*), FILENAME*80
      INTEGER           KI(*), I
C ----------------------------------------------------------------
C
C     Scan data.  These parameter have a separate entry for each scan.
C
      CALL KPACK( '        ', BLANK )
      CALL KEYADD( 'STATions', 0.D0, MAXSTA, KD, KC, KI )
      CALL KEYCHR( 'SOURCE', 'DUMMY', 12, KD, KC, KI )
      CALL KEYADD( 'QUAL', 0.D0, 1, KD, KC, KI )
      CALL KEYCHR( 'DOPSRC', ' ', 12, KD, KC, KI )
      CALL KEYADD( 'START', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'STOP', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'YEAR', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'MONTH', 1.D0, 1, KD, KC, KI )
      CALL KEYADD( 'DAY', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'DURation', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'PRESCAN', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'DWELL', 0.D0, 3, KD, KC, KI )
      CALL KEYADD( 'GAP', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'REPeat', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'GROUP', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'HIGROUP', 1.D0, 1, KD, KC, KI )
      CALL KEYADD( 'PEAK', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'NOPEAK', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'TSYS', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'NOTSYS', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'TANTSTA1', BLANK, MAXSTA, KD, KC, KI )
      CALL KEYADD( 'TANTSTA2', BLANK, MAXSTA, KD, KC, KI )
      CALL KEYADD( 'TANT1', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'TANT2', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'FREQ', 0.D0, MAXCHN, KD, KC, KI )
      CALL KEYADD( 'BW', 0.D0, MAXCHN, KD, KC, KI )
      CALL KEYADD( 'CRDNCH', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'CRDCH1', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'CRDSETCH', 0.D0, 4, KD, KC, KI )
      CALL KEYADD( 'CRDFREQ', 0.D0, MAXCHN, KD, KC, KI )
      CALL KEYADD( 'CRDBW', 0.D0, MAXCHN, KD, KC, KI )
      CALL KEYADD( 'CRDDOP', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'CRDNODOP', UNSET, 1, KD, KC, KI )
      CALL KEYCHR( 'COMMENT', ' ', 128, KD, KC, KI )
      CALL KEYCHR( 'LINENAME', ' ', 8, KD, KC, KI )
      CALL KEYADD( 'DOPCAL', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'DOPPLER', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'NODOP', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'DOPINCR', 0.0D0, 2, KD, KC, KI )
      CALL KEYCHR( 'PCAL', ' ', 4, KD, KC, KI )
      CALL KEYCHR( 'PREEMPT', '--', 5, KD, KC, KI )
      DO I = 1, MSCINT
         CALL KEYCHR( 'INTENTs', ' ', 80, KD, KC, KI )
      END DO
C
C     Tape control and eVLBI datapath control.
C     TAPE, REWIND, FASTFOR, and REVERSE can have separate 
C     data for each station and scan.
C
      CALL KEYADD( 'RECord', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'NORECord', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'TPREF', -1.D0, 1, KD, KC, KI )
      CALL KEYADD( 'AUTOTAPE', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'MINPAUSE', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'PRESTART', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'TAPE', UNSET, MAXSTA, KD, KC, KI )
      CALL KEYADD( 'REWIND', UNSET, MAXSTA, KD, KC, KI )
      CALL KEYADD( 'FASTFOR', UNSET, MAXSTA, KD, KC, KI )
      CALL KEYADD( 'REVERSE', UNSET, MAXSTA, KD, KC, KI )
      CALL KEYCHR( 'DATAPATH', 'IN2DISK', 8, KD, KC, KI )
      CALL KEYCHR( 'GRABTO', 'NONE', 4, KD, KC, KI )
      CALL KEYADD( 'GRABTIME', UNSET, 2, KD, KC, KI )
      CALL KEYADD( 'GRABGAP', 0.D0, 1, KD, KC, KI )
C
C     Program control including external input flags.
C
      CALL KEYADD( 'SRCCAT', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'STACAT', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'TAPEINI', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'LINEINIT', UNSET, 1, KD, KC, KI )
      CALL KEYCHR( 'SETINIT', ' ', 80, KD, KC, KI )
      CALL KEYADD( 'PEAKINIT', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'SATINIT', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'COVERLET', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'PCENTERS', UNSET, 1, KD, KC, KI )
      CALL KEYCHR( 'CENTERS', ' ', 12, KD, KC, KI )
      CALL KEYCHRA( 'GEOSRCS', ' ', 12, MGEO, KD, KC, KI )
      CALL KEYADD( 'DOVEX', 0.D0, 1, KD, KC, KI )
      CALL KEYCHR( 'DOSTA', 'ALL', 8, KD, KC, KI )
      CALL KEYADD( 'LST', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'DODOWN', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'AUTOPEAK', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'PKWATCH', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'EXIT', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'PLOT', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'DEBUG', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'OVERRIDE', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'OVERwrit', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'SUMITEM', UNSET, 10, KD, KC, KI )
      CALL KEYCHR( 'SCANTAG', ' ', 4, KD, KC, KI )
      CALL KEYADD( 'FREQLIST', UNSET, 2, KD, KC, KI )
      CALL KEYADD( 'PRECDATE', 1979.9D0, 1, KD, KC, KI )
      CALL KEYADD( 'POINT', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'PUBPLOT', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'VEXTEST', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'DOMKA', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'DOSCANS', 0.D0, 2, KD, KC, KI )
      CALL KEYADD( 'WRAP24', UNSET, 1, KD, KC, KI )
C
C     Files.
C
      CALL KEYCHR( 'SCHedule', ' ', 80, KD, KC, KI )
      CALL SCHDEFS( 'stations', FILENAME )
      CALL KEYCHR( 'STAFILE', FILENAME, 80, KD, KC, KI )
      CALL SCHDEFS( 'sources', FILENAME )
      CALL KEYCHR( 'SRCFILE', FILENAME, 80, KD, KC, KI )
      CALL KEYCHR( 'SRCFILE2', 'NONE', 80, KD, KC, KI )
      CALL KEYCHR( 'SETUP', 'DUMMY', 80, KD, KC, KI )
      CALL KEYADD( 'NOSETUP', UNSET, 1, KD, KC, KI )
      CALL KEYCHR( 'TAPEFILE', 'NONE', 80, KD, KC, KI )
      CALL KEYCHR( 'EPHFILE', 'NONE', 80, KD, KC, KI )
      CALL SCHDEFS( 'frequency', FILENAME )
      CALL KEYCHR( 'FREQFILE', FILENAME, 80, KD, KC, KI )
      CALL SCHDEFS( 'location', FILENAME )
      CALL KEYCHR( 'LOCFILE', FILENAME, 80, KD, KC, KI )
      CALL SCHDEFS( 'messages', FILENAME )
      CALL KEYCHR( 'MSGFILE', FILENAME, 80, KD, KC, KI )
      CALL SCHDEFS( 'peakcommand', FILENAME )
      CALL KEYCHR( 'PEAKFILE', FILENAME, 80, KD, KC, KI )
C
C
C     Cover and experiment information.
C
      CALL KEYCHR( 'EXPCODE', 'NUG', 8, KD, KC, KI )
      CALL KEYCHR( 'EXPT', ' ', 72, KD, KC, KI )
      CALL KEYADD( 'LINEPG', 55.D0, 1, KD, KC, KI )
      CALL KEYCHR( 'OBSTYPE', 'NONE', 8, KD, KC, KI )
      CALL KEYADD( 'VERSION', 0.D0, 1, KD, KC, KI )
      CALL KEYCHR( 'PINAME', ' ', 64, KD, KC, KI )
      CALL KEYCHR( 'PHONE', ' ', 64, KD, KC, KI )
      CALL KEYCHR( 'ADDRESS1', ' ', 64, KD, KC, KI )
      CALL KEYCHR( 'ADDRESS2', ' ', 64, KD, KC, KI )
      CALL KEYCHR( 'ADDRESS3', ' ', 64, KD, KC, KI )
      CALL KEYCHR( 'ADDRESS4', ' ', 64, KD, KC, KI )
      CALL KEYCHR( 'EMAIL', ' ', 64, KD, KC, KI )
      CALL KEYCHR( 'FAX', ' ', 64, KD, KC, KI )
      CALL KEYCHR( 'OBSPHONE', ' ', 64, KD, KC, KI )
      CALL KEYCHR( 'OBSMODE', ' ', 64, KD, KC, KI )
      CALL KEYCHR( 'NOTE1', ' ', 128, KD, KC, KI )
      CALL KEYCHR( 'NOTE2', ' ', 128, KD, KC, KI )
      CALL KEYCHR( 'NOTE3', ' ', 128, KD, KC, KI )
      CALL KEYCHR( 'NOTE4', ' ', 128, KD, KC, KI )
C
C     Correlator information.
C
      CALL KEYCHR( 'CORREL',  ' ', 64, KD, KC, KI )
      CALL KEYADD( 'CORAVG',  0.D0, 2, KD, KC, KI )
      CALL KEYADD( 'CORAVG2', 0.D0, 2, KD, KC, KI )
      CALL KEYADD( 'CORCHAN', 0.D0, 2, KD, KC, KI )
      CALL KEYADD( 'CORNANT', 0.D0, 1, KD, KC, KI )
      CALL KEYCHR( 'CORPOL', ' ', 3, KD, KC, KI )
      CALL KEYCHR( 'CORSRCS', 'Not specified.', 64, KD, KC, KI )
      CALL KEYCHR( 'CORWTFN', 'UNIFORM', 16, KD, KC, KI )
      CALL KEYCHR( 'CORTAPE', ' ', 16, KD, KC, KI )
      CALL KEYCHR( 'CORDFMT', 'FITS', 8, KD, KC, KI )
      CALL KEYCHR( 'CORSHIP1', ' ', 64, KD, KC, KI )
      CALL KEYCHR( 'CORSHIP2', ' ', 64, KD, KC, KI )
      CALL KEYCHR( 'CORSHIP3', ' ', 64, KD, KC, KI )
      CALL KEYCHR( 'CORSHIP4', ' ', 64, KD, KC, KI )
      CALL KEYCHR( 'CORNOTE1', ' ', 128, KD, KC, KI )
      CALL KEYCHR( 'CORNOTE2', ' ', 128, KD, KC, KI )
      CALL KEYCHR( 'CORNOTE3', ' ', 128, KD, KC, KI )
      CALL KEYCHR( 'CORNOTE4', ' ', 128, KD, KC, KI )
C
C     Optimization and plotting.
C
      CALL KEYADD( 'GEOSEG', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'GEOPRT', -1.D0, 1, KD, KC, KI )
      CALL KEYADD( 'GEOTRIES', 20.D0, 1, KD, KC, KI )
      CALL KEYADD( 'GEOBACK', 100.D0, 1, KD, KC, KI )
      CALL KEYADD( 'GEOSLEW', 1.D0, 1, KD, KC, KI )
      CALL KEYADD( 'GEOSLOW', 40.D0, 1, KD, KC, KI )
      CALL KEYADD( 'GEOSREP', 100.D0, 1, KD, KC, KI )
      CALL KEYADD( 'GEOHIEL', 40.D0, 1, KD, KC, KI )
      CALL KEYADD( 'GEOLOWEL', 23.D0, 1, KD, KC, KI )
      CALL KEYCHR( 'OPTMODE', 'NONE', 8, KD, KC, KI )
      CALL KEYADD( 'OPMINANT', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'OPDUR', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'OPMINEL', 2.D0, 1, KD, KC, KI )
      CALL KEYADD( 'OPMISS', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'OPNOSUB', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'OPSKIP', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'OPELPRIO', 0.D0, 4, KD, KC, KI )
      CALL KEYADD( 'OPTSLEW', 1.D0, 1, KD, KC, KI )
      CALL KEYADD( 'OPTLOWT', 15.D0, 1, KD, KC, KI )
      CALL KEYCHR( 'OPHASTA', 'PT', 8, KD, KC, KI )
      CALL KEYADD( 'OPMINSEP', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'OPSLEWWT', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'OPSLEWTI', 360.D0, 1, KD, KC, KI )
      CALL KEYADD( 'OPHLIMWT', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'OPHLIMTI', 1800.D0, 1, KD, KC, KI )
      CALL KEYADD( 'OPHMAXDT', 7200.D0, 1, KD, KC, KI )
      CALL KEYADD( 'OPHA', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'OPHAWID', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'OPHAWT', 1.D0, 1, KD, KC, KI )
      CALL KEYADD( 'OPPRTLEV', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'TAPESYNC', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'MAPLIM', 0.D0, 4, KD, KC, KI )
      CALL KEYADD( 'GRIDNR', 20.D0, 1, KD, KC, KI )
      CALL KEYADD( 'GRIDNT', 36.D0, 1, KD, KC, KI )
      CALL KEYADD( 'GRIDMIN', 25.D0, 1, KD, KC, KI )
      CALL KEYADD( 'GRIDMAX', 250.D0, 1, KD, KC, KI )
      CALL KEYADD( 'GRIDW0', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'GRIDSTEP', 3.D0, 1, KD, KC, KI )
      CALL KEYCHR( 'GRIDMEAS', 'COUNT', 8, KD, KC, KI )
      CALL KEYADD( 'GRIDVLA', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'UVMFS', 1.D0, 2, KD, KC, KI )
C
C     Special VLBA stuff.
C
      CALL KEYADD( 'PTDUR', 20.D0, 1, KD, KC, KI )
      CALL KEYADD( 'PTSLEW', 160.D0, 1, KD, KC, KI )
      CALL KEYADD( 'TAVLBA', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'NOTAVLBA', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'PTVLBA', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'NOPTVLBA', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'PN3DB', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'NOPN3DB', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'CALTIME', 120.D0, 1, KD, KC, KI )
      CALL KEYADD( 'FOCUS', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'ROTATION', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'AZCOLIM', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'ELCOLIM', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'ROTPAT', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'ROTOFF', 0.D0, 20, KD, KC, KI )
      CALL KEYADD( 'FOCOFF', 0.D0, 20, KD, KC, KI )
      CALL KEYCHR( 'CRDLINE', ' ', 80, KD, KC, KI )
C
C     VLA Stuff.  'ZZ' defaults chosen to raise flags later.
C     They are here for backward compatability.
C     VLATSYS and VLANTSYS are a toggle pair for on-line tsys corr.
C
      CALL KEYCHR( 'VLAPSRC', ' ', 12, KD, KC, KI )
      CALL KEYCHR( 'VLAMODE', 'ZZ', 2, KD, KC, KI )
      CALL KEYADD( 'VLAPTIME', 10.D0, 1, KD, KC, KI )
      CALL KEYCHR( 'VLATYPE', 'VLBI', 9, KD, KC, KI )
      CALL KEYADD( 'VLAUSERN', 600.D0, 1, KD, KC, KI )
      CALL KEYADD( 'IATUTC', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'VLATSYS', UNSET, 1, KD, KC, KI )
      CALL KEYADD( 'VLANTSYS', UNSET, 1, KD, KC, KI )
      CALL KEYCHR( 'VLAPEAK', 'OFF', 9, KD, KC, KI )
      CALL KEYADD( 'VLAINTEG', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'VLARFANT', 10.D0, 1, KD, KC, KI )
C
C     Retired parameters kept for backwards compatability.
C
      CALL KEYADD( 'NCHAN', UNSET, 1, KD, KC, KI )
      CALL KEYCHR( 'VLABAND', 'ZZ', 2, KD, KC, KI )
      CALL KEYCHR( 'VLABW', 'ZZZZ', 4, KD, KC, KI )
C
      RETURN
      END
