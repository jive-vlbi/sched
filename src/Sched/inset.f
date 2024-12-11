      SUBROUTINE INSET( KD, KC, KI )
C
C     Routine for SCHED called by RDSET that sets up the KEYIN
C     variable names for reading setup files.
C
C     **** All defaults are set by RDSET later (to be redone each
C     call), so only use KEYCHR when a string longer than 8 characters
C     is needed.
C
C     The include file is needed for array sizes.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      DOUBLE PRECISION  KD(*)
      CHARACTER         KC(*)*(*)
      INTEGER           KI(*)
C ----------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'INSET starting.' )
C
C     Defaults below not used - see above.
C
C     General paramters.
C
      CALL KEYADD( 'STATION', 0.D0, MANT, KD, KC, KI )
      CALL KEYADD( 'ENDSET', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'BAND', 0.D0, 1, KD, KC, KI )
C
C        Channel setup
C
      CALL KEYADD( 'NCHAN', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'FREQREF', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'NETSIDE', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'FREQOFF', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'FIRSTLO', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'BBC', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'BBSYN', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'BBFILTER', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'SIDEBAND', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'BITS', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'SAMPRATE', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'RCHAN', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'LCHAN', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'POL', 0.D0, MCHAN, KD, KC, KI )
C
C         Frequency switching
C
      CALL KEYADD( 'FRSWITCH', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'SWTCHDUR', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'BBSYN2', 0.D0, MCHAN, KD, KC, KI )
C
C         Recordings
C
      CALL KEYCHR( 'DBE',    ' ', 8, KD, KC, KI )
      CALL KEYCHR( 'FIRMFILE' , ' ', 80, KD, KC, KI )
      CALL KEYADD( 'FORMAT', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'MODETEST', NOTSET, 1, KD, KC, KI )
      CALL KEYADD( 'TRACK1', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'TRACK2', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'TRACK3', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'TRACK4', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'TRACK5', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'TRACK6', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'TRACK7', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'TRACK8', 0.D0, MCHAN, KD, KC, KI )
C
C         Tapes (Obsolete)
C
      CALL KEYADD( 'TPMODE', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'TPSPEED', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'TPSPEEDH', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'TPSPEEDL', 0.D0, 1, KD, KC, KI )
C
C        Phase cal
C
      CALL KEYADD( 'PCAL', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'PCALXB1', 0.D0, MAXPC, KD, KC, KI )
      CALL KEYADD( 'PCALXB2', 0.D0, MAXPC, KD, KC, KI )
      CALL KEYADD( 'PCALFR1', 0.D0, MAXPC, KD, KC, KI )
      CALL KEYADD( 'PCALFR2', 0.D0, MAXPC, KD, KC, KI )
C
C         Arbitrary strings.
C
      CALL KEYCHR( 'STRING1', ' ', 80, KD, KC, KI )
      CALL KEYCHR( 'STRING2', ' ', 80, KD, KC, KI )
      CALL KEYCHR( 'STRING3', ' ', 80, KD, KC, KI )
      CALL KEYCHR( 'STRING4', ' ', 80, KD, KC, KI )
C
C     VLBA specific parameters.
C
      CALL KEYADD( 'LOGGING', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'FE', 0.D0, 4, KD, KC, KI )
      CALL KEYADD( 'SYNTH', 0.D0, 3, KD, KC, KI )
      CALL KEYADD( 'IFCHAN', 0.D0, MCHAN, KD, KC, KI )
      CALL KEYADD( 'IFDIST', 0.D0, 4, KD, KC, KI )
      CALL KEYADD( 'NOISE', 0.D0, 4, KD, KC, KI )
      CALL KEYADD( 'NOISEFRQ', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'PERIOD', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'DUALX', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'LCP50CM', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'RCP50CM', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'LEVEL', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'AZCOLIM', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'ELCOLIM', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'PTINCR', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'PTOFF', 0.D0, 1, KD, KC, KI )
      CALL KEYCHR( 'BARREL', 'not_set', 9, KD, KC, KI )
C
C     VLA parameters. 'ZZ' will cause error.
C     All of these are obsolete.  They should be removed at
C     some point.
C
      CALL KEYCHR( 'VLABAND', 'ZZ', 2, KD, KC, KI )
      CALL KEYCHR( 'VLABW', 'ZZZZ', 4, KD, KC, KI )
      CALL KEYADD( 'FLUKESET', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'FLUKEA', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'FLUKEB', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'VLAFEAB', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'VLAFECD', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'VLASYNA', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'VLASYNB', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'FEFILTER', 0.D0, 1, KD, KC, KI )
      CALL KEYCHR( 'VLAIF', ' ', 10, KD, KC, KI )
      CALL KEYCHR( 'VLAROT', ' ', 10, KD, KC, KI )
C
C     Mark4 parameters.
C
      CALL KEYCHR( 'M4PATCH', 'ASTRO', 8, KD, KC, KI )
C
C
      IF( DEBUG ) THEN
         WRITE( MSGTXT, '( A, I5, A, I5, A, I5 )' )
     1        'INSET: MSETV: ', KI(1), '  NSETV: ', KI(2),
     2        '  NKEYS: ', KI(3)
         CALL WLOG( 0, MSGTXT )
      END IF
      RETURN
      END
