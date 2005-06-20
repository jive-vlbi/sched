      SUBROUTINE INFREQ( DEBUG, KD, KC, KI )
C
C     Routine for SCHED called by GETFREQ that sets up the KEYIN
C     variable names for reading the frequency catalog.
C
C     All defaults are set by GETFREQ later (to be redone each
C     call), so only use KEYCHR when a string longer than 8 characters
C     is needed.
C
      INCLUDE  'schfreq.inc'
C
      DOUBLE PRECISION  KD(*)
      CHARACTER         KC(*)*(*), MSGTXT*80
      INTEGER           KI(*)
      LOGICAL           DEBUG
C ----------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'INFREQ: Starting.' )
C
C     Defaults below not used - see above.
C
      CALL KEYCHR( 'VERSION', ' ', 20, KD, KC, KI )
      CALL KEYADD( 'STATIONs', 0.D0, MFSTA, KD, KC, KI )
      CALL KEYCHR( 'NAME', ' ', 12, KD, KC, KI )
      CALL KEYCHR( 'NOTE', ' ', 80, KD, KC, KI )
      CALL KEYADD( 'PRIOrity', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'IFNAME', 0.D0, MFIF, KD, KC, KI )
      CALL KEYADD( 'ALTIFN', 0.D0, MFIF, KD, KC, KI )
      CALL KEYADD( 'RF1', 0.D0, MFIF, KD, KC, KI )
      CALL KEYADD( 'RF2', 0.D0, MFIF, KD, KC, KI )
      CALL KEYADD( 'LO1', 0.D0, MFIF, KD, KC, KI )
      CALL KEYADD( 'FE', 0.D0, MFIF, KD, KC, KI )
      CALL KEYADD( 'POL', 0.D0, MFIF, KD, KC, KI )
      CALL KEYADD( 'SYN', 0.D0, 3, KD, KC, KI )
      CALL KEYADD( 'DUALX', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'LCP50CM', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'RCP50CM', 0.D0, 1, KD, KC, KI )
C
C     Special parameter to get 2 cm right where the filter
C     choice is made based on channel 1.
C
      CALL KEYADD( 'CH1RF1', 0.D0, MFIF, KD, KC, KI )
      CALL KEYADD( 'CH1RF2', 0.D0, MFIF, KD, KC, KI )
C
C     VLA parameters. 'ZZ' will cause error.
C
      CALL KEYCHR( 'VLABAND', 'ZZ', 2, KD, KC, KI )
      CALL KEYCHR( 'VLABW', 'ZZZZ', 4, KD, KC, KI )
      CALL KEYADD( 'FLUKEA', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'FLUKEB', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'VLAFEAB', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'VLAFECD', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'VLASYNA', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'VLASYNB', 0.D0, 1, KD, KC, KI )
      CALL KEYADD( 'FEFILTER', 0.D0, 1, KD, KC, KI )
C
C     Restrict IF to station (VLA1, VLA27) to keep down number
C     of entries and still allow some special cabling entries.
C
      CALL KEYADD( 'CHNSTA', 0.D0, MFIF, KD, KC, KI )
C
      IF( DEBUG ) THEN
         WRITE( MSGTXT, '( A, I5, A, I5, A, I5 )' )
     1        'INFREQ: MFRQV: ', KI(1), '  NFRQV: ', KI(2),
     2        '  NFRQS: ', KI(3)
         CALL WLOG( 0, MSGTXT )
      END IF
      RETURN
      END
