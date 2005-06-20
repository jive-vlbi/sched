      SUBROUTINE INFDB( ISCN, VALUE, KC, KI )
C
C     Routine for SCHED called by SCHIN to decode frequency and
C     doppler requests. (Used to be infreq.f, but that is now something
C     else - Don't ask how that happened and how I almost lost this
C     routine!)
C
C     The call should come after the SETUP is determined.
C
C     GOTFREQ (from sched.inc) tests if there are any frequency 
C     changes specified in the main schedule.  It only seems to 
C     be used by SNAP and by this routine (also commented code in 
C     GETSET).  GOTBW is used to help prevent use of BW when VEX
C     or VSOP (DRUDG) is being written.
C
      INCLUDE 'sched.inc'
C
      DOUBLE PRECISION  VALUE(*), DOPC
      INTEGER           I1, I2, KEYPTR, KI(*), ICHAN, ISCN
      CHARACTER         KC(*)*(*), KCHAR*80
C ------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LT. 3 ) CALL WLOG( 0, 'INFDB: Starting' )
C
C     Get freq and bw.  Default BW to first channel if others not given.
C
      I1 = KEYPTR( 'FREQ', KC, KI ) - 1
      I2 = KEYPTR( 'BW', KC, KI ) - 1
      DO ICHAN = 1, MAXCHN
         FREQ(ICHAN,ISCN) = VALUE(I1+ICHAN)
         BW(ICHAN,ISCN)   = VALUE(I2+ICHAN)
         IF( GOTFREQ .OR. FREQ(ICHAN,ISCN) .NE. 0.0 .OR. 
     1         BW(ICHAN,ISCN) .NE. 0.0 ) GOTFREQ = .TRUE.
         IF( BW(ICHAN,ISCN) .EQ. 0.0 ) 
     1         BW(ICHAN,ISCN) = BW(1,ISCN)
         IF( BW(ICHAN,ISCN) .NE. 0.0 ) GOTBW = .TRUE.
      END DO
C
C     Source name for Doppler calibration and default to scan source.
C     Get flag for whether or not to do Doppler calibration.
C
      DOPSRC(ISCN) = KCHAR( 'DOPSRC', 12, .TRUE., VALUE, KC, KI )
      IF( DOPSRC(ISCN)(1:1) .EQ. ' ' ) DOPSRC(ISCN) = SCNSRC(ISCN)
C
C     Jump through some hoops to allow old schedules.
C
      DOPC = VALUE( KEYPTR( 'DOPCAL', KC, KI ) )
      IF( DOPC .NE. UNSET ) THEN
         CALL WLOG( 1, 'INFREQ: Use DOPPLER, not DOPCAL.' )
         DOPCAL(ISCN) = DOPC .EQ. 0.D0
      ELSE
         CALL TOGGLE( DOPCAL, ISCN, 'DOPPLER', 'NODOP', UNSET,
     1                VALUE, KC, KI )
      END IF
      IF( DOPCAL(ISCN) ) GOTFREQ = .TRUE.
      LINES(ISCN) = KCHAR( 'LINENAME', 8, .TRUE., VALUE, KC, KI )
C
C     Get rounding increment for doppler calculations.  Default to
C     10 kHz.
C
      DOPINCR(ISCN,1) = VALUE( KEYPTR( 'DOPINCR', KC, KI ) )
      DOPINCR(ISCN,2) = VALUE( KEYPTR( 'DOPINCR', KC, KI ) + 1 )
      IF( DOPINCR(ISCN,1) .EQ. 0.0D0 ) DOPINCR(ISCN,1) = 10.0D0
C
C     Check that frequencies are not defaulting to previous
C     values when a setup is changed.  This has to come after FREQ
C     and SETUP are decoded.
C
      IF( ISCN .NE. 1 .AND. GOTFREQ ) THEN
         IF( SETNUM(ISCN) .NE. SETNUM(ISCN-1) .AND.
     1       FREQ(1,ISCN) .EQ. FREQ(1,ISCN-1) .AND.
     1       FREQ(1,ISCN) .NE. 0.0 )  THEN
            CALL WLOG( 1, 
     1       'INFDB: Setup changed and specified frequency didn''t. ' )
            CALL WLOG( 1, ' Is this really what you wanted? ' )
            CALL WLOG( 1, ' Use FREQ=0 to get new default.' )
         END IF
      END IF
C
      RETURN
      END
