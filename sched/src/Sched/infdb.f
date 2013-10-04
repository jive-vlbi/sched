      SUBROUTINE INFDB( ISCN, VALUE, KC, KI )
C
C     Routine for SCHED called by SCHIN to decode frequency and
C     doppler requests. (Used to be infreq.f, but that is now something
C     else - Don't ask how that happened and how I almost lost this
C     routine!)
C
C     The call should come after the SETUP is determined.
C
C     GOTFREQ and GOTBW fell out of use and have been eliminated
C     as of July 1, 2013.  RCW.
C
C     Adding CRDNCH, CRDBW, CRDFREQ, CRDDOP, and CRDNODOP to allow 
C     doppler and frequency settings of the BBCs of the legacy system 
C     (crd file) while the RDBE_PFB is being used.  This prevents the 
C     need for separate MARK5C and MARK5A schedules.  July, 2013 RCW.
C
      INCLUDE 'sched.inc'
C
      DOUBLE PRECISION  VALUE(*), DOPC
      INTEGER           I1, I2, KEYPTR, KI(*), ICHAN, ISCN
      CHARACTER         KC(*)*(*), KCHAR*256
C ------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LT. 3 ) CALL WLOG( 0, 'INFDB: Starting' )
C
C     Get freq and bw.  Default to first channel if others not given.
C     Will not really know the number of channels until we have the
C     setup files, so for now just process all slots in the arrays.
C
      I1 = KEYPTR( 'FREQ', KC, KI ) - 1
      I2 = KEYPTR( 'BW', KC, KI ) - 1
      DO ICHAN = 1, MAXCHN
C
         FREQ(ICHAN,ISCN) = VALUE(I1+ICHAN)
         IF( FREQ(ICHAN,ISCN) .EQ. 0.0D0 ) 
     1         FREQ(ICHAN,ISCN) = FREQ(1,ISCN)
C
         BW(ICHAN,ISCN)   = VALUE(I2+ICHAN)
         IF( BW(ICHAN,ISCN) .EQ. 0.0D0 ) 
     1         BW(ICHAN,ISCN) = BW(1,ISCN)
C
C        When BBSYN, BBFILT, BW etc were made double precision, they 
C        ended up with some spurious digits at insignificant levels that 
C        confounded some value tests.  Try to prevent that.  But allow
C        sub Hz settings - original rounding to 1 kHz was not good.
C
         FREQ(ICHAN,ISCN) = DNINT( FREQ(ICHAN,ISCN)*1.D7 ) / 1.D7
         BW(ICHAN,ISCN) = DNINT( BW(ICHAN,ISCN)*1.D7 ) / 1.D7
C
      END DO
C
C     Get CRDNCH and CRDCH1.  Do not attempt to fix the case when is
C     zero yet.  Will do that later when the setups are available.
C     Don't allow CRDCH1 without setting CRDNCH.
C
      CRDNCH(ISCN) = VALUE( KEYPTR( 'CRDNCH', KC, KI ) )
      CRDCH1(ISCN) = VALUE( KEYPTR( 'CRDCH1', KC, KI ) )
      IF( CRDCH1(ISCN) .NE. 1 .AND. CRDNCH(ISCN) .EQ. 0 ) THEN
         CALL ERRLOG( 
     1        'INFDB:  If CRDCH1 is set, CRDNCH must also be set.' )
      END IF
C
C     Get CRDDOP, CRDFREQ and CRDBW.  Default CRDFREQ and CRDBW to 
C     the first channel if others not given.
C     GOTCRD keeps track of whether CRDFREQ or CRDDOP were specified 
C     for this scan.
C
      I1 = KEYPTR( 'CRDFREQ', KC, KI ) - 1
      I2 = KEYPTR( 'CRDBW', KC, KI ) - 1
      CALL TOGGLE( CRDDOP, ISCN, 'CRDDOP', 'CRDNODOP', UNSET,
     1             VALUE, KC, KI )
      GOTCRD(ISCN) = CRDDOP(ISCN)
C
      DO ICHAN = 1, MAXCHN
C
         CRDFREQ(ICHAN,ISCN) = VALUE(I1+ICHAN)
         IF( CRDFREQ(ICHAN,ISCN) .EQ. 0.0D0 ) 
     1         CRDFREQ(ICHAN,ISCN) = CRDFREQ(1,ISCN)
C
         CRDBW(ICHAN,ISCN)   = VALUE(I2+ICHAN)
         IF( CRDBW(ICHAN,ISCN) .EQ. 0.0D0 ) 
     1         CRDBW(ICHAN,ISCN) = CRDBW(1,ISCN)
C
C        Again, try to avoid spurious digits.
C
         CRDFREQ(ICHAN,ISCN) = 
     1        DNINT( CRDFREQ(ICHAN,ISCN)*1.D7 ) / 1.D7
         CRDBW(ICHAN,ISCN) = DNINT( CRDBW(ICHAN,ISCN)*1.D7 ) / 1.D7
C
         GOTCRD(ISCN) = GOTCRD(ISCN) .OR. CRDFREQ(ICHAN,ISCN) .GT. 0.D0
C
      END DO
C
C     Require CRDNCH if CRDFREQ or CRDDOP were set.  Also don't allow
C     it to be greater than 8, the number of BBCs, because we don't
C     want the complications of trying to use upper/lower sidebands
C     and will only use one channel per BBC for the CRD parameters.
C
      IF( ( CRDNCH(ISCN) .LT. 1 .OR. CRDNCH(ISCN) .GT. 8 )
     1      .AND. GOTCRD(ISCN) ) THEN
         CALL WLOG( 1, 'INFDB: CRDNCH is required '//
     1       'and must be between 1 and 8 ' )
         CALL WLOG( 1, '       when CRDFREQ or CRDDOP is specified.' )
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I3, A, I5 )' ) 
     1     '       CRDNCH is ', CRDNCH(ISCN), ' on scan ', iscn
         CALL ERRLOG( MSGTXT )
      END IF
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
      IF( ISCN .NE. 1 .AND. FREQ(1,ISCN) .NE. 0.D0 ) THEN
         IF( SETNUM(ISCN) .NE. SETNUM(ISCN-1) .AND.
     1       FREQ(1,ISCN) .EQ. FREQ(1,ISCN-1) ) THEN
            CALL WLOG( 1, 
     1       'INFDB: Setup changed and specified frequency didn''t. ' )
            CALL WLOG( 1, ' Is this really what you wanted? ' )
            CALL WLOG( 1, ' Use FREQ=0 to get the new setup default.' )
         END IF
      END IF
C
      RETURN
      END
