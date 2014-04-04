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
C     Adding CRDSETCH Feb. 2014.  RCW
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
C     Get CRDNCH, CRDCH1, and CRDSETCH.  These are parameters for the
C     crd files for the VLBA to set the legacy system when the main
C     data path is to the RDBE.  CRDNCH is the number of legacy system
C     channels, CRDCH1 is the first of a continuous block of new system
C     channels to use to get many of the baseband channel parameters 
C     for the legacy system channels.  CRDSETCH is a list of setup 
C     file (new system) channels to use for channel parameters for the
C     legacy system.  CRDCH1 and CRDSETCH should not both be used. 
C     Prevent that.
C
C     Do not attempt to fix  the case when is CRDNCH is zero yet.  
C     Will do that later when the setups are available.   Don't allow 
C     CRDCH1 or CRDSETCH without setting CRDNCH.
C
C     If defaults are taken, they will be dealt with in GETCRDN
C
      CRDNCH(ISCN) = VALUE( KEYPTR( 'CRDNCH', KC, KI ) )
      CRDCH1(ISCN) = VALUE( KEYPTR( 'CRDCH1', KC, KI ) )
      DO ICHAN = 1, MAXCRD
         CRDSETCH(ICHAN,ISCN) = 
     1        VALUE( KEYPTR( 'CRDSETCH', KC, KI )+ICHAN-1 )
      END DO
C
      IF( ( CRDCH1(ISCN) .NE. 0 .OR. CRDSETCH(1,ISCN) .NE. 0 ) 
     1           .AND. CRDNCH(ISCN) .EQ. 0 ) THEN
         CALL ERRLOG( 'INFDB:  If CRDCH1 or CRDSETCH is '//
     1         'set, CRDNCH must also be set.' )
      END IF
      IF( CRDCH1(ISCN) .NE. 0 .AND. CRDSETCH(1,ISCN) .NE. 0 ) THEN
         CALL ERRLOG( 'INFDB:  Do not use both CRDCH1 and CRDSETCH!' )
      END IF
      IF( CRDNCH(ISCN) .GT. MAXCRD ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I4, A, I4, A, I5 )' )
     1     'INFDB:  CRDNCH set too high.  Maximum is', MAXCRD,
     2     '.  It is ', CRDNCH(ISCN), ' in input scan ', ISCN
         CALL ERRLOG( MSGTXT )
      END IF
C
C     If CRDCH1 was used, fill in CRDSETCH so we can use that from
C     this point forward.
C
      IF( CRDCH1(ISCN) .NE. 0 ) THEN
         DO ICHAN = 1, CRDNCH(ISCN)
            CRDSETCH(ICHAN,ISCN) = CRDCH1(ISCN) + ICHAN - 1
         END DO
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
C     it to be greater than MAXCRD, the number of BBCs( 4 after 
C     early 2014), because we don't want the complications of trying 
C     to use upper/lower sidebands and will only use one channel per 
C     BBC for the CRD parameters.
C
      IF( ( CRDNCH(ISCN) .LT. 1 .OR. CRDNCH(ISCN) .GT. MAXCRD )
     1      .AND. GOTCRD(ISCN) ) THEN
         WRITE( MSGTXT, '( A, A, I3 )' ) 
     1       'INFDB: CRDNCH is required and must be between 1 and ', 
     2       MAXCRD
         CALL WLOG( 1, MSGTXT )
         CALL WLOG( 1, '       when CRDFREQ or CRDDOP is specified.' )
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I3, A, I5 )' ) 
     1     '       CRDNCH is ', CRDNCH(ISCN), ' on scan ', iscn
         CALL ERRLOG( MSGTXT )
      END IF
C
C     Require CRDCH1 or CRDSETCH to have been set if CRDDOP is invoked.
C     When COPCRD is called, a station is not selected so GETCRDN, which
C     sets the defaults of CRSETC, needs.  So just force the user to
C     think about it.  Note CRDCH1 has been used to fill out CRDSETCH
C     above if needed so only check CRDSETCH.
C
      IF( CRDDOP(ISCN) .AND. CRDSETCH(1,ISCN) .LT. 1 ) THEN
         CALL ERRLOG( 'INFDB:  If invoking DOPCRD, please also set '//
     1       'CRDCH1 or CRDSETCH' )
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
C     Get rounding increment for doppler calculations.  Leave a 
C     default zero value to be set later.  We need to know the
C     recording systems in use to find the right default and we
C     might not know that when this routine is run.
C
      DOPINCR(ISCN,1) = VALUE( KEYPTR( 'DOPINCR', KC, KI ) )
      DOPINCR(ISCN,2) = VALUE( KEYPTR( 'DOPINCR', KC, KI ) + 1 )
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
