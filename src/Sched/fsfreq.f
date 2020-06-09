      SUBROUTINE FSFREQ( KF, LOSUM, BBCFREQ, BBCBW,
     1         CRDN, CRDF, CRDB, CRDS, CRDLOSUM, CRSETC )
C
C     Routine for SCHED that gets the LO sum, the baseband frequency,
C     and the bandwidth for each channel for a frequency set.
C     It has to take into account any FREQ and BW requests in
C     the schedule, besides the setup file information.  Note that
C     the bandwidth must be half the sample rate with most of the 
C     modern systems, so the bandwidth setting capability is no
C     longer all that useful.  This could be changed in SCHED by
C     adjusting the sample rate, but that is a big job left for
C     some later time.
C
C     This routine also provides the values for the VLBA legacy system
C     which might be different for reference pointing or if the 
C     new system is being used at too wide a bandwidth or with
C     too many channels.  These parameters are:
C       CRDN          Number of channels for the BBCs (1 per BBC).
C       CRDF(ICH)     Baseband frequency for channel.
C       CRDB(ICH)     Bandwidth for channel.
C       CRDS(ICH)     Baseband sideband for channel (U/L)
C       CRDLOSUM(ICH) LO sum to use for setting up PCALX.
C     The legacy system sample rate can 
C     Note CRDS is from SIDEBD, which is SIDEBAND in the setup.
C
C     Note that, for historical reasons, there is a potential 
C     confusion because the BBC parameters of the call
C     refer to the RDBE channels for the VLBA when that is
C     in use while the CRD parameters are for the legacy BBCs.
C
C     Do not call ERRSET from this routine despite the temptation.
C     It is called by ERRSET, so that can cause big trouble.
C
C     This routine is called by a number of routines including:
C     CHKSCN, FSMATCH, OMSFREQ, PCALFQ, PRTFREQ, PRTSCH, STAFRD, and
C     WRTFREQ.
C
      INCLUDE     'sched.inc'
      INCLUDE     'schset.inc'
      INCLUDE     'schfreq.inc'
C
      INTEGER            KF, KS, KSCN, ILC, ISETF, KSTA, ISTA
      INTEGER            IFIF, IFCAT, LEN1, ICH
      CHARACTER          SDAR*5
      DOUBLE PRECISION   LOSUM(*)
      DOUBLE PRECISION   BBCFREQ(*), BBCBW(*) 
      DOUBLE PRECISION   CRDF(*), CRDB(*), CRDLOSUM(*), FDROP
      LOGICAL            FWARN, ERRS, DEQUAL
C
      INTEGER            CRDN, CRSETC(*), ICHS
      CHARACTER          CRDS(*)*1
      INTEGER            ICRDS(MCHAN)
      INTEGER            ISIDE1(MCHAN), INETSIDE(MCHAN)
      DOUBLE PRECISION   FSHIFT
C
      DATA               FWARN / .TRUE. /
      SAVE               FWARN
C ----------------------------------------------------------------------
      IF( DEBUG ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I5 )' )
     1     'FSFREQ: starting for freq group ', KF
         CALL WLOG( 0, MSGTXT )
      END IF
C
C     Make sure this wasn't mistakenly called with KF = 0, perhaps 
C     when NOSETUP was specified.  I had a complaint from JIVE about
C     this happening.  It is probably ok to just not take action.
C
      IF( KF .EQ. 0 ) THEN
         CALL WLOG( 1, 'FSFREQ: Called for frequency set zero.  '//
     1        'Non-fatal (?) programming issue.' )
         GO TO 999
      END IF
C
C     Get the setup group, the defining scan of the frequency set,
C     and the setup file.  Also get the schedule station number of
C     the catalog station.
C
      KS    = FSETKS(KF)
      KSCN  = FSETSCN(KF)
      KSTA  = ISETSTA(KS)
      ISETF = ISETNUM(KS)
      ISTA  = ISCHSTA(KSTA)
      IF( ISTA .LE. 0 ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I5, A, I5 )' )
     1      'FSFREQ:  Schedule station corresponding to setup ', 
     2      'station ', KSTA, ' is ', ISTA 
         CALL WLOG( 1, MSGTXT )
         CALL ERRLOG( '         That is now allowed. ' //
     1      'Programming error? ' )
      END IF
C
C     Get the number of channels and the list of setup file channels
C     to use for parameters other than frequency and bandwidth.
C
      CALL GETCRDN( KSCN, ISTA, CRDN, CRSETC )
C
C----------------------------------------
C     Deal with the VLBI channels (These are for the Vex file).
C----------------------------------------
C
C     On the VLBA, these are likely to be in the RDBE as the MARK5A
C     system is retired.
C 
C     Loop through the setup group channels.
C
      DO ICH = 1, NCHAN(KS)
C
C        If we are getting frequencies from the scan inputs, they
C        must be for the correct logical channel, which just might
C        not be for that same channel number in the setup group.
C
C        Note that, if FREQ or BW come from scan input and OKXC is
C        false, the program should have aborted in DOPFQ.  OKXC
C        is allowed to be false if the frequencies are from the
C        setup group only.
C
C        Get the setup file "logical" channel.  Note that, if
C        OKXC is false, ILC will not be used except in some
C        IF statments that should fail.  It is set here to ICH
C        to keep those IF statements from having addressing 
C        exceptions.
C
         IF( OKXC(ISETF) ) THEN
            ILC = SFCHAN(ICH,KS)
         ELSE
            ILC = ICH
         END IF
C
C        Assume that the sideband structure and firstlo are the
C        same as the setup file.
C
         IF( SIDE1(ICH,KS) .EQ. 'U' ) THEN
            ISIDE1(ICH) = 1
         ELSE IF( SIDE1(ICH,KS) .EQ. 'L' ) THEN
            ISIDE1(ICH) = -1
         ELSE
            CALL WLOG( 1,
     1         'FSFREQ: First LO sideband not correctly specified.' )
            CALL WLOG( 1, ' Setup file: ' // 
     1         SETNAME(KS)(1:LEN1(SETNAME(KS))) )
            CALL ERRLOG( ' Fix the problem ' )
         END IF
C
C        Get the BBC frequency using the FREQ input or setup value.

         IF( OKXC(ISETF) .AND. FREQ(ILC,KSCN) .GT. 0.0D0 ) THEN
C
C           Use the frequency from the main schedule.  The fact that
C           it is not zero implies that either DOPPLER or FREQ was
C           specified for the scan(s).  Avoid spurious low significant 
C           digits but allow down to fractional Hz.  The allowed digits
C           will be set elsewhere and we don't want to preclude those
C           tests here.  FREQ will be in the system (logical channels)
C           before the sideband inversion reflected in CORINV for
C           RDBE/PFB stations.  So CORINV needs to be added.
C
            BBCFREQ(ICH) = ABS( FREQ(ILC,KSCN) + CORINV(ICH,KS) - 
     1                        FIRSTLO(ICH,KS) )
            BBCFREQ(ICH) =  DNINT( BBCFREQ(ICH) * 1.D7 ) / 1.D7
C
C           Check that the assumed sideband is correct.  Pedantically,
C           this should be corrected for CORINV, but it won't matter.
C
            IF( ( FREQ(ILC,KSCN) -
     1           FIRSTLO(ICH,KS) ) * ISIDE1(ILC) .LT. 0 ) THEN
               CALL WLOG( 1, 'FSFREQ: Frequency from main schedule' //
     1              ' not consistent with sideband from setup file.' )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, 2F14.5, I3 )' )
     1             '        Frequency, first LO, sideband: ',
     2              FREQ(ILC,KSCN), FIRSTLO(ICH,KS), ISIDE1(ILC)
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1, ' Setup file: ' //
     1                    SETNAME(KS)(1:LEN1(SETNAME(KS))) )
               CALL ERRLOG( ' Fix frequencies ' )
            END IF
C
C           Check that FREQ is in the RF range for the current
C           frequency catalog group, if a frequency catalog group
C           was used.  The frequency catalog group is IFREQNUM, which
C           can be channel dependent, as determined in SETFCAT).  
C           This check only needed when a frequency is specified in 
C           the schedule, which it is if you got through the last 
C           IF statement.  The check has already been checked for 
C           setup file freqs.
C
            IF( IFREQNUM(ICH,KS) .GE. 1 .AND. FWARN ) THEN
               IFIF = IFREQIF(ICH,KS)
               IFCAT = IFREQNUM(ICH,KS)
               IF( ( FREQ(ILC,KSCN) + CORINV(ICH,KS) .LT. 
     1                 FRF1(IFIF,IFCAT) .OR. 
     2               FREQ(ILC,KSCN) + CORINV(ICH,KS) .GT. 
     3                 FRF2(IFIF,IFCAT) ) ) THEN
                  CALL WLOG( 1, 'FSFREQ: -- WARNING -- Frequency ' //
     1                 'specified in  schedule (or by Doppler) is ' )
                  CALL WLOG( 1, '          outside limits for IF.' )
                  CALL WLOG( 1, '        Setup file: ' //
     1                       SETNAME(KS)(1:LEN1(SETNAME(KS))) )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I4, A, A )' ) 
     1              '        Setup group:', KS, '  Station: ', 
     2                       SETSTA(1,KS)
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, F9.2 )' )
     1             '        Frequency:', FREQ(ILC,KSCN) + CORINV(ICH,KS)
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, F9.2, A, F9.2 )' )
     1              '        Freq catalog limits: ', FRF1(IFIF,IFCAT),
     2              ' to ', FRF2(IFIF,IFCAT)
                  CALL WLOG( 1, MSGTXT )
                  FWARN = .FALSE.
               END IF
            END IF
C
         ELSE
C
C           Use the SETUP file value.  Simple case.
C
            BBCFREQ(ICH) = BBSYN(ICH,KS)
C
         END IF
C
C        Get the BBC bandwidth specification, again using program input
C        or the setup file value.
C
         IF( OKXC(ISETF) .AND. BW(ILC,KSCN) .NE. 0.0D0 ) THEN
C
C           Set the bandwidth from the BW command and check 
C           against sample rate.  ABS in case of old files with
C           sign of BW indicating sideband.
C
            BBCBW(ICH) = ABS( BW(ILC,KSCN) )
C
            IF( BBCBW(ICH) .GT. 0.50001D0 * SAMPRATE(KS) ) THEN
               CALL WLOG( 1, 'FSFREQ: **** Bandwidth from schedule '
     1                  // ' more than half of sample rate.' )
               CALL WLOG( 1, ' Setup file: ' //
     1                 SETNAME(KS)(1:LEN1(SETNAME(KS))) )
               CALL ERRLOG( ' Fix bandwidth or sample rate ' )
            END IF
C
         ELSE
C
C           Otherwise use the setup file value.
C
            BBCBW(ICH) = BBFILT(ICH,KS)
C
         END IF
C
C        Get the LO sum.  Clean up any odd digits.
C
         LOSUM(ICH) = FIRSTLO(ICH,KS) + ISIDE1(ICH) * BBCFREQ(ICH)
         LOSUM(ICH) =  DNINT( LOSUM(ICH) * 1.D7 ) / 1.D7
C
      END DO
C
C     Check some of the results.
C
      SDAR = DAR(KSTA)
C
C     Make some VLBA specific checks.
C
      IF( SDAR .EQ. 'VLBA' ) THEN
C
C        Check that the BBC frequency is in range for VLBA BBCs and     
C        that the requested bandwidth is valid.
C
         DO ICH = 1, NCHAN(KS)
            IF( BBCFREQ(ICH) .LT. 500.0D0 .OR.
     5          BBCFREQ(ICH) .GE. 1000.0D0 ) THEN
               WRITE( MSGTXT, '( A, I3, F10.2, I5, F10.2, I5 )' )
     1            'FSFREQ: chan, firstLO, setup, freq, freq set: ', 
     2            ICH, FIRSTLO(ICH,KS), KS, LOSUM(ICH), KF
               CALL WLOG( 1, MSGTXT )
               WRITE( MSGTXT, '( A, F10.2 )' )
     1            'FSFREQ: Bad VLBA BBC frequency:', BBCFREQ(ICH)
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1, ' Setup file: ' //
     1             SETNAME(KS)(1:LEN1(SETNAME(KS))) )
               CALL ERRLOG( ' Fix frequencies ' )
            END IF
C
            IF( .NOT. ( DEQUAL( BBCBW(ICH), 0.0625D0 ) .OR. 
     1                  DEQUAL( BBCBW(ICH), 0.125D0 ) .OR.
     2                  DEQUAL( BBCBW(ICH), 0.250D0 ) .OR. 
     3                  DEQUAL( BBCBW(ICH), 0.5D0 ) .OR.
     4                  DEQUAL( BBCBW(ICH), 1.0D0 ) .OR. 
     5                  DEQUAL( BBCBW(ICH), 2.0D0 ) .OR.
     6                  DEQUAL( BBCBW(ICH), 4.0D0 ) .OR. 
     7                  DEQUAL( BBCBW(ICH), 8.0D0 ) .OR.  
     8                  DEQUAL( BBCBW(ICH), 16.D0 ) ) ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F8.2 )' )
     1            'FSFREQ: Invalid bandwidth request (in scan?): ',
     2            BBCBW(ICH)
               CALL ERRLOG( MSGTXT )
            END IF
         END DO
      END IF
C
C     Similar checks for the RDBE or DBBC, but put in subroutine that
C     is called from here and from CHKRDBE or CHKDBBC.  First the RDBE.
C
      ERRS = .FALSE.
      IF( DBE(KS) .EQ. 'RDBE_PFB' .OR. DBE(KS) .EQ. 'RDBE_DDC' ) THEN
         CALL CHKRDFQ( KS, KF, BBCBW, BBCFREQ, ERRS )
      END IF
C
C     DBBC (EVN etc.)
C
      IF( DBE(KS) .EQ. 'DBBC_PFB' .OR. DBE(KS) .EQ. 'DBBC_DDC' ) THEN
         CALL CHKDBFQ( KS, BBCBW, BBCFREQ, ERRS, .FALSE. )
      END IF
C
      IF( ERRS ) THEN
         CALL WLOG( 1, 'FSFREQ:  Fix problem with in-line '//
     1    'or Doppler derived frequencies or bandwidths.' )
         CALL ERRSET( KS )
      END IF
C
C     Similar checks for Mark 4?
C
C
C----------------------------------------------
C     CRD parameters for legacy BBCs.
C----------------------------------------------
C
C     Get the CRD parameters if they are needed, which they always will
C     be for a VLBA station until the legacy system is gone.  For some
C     cases (MARK5A observations, narrow band DDC setups ...), these
C     values will duplicate items in the normal setup, but it is easier
C     for the calling routines if they can always use the value from
C     here so set them regardless.
C
C     First the cases when the CRD parameters should just be the same as
C     the setup or the main FREQ and DOPPLER numbers.  This is when 
C     either a VLBA control system is not being used or the RDBE is 
C     not being used.
C
      IF( SDAR(1:4) .NE. 'RDBE' .OR. 
     1    ( CONTROL(KSTA) .NE. 'VLBA' .AND. .NOT. VLBADAR(KSTA) ) ) THEN
C
C        For this case, the setup values, possibly adjusted by FREQ and 
C        DOPPLER should be used.  On the VLBA, this will likely be a 
C        MARK5A recording case and we should not muck with the BBC 
C        frequencies using the CRD parameters.  Other stations  don't 
C        use the CRD parameters, so just put in the setup values
C        as dummies.
C
         DO ICH = 1, CRDN
            ICHS = CRSETC(ICH)
            CRDF(ICH) = BBCFREQ(ICHS)
            CRDB(ICH) = BBCBW(ICHS)
            CRDS(ICH) = SIDEBD(ICHS,KS)
            CRDLOSUM(ICH) = LOSUM(ICHS)
         END DO
C
      ELSE 
C
C        Now the case when using the RDBE on the VLBA or a station with 
C        the legacy VLBA control system (GBT and EB are the only ones in
C        the station catalog).  The legacy BBCs are used to generate 
C        some Tsys and pulse cal data, but not VLBI data.  More 
C        importantly, the legacy BBCs generate the power data used for 
C        reference pointing on the VLBA.  Since the VLBI data are not
C        affected, we are free to change the frequencies, bandwidths, 
C        and number of channels.
C
C        First get CRDB and CRDS - the legacy BBC bandwidth and 
C        sideband - out of the way as they need the same treatment for
C        all of the cases dealt with later.  Here use CRDBW if it is 
C        not zero and BBCBW if it is.  If BBCBW is over 16 MHz, the 
C        maximum that the BBCs allow, use 16 MHz.  Do not let CRDB
C        exceed BBCBW.  That would create more issues with the samplerate
C        and format than are worth trying to deal with for a case 
C        that is not likely to be used.
C
C        Using CRDBW even in scans that do not have CRDFREQ or CRDDOP 
C        allows adjustment of the BBC bandwidth without setting the 
C        other CRD parameters.  I'm not too sure why you would do that,
C        but it's easy.
C
C        Do for the number of channels given in GETCRDN and relate 
C        those to the specified setup file channels.
C
         DO ICH = 1, CRDN
            ICHS = CRSETC(ICH)
            IF( CRDBW(ICH,KSCN) .LE. 0.D0 ) THEN
               CRDB(ICH) = MIN( BBCBW(ICHS), 16.D0 )
            ELSE IF( CRDBW(ICH,KSCN) .GT. BBCBW(ICHS) ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( 2A )' )
     1            'FSFREQ:  CRDBW value greater than setup file ',
     2            'value.  Don''t do that!'
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I4, A, F10.2 )' )
     1            '         Setup: ', KS, '  CRDBW: ', CRDBW(ICH,KSCN)
               CALL ERRLOG( MSGTXT )
            ELSE
               CRDB(ICH) = CRDBW(ICH,KSCN)
            END IF
C
C           Deal with sidebands.  Keep same as RDBE before any
C           inversions.  Note that we need to do the same as in DOPCRD
C           or the frequencies will be wrong.
C
C    *******************  I think I need SIDEDB not CRDSIDE - need to use
C                         the final sideband setting.
C           Note that CRDSIDE is a saved version of SIDEDB from before
C           any sideband inversions were done.  The channel index is
C           the setup file channel index, not the crd channel.
C
            IF( SIDEBD(ICHS,KS) .EQ. 'U' ) THEN
               CRDS(ICH) = 'U'
               ICRDS(ICH) = 1
            ELSE
               CRDS(ICH) = 'L'
               ICRDS(ICH) = -1
            END IF
         END DO
C
C        If CRDFREQ or CRDDOP were used, GOTCRD should be set and CRDFREQ
C        should either be the input value or one calculated by DOPCRD.
C        Since the CRD stuff is scan dependent, we get the needed information
C        from the reference scan for the frequency set.  That was 
C        set above as KSCN.
C
C        CRDNCH is a required user input if CRDFREQ or CRDDOP is used
C        but can be given any time.  Use it if set.  INFDB will already
C        have require that it be set if CRDFREQ or CRDDOP is used.
C        DOPCRD has enforced reasonable values for all scans.  The user
C        could have used CRDCH1 or CRDSETSC to specify that the mapping
C        between setup channels and legacy system channels.  This will 
C        be useful for dual band cases when some CRD channels are 
C        desired from both bands, although SCHED's defaults will also 
C        enforce having all IFs represented.  Anyway, because of this 
C        option, we need to be careful about the meaning of the channel 
C        numbers.  Here ICH is an output CRD channel and ICHS is the 
C        corresponding setup file channel.
C
         IF( GOTCRD(KSCN) ) THEN
            DO ICH = 1, CRDN
               ICHS = CRSETC(ICH)
               CRDF(ICH) = ABS( CRDFREQ(ICH,KSCN) - FIRSTLO(ICHS,KS) )
               CRDB(ICH) = CRDBW(ICH,KSCN)
               CRDLOSUM(ICH) = FIRSTLO(ICHS,KS) + ISIDE1(ICHS) * 
     1               CRDF(ICH)
               CRDLOSUM(ICH) = DNINT( CRDLOSUM(ICH) * 1.D7 ) / 1.D7
C
C              Complain if CRDFREQ has not been set (happened during
C              debugging).
C
               IF( CRDFREQ(ICH,KSCN) .LE. 0.D0 ) THEN
                  CALL WLOG( 1, 'FSFREQ: CRDFREQ zero when either '//
     1                'it or CRDDOP was set by user.' )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I5, A, I5, A, F12.4 )' )
     1                '        KSCN:', KSCN, '    ICH:', ICH,
     2                '    CRDFREQ:', CRDFREQ(ICH,KSCN)
                  CALL WLOG( 1, MSGTXT )
                  CALL ERRLOG( ' Probable programming error.  ' )
               END IF
            END DO
         ELSE
C
C           Now deal with the case where valid BBC frequencies need
C           to be established based on the setup and FREQ/BW entries.
C           They cannot necessarily be the same as in the setup because
C           the RDBE can handle more channels (PFB) and wider bandwidths
C           (DDC and PFB) than the legacy system.  We will try for a 
C           reasonable match.
C
C           This code used to be in WRTFREQ, but has been moved in
C           case I want to use it other than in writing the crd files.
C           The CRDDOP etc parameters forced enough of a restructuring
C           to make it reasonable to put it here.
C
C           Deal with which setup channels to try to match.  We will only
C           do 8 (one per BBC).  This is fine for the DDC, but for the
C           PFB there are 16 channels, so we need to be selective.  
C           For now, do the first 8.  We used to do the middle 8 but 
C           it is easier to do the first 8, although the benefits are
C           less strong than I thought when I switched.  We could go
C           back to the middle 8 some day.  If so, neet to deal with 
C           the bandwidth above.
C
C           Allow the user to have specified the number of channels 
C           even if not doing CRDFREQ or CRDDOP.
C
C
C           Loop over the channels assigning data to use.
C
            DO ICH = 1, CRDN
               ICHS = CRSETC(ICH)
C
C              Get an integer version of NETSIDE for multiplications.
C
               IF( NETSIDE(ICHS,KS) .EQ. 'U' ) THEN
                  INETSIDE(ICH) = 1
               ELSE
                  INETSIDE(ICH) = -1
               END IF
C
C              Start with setting the bandwidth and frequency to the
C              setup file versions.
C
               CRDF(ICH) = BBCFREQ(ICHS)
C
C              If the bandwidth had to be limited to the BBC hardware
C              max, get the amount to shift the frequency to center
C              on the RDBE band.
C
C              Be sure the shift is a multiple of 10 kHz.
C
               FSHIFT = 0.0D0
               IF( BBCBW(ICHS) .GT. 16.0D0 ) THEN
                  CRDB(ICH) = 16.0D0
                  FSHIFT = ( BBCBW(ICHS) - CRDB(ICH) ) / 2.D0
                  FSHIFT = NINT( FSHIFT * 100.D0 ) / 100.D0
               END IF
C
C              Get the new BBC frequency and LO sum.  Note that
C              the effects of the sideband can differ.  Round the
C              new BBC frequency to 10 kHz in case it doesn't 
C              conform to that BBC constraint.  Note that DDC
C              frequency rules might well generate main basebands 
C              that do not conform to the legacy constraints.
C
               CRDF(ICH) = CRDF(ICH) + ICRDS(ICH) * FSHIFT
               CRDF(ICH) = NINT( CRDF(ICH) * 100.D0 ) / 100.D0
C
C              Get the LO sum for the legacy channels.
C
               CRDLOSUM(ICH) = FIRSTLO(ICHS,KS) + 
     1               ISIDE1(ICHS) * CRDF(ICH)
C
C              If the LO sum is an integer number of MHz, the
C              pulse cal tones can be subject to aliasing.  Since
C              the exact frequency settings here are not critical
C              (not going to be used for the recordings), 
C              shift the CRDF and CRDLOSUM in such cases by 10 kHz 
C              so that the tone is at 10 kHz in the baseband.
C
C              Do this before the FDROP adjustment in case this provokes
C              crossing the boundary of allowed values.
C
C              Note that, in some cases with a FIRSTLO that is not an
C              integer MHz (eg new VLBA synthesizer), CKSCHED may 
C              reject the specified tone frequencies
C              even if they are right.  Don't worry about that for the 
C              moment.  Hopefully we'll have pulse cal detection in the 
C              new system before the arrival of full use of the new
C              synthesizers makes this an issue.
C
               IF( ( FSPCAL(KF) .EQ. '1MHz' .OR. 
     1               FSPCAL(KF) .EQ. '5MHz' ) .AND. 
     2             MOD( CRDLOSUM(ICH), 1.D0 ) .EQ. 0.D0 ) THEN
                  CRDLOSUM(ICH) = CRDLOSUM(ICH) - INETSIDE(ICH) * 0.01D0
                  CRDF(ICH) = ABS( FIRSTLO(ICHS,KS) - CRDLOSUM(ICH) )
               END IF
C
C              Get away from the frequency range that can't be 
C              covered by the old BBCs but are ok for the RDBE 
C              (1000.0 to 1024.0 - or even up to 1040 in some cases
C              with a sacrificial channel).  Go by a multiple of 
C              5 MHz to keep pcal detection frequencies for 5 MHz
C              tones.
C
               IF( CRDF(ICH) .GE. 999.95D0 ) THEN
                  FDROP = CRDF(ICH) - 999.95D0
                  FDROP = 5.D0 * ( 1.D0 + DINT( FDROP / 5.D0 ))
                  CRDF(ICH) = CRDF(ICH) - FDROP
                  CRDLOSUM(ICH) = FIRSTLO(ICHS,KS) + 
     1                            ISIDE1(ICHS) * CRDF(ICH)
               END IF
            END DO
         END IF
      END IF

C
  999 CONTINUE
      IF( DEBUG ) CALL WLOG( 0, 'FSFREQ: Ending' )
      RETURN
      END
