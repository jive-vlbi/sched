      SUBROUTINE FSFREQ( KF, LOSUM, BBCFREQ, BBCBW,
     1         CRDN, CRDF, CRDB, CRDS, CRDLOSUM )
C
C     Routine for SCHED that gets the LO sum, the baseband frequency,
C     and the bandwidth for each channel for a frequency set.
C     It has to take into account any FREQ and BW requests in
C     the schedule, besides the setup file information.
C
C     Also provide the values for the VLBA legacy system
C     which might be different for reference pointing or if the 
C     new system is being used at too wide a bandwidth or with
C     too many channels.  These parameters are:
C       CRDN          Number of channels for the BBCs (1 per BBC).
C       CRDF(ICH)     Baseband frequency for channel.
C       CRDB(ICH)     Bandwidth for channel.
C       CRDS(ICH)     Sideband for channel.
C       CRDLOSUM(ICH) LO sum to use for setting up PCALX.
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
      INTEGER            KF, KS, KSCN, ILC, ISETF, ISIDE1, KSTA
      INTEGER            IFIF, IFCAT, LEN1, FSCN, ICH
      CHARACTER          SDAR*5
      DOUBLE PRECISION   LOSUM(*)
      DOUBLE PRECISION   BBCFREQ(*), BBCBW(*) 
      DOUBLE PRECISION   CRDF(*), CRDB(*), CRDLOSUM(*), FDROP
      LOGICAL            FWARN, VLAWARN, ERRS, DEQUAL
C
      INTEGER            CRDN
      CHARACTER          CRDS(*)*1
      INTEGER            NCH, ISIDE(MCHAN)
      INTEGER            IFSIDE(MCHAN), RFSIDE(MCHAN)
      DOUBLE PRECISION   FSHIFT
C
      DATA               FWARN, VLAWARN / .TRUE., .TRUE. /
      SAVE               FWARN, VLAWARN
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
C     and the setup file.
C
      KS = FSETKS(KF)
      KSCN = FSETSCN(KF)
      ISETF = ISETNUM(KS)
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
C        IF statments that should fail.  It is set here to I
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
            ISIDE1 = 1
         ELSE IF( SIDE1(ICH,KS) .EQ. 'L' ) THEN
            ISIDE1 = -1
         ELSE
            CALL WLOG( 1,
     1         'FSFREQ: First LO sideband not correctly specified.' )
            CALL WLOG( 1, ' Setup file: ' // 
     1         SETNAME(KS)(1:LEN1(SETNAME(KS))) )
            CALL ERRLOG( ' Fix the problem ' )
         END IF
C
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
            IF( ( FREQ(ILC,KSCN) - FIRSTLO(ICH,KS) ) * ISIDE1 .LT. 0 )
     1           THEN
               CALL WLOG( 1, 'FSFREQ: Frequency from main schedule' //
     1              ' not consistent with sideband from setup file.' )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, 2F14.5, I3 )' )
     1             '        Frequency, first LO, sideband: ',
     2              FREQ(ILC,KSCN), FIRSTLO(ICH,KS), ISIDE1
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
C           Use the SETUP file value.
C
            BBCFREQ(ICH) = BBSYN(ICH,KS)
C
         END IF
C
C        Warn if outside of VLA 50 MHz window at VLA
C
         IF( CONTROL(ISETSTA(KS)) .EQ. 'VLA' .AND. VLAWARN) THEN
            IF( BBCFREQ(ICH) .LT. 600.0D0 .OR.
     1          BBCFREQ(ICH) .GT. 650.0D0 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F10.2, A )' )
     1           'FSFREQ: **** VLA BBC frequency (',
     2           BBCFREQ(ICH), ') outside normal'
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1, 'FSFREQ:     range (600-650) at the VLA' )
            END IF
            VLAWARN = .FALSE.
         END IF
C
C        Get the bandwidth specification.
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
               CALL WLOG( 1, 'FSFREQ: **** Bandwidth from schedule more'
     1                  // ' than half of sample rate.' )
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
C        Get the LO sum.  
C
         LOSUM(ICH) = FIRSTLO(ICH,KS) + ISIDE1 * BBCFREQ(ICH)
         LOSUM(ICH) =  DNINT( LOSUM(ICH) * 1.D7 ) / 1.D7
C
      END DO
C
C     Check some of the results.
C
      KSTA = ISETSTA(KS)
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
         CALL CHKRDFQ( KS, BBCBW, BBCFREQ, ERRS )
      END IF
C
C     DBBC (EVN etc.)
C
      IF( DBE(KS) .EQ. 'DBBC_PFB' .OR. DBE(KS) .EQ. 'DBBC_DDC' ) THEN
         CALL CHKDBFQ( KS, BBCBW, BBCFREQ, ERRS )
      END IF
C
      IF( ERRS ) CALL ERRLOG( 'FSFREQ:  Fix problem with in-line '//
     1    'or Doppler derived frequencies or bandwidths.' )
C
C     Similar checks for Mark 4?
C
C     Get the CRD parameters if they are needed.  The values returned
C     should be usable at the VLBA for setting the BBC's in any situation.
C     They will duplicate the other values where that is appropriate, such
C     as with MARK5A observations.
C
      IF( SDAR(1:4) .EQ. 'RDBE' .AND. STATION(KSTA)(1:4) .EQ. 'VLBA' ) 
     1   THEN
C
C        Check if CRDFREQ or CRDDOP were used and frequencies already
C        were derived.  Since the CRD stuff is scan dependent, we need
C        the scan number associated with the frequency set.
C
         FSCN = FSETSCN(KF)
C
         IF( CRDNCH(FSCN) .GT. 0 ) THEN
            CRDN = CRDNCH(FSCN)
            DO ICH = 1, CRDN
               CRDF(ICH) = ABS( CRDFREQ(ICH,KSCN) - FIRSTLO(ICH,KS) )
               CRDB(ICH) = CRDBW(ICH,KSCN)
               CRDS(ICH) = CRDSIDE(ICH,KS)
               CRDLOSUM(ICH) = FIRSTLO(ICH,KS) + ISIDE1 * CRDB(ICH)
               CRDLOSUM(ICH) = DNINT( CRDLOSUM(ICH) * 1.D7 ) / 1.D7
            END DO
         ELSE
C
C           Now deal with the case where valid BBC frequencies need
C           to be established based on the setup and FREQ/BW entries.
C           They cannot necessarily be the same as in the setup because
C           the RDBE can handle more channels (PFB) and wider bandwidths
C           than the legacy system.  We will try for a reasonable match.
C
C           This code used to be in WRTFREQ, but has been moved in
C           case I want to use it other than in writing the crd files.
C           The CRDDOP etc parameters forced enough of a restructuring
C           to make it reasonable to put it here.
C
C           Deal with which setup channels to try to match.  We will only
C           do 8 (one per BBC) where the RDBE_PFB would have 16, so we
C           need to be selective.  Originally I went for the middle set,
C           but that makes it hard to explain what the CRDFREQ and CRDBW
C           are for.  This could cause use of wrong IF, FIRSTLO etc.
C           So just take the first 8.
C
            CRDN = MIN( 8, NCHAN(KS) )
C
C           Loop over the channels assigning data to use.
C
            DO ICH = 1, CRDN
               ISIDE(ICH) = 1
               IF( SIDEBD(ICH,KS) .EQ. 'L' ) ISIDE(ICH) = -1
               RFSIDE(ICH) = 1
               IF( NETSIDE(ICH,KS) .EQ. 'L' ) RFSIDE(ICH) = -1
               IFSIDE(ICH) = ISIDE(ICH) * RFSIDE(ICH)
               CRDB(ICH) = BBCBW(ICH)
               CRDF(ICH) = BBCFREQ(ICH)
C
C              If the observing band is too wide for the BBCs, reduce
C              the BBC bandwidth and center the band on the RDBE band.
C              Be sure the shift is a multiple of 10 kHz.
C
               FSHIFT = 0.0D0
               IF( CRDB(ICH) .GT. 16.0D0 ) THEN
                  CRDB(ICH) = 16.0D0
                  FSHIFT = ( BBCBW(ICH) - CRDB(ICH) ) / 2.D0
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
               CRDF(ICH) = CRDF(ICH) + ISIDE(ICH) * FSHIFT
               CRDF(ICH) = NINT( CRDF(ICH) * 100.D0 ) / 100.D0
               CRDLOSUM(ICH) = FIRSTLO(ICH,KS) + IFSIDE(ICH) * CRDF(ICH)
C
C              Get away from the frequency range that can't be 
C              covered by the old BBCs.  Go by a multiple of 
C              5 MHz to keep pcal detection frequencies for 5 MHz
C              tones.  Here I need the IF sideband which is
C              RFSIDE * ISIDE.  Note that requesting 1000.0 might
C              be ok for the BBCs, but blows the format used for
C              this parameter, so avoid it.
C
               IF( CRDF(ICH) .GE. 999.95D0 ) THEN
                  FDROP = CRDF(ICH) - 999.95D0
                  FDROP = 5.D0 * ( 1.D0 + DINT( FDROP / 5.D0 ))
                  CRDF(ICH) = CRDF(ICH) - FDROP
                  CRDLOSUM(ICH) = CRDLOSUM(ICH) - IFSIDE(ICH) * FDROP
               END IF
C
C              If the LO sum is an even number of MHz, the
C              pulse cal tones can be subject to aliasing.  Since
C              the exact frequency settings here are not critical
C              (not going to be used for the recordings), 
C              shift the CRDF and CRDLOSUM in such cases by 10 kHz 
C              so that the tone is at 10 kHz in the baseband.
C
C              Note that, in some cases with a FIRSTLO that is not an
C              integer MHz, CKSCHED may reject the specified tone frequencies
C              even if they are right.  Don't worry about that for the 
C              moment.  Hopefully we'll have pulse cal detection in the 
C              new system before the arrival of full use of the new
C              synthesizers makes this an issue.
C
               IF( ( FSPCAL(KF) .EQ. '1MHz' .OR. 
     1               FSPCAL(KF) .EQ. '5MHz' ) .AND. 
     2             MOD( CRDLOSUM(ICH), 1.D0 ) .EQ. 0.D0 ) THEN
                  CRDLOSUM(ICH) = CRDLOSUM(ICH) - RFSIDE(ICH) * 0.01D0
                  CRDF(ICH) = CRDF(ICH) - ISIDE(ICH) * 0.01D0
               END IF
            END DO
         END IF
      ELSE
C
C        Jump here if not using the RDBE on a VLBA station.  In this case
C        the setup values, possibly adjusted by FREQ and DOPPLER should be
C        used.  On the VLBA, this will likely be a MARK5A recording case and
C        we should not muck with the BBC frequencies.  On other stations, 
C        there probably isn't an ability, or need, to set one set of hardware
C        differently from another, and again we need to keep the original
C        frequencies so the interferometry works.
C
C        Just set these to the normal values derived for the setup
C        or FREQ and BW entries (which may include DOPPLER results).
C
         CRDN = NCHAN(KS)
         DO ICH = 1, CRDN
            CRDF(ICH) = BBCFREQ(ICH)
            CRDB(ICH) = BBCBW(ICH)
            CRDS(ICH) = SIDEBD(ICH,KS)
            CRDLOSUM(ICH) = LOSUM(ICH)
         END DO
      END IF

C
  999 CONTINUE
      IF( DEBUG ) CALL WLOG( 0, 'FSFREQ: Ending' )
      RETURN
      END
