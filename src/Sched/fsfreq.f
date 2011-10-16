      SUBROUTINE FSFREQ( KF, LOSUM, BBCFREQ, BBCBW )
C
C     Routine for SCHED that gets the LO sum, the baseband frequency,
C     and the bandwidth for each channel for a frequency set.
C     It has to take into account any FREQ and BW requests in
C     the schedule, besides the setup file information.
C
C     Do not call ERRSET from this routine despite the temptation.
C     It is called by ERRSET, so that can cause big trouble.
C
      INCLUDE     'sched.inc'
      INCLUDE     'schset.inc'
      INCLUDE     'schfreq.inc'
C
      INTEGER            KF, I, KS, KSCN, ILC, ISETF, ISIDE1, KSTA
      INTEGER            IFIF, IFCAT, LEN1
      CHARACTER          SDAR*5
      DOUBLE PRECISION   LOSUM(*)
      DOUBLE PRECISION   BBCFREQ(*), BBCBW(*), FRAD1, FRAD2
      LOGICAL            FWARN, VLAWARN, ERRS, DEQUAL
C
      DATA               FWARN, VLAWARN / .TRUE., .TRUE. /
      SAVE               FWARN, VLAWARN
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'FSFREQ starting.' )
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
      DO I = 1, NCHAN(KS)
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
            ILC = SFCHAN(I,KS)
         ELSE
            ILC = I
         END IF
C
C        Assume that the sideband structure and firstlo are the
C        same as the setup file.
C
         IF( SIDE1(I,KS) .EQ. 'U' ) THEN
            ISIDE1 = 1
         ELSE IF( SIDE1(I,KS) .EQ. 'L' ) THEN
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
C           digits.  FREQ will be in the system (logical channels)
C           before the sideband inversion reflected in CORINV for
C           RDBE/PFB stations.  So CORINV needs to be added.
C
            BBCFREQ(I) = ABS( FREQ(ILC,KSCN) + CORINV(I,KS) - 
     1                        FIRSTLO(I,KS) )
            BBCFREQ(I) =  DNINT( BBCFREQ(I) * 1000.D0 ) / 1000.D0
C
C           Check that the assumed sideband is correct.  Pedantically,
C           this should be corrected for CORINV, but it won't matter.
C
            IF( ( FREQ(ILC,KSCN) - FIRSTLO(I,KS) ) * ISIDE1 .LT. 0 )
     1           THEN
               CALL WLOG( 1, 'FSFREQ: Frequency from main schedule' //
     1              ' not consistent with sideband from setup file.' )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, 2F14.5, I3 )' )
     1             '        Frequency, first LO, sideband: ',
     2              FREQ(ILC,KSCN), FIRSTLO(I,KS), ISIDE1
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
            IF( IFREQNUM(I,KS) .GE. 1 .AND. FWARN ) THEN
               IFIF = IFREQIF(I,KS)
               IFCAT = IFREQNUM(I,KS)
               CALL FRFADJ( KS, IFIF, IFCAT, FRAD1, FRAD2 )
               IF( ( FREQ(ILC,KSCN) + CORINV(I,KS) .LT. FRAD1 .OR. 
     1               FREQ(ILC,KSCN) + CORINV(I,KS) .GT. FRAD2 ) ) THEN
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
     1              '        Frequency:', FREQ(ILC,KSCN) + CORINV(I,KS)
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, F9.2, A, F9.2 )' )
     1              '        Freq catalog limits: ', FRAD1,
     2              ' to ', FRAD2
                  CALL WLOG( 1, MSGTXT )
                  FWARN = .FALSE.
               END IF
            END IF
C
         ELSE
C
C           Use the SETUP file value.
C
            BBCFREQ(I) = BBSYN(I,KS)
C
         END IF
C
C        Warn if outside of VLA 50 MHz window at VLA
C
         IF( CONTROL(ISETSTA(KS)) .EQ. 'VLA' .AND. VLAWARN) THEN
            IF( BBCFREQ(I) .LT. 600.0D0 .OR.
     1          BBCFREQ(I) .GT. 650.0D0 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F10.2, A )' )
     1           'FSFREQ: **** VLA BBC frequency (',
     2           BBCFREQ(I), ') outside normal'
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
            BBCBW(I) = ABS( BW(ILC,KSCN) )
C
            IF( BBCBW(I) .GT. 0.50001D0 * SAMPRATE(KS) ) THEN
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
            BBCBW(I) = BBFILT(I,KS)
C
         END IF
C
C        Get the LO sum.  
C
         LOSUM(I) = FIRSTLO(I,KS) + ISIDE1 * BBCFREQ(I)
         LOSUM(I) =  DNINT( LOSUM(I) * 1000.D0 ) / 1000.D0
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
         DO I = 1, NCHAN(KS)
            IF( BBCFREQ(I) .LT. 500.0D0 .OR.
     5          BBCFREQ(I) .GE. 1000.0D0 ) THEN
               WRITE( MSGTXT, '( A, I3, F10.2, I5, F10.2, I5 )' )
     1            'FSFREQ: chan, firstLO, setup, freq, freq set: ', 
     2            I, FIRSTLO(I,KS), KS, LOSUM(I), KF
               CALL WLOG( 1, MSGTXT )
               WRITE( MSGTXT, '( A, F10.2 )' )
     1            'FSFREQ: Bad VLBA BBC frequency:', BBCFREQ(I)
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1, ' Setup file: ' //
     1             SETNAME(KS)(1:LEN1(SETNAME(KS))) )
               CALL ERRLOG( ' Fix frequencies ' )
            END IF
C
            IF( .NOT. ( DEQUAL( BBCBW(I), 0.0625D0 ) .OR. 
     1                  DEQUAL( BBCBW(I), 0.125D0 ) .OR.
     2                  DEQUAL( BBCBW(I), 0.250D0 ) .OR. 
     3                  DEQUAL( BBCBW(I), 0.5D0 ) .OR.
     4                  DEQUAL( BBCBW(I), 1.0D0 ) .OR. 
     5                  DEQUAL( BBCBW(I), 2.0D0 ) .OR.
     6                  DEQUAL( BBCBW(I), 4.0D0 ) .OR. 
     7                  DEQUAL( BBCBW(I), 8.0D0 ) .OR.  
     8                  DEQUAL( BBCBW(I), 16.D0 ) ) ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F8.2 )' )
     1            'FSFREQ: Invalid bandwidth request (in scan?): ',
     2            BBCBW(I)
               CALL ERRLOG( MSGTXT )
            END IF
         END DO
      END IF
C
C     Similar checks for the RDBE, but put in subroutine.
C
      ERRS = .FALSE.
      CALL CHKRDFQ( KS, BBCBW, BBCFREQ, ERRS )
      IF( ERRS ) CALL ERRLOG( 'FSFREQ:  Fix problem with in-line '//
     1    'or Doppler derived frequencies or bandwidths.' )
C
C     Similar checks for Mark 4?
C
C
C     END
C
  999 CONTINUE
      RETURN
      END
