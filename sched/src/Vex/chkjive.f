      SUBROUTINE CHKJIVE
C
C     Routine for SCHED that checks some basic JIVE 
C     correlator restrictions.
C
      INCLUDE     'sched.inc'
      INCLUDE     'schset.inc'
C
      INTEGER        KS, ROLLERR, HD2ERR, SPUPERR
      INTEGER        VXNHDS, SAMPERR, ICH
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'CHKJIVE: starting.' )
C
C     First, only do this if the experiment is to be processed
C     on the JIVE correlator.
C
      IF( CORREL .EQ. 'JIVE' ) THEN
C
C        The philosophy is to block off all possibilities that
C        will not be available, even if the comissioning phase
C        is complete. During the comissioning several items
C        may be un-available at time, but this is considered
C        a transition phase, on a shorter timescale than typical
C        Sched releases. The comissioning phase of the JIVE correlator
C        is supposed to end in June 1999.
C
C        The following items are expected to be unsupported:
C
         ROLLERR = 0
         DO KS = 1, NSET
            IF (.NOT. MODETEST(KS) ) THEN
C
               IF( BARREL(KS) .NE. 'roll_off' ) THEN
                  ROLLERR = KS
               END IF
            END IF            
         END DO
C
C        Write the error messages.
C
         IF( ROLLERR .NE. 0 ) THEN
            CALL WLOG( 1, 'CHKJIVE:  JIVE Correlator does '//
     1           'not do barrel rolling. Please set barrel="roll_off"' )
            CALL WRTMSG( 0, 'CHKJIVE', 'barrelroll')
            CALL ERRSET( ROLLERR )
         END IF
C
C        Stop two head recording
C
         HD2ERR = 0
         DO KS = 1, NSET
            IF (.NOT. MODETEST(KS) ) THEN
C
               IF( VXNHDS( KS ) .GT. 1 ) THEN
                  HD2ERR = KS
               END IF
            END IF
         ENDDO
C
C        Write the error messages.
C
C         IF( HD2ERR .NE. 0 ) THEN
C            CALL PUTOUT( 'CHKJIVE: JIVE Correlator is not '//
C     1           'expected to deal with 512 MB/s data ' )
C            CALL PUTOUT( '         until the middle of 2001 ')
C            CALL PUTOUT( '         or testers set MODETEST ')
C            CALL SETERR( HD2ERR )
C         END IF
C
C        Also Stop 40 ips and 66 ips recordings
C
         SPUPERR = 0
         DO KS = 1, NSET
            IF (.NOT. MODETEST(KS) ) THEN
C
               IF( SAMPRATE(KS)/FANOUT(KS) .LE. 2.01 ) THEN
                  SPUPERR = KS
               END IF
            END IF            
         END DO
C
C        Write the error messages.
C
         IF( SPUPERR .NE. 0 ) THEN
            CALL WLOG( 1, 'CHKJIVE:  JIVE Correlator does not '//
     1          'currently do speed-up correlation (Oct 2004)' )
            CALL WLOG( 1, '         or testers set MODETEST ')
            CALL WRTMSG( 0, 'CHKJIVE', 'slowdatarate')
            CALL ERRSET( SPUPERR )
         END IF
C
C        Prevent oversampling of 8 (or more)
C
         SAMPERR = 0
         DO KS = 1, NSET
           IF ( .NOT. MODETEST(KS) ) THEN
             DO ICH = 1, NCHAN(KS)
               IF( SAMPRATE(KS) .GT. 8.0*BBFILT(ICH,KS) ) THEN
                 SAMPERR = KS
               END IF
             END DO
           END IF
         END DO
C
C        Write the error messages.
C
         IF( SAMPERR .NE. 0 ) THEN
            CALL WLOG( 1, 'CHKJIVE:  JIVE Correlator has a '//
     1          'maximum oversampling factor of 4. ' )
            CALL WLOG( 1, 'This is exceeded in setup ' // 
     1                  SETNAME(SAMPERR) )
            CALL WLOG( 1, '         Testers *only* may set MODETEST ')
            CALL WRTMSG( 0, 'CHKJIVE', 'oversamp')
            CALL ERRSET( SAMPERR )
         END IF




      END IF
C
      RETURN
      END

