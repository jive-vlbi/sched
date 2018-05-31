      SUBROUTINE SETFREQ( KS, NEEDCAT )
Cf2py intent(out) NEEDCAT
C
C     Routine called by setup defaulting routine, SETDEFS,
C     to establish the frequencies and sidebands of all 
C     channels of an experiment.  It balks if not enough 
C     information is provided to deduce all parameters and it
C     checks consistency when more parameters are specified
C     than the minimum required.  Technically, there are some
C     incomplete combinations of information that would allow
C     valid schedules to be produced, but they don't allow proper
C     "extlo" specifications for non VLBA stations or good pcal
C     defaults.
C
C     Force there to be enough information to deduce all of the
C     following for each channel (note FREQREF here is 
C     FREQREF+FREQOFF from setup file): 
C
C        LOSUM    - sum of all LO's.   - FREQREF+FREQOFF in setup..
C        NETSD    - net sideband.      - NETSIDE in setup.
C        FIRSTLO  - LO sum before BBC. - FIRSTLO in setup.
C        SIDE1    - first LO sideband. - Not in setup.
C        BBSYN    - BBC frequency.     - BBSYN in setup.
C        SIDEBD   - BBC sideband.      - SIDEBAND in setup.
C
C     The parameters are related by (where sidebands are +-1)
C        LOSUM = FIRSTLO + SIDE1*BBSYN
C        SIDE1 = NETSD*SIDEBD
C        SIDE1 = U if LOSUM .GT. FIRSTLO else = L
C
C     The following 2 lines each give a complete list of adequate 
C     combinations of inputs.  Require that one be present.
C        FREQREF, FIRSTLO, (NETSIDE or SIDEBAND)
C        BBSYN, SIDEBAND, NETSIDE, (FREQREF or FIRSTLO)
C
C     Note that the LOSUM and BBSYN deduced here may be modified in 
C     routine WRTFREQ when FREQ from the main schedule is applied.
C     That routine will not allow sideband or first LO changes.
C
C     NEEDCAT is returned .FALSE. if all of the above can be
C     determined.  If FREQREF and NETSIDE are available, it should
C     be possible to determine the rest based on the frequency
C     catalog.  If the catalog will be required for a complete set,
C     set NEEDCAT true.
C
C     VLA note - the sidebands etc work out ok as long as FIRSTLO
C     is set right.  For BBC settings above 600 MHz and keeping
C     the same IF sideband as the VLA, use FIRSTLO = VLALO - 600.
C     For BBC below 600 (other sideband) use VLALO + 600.
C
C     For non-VLBA stations with VLBA control files, FIRSTLO and
C     NETSD will be used for keywords "extlo" and "extlosideband".
C     This is needed by the correlator and is part of why a complete 
C     set will now be required.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER            KS, ICH
      INTEGER            INETSD, ISIDE1, ISIDEBD
      LOGICAL            GOTNETS, GOTNETF, GOTBBS, GOTBBF, GOTLO1
      LOGICAL            NEEDCAT
      DOUBLE PRECISION   TEMP8
      CHARACTER          STDMSG*70
C-------------------------------------------------------------------
      IF( KS .LE. 3 .AND. SDEBUG ) CALL WLOG( 0, 'SETFREQ: Starting' )
C
      NEEDCAT = .FALSE.
      STDMSG =  'SETFREQ: Overspecified and inconsistent'//
     1      ' frequencies and sidebands.'
C
C     Allow frequency to be set by way of the BAND.
C
      CALL SETBAND( KS )
C
C     Loop over channels.
C
      DO ICH = 1, NCHAN(KS)
C
C        See what we've got.
C        Net sideband:
C
         INETSD = 0
         IF( NETSIDE(ICH,KS) .EQ. 'U' ) INETSD = 1
         IF( NETSIDE(ICH,KS) .EQ. 'L' ) INETSD = -1
         GOTNETS = INETSD .NE. 0       
C
C        BBC sideband:
C
         ISIDEBD = 0
         IF( SIDEBD(ICH,KS) .EQ. 'U' ) ISIDEBD = 1
         IF( SIDEBD(ICH,KS) .EQ. 'L' ) ISIDEBD = -1
         GOTBBS = ISIDEBD .NE. 0       
C
C        Sky frequency, First LO, and Baseband frequency.
C
         GOTNETF = FREQREF(ICH,KS) .GT. 0.D0
         GOTLO1  = FIRSTLO(ICH,KS) .NE. NOTSET
         GOTBBF  = BBSYN(ICH,KS) .NE. 0.0D0
C
C        Now go through the 4 possible combinations that work - see
C        comments at top of routine.
C
C        First cases that need BBSYN
C
         IF( GOTNETF .AND. GOTLO1 .AND. ( GOTNETS .OR. GOTBBS ) ) THEN
            IF( FREQREF(ICH,KS) .GT. FIRSTLO(ICH,KS) ) THEN
               ISIDE1 = 1
            ELSE
               ISIDE1 = -1
            END IF
            TEMP8 = ISIDE1 * ( FREQREF(ICH,KS) - FIRSTLO(ICH,KS) )
            IF( GOTBBF .AND. ABS(TEMP8-BBSYN(ICH,KS)) .GT. 1.D-3 ) THEN
               CALL WLOG( 1, 'SETFREQ: BBSYN, FREQREF+FREQOFF, and '//
     1             ' FIRSTLO specified but not consistent.' )
               CALL ERRLOG( STDMSG )
            ELSE IF( GOTNETS .AND. GOTBBS .AND. 
     1           ISIDE1 * INETSD .NE. ISIDEBD ) THEN
               CALL WLOG( 1, 'SETFREQ: Inconsistent sidebands.' )
               CALL ERRLOG( STDMSG )
            END IF
            BBSYN(ICH,KS) = TEMP8
            IF( GOTNETS ) ISIDEBD = ISIDE1 * INETSD
            IF( GOTBBS )  INETSD  = ISIDE1 * ISIDEBD
C
C        Then those that have BBSYN but need other things.
C
         ELSE IF( GOTBBF .AND. GOTBBS .AND. GOTNETS .AND.
     1        ( GOTNETF .OR. GOTLO1 ) ) THEN
            ISIDE1 = ISIDEBD * INETSD
            IF( GOTNETF ) THEN
               TEMP8 = FREQREF(ICH,KS) - ISIDE1 * BBSYN(ICH,KS)
               IF( GOTLO1 .AND. ABS( TEMP8 - FIRSTLO(ICH,KS) ) .GT.
     1              0.0000001 ) THEN
                  CALL WLOG( 1, 'SETFREQ: Inconsistent frequencies' )
                  CALL ERRLOG( STDMSG )
               END IF              
               FIRSTLO(ICH,KS) = TEMP8
            ELSE IF( GOTLO1 ) THEN
               TEMP8 = FIRSTLO(ICH,KS) + ISIDE1 * BBSYN(ICH,KS)
               IF( GOTNETF .AND. 
     1             ABS( FREQREF(ICH,KS) - TEMP8 ) .GT. 0.0000001 ) THEN
                  CALL WLOG( 1, 'SETFREQ: Inconsistent frequencies' )
                  CALL ERRLOG( STDMSG )
               END IF              
               FREQREF(ICH,KS) = TEMP8
            END IF
C
C        If don't have a minimal complete set.  If have FREQREF
C        and NETSIDE, it can be done from the frequencies file,
C        if there is an appropriate entry in that file.  Note
C        that the cases where these items can be calculated from
C        others are covered in the complete sets.
C
         ELSE IF( GOTNETF .AND. GOTNETS ) THEN
C
            NEEDCAT = .TRUE.
C
C        Finally, abort if there just isn't enough information.
C
         ELSE
            CALL WLOG( 1, ' ' )
            CALL WLOG( 1, ' ' )
            SETMSG = ' '
            WRITE( SETMSG, '( A, A, I2, A )' )
     1         ' Incomplete frequency and sideband specifications ',
     2         'for channel ', ICH, ' in setup: '
            CALL WLOG( 1, SETMSG )
            CALL WLOG( 1, '        ' // SETNAME(KS) )
C
C           Explain the problem.
C
            CALL WLOG( 1,
     1         'SETFREQ: Need one of the following combinations:')
            CALL WLOG( 1, 'SETFREQ:'//
     1       '      FREQREF, FIRSTLO, and (NETSIDE or SIDEBAND)' )
            CALL WLOG( 1, 'SETFREQ:'//
     1       '   or BBSYN, SIDEBAND, NETSIDE, and (FREQREF or FIRSTLO)')
C
            IF( GOTNETS ) THEN
               CALL WLOG( 1, '   Have net sideband - NETSIDE.' )
            ELSE
               CALL WLOG( 1, '   Missing net sideband - NETSIDE.' )
            END IF
C
            IF( GOTBBS ) THEN
               CALL WLOG( 1, '   Have BBC sideband - SIDEBAND. ' )
            ELSE
               CALL WLOG( 1, '   Missing BBC sideband - SIDEBAND. ' )
            END IF
C
            IF( GOTNETF ) THEN
               CALL WLOG( 1, '   Have reference frequency - FREQREF. ' )
            ELSE
               CALL WLOG( 1, 
     1              '   Missing reference frequency - FREQREF.')
            END IF
C
            IF( GOTLO1 ) THEN
               CALL WLOG( 1, '   Have FIRSTLO. ' )
            ELSE
               CALL WLOG( 1, '   Missing FIRSTLO. ' )
            END IF
C
            IF( GOTBBF ) THEN
               CALL WLOG( 1, '   Have BBC frequency - BBSYN. ' ) 
            ELSE
               CALL WLOG( 1, '   Missing BBC frequency - BBSYN. ' ) 
            END IF
C
            CALL ERRLOG('SETFREQ: Note that old setups might not work.')
         END IF
C
C        Now have everything.  Put net sideband into proper form.
C        Put the other sideband into proper form after dealing
C        with possible information from the frequency catalog.
C
         IF( INETSD  .EQ.  1 ) NETSIDE(ICH,KS) = 'U'
         IF( INETSD  .EQ. -1 ) NETSIDE(ICH,KS) = 'L'
C
      END DO   ! Channel loop.
C
      RETURN
      END



