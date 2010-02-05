      SUBROUTINE SCHTIM 
C
C     Subroutine for SCHED that:
C       1. Checks that each station has a monotonic time sequence.  
C       2. Check for schedule gaps that might cause non-simultaneous 
C          tape changes.
C     Get total time range.  Check the TFIRST from SATTIM against
C     the final, "optimized" schedule.
C     Get first time for use in setting tape changes.
C     Count readback opportunities.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER           ISCN, ISTA, NCNT
      INTEGER           SY, SD, TY, TD, MINRDB, MAXSTOP
      LOGICAL           RDBWARN, TSWARN, RECLAST(MAXSTA)
      DOUBLE PRECISION  ST, TL, FIRSTT, NRSEC, RECTIME
      DOUBLE PRECISION  ONEMIN, ONEHR, TLAST(MAXSTA)
      CHARACTER         TFORM*8, CSTART*8, CSTOP*8
C  -----------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SCHTIM: Starting.' )
C
C     Initialize.  Prevent counting start of expt as a readback.
C     NRDBCK and NSTOP set to -1 to ignore the break before obs start.
C
      ONEMIN = 60.D0 * ONESEC
      ONEHR = 3600.D0 * ONESEC
      FIRSTT = 999.D9
      TEND   = -999.D9
      RDBWARN = .FALSE.
      TSWARN = .FALSE.
      DO ISTA = 1, NSTA
         TLAST(ISTA) = UNSET
         NSTSC(ISTA) = 0
         NRDBCK(ISTA) = 0
         SCNHR(ISTA) = 0.0
         TPHR(ISTA) = 0.0
         TGBYTES(ISTA) = 0.0
         NSTOP(ISTA) = -1
         RECLAST(ISTA) = .TRUE.
      END DO
      IF( DEBUG ) CALL WLOG( 0, 'SCHTIM:  Counting readback tests.' )
C
C     First be sure that this schedule has at least one scan.
C
      NCNT = 0
      DO ISCN = SCAN1, SCANL
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
               NCNT = NCNT + 1
            END IF
         END DO
      END DO
      IF( NCNT .EQ. 0 ) THEN
         CALL ERRLOG( 'SCHTIM: There are no scheduled scans.' )
      END IF
C
C     Now count the readbacks and tape stoppages.
C
      DO ISCN = SCAN1, SCANL
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
C
C              Save first and last times of scans actually used.
C
               FIRSTT = MIN( STARTJ(ISCN), FIRSTT )
               TEND   = MAX( STOPJ(ISCN), TEND )
C
C              Deal with scans out of order.  Note that this can
C              be legal for OPTMODE=UPTIME.
C
               IF( STARTJ(ISCN) - TPSTART(ISCN,ISTA) + ONESEC .LT. 
     1             TLAST(ISTA) .AND. .NOT.
     2             OPTMODE .EQ. 'UPTIME' ) THEN
                  CALL TIMEJ( STARTJ(ISCN), SY, SD, ST )
                  CALL TIMEJ( TLAST(ISTA), TY, TD, TL )
                  CSTART = TFORM( ST, 'T', 0, 2, 2, '::@' )
                  CSTOP = TFORM( TL, 'T', 0, 2, 2, '::@' )
                  WRITE(6, '( A, I4, 1X, 2A, I4, 1X, A )' )
     1               'SCHTIM:  Start: ', SD, CSTART, ' Last stop: ', 
     2               TD, CSTOP
                  CALL ERRLOG ( 'SCHTIM: Two scans for '//
     1               STATION(STANUM(ISTA))//' overlap in time.' )
               END IF
C
C              Count scans and total time.
C
               NSTSC(ISTA) = NSTSC(ISTA) + 1
               SCNHR(ISTA) = SCNHR(ISTA) + 
     1             ( STOPJ(ISCN) - STARTJ(ISCN) ) / ONEHR

               IF( .NOT. NOREC(ISCN) ) THEN
                  RECTIME = ( STOPJ(ISCN) - STARTJ(ISCN) + 
     1                        TPSTART(ISCN,ISTA) )
                  TPHR(ISTA) = TPHR(ISTA) + RECTIME / ONEHR
C
C                 Get the total bytes recorded.  This will be the
C                 same as the bytes at the end of the last scan
C                 this station is in.
C
                  TGBYTES(ISTA) = GBYTES(ISCN,ISTA)
               END IF
C
C              Count readbacks.  They can happen in gaps or during
C              no-record scans.  It seems that the on-line system
C              really needs one minute plus one second per track
C              to do a test.  There are a maximum of 32 tracks, so
C              1:32 is the actual amount needed.  Allow a bit
C              of slop so test against 1:45.  We tell users 2:00,
C              so that is generous.  With LST scheduling, a gap
C              (but not a prescan) of 2:00 comes out slightly under
C              two minutes and was triggering warnings.
C              In two tape mode, the readbacks are done sequentially
C              so test against 3:30.
C
               IF( TWOHEAD ) THEN
                  NRSEC = 210.D0
               ELSE
                  NRSEC = 105.D0
               END IF
               MSGTXT = ' '
C
C              Count gaps before scans, but not at first scan.
C
               IF( STARTJ(ISCN) - TPSTART(ISCN,ISTA) - TLAST(ISTA) .GE. 
     1              NRSEC * ONESEC .AND. TLAST(ISTA) .NE. UNSET ) THEN
                  NRDBCK(ISTA) = NRDBCK(ISTA) + 1
                  CALL TIMEJ( STARTJ(ISCN), SY, SD, ST )
                  CSTART = TFORM( ST, 'T', 0, 2, 2, '::@' )
                  IF( DEBUG ) THEN
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( A, I5, A, A, A, I5, A, A )' )
     1                   '         Readback number', NRDBCK(ISTA), 
     2                   ' for ', STATION(STANUM(ISTA)), 
     3                   ' is before scan', ISCN, ' at ', CSTART
                     CALL WLOG( 0, MSGTXT )
                  END IF
               END IF
C
C              Count long norecording scans, but not first.
C
               IF( NOREC(ISCN) .AND. TLAST(ISTA) .NE. UNSET .AND.
     1             STOPJ(ISCN) - STARTJ(ISCN) .GE. NRSEC * ONESEC ) THEN
                  NRDBCK(ISTA) = NRDBCK(ISTA) + 1
                  IF( DEBUG ) THEN
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( A, I5, A, A, A, I5, A, A )' )
     1                   '         Readback number', NRDBCK(ISTA), 
     2                   ' for ', STATION(STANUM(ISTA)), 
     3                   ' is during scan', ISCN, ' at ', CSTART
                     CALL WLOG( 0, MSGTXT )
                  END IF
               END IF
C
C              Count the tape stoppages of all lengths.  If recording
C              this scan, count if there is a gap since the last
C              scan, if that was also a recording scan.  If not
C              recording, count this scan if the last scan was a
C              recording scan.  I'm only trying to count a string
C              of non-recording scans as one.
C
               IF( .NOT. NOREC(ISCN) ) THEN
                  IF( STARTJ(ISCN) - TPSTART(ISCN,ISTA) - TLAST(ISTA)
     1                   .GT. ONESEC .AND. RECLAST(ISTA) ) THEN
                     NSTOP(ISTA) = NSTOP(ISTA) + 1
                  ENDIF
               ELSE
                  IF( RECLAST(ISTA) ) THEN
                     NSTOP(ISTA) = NSTOP(ISTA) + 1
                  END IF
               END IF
C
C              Save end of last scan and whether it was recording.
C
               TLAST(ISTA) = STOPJ(ISCN)
               RECLAST(ISTA) = .NOT. NOREC(ISCN)
            END IF
         END DO
      END DO
C
C     Warn if SCNHR is zero for some station.  Abort if using VEX.
C
      DO ISTA = 1, NSTA
         IF( SCNHR(ISTA) .LE. 0.0 .AND. CONTROL(STANUM(ISTA)) .EQ. 'VEX'
     1           .AND. .NOT. DODOWN .AND. 
     2           ( ( AUTOALOC(ISTA) .AND. AUTOREV(ISTA) ) .OR.
     3             ( USEDISK(ISTA) .AND. .NOT. USETAPE(ISTA) ) ) ) THEN
            MSGTXT = 'SCHTIM:  ****** ERROR: Station ' // 
     1          STANAME(ISTA) // ' is not in any scans.'
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, '      This can happen if the source is ' //
     1                 'not up in any of of the assigned scans.' )
            MSGTXT = ' '
            CALL ERROR( '      SCHED cannot make a Vex file.' )
         ELSE IF( SCNHR(ISTA) .LE. 0.0 ) THEN
            MSGTXT = 'SCHTIM:  ****** WARNING: Station ' // 
     1          STANAME(ISTA) // ' is not in any scans.'
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, '      This can happen if the source is ' //
     1                 'not up in any of of the assigned scans.' )
            MSGTXT = ' '
         END IF
      END DO
C
C     Warn if TFIRST .NE. FIRSTT (first time specified in input 
C     schedule vs. first time in optimized schedule.  Actually
C     allow a couple of seconds tolerance.
C
      IF( ABS( TFIRST - FIRSTT ) .GT. 2.0D0 / 86400.D0 ) THEN
         CALL WLOG( 1, 'SCHTIM: Earliest output scan start time ' //
     1       'differs from earliest in input schedule.' )
         IF( AUTOPEAK )  CALL WLOG( 1,
     1                '        This may be due to an inserted ' //
     2       'reference pointing scan.' )
      END IF
C
C     Complain about too few readback tests for stations with a
C     VLBA control system.  Ask for one each 2 hours min.
C     Don't worry unless tapes are being used.
C
      MINRDB = ( TEND - TFIRST ) / ( 120.D0 * ONEMIN )
      DO ISTA = 1, NSTA
         IF( ( RECORDER(STANUM(ISTA)) .EQ. 'VLBA' .OR.
     1         RECORDER(STANUM(ISTA)) .EQ. 'MKIV' .OR.
     2         RECORDER(STANUM(ISTA)) .EQ. 'MKIII' ) .AND. 
     3         VLBITP .AND. USETAPE(ISTA) ) THEN
            IF( NRDBCK(ISTA) .LT. MINRDB ) THEN
               MSGTXT = ' '
               IF( TWOHEAD ) THEN
                  WRITE( MSGTXT, '( A, I5, A, A )' )
     1               'SCHTIM: Caution: Only ', NRDBCK(ISTA), 
     2               ' readback tests (4 min gaps for 2 tape mode) at ',
     3               STANAME(ISTA)
               ELSE
                  WRITE( MSGTXT, '( A, I5, A, A )' )
     1               'SCHTIM: Caution: Only ', NRDBCK(ISTA), 
     2               ' readback tests (2 min gaps) at ', STANAME(ISTA)
               END IF
               CALL WLOG( 1, MSGTXT )
               RDBWARN = .TRUE.
            END IF
         END IF       
      END DO
      IF( RDBWARN ) THEN
         CALL WLOG( 1, '        Please try for at least one readback'
     1       // ' each two hours.' )
      END IF
C
C     Complain about too many tape stoppages.  Complain if more
C     than 10 per hour.  Don't worry if tapes are not being used.
C
C     Jan. 2009.  For Mark5A and Mark5B there is a maximum of 1000
C     recording scans (blocks between stoppages) per module.  So
C     a warning is needed, although it can be for much more than
C     10 per hour.  This will go away with Mark5C.  For now, warn
C     if the rate is enough to fill the 1000 in 24 hr.
C
      IF( USETAPE(ISTA) ) THEN
         MAXSTOP = ( TEND - TFIRST ) * 24.0 * 10.0
      ELSE
         MAXSTOP = ( TEND - TFIRST ) * 1000.0
      END IF
      DO ISTA = 1, NSTA
         IF( NSTOP(ISTA) .GT. MAXSTOP .AND. VLBITP ) THEN
            CALL WLOG( 1,
     1        'SCHTIM:  ' // STANAME(ISTA) // 
     2        ' averages more than 10 tape stops per hour.' )
            TSWARN = .TRUE.     
         END IF
      END DO
      IF( TSWARN .AND. CORREL(1:7) .EQ. 'FXCORR' ) THEN
         CALL WLOG( 0, 
     1    '   Tape synchronization on the  VLBA hardware correlator ' )
         CALL WLOG( 0, 
     1    '   is better when the tape does not stop often. ' )
         CALL WLOG( 0,
     1    '   Parameters  MINPAUSE and PRESTART may be used to '
     2    // ' encourage continuous tape motion.' )
      END IF
C
      RETURN
      END
