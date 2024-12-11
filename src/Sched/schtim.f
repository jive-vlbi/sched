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
      INTEGER           SY, SD, TY, TD, MAXSTOP
      REAL              C1GBYTES(MAXSTA), D1GBYTES(MAXSTA)
      LOGICAL           TSWARN, RECLAST(MAXSTA), GOTCORE(MAXSTA)
      LOGICAL           DOSC
      DOUBLE PRECISION  ST, TL, FIRSTT, NRSEC, RECTIME
      DOUBLE PRECISION  ONEMIN, ONEHR, TLAST(MAXSTA)
      CHARACTER         TFORM*8, CSTART*8, CSTOP*8
C  -----------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SCHTIM: Starting.' )
C
C     Initialize.  Prevent counting start of expt as a readback.
C     NRDBCK and NSTOP set to -1 to ignore the break before obs start.
C     NRDBCK is no longer used outside this routine.  Remove it some day.
C     It was a tape issue.
C
      ONEMIN = 60.D0 * ONESEC
      ONEHR = 3600.D0 * ONESEC
      FIRSTT = 999.D9
      TEND   = -999.D9
      TSWARN = .FALSE.
      DO ISTA = 1, NSTA
         TLAST(ISTA) = UNSET
C
         NSTSC(ISTA) = 0
         SCNHR(ISTA) = 0.0
         TPHR(ISTA) = 0.0
         TPSCN(ISTA) = 0
         TGBYTES(ISTA) = 0.0
C
         ENSTSC(ISTA) = 0
         ESCNHR(ISTA) = 0.0
         ETPHR(ISTA) = 0.0
         ETPSCN(ISTA) = 0
         EGBYTES(ISTA) = 0.0
C
         DNSTSC(ISTA) = 0
         DSCNHR(ISTA) = 0.0
         DTPHR(ISTA) = 0.0
         DTPSCN(ISTA) = 0
         DGBYTES(ISTA) = 0.0
C
         C1GBYTES(ISTA) = 0.0
         D1GBYTES(ISTA) = 0.0
         GOTCORE(ISTA) = .FALSE.
         NSTOP(ISTA) = -1
         RECLAST(ISTA) = .TRUE.
         NRDBCK(ISTA) = 0
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
C              Simplify later testing for DOSCANS range.
C
               DOSC = DOSCANS(1) .EQ. 0 .OR.
     1           ( ISCN .GE. DOSCANS(1) .AND. ISCN .LE. DOSCANS(2) )
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
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I5, A, I4, 1X, 2A, I4, 1X, A )' )
     1               'SCHTIM:  Scan overlap details - Scan: ', ISCN, 
     2               '  Start: ', SD, CSTART, ' Last stop: ', TD, CSTOP
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
                  CALL ERRLOG ( 'SCHTIM: Two scans for '//
     1               STATION(STANUM(ISTA))//' overlap in time.' )
               END IF
C
C              Count scans and total time.  Also count disk scans
C              (TPSCN) which are blocks of recording without a break.
C
               ENSTSC(ISTA) = ENSTSC(ISTA) + 1
               IF( PREEMPT(ISCN) .NE. 'EXTRA' ) 
     1             NSTSC(ISTA) = NSTSC(ISTA) + 1
               IF( DOSC ) DNSTSC(ISTA) = DNSTSC(ISTA) + 1
C
               ESCNHR(ISTA) = ESCNHR(ISTA) + 
     1             ( STOPJ(ISCN) - STARTJ(ISCN) ) / ONEHR
               IF( PREEMPT(ISCN) .NE. 'EXTRA' ) 
     1             SCNHR(ISTA) = SCNHR(ISTA) + 
     2                ( STOPJ(ISCN) - STARTJ(ISCN) ) / ONEHR
               IF( DOSC ) DSCNHR(ISTA) = DSCNHR(ISTA) + 
     2                ( STOPJ(ISCN) - STARTJ(ISCN) ) / ONEHR
C
               IF( .NOT. NOREC(ISCN) ) THEN
C
C                 The recording time depends on the system.  Nominally
C                 the media start at STARTJ - TPSTART.  But the 
C                 RDBE/VLBA and VLA systems look instead at the good
C                 data start time in the VEX file.  So try to account
C                 for that difference here.  I'm not sure about the
C                 DBBC.
C
                  IF( USEONSRC(STANUM(ISTA)) ) THEN
                     RECTIME =  STOPJ(ISCN) - MAX( ( STARTJ(ISCN) - 
     1                    TPSTART(ISCN,ISTA) ), TONSRC(ISCN,ISTA) )
                  ELSE
                     RECTIME = ( STOPJ(ISCN) - STARTJ(ISCN) + 
     1                        TPSTART(ISCN,ISTA) )
                  END IF
C
                  ETPHR(ISTA) = ETPHR(ISTA) + RECTIME / ONEHR
                  IF( PREEMPT(ISCN) .NE. 'EXTRA' ) 
     1               TPHR(ISTA) = TPHR(ISTA) + RECTIME / ONEHR
                  IF( DOSC ) DTPHR(ISTA) = DTPHR(ISTA) + 
     1               RECTIME / ONEHR
C
                  IF( STARTJ(ISCN) - TPSTART(ISCN,ISTA) - ONESEC .GT. 
     1                TLAST(ISTA) .OR. .NOT. RECLAST(ISTA) ) THEN
                     ETPSCN(ISTA) = ETPSCN(ISTA) + 1
                     IF( PREEMPT(ISCN) .NE. 'EXTRA' ) 
     1                  TPSCN(ISTA) = TPSCN(ISTA) + 1
                     IF( DOSC ) DTPSCN(ISTA) = DTPSCN(ISTA) + 1
                  END IF
C
C                 Take the opportunity to get the total bytes recorded.  
C                 This will be the same as the bytes at the end of the 
C                 last scan this station is in.  Also get the bytes in the
C                 core scans.  Keep C1GBYTES and CLGBYTES as the values
C                 at the start and end of the core.  The core total is
C                 the difference.
C
C                 Record the GBYTES of the last EXTRA scan seen.
C                 This will end up being the value at the start of the core.
C                 Also get the GBYTES of the last non-DOSCANS scan.
C
                  IF( PREEMPT(ISCN) .NE. 'EXTRA' ) 
     1                 GOTCORE(ISTA) = .TRUE.
                  IF( .NOT. GOTCORE(ISTA) ) 
     1                 C1GBYTES(ISTA) = GBYTES(ISCN,ISTA)
                  IF( DOSCANS(1) .GT. 0 .AND. ISCN .LT. DOSCANS(1) )
     1                 D1GBYTES(ISTA) = GBYTES(ISCN,ISTA)
C
C                 Record the total core gbytes as of the last core scan 
C                 seen and the total of all scans on the last scan seen.
C
                  EGBYTES(ISTA) = GBYTES(ISCN,ISTA)
                  IF( PREEMPT(ISCN) .NE. 'EXTRA' ) THEN
                     TGBYTES(ISTA) = GBYTES(ISCN,ISTA) - C1GBYTES(ISTA)
                  END IF
                  IF( DOSC ) THEN
                     DGBYTES(ISTA) = GBYTES(ISCN,ISTA) - D1GBYTES(ISTA)
                  END IF
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
                  END IF
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
C     If no scans will for a station will be written to the output
C     files, abort.  The VEX file does not like this.  It is possible
C     that this will make it difficult to put a station in the EXTRA
C     scans that are not in the core, but that can still be done by
C     giving DOSCANS.  
C
C     (Removed ".NOT. DODOWN" from the SCNHR IF statement below.  Not
C     sure why it was there, and now DODOWN is scan dependent and
C     this is not a scan dependent place in the code.  Dec 2010 RCW).
C
      DO ISTA = 1, NSTA
         IF( DNSTSC(ISTA) .EQ. 0 .AND. DOSCANS(1) .NE. 0 ) THEN
            CALL WLOG( 1, 'SCHTIM: There are no scans for '//
     1         STANAME(ISTA)//' with the source up in the range '//
     2         'specified with DOSCANS.' )
            CALL ERRLOG( 'Please add scans or take out the station.' )
         END IF
C
         IF( NSTSC(ISTA) .EQ. 0 .AND. FUZZY ) THEN
            CALL WLOG( 1, 'SCHTIM: There are no scans for '//
     1         STANAME(ISTA)//' with the source up and PREEMPT '//
     2         'not set to EXTRA.' )
            CALL ERRLOG( 'Please add scans or take out the station.' )
         END IF
C
         IF( ESCNHR(ISTA) .LE. 0.0  ) THEN
            CALL WLOG( 1, 'SCHTIM: There are no scans for '//
     1         STANAME(ISTA)//' with the source up.' )
            CALL ERRLOG( 'Please add scans or take out the station.' )
         END IF
      END DO
C
C     Warn if TFIRST .NE. FIRSTT (first time specified in input 
C
      IF( ABS( TFIRST - FIRSTT ) .GT. 2.0D0 / 86400.D0 ) THEN
         CALL WLOG( 1, 'SCHTIM: Earliest output scan start time ' //
     1       'differs from earliest in input schedule.' )
         IF( AUTOPEAK )  CALL WLOG( 1,
     1                '        This may be due to an inserted ' //
     2       'reference pointing scan.' )
      END IF
C
C     Readback test opportunity warning removed July 23, 2010 - tape only.
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
      MAXSTOP = ( TEND - TFIRST ) * 1000.0
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
