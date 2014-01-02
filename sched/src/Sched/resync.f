      SUBROUTINE RESYNC
C
C     Routine called by CHKSCN that tries to predict the time that the
C     correlator will start to deliver good data for each scan/station
C     (TCORR).  This is just synchronized correlation with no consideration
C     of slew or band switch times.  This can be used to look for excessive
C     time lost to resync etc.  Resyncs are required when the recording
C     medium is started after being stopped and when a formatter reconfigure
C     knocks the correlator out of sync by giving invalid time codes
C     (constant time in the VLBA case).
C
C     With the RDBE/MARK5C system and DiFX, the scans actually start at
C     TONSRC and there are no resync or reconfigure losses.  So deal with
C     that simple case early, but do put in values for TCORR as that is
C     one possible item for the summary.
C
C     Assumed sync times:
C         Old VLBA correlator with tape (time/speedup): 8s/1   13s/2   25s/4
C         Old VLBA correlator with disk:  tape resync + 10s (improve soon?)
C         VLBADIFX - instant sync.
C         Others (JIVE, Bonn, Haystack) with disk: 1s
C     Assumed reconfigure times (need confirmation):
C         VLBA Formatter: 8 sec.
C         MarkIV Formatter: 3 sec.  But no big effect on resync.
C         RDBE - 5 seconds
C     A reconfigure slows sync on the correlator. 
C         Old VLBA:  Add 7s to reconfigure+resync 
C            Much longer sync times occur on 10-20% of stations
C         JIVE:  20-30s.  Call it 25s.  Consistent?
C              This is when station setup really changes (subjob change),
C              not just a reconfigure, which happens every scan on MkIV.
C         DiFX:  instant.
C         Others:  Not sure, assume no effect.
C
C     Note that a pcal detector change triggers a reconfigure in VLBA
C     formatters, but not MarkIV formatters (they don't have detectors).
C
C     Note: LSCN is the last scan the station was in.
C           LRSCN was the last scan with FORMAT not equal 'NONE'.
C           TCORR(ISCN,ISTA) is the time the correlator is expected to be
C                synced up.
C           NRECONF(1,ISTA) is the total number of reconfigures at the 
C                station
C           NRECONF(2,ISTA) is the number of reconfigures at the 
C                station done while recording.
C           TTSYNC is the total time lost to resynchronizing.
C           TRECON is the time any reconfigure was expected to be done.
C
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER    ISCN, ISTA, LSCN, LRSCN, KS
      INTEGER    NSSCAN, NRWARN
      LOGICAL    RECWARN, NEWCONF, NEWPCAL, CONFIGW
      LOGICAL    SCNRCF, NEWSYNC
      DOUBLE PRECISION  TRECON, TSADD, TTADD
      DATA       NRWARN / 0 /
      SAVE       NRWARN
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'RESYNC starting' )
C
C
      RECWARN = .FALSE.
      CONFIGW = .FALSE.
      DO ISTA = 1, NSTA
         LSCN = 0
         LRSCN = 0
         NRECONF(1,ISTA) = 0
         NRECONF(2,ISTA) = 0
         TTSYNC(ISTA) = 0.0
         NSSCAN = 0
         DO ISCN = SCAN1, SCANL
            TCORR(ISCN,ISTA) = 0.D0
C
C           Deal with the simple case of RDBE/MARK5C and DiFX.  
C           Also applies to DBBC going to DiFX.
C           Eventually see if the DBBC and/or JIVE correlator 
C           should be here.
C
            IF( USEONSRC(STANUM(ISTA)) .AND.
     1          ( CORREL(1:7) .EQ. 'SOCORRO' .OR.
     2            CORREL(1:8) .EQ. 'VLBADIFX' .OR.
     3          CORREL(1:4) .EQ. 'VLBA' ) ) THEN
C
               TCORR(ISCN,ISTA) = 
     1            MAX( ( STARTJ(ISCN) - TPSTART(ISCN,ISTA) ),  
     2                   TONSRC(ISCN,ISTA) )
               TTSYNC(ISTA) = 0.D0
C
C           Deal with the other cases.
C
            ELSE IF( STASCN(ISCN,ISTA) ) THEN
               KS = NSETUP(ISCN,ISTA)
               NSSCAN = NSSCAN + 1
               SCNRCF = .FALSE.
               TRECON = 0.D0
C
C              Set the default TCORR for a scan the station is in. This
C              assumes the correlator is synced from the previous scan
C              and the media has not stopped.  Note TPSTART=0 for non
C              recording scans, which gives the desired behavior.
C
               TCORR(ISCN,ISTA) = STARTJ(ISCN) - TPSTART(ISCN,ISTA)
C
C         write(*,*) 'resync: tcorr ', TCORR(ISCN,ISTA), 
C     1          '  startj:', STARTJ(ISCN), 
C     2          '  tpstart:',  TPSTART(ISCN,ISTA), 
C     3          MINPAUSE(1), PRESTART(1)
C
C              Deal with formatter reconfigures.  Note that a VLBA 
C              reconfigure will start at the end time of the previous 
C              scan, which could be before the start of the current 
C              scan.  This is because there really is a non-recording 
C              scan in the gap with the setup of the following scan.  
C              For MarkIV, there is a reconfigure every scan which takes
C              about 3 seconds, but doesn't cause correlator problems.
C
C              If FORMAT='NONE', the formatter is not used, or reconfigured,
C              at least for VLBA systems so set TCORR to the start time.
C              Vex systems don't currently tolerate FORMAT='NONE' scans.
C
C              TRECON is the time when the reconfigure, if any, is over.
C              The reconfiguration will not happen during a preceeding scan
C              even if that scan is not recording, so use the stop time
C              of the last scan of any type as the start of the reconfigure.
C              But only test for the need to reconfigure against the 
C              last scan that did not have FORMAT='NONE'.
C              Don't worry about any of this with FORMAT='NONE' scans.
C              Note that a reconfigure can happen during a non-recording
C              scan but that is accounted for automatically.
C              Assume that the reconfigure is over by the end of the scan.
C              This might not always be true, but the effort to deal with
C              exceptions doesn't seem worth it.
C
C              The JIVE correlator takes 20-30 seconds to sync after the 
C              station units have to change configuration - which happens
C              by the same criteria as VLBA reconfigures except that they
C              are not subject to pcal detector setups.
C
               IF( FORMAT(KS) .EQ. 'NONE' ) THEN
                  TCORR(ISCN,ISTA) = STARTJ(ISCN) - TPSTART(ISCN,ISTA)
               ELSE
                  CALL RECONFIG( ISCN, LRSCN, ISTA, NEWCONF, NEWPCAL )
C
C                 VLBA Formatter.
C
                  IF( DAR(STANUM(ISTA))(1:4) .EQ. 'VLBA' ) THEN
                     IF( NEWCONF .OR. NEWPCAL ) THEN
C         
                        IF( LSCN .EQ. 0 ) THEN
                           TRECON = STARTJ(ISCN) - 20.D0 / 86400.D0
                        ELSE
                           TRECON = MIN( STOPJ(LSCN) + 8.D0 / 86400.D0, 
     1                                STOPJ(ISCN) )
                        END IF
                        NRECONF(1,ISTA) = NRECONF(1,ISTA) + 1
C         
C                       Count and flag reconfigures during recording.
C         
                        IF( TRECON .GT. STARTJ(ISCN) - 
     1                       TPSTART(ISCN,ISTA) ) THEN
                           NRECONF(2,ISTA) = NRECONF(2,ISTA) + 1
                           SCNRCF = .TRUE.
                        END IF
C                    
                     END IF
C
C                 MarkIV formatter.  There is a reconfigure every scan, but
C                 they don't seem to knock the VLBA correlator out of sync.
C                 Don't count them, but assume that TRECON is 3 sec after
C                 recording start.  But if there is a real change of setup,
C                 requiring a new subjob at JIVE, count that as a reconfigure.
C                 Assume it won't affect VLBA playback so don't set SCNRCF.
C                 As for JIVE playback, NEWCONF is an adequate test for
C                 a long sync.
C
                  ELSE IF( DAR(STANUM(ISTA))(1:4) .EQ. 'MKIV' ) THEN
C
                     TRECON = MIN( TCORR(ISCN,ISTA) + 3.D0 / 86400.D0,
     1                             STOPJ(ISCN) )
                     IF( NEWCONF ) THEN
                        NRECONF(1,ISTA) = NRECONF(1,ISTA) + 1
                     END IF
C
C                 Others - unknown so set TRECON to stop time, but don't
C                 count reconfigures.  This will include DAR=RDBE
C
                  ELSE IF( NEWCONF ) THEN
                     IF( LSCN .EQ. 0 ) THEN
                        TRECON = STARTJ(ISCN) - 20.D0 / 86400.D0
                     ELSE
                        TRECON= STOPJ(LSCN)
                     END IF
                  END IF
C
C                 Get the time that we expect the correlator to be
C                 synced.  The considerations are different for the
C                 different correlators.  TSADD is the time to add
C                 to the later of TRECON or the record start time
C                 to get the sync time.
C
                  TSADD = 0.D0
C
C                 Start with the old VLBA hardware correlator.
C
                  IF( LSCN .EQ. 0 ) THEN
                     NEWSYNC = .TRUE.
                  ELSE
                     NEWSYNC = ( STARTJ(ISCN) - TPSTART(ISCN,ISTA) - 
     1                         STOPJ(LSCN) ) .GT. ONESEC .OR. 
     2                         NOREC(LSCN) .OR. 
     3                         SCNRCF
                  END IF
                  IF( CORREL(1:7) .EQ. 'FXCORR' ) THEN
C
C                    Recording stopped or reconfigure happened while
C                    recording.
C 
                     IF( NEWSYNC ) THEN
C
                        IF( SPEEDUP(KS) .EQ. 1.0 ) THEN
                           TSADD = 8.D0 / 86400.D0
                        ELSE IF( SPEEDUP(KS) .EQ. 2.0 ) THEN
                           TSADD = 13.D0 / 86400.D0
                        ELSE
                           TSADD = 25.D0 / 86400.D0
                        END IF
                        IF( USEDISK(ISTA) ) THEN
                           TSADD = TSADD + 10.D0 / 86400.D0
                        END IF
C
C                       Reconfigured during recording.
C
                        IF( SCNRCF ) THEN
                           TSADD = TSADD + 7.D0 / 86400.D0
                        END IF
                     END IF
C
C                 JIVE correlator.  Recall that TCORR going into
C                 this is the recording medium start time.
C                 Tape case removed.
C
                  ELSE IF( CORREL(1:7) .EQ. 'JIVE' ) THEN
                     IF( NEWCONF ) THEN
                        TSADD = 25.D0 / 86400.D0
                     ELSE
                        TSADD = 1.0D0 / 86400.D0
                     END IF
C
C                 Other correlators
C
                  ELSE
                     IF( CORREL(1:7) .EQ. 'SOCORRO' .OR.
     1                   CORREL(1:4) .EQ. 'VLBA' .OR.
     2                   CORREL(1:8) .EQ. 'VLBADIFX' ) THEN
                        TSADD = 0.D0
                     ELSE
                        TSADD = 1.D0 / 86400.D0
                     END IF
                  END IF
C
C                 Now convert all this to a start time for
C                 the correlator synced data.  Note that up to here,
C                 TCORR is the start of the recording.  
C                 Also make sure TCORR is not past the stop time.
C
                  TCORR(ISCN,ISTA) = TSADD + 
     1                  MAX( TRECON, TCORR(ISCN,ISTA) )
C
C       write(*,*) 'rsync ', ista, iscn,  
C     1      ' start stop: ', startj(iscn),  stopj(iscn),
C     2     ' tsadd, tcorr: ', tsadd, tcorr(iscn,ista), 
C     3     ' tonsrc, trecon:', tonsrc(iscn,ista), trecon
C
                  TCORR(ISCN,ISTA) = MIN( TCORR(ISCN,ISTA), 
     1                  STOPJ(ISCN) )
C
C                 Get the time lost to resynchronizing - including
C                 resync time.  Don't count slewing time.
C
                  TTADD = TCORR(ISCN,ISTA) - 
     1                 MAX( TONSRC(ISCN,ISTA), STARTJ(ISCN) ) 
                  TTSYNC(ISTA) = TTSYNC(ISTA) + MAX( 0.0, TTADD )
C
C                 Warn when a scan is expected to be lost to 
C                 resync.
C
                  IF( TCORR(ISCN,ISTA) .EQ. STOPJ(ISCN) ) THEN
                     NRWARN = NRWARN + 1
                     IF( NRWARN .LE. 100 ) THEN
                        WRITE( MSGTXT, '( A, I5, 3A, F6.2, A )' )
     1                    'RESYNC:  Scan ', ISCN, ' at ', STANAME(ISTA),
     2                    ' at ', MOD(STOPJ(ISCN), 1.D0) * 24.D0, 
     3                    ' hr will be lost to correlator resync.'
                        CALL WLOG( 1, MSGTXT )
                        MSGTXT = ' '
                     ELSE IF( NRWARN .EQ. 101 ) THEN
                        CALL WLOG( 1, 
     1                    'RESYNC:  --- More scans lost to resync.' )
                     END IF
                  END IF
C
                  LRSCN = ISCN
               END IF                  
               LSCN = ISCN
            END IF
         END DO
         IF( TTSYNC(ISTA) * 24.0 .GT. 0.1 * SCNHR(ISTA) ) 
     1           RECWARN = .TRUE.
      END DO
C
C     If there was excessive time spent resyncing, warn and explain.
C
      IF( RECWARN ) THEN
         CALL WLOG( 1, ' ' )
         CALL WLOG( 1, ' **** WARNING ****' )
         CALL WLOG( 1, '    Excessive losses to '//
     1      'correlator resyncs.' )
         CALL WLOG( 1, '    See sched.runlog for '//
     1      'more details.' )
         CALL WRTMSG( 0, 'chkscn', 'resync' )
      END IF
C
      RETURN
      END
