      SUBROUTINE STSUM
C
C     This routine, called by SCHSUM, writes some summary 
C     information about about the numbers of scans, and hours at
C     each station. Much of the data were gathered in SCHTIM.  
C     Note NOTAPE really means no recordings.
C
C     This was made a separate routine from SCHSUM when there
C     got to be too many combinations of FUZZY and MK5C vs MK5A
C     so a lot of code was needed.
C
C     For the legacy VLBA system, we want to keep track of 
C     formatter reconfigures.  Don't bother when they are 
C     not needed.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER     ISCN, ISTA, KSTA, LEN1, IC
      INTEGER     NTPS
      LOGICAL     GOTTPS, DORECONF
      CHARACTER   LINE*90
C ----------------------------------------------------------------
C     Sense if there are any stations with a VLBA DAR.  Those are
C     the ones that need to worry about the reconfigures.  These
C     are the legacy (MARK5A/Tape) DARs that are no longer in use
C     for recording, so this might be removed some day.
C
      DORECONF = .FALSE.
      DO ISTA = 1, NSTA
         KSTA = STANUM(ISTA)
         IF( DAR(KSTA) .EQ. 'VLBA' ) DORECONF = .TRUE.
      END DO
C
C     Do most of this only if stations were recording.
C     Note NOTAPE means that the whole observation is a non-recording
C     observation, like OBSTYP = PTVLBA or CONFIG.
C
      IF( .NOT. NOTAPE ) THEN
C
C        Potentially do tables for the core scans, the all scans
C        including the PREEMPT=EXTRA scans, and the DOSCANS range.
C        Only do the latter two if needed.
C
         WRITE( ISUM, '( 1X, /, A )' ) 'STATION SCAN SUMMARIES:'
C
         IF( FUZZY ) THEN
            WRITE( ISUM, '( 1X, /, A )' )  
     1       '  Summary for core scans (PREEMPT not set to ''EXTRA''): '
         END IF         
C
C        Write the table.  It will either be the only one, or
C        the one for the core depending on FUZZY and DOSCANS.
C
         LINE = '  Station  Control   Scans   Scan   Record  Record'//
     1       '   Gbytes'
         IF( DORECONF ) LINE = LINE(1:LEN1(LINE))//
     1       '     Formatter       Sync '
         WRITE( ISUM, '( A )' ) LINE(1:LEN1(LINE))
C
         LINE = ' '
         LINE(29:52) = 'Hours   Hours   Scans'
         IF( DORECONF ) LINE(64:86) = 'Reconfigures    Hours'
         WRITE( ISUM, '( A )' ) LINE(1:LEN1(LINE))
C
         DO ISTA = 1, NSTA
            LINE = ' '
            WRITE( LINE, '( 2X, A8, 2X, A5, 2X, I6, F8.2, F8.2,
     1         I8, 2X, F8.0 )' )
     2         STANAME(ISTA), CONTROL(STANUM(ISTA)), NSTSC(ISTA), 
     3         SCNHR(ISTA), TPHR(ISTA), TPSCN(ISTA), TGBYTES(ISTA)
            IC = LEN1( LINE ) + 1
            IF( DORECONF ) THEN
               IF( DAR(STANUM(ISTA))(1:4) .NE. 'VLBA' ) THEN
                  WRITE( LINE(IC:IC+30), '( I13, F12.2 )' )
     1                 NRECONF(1,ISTA), TTSYNC(ISTA) * 24.0
               ELSE
                  WRITE( LINE(IC:IC+30), '( I9, A, I3, F12.2 )' )
     1               NRECONF(1,ISTA), '/', NRECONF(2,ISTA), 
     2               TTSYNC(ISTA) * 24.0
               END IF
            END IF
            WRITE( ISUM, '( A )' ) LINE(1:LEN1(LINE))
         END DO
C
C        If there are EXTRA scans, then write the table over
C        for the the full time.
C
         IF( FUZZY ) THEN
C
C
            WRITE( ISUM, '( 1X, /, A )' )  
     1        '  Summary for all scans (including PREEMPT = ''EXTRA''):'
            LINE = '  Station  Control   Scans   Scan   Record  '//
     1        'Record   Gbytes'
            IF( DORECONF ) LINE = LINE(1:LEN1(LINE))//
     1        '     Formatter       Sync '
            WRITE( ISUM, '( A )' ) LINE(1:LEN1(LINE))
C
            LINE = ' '
            LINE(29:52) = 'Hours   Hours   Scans'
            IF( DORECONF ) LINE(64:86) = 'Reconfigures    Hours'
            WRITE( ISUM, '( A )' ) LINE(1:LEN1(LINE))
C
            DO ISTA = 1, NSTA
               LINE = ' '
               WRITE( LINE, '( 2X, A8, 2X, A5, 2X, I6, F8.2, F8.2,
     1            I8, 2X, F8.0 )' )
     2            STANAME(ISTA), CONTROL(STANUM(ISTA)), 
     3            ENSTSC(ISTA), ESCNHR(ISTA), ETPHR(ISTA), 
     4            ETPSCN(ISTA), EGBYTES(ISTA)
               IC = LEN1( LINE ) + 1
               IF( DORECONF ) THEN
                  IF( DAR(STANUM(ISTA))(1:4) .NE. 'VLBA' ) THEN
                     WRITE( LINE(IC:IC+30), '( I13, F12.2 )' )
     1                    NRECONF(1,ISTA), TTSYNC(ISTA) * 24.0
                  ELSE
                     WRITE( LINE(IC:IC+30), '( I9, A, I3, F12.2 )' )
     1                  NRECONF(1,ISTA), '/', NRECONF(2,ISTA), 
     2                  TTSYNC(ISTA) * 24.0
                  END IF
               END IF
               WRITE( ISUM, '( A )' ) LINE(1:LEN1(LINE))
            END DO
         END IF
C
C        Do a set for a DOSCANS range.
C
         IF( DOSCANS(1) .GT. 0 ) THEN
C
            WRITE( ISUM, '( 1X, /, A )' )  
     1        '  Summary for all scans selected by DOSCANS:'
            LINE = '  Station  Control   Scans   Scan   Record  '//
     1        'Record   Gbytes'
            IF( DORECONF ) LINE = LINE(1:LEN1(LINE))//
     1        '     Formatter       Sync '
            WRITE( ISUM, '( A )' ) LINE(1:LEN1(LINE))
C
            LINE = ' '
            LINE(29:52) = 'Hours   Hours   Scans'
            IF( DORECONF ) LINE(64:86) = 'Reconfigures    Hours'
            WRITE( ISUM, '( A )' ) LINE(1:LEN1(LINE))
C
C probably still need to get case dependent nreconfs.  Deal with
C after SCHED 11.0
C
            DO ISTA = 1, NSTA
               LINE = ' '
               WRITE( LINE, '( 2X, A8, 2X, A5, 2X, I6, F8.2, F8.2,
     1            I8, 2X, F8.0 )' )
     2            STANAME(ISTA), CONTROL(STANUM(ISTA)), 
     3            DNSTSC(ISTA), DSCNHR(ISTA), DTPHR(ISTA), 
     4            DTPSCN(ISTA), DGBYTES(ISTA)
               IC = LEN1( LINE ) + 1
               IF( DORECONF ) THEN
                  IF( DAR(STANUM(ISTA))(1:4) .NE. 'VLBA' ) THEN
                     WRITE( LINE(IC:IC+30), '( I13, F12.2 )' )
     1                    NRECONF(1,ISTA), TTSYNC(ISTA) * 24.0
                  ELSE
                     WRITE( LINE(IC:IC+30), '( I9, A, I3, F12.2 )' )
     1                  NRECONF(1,ISTA), '/', NRECONF(2,ISTA), 
     2                  TTSYNC(ISTA) * 24.0
                  END IF
               END IF
               WRITE( ISUM, '( A )' ) LINE(1:LEN1(LINE))
            END DO
         END IF
C
C
      ELSE
         WRITE( ISUM, '( A, /, A )' )  'STATION SCAN SUMMARIES: ',
     1       '  Station  Control   Scans '
         DO ISTA = 1, NSTA
            WRITE( ISUM, '( 2X, A8, 2X, A5, 2X, I6 )' ) 
     1          STANAME(ISTA), CONTROL(STANUM(ISTA)), NSTSC(ISTA) 
         END DO
      END IF
C
C     Explain Sync Hours.
C
      WRITE( ISUM, '( 1X )' )
      WRITE( ISUM, '( A )' )
     1      '  Notes on the station scan summaries: '
      WRITE( ISUM, '( 1X )' )
      WRITE( ISUM, '( A, /, A, /, A, /, A, /, A, /, A )' )
     1      '    "Record Scans" are periods of recording with no '//
     2      'gap.  The Mark5A disk systems ', 
     3      '    have a limit of 1024 such scans.  There are often '//
     4      'multiple projects on a disk pack.',
     5      '    If using MARK5A, try to keep above about 6 GB per '//
     6      'record scan by using MINPAUSE ',
     7      '    and PRESTART to prevent short gaps.  However, '//
     8      'also try to prevent record scans ', 
     9      '    of more than an hour to minimize risk to data '//
     A      'from playback problems.',
     B      '    MARK5C will have one observe scan per record scan '//
     C      'and there is no limit to the number.'
      WRITE( ISUM, '( 1X )' )
C
C     Only talk about reconfigurations and sync when they might be an issue.
C
      IF( .NOT. NOTAPE .AND. DORECONF ) THEN
         WRITE( ISUM, '( A, /, A)' )
     1      '    "Sync Hours" is on-source, in-scan time ' //
     2      'lost during correlation to resyncing',
     3      '    recordings.  Resyncs follow tape stoppages '//
     4      'and formatter reconfigures.'
         WRITE( ISUM, '( 1X )' )
         WRITE( ISUM, '( A, /, A, /, A )' )
     1      '    For VLBA DAR stations, total reconfigures and ' //
     2      'reconfigures during recording are shown.',
     3      '    Reconfigures during recording can cause ' //
     4      'slow correlator sync.',
     5      '    Any reconfigure can slow sync at JIVE.'
      END IF
C
C     Set up a warning about early tape starts.
C
      NTPS = 0
      GOTTPS = .FALSE.
      DO ISCN = SCAN1, SCANL
         DO ISTA = 1, NSTA
            IF( TPSTART(ISCN,ISTA) .NE. 0.D0 ) THEN
               GOTTPS = .TRUE.
               NTPS = NTPS + 1
            END IF
         END DO
      END DO
      IF( GOTTPS ) THEN
         WRITE( ISUM, '( 1X )' )
         WRITE( ISUM, '( 2A, I5, A )' ) '    Recording started',
     1         ' before scan start time ', NTPS, 
     2         ' times.  See PRESTART and MINPAUSE.'
         WRITE( ISUM, '( 2A )' )
     1         '    They may have been kept running through ',
     2         'short scan gaps.'
         WRITE( ISUM, '( 2A )' )
     1         '    The number can be large because each ',
     2         'station/scan combination counts.'
      END IF
      WRITE( ISUM, '( 1X, /, 1X, / )' )
C
      RETURN
      END
