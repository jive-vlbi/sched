      SUBROUTINE CORLST
C
C     Routine for SCHED that writes the correlator information
C     to the summary file.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
C     Note that the "E" accumulators are to include the "EXTRA" 
C     scans while the original versions are for the core.
C
      INTEGER           ISTA, ISCN, I, NSANT, NBAS, NPOL, LEN1, IC
      REAL              DATARATE, MAXSPD, EMAXSPD, DMAXSPD
      DOUBLE PRECISION  STIME
      DOUBLE PRECISION  TTIME, TRTIME, TBTIME
      DOUBLE PRECISION  TIMEB, TIMEL, TLAST
      DOUBLE PRECISION  ETTIME, ETRTIME, ETBTIME
      DOUBLE PRECISION  ETIMEB, ETIMEL, ETLAST
      DOUBLE PRECISION  DTTIME, DTRTIME, DTBTIME
      DOUBLE PRECISION  DTIMEB, DTIMEL, DTLAST
      LOGICAL           OVERLAP, GOTCENT, DOSC
      CHARACTER*90      LINEH, LINE1, LINE2, LINE3, LINE4, LINE5, LINE6
C
C     Note that MAXDR and DATASIZE are passed through the include
C     to the correlator specific routines and the OMS routines 
C     which must be called later.


C  *************  go looking for MAXDR and DATASIZE and add E equiv.


C ------------------------------------------------------------------
C     Only do this if tapes are being used.
C
      IF( NOTAPE ) THEN
         WRITE( ISUM, '( 1X, /, A )' )
     1      'No recordings.  Correlation requests ignored.'
      ELSE
C
C        Write the first line of CORSTUFF - the section header.
C
         WRITE( ISUM, '( 1X, /, A )' ) CORSTUFF(1)(1:LEN1(CORSTUFF(1)))
C
C        Flag a request for multiple correlation centers.
C
         IF( NPAIR .GT. 0 ) THEN
            WRITE( ISUM, '( 1X,/, A ) ' )
     1           '  THIS PROJECT USES MULTIPLE PHASE CENTERS.'
C
C           Check that the number of FFT channels is reasonable for
C           multiple phase centers.  It might be better in CHKCOR, 
C           but it is not necessarily clear by then that the multiple
C           phase centers are being used.  Use a crude check.
C
C           Eventually examine the field of view used in each group
C           of centers and the baseband bandwidth and make a 
C           recommendation.  But that is work so it is put off for
C           now (Feb 2014).
C
            IF( CORFFT .LT. 4000 ) THEN
               CALL WLOG( 1, 'CORLST:   WARNING - your number of '//
     1            'channels in the correlator FFT may not ' )
               CALL WLOG( 1, '          be adequate for multiple '//
     1            'phase centers.' )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I6, A )' ) 
     1            '          You have specified CORCHAN(2) =', 
     2            CORFFT
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1, '          More typical is several '//
     1            'thousand to avoid smearing on offset phase centers.')
               CALL WLOG( 1, '          Please consider CORCHAN(2) '//
     1            'carefully.')
               CALL WLOG( 1, '          See the SCHED manual '//
     1            'sections on multiple phase centers and on '//
     2            'parameter CORCHAN.')
               WRITE( ISUM, '( A )' )
     1            '    Worry about possible inadequate FFT channels.' 
            END IF
         END IF
C
C        Write the correlator summary stuff that was compiled earlier.
C
         WRITE( ISUM, '( 1X, /,  9(A,/), 1X, /, ' // 
     1        ' 5(A,/), 1X, /, 5(A,/) )' ) 
     2      ( CORSTUFF(I)(1:LEN1(CORSTUFF(I))), I = 2, MCOR )
C
C        Get the parameters of the observation that are needed to get 
C        the data rates and volume.
C
C        Get the factor related to the polarization processing.
C
         NPOL = 1
         IF( CORPOL ) NPOL = 2
C
C        Initialize some accumulators.
C
         TTIME = 0.D0
         TRTIME = 0.D0
         TBTIME = 0.D0
         MAXDR = 0.0
         TIMEB = 1.D10
         TIMEL = 0.D0
         TLAST = 0.D0
         DATASIZE = 0.D0
         OVERLAP = .FALSE.
         GOTCENT = .FALSE.
         MAXSPD = 0.0
C
         ETTIME = 0.D0
         ETRTIME = 0.D0
         ETBTIME = 0.D0
         EMAXDR = 0.0
         ETIMEB = 1.D10
         ETIMEL = 0.D0
         ETLAST = 0.D0
         EDATASIZ = 0.D0
         EMAXSPD = 0.0
C
         DTTIME = 0.D0
         DTRTIME = 0.D0
         DTBTIME = 0.D0
         DMAXDR = 0.0
         DTIMEB = 1.D10
         DTIMEL = 0.D0
         DTLAST = 0.D0
         DDATASIZ = 0.D0
         DMAXSPD = 0.0
C
C        Loop through scans.
C
         DO ISCN = SCAN1, SCANL
C
C           Simplify the repeated test for DOSCANS.
C
            DOSC = DOSCANS(1) .EQ. 0 .OR.
     1        ( ISCN .GE. DOSCANS(1) .AND. ISCN .LE. DOSCANS(2) )
C
C           Start and end times of experiment to determine elapsed time.
C
            ETIMEB = MIN( ETIMEB, STARTJ(ISCN) )
            ETIMEL = MAX( ETIMEL, STOPJ(ISCN) )
            IF( PREEMPT(ISCN) .NE. 'EXTRA' ) THEN
               TIMEB = MIN( TIMEB, STARTJ(ISCN) )
               TIMEL = MAX( TIMEL, STOPJ(ISCN) )
            END IF
            IF( DOSC ) THEN
               DTIMEB = MIN( DTIMEB, STARTJ(ISCN) )
               DTIMEL = MAX( DTIMEL, STOPJ(ISCN) )
            END IF
C
C           Get number of stations in the scan.
C
            NSANT = 0
            DO ISTA = 1, NSTA
               IF( STASCN(ISCN,ISTA) ) THEN
                  NSANT = NSANT + 1            
               END IF
            END DO
C
C           Get time in all scans, including non-recording scans.
C
            IF( NSANT .GT. 0 ) THEN
               STIME = ( STOPJ(ISCN) - STARTJ(ISCN) ) * 86400.D0
               ETTIME = ETTIME + STIME
               IF( PREEMPT(ISCN) .NE. 'EXTRA' ) THEN
                  TTIME = TTIME + STIME
               END IF
               IF( DOSC ) THEN
                  DTTIME = DTTIME + STIME
               END IF
            END IF
C
C           For scans recording data, get the time and the time 
C           multiplied by the number of baselines.  Look for signs
C           that the derived data volume will be wrong.
C
C           The estimate here will be an overestimate of the actual
C           time used in some cases.  This is because the time that 
C           the recorders will be on actually depends on the 
C           on-source time for VLA and VLBA MARK5C observations.
C           Thus the starts are station dependent.  For now, just 
C           warn of the over estimate of baseline time, but don't
C           jump through the hoops to fix it.  Some day.....
C
            IF( NSANT .GT. 0 .AND. .NOT. NOREC(ISCN) ) THEN
               NBAS = NSANT * ( NSANT - 1 ) / 2 + NSANT
               ETRTIME = ETRTIME + STIME
               ETBTIME = ETBTIME + STIME * NBAS
               IF( PREEMPT(ISCN) .NE. 'EXTRA' ) THEN
                  TRTIME = TRTIME + STIME
                  TBTIME = TBTIME + STIME * NBAS
               END IF
               IF( DOSC ) THEN
                  DTRTIME = DTRTIME + STIME
                  DTBTIME = DTBTIME + STIME * NBAS
               END IF
C
C              Test for backward time jumps.
C
               IF( STARTJ(ISCN) .LT. TLAST ) OVERLAP = .TRUE.
               TLAST = STOPJ(ISCN)
C
C              Get the data rate based on a Romney formula.
C              The data rate in bytes/sec is given APPROXIMATELY as
C              This is for the VLBA correlator, but is probably 
C              pretty close for any correlator.
C
C               4 * Ns * (Ns + 1) * Nc * Nsp * P * Su / Tavg
C
C               where Ns   = number of stations
C                     Nc   = number of (BaseBand) channels (1, ... 32)
C                     Nsp  = spectral resolution (8, 16, 32 ... 512)
C                     P    = 2 for polarization, 1 for none
C                     Su   = speed up factor (1, 2, 4)
C                    Tavg = time average in seconds
C
C              Note that FSPEED will now always be 1.0.  It is somewhat
C              of a meaningless parameter any more with disk recordings.
C
               IF( CORAVG .GT. 0.0 .AND. .NOT. NOSET ) THEN
                  DATARATE = 4.0 * NSANT * (NSANT+1) * 
     1                       MSCHN(SETNUM(ISCN)) * CORCHAN *
     2                       NPOL * FSPEED(SETNUM(ISCN)) / CORAVG
               ELSE
                  DATARATE = 0.0
               END IF
               EMAXDR = MAX( EMAXDR, DATARATE )
               EMAXSPD = MAX( EMAXSPD, FSPEED(SETNUM(ISCN)))
               IF( PREEMPT(ISCN) .NE. 'EXTRA' ) THEN
                  MAXDR = MAX( MAXDR, DATARATE )
                  MAXSPD = MAX( MAXSPD, FSPEED(SETNUM(ISCN)))
               END IF
               IF( DOSC ) THEN
                  DMAXDR = MAX( DMAXDR, DATARATE )
                  DMAXSPD = MAX( DMAXSPD, FSPEED(SETNUM(ISCN)))
               END IF
C
C              Accumulate the data size.  Note double precision - for 
C              multiple Gbytes, small numbers being added to large 
C              numbers might stop accumulating.
C
               IF( FSPEED(SETNUM(ISCN)) .NE. 0.0 ) THEN
                  EDATASIZ = EDATASIZ + DATARATE * STIME / 
     1                       FSPEED(SETNUM(ISCN))
                  IF( PREEMPT(ISCN) .NE. 'EXTRA' ) THEN
                     DATASIZE = DATASIZE + DATARATE * STIME / 
     1                       FSPEED(SETNUM(ISCN))
                  END IF
                  IF( DOSC ) THEN
                     DDATASIZ = DDATASIZ + DATARATE * STIME / 
     1                       FSPEED(SETNUM(ISCN))
                  END IF
               END IF
C
               IF( ICENT(ISCN) .NE. 0 ) GOTCENT = .TRUE.
C
            END IF
         END DO
C
C        Now write the above results.  Only make the Core/Extras 
C        distinction if there are differences.
C
         WRITE( ISUM, '( 1X, /, A )' )
     1       'DERIVED INFORMATION FOR CORRELATION: '
         LINEH = ' '
         LINE1 = '  Elapsed time for project (hours):' 
         LINE2 = '  Total time in scheduled scans (hours):'
         LINE3 = '  Total time in recording scans (hours): '
         LINE4 = '  Total baseline hours (recording scans):'
         LINE5 = '  Projected max correlator output rate (kB/s):'
         LINE6 = '  Projected correlator output data size (MB):'
         IC    = 47
C
C        Write the core data to the lines if there were fuzzy ends.
C
         IF( FUZZY ) THEN
            WRITE( LINEH(IC+6:IC+10), '( A )' ) 'Core'
            WRITE( LINE1(IC+2:IC+9), '( F8.2 )' )
     1           ( TIMEL - TIMEB ) * 24.D0
            WRITE( LINE2(IC+2:IC+9), '( F8.2 )' ) TTIME / 3600.D0
            WRITE( LINE3(IC+2:IC+9), '( F8.2 )' ) TRTIME / 3600.D0
            WRITE( LINE4(IC+2:IC+9), '( F8.2 )' ) TBTIME / 3600.D0
            WRITE( LINE5(IC:IC+9), '( F10.1 )' ) MAXDR / 1000.0
            WRITE( LINE6(IC:IC+9), '( F10.1 )' ) DATASIZE / 1.D6
            IC = IC + 10
         END IF
C
C        Now write the sums for all data.  Always do this.
C
         WRITE( LINEH(IC+2:IC+10), '( A )' ) 'All scans'
         WRITE( LINE1(IC+2:IC+9), '( F8.2 )' )
     1        ( ETIMEL - ETIMEB ) * 24.D0
         WRITE( LINE2(IC+2:IC+9), '( F8.2 )' ) ETTIME / 3600.D0
         WRITE( LINE3(IC+2:IC+9), '( F8.2 )' ) ETRTIME / 3600.D0
         WRITE( LINE4(IC+2:IC+9), '( F8.2 )' ) ETBTIME / 3600.D0
         WRITE( LINE5(IC:IC+9), '( F10.1 )' ) EMAXDR / 1000.0
         WRITE( LINE6(IC:IC+9), '( F10.1 )' ) EDATASIZ / 1.D6
         IC = IC + 10
C
C        Then the sums for the DOSCANS.
C
         IF( DOSCANS(1) .GT. 0 ) THEN
            WRITE( LINEH(IC+4:IC+10), '( A )' ) 'DOSCANS'
            WRITE( LINE1(IC+2:IC+9), '( F8.2 )' )
     1           ( DTIMEL - DTIMEB ) * 24.D0
            WRITE( LINE2(IC+2:IC+9), '( F8.2 )' ) DTTIME / 3600.D0
            WRITE( LINE3(IC+2:IC+9), '( F8.2 )' ) DTRTIME / 3600.D0
            WRITE( LINE4(IC+2:IC+9), '( F8.2 )' ) DTBTIME / 3600.D0
            WRITE( LINE5(IC:IC+9), '( F10.1 )' ) DMAXDR / 1000.0
            WRITE( LINE6(IC:IC+9), '( F10.1 )' ) DDATASIZ / 1.D6
         END IF
C
C        Now write the lines.
C

         WRITE( ISUM, '( A, 6( /, A ) )' ) LINEH(1:LEN1(LINEH)), 
     1         LINE1(1:LEN1(LINE1)), LINE2(1:LEN1(LINE2)),
     2         LINE3(1:LEN1(LINE3)), LINE4(1:LEN1(LINE4)), 
     3         LINE5(1:LEN1(LINE5)), LINE6(1:LEN1(LINE6))
C
         WRITE( ISUM, '( A, A, /, A, A, /, A, A, /, A, A )' )
     1       ' NOTES:  Above numbers assume the same correlator ',
     2       'parameters are used for all data.',
     3       '         The ''baseline hours'' include the ',
     4       'autocorrelations (for data size calculations).',
     3       '         The correlator output data rate is for ',
     4       'real-time processing.',
     5       '         Above numbers assume the same correlator',
     6       ' parameters are used for all data.'
         WRITE( ISUM, '( A, A, /, A, A )' )
     1       '         The recordings for a scan are assumed to ',
     2       'start at the scan START minus PRESTART.',
     3       '         Some stations (VLA, VLBA/MARK5C) wait for ',
     4       'good data so the estimates may be high.'
         IF( FUZZY ) WRITE( ISUM, '( A )' )
     1       '         ''Core'' are scans with PREEMPT not ''EXTRA''.'
         IF( DOSCANS(1) .NE. 0 ) WRITE( ISUM, '( A, A )' )
     1       '         Only scans in the DOSCANS range will be written',
     2       ' to the Vex and other files.'
         IF( GOTCENT ) WRITE( ISUM, '( A, /, A )' ) 
     1       '         Multiple phase center processing requested.',
     2       '     Correlator output numbers are for first file only.'

         IF( DATASIZE .GT. 16.0E9 ) THEN
            WRITE( ISUM, '( A )' )
     1       '  NOTE:  Output data set size large.  '
         END IF
C
C        Deal with correlator specific situations.  Only Socorro 
C        hardware correlator for now (and that has been shut down).
C
         CALL CORSOC
C
C        Warn if there are reasons to believe the data rates are wrong.
C
         IF( OVERLAP ) THEN
            WRITE( ISUM, '( 1X, /, A, /, 4( A, / ) )' )
     1      ' **** WARNING *****', 
     2      '      Overlapping scans were detected.  Either there '//
     3            'was subarraying',
     4      '      or some stations were scheduled separtely from '//
     5            'others.',
     6      '      If the latter, the output data rates and volume '//
     7            'are underestimated',
     8      '      because the number of baselines is underestimated.'
C
C           I think any pointing scans should have NOREC set and so 
C           shouldn't trigger this warning.  But I'm leaving the code
C           here in case I'm missing something.
C
C            IF( AUTOPEAK ) THEN
C               WRITE( ISUM, '( A, /, A )' )
C     1         '      This might be due to inserted pointing scans '//
C     2               'in which case', 
C     3         '      there is no impact on data estimates.'
C            END IF
C
            WRITE( ISUM, '( 1X, / )' )
         END IF
      END IF
C
      RETURN
      END
