      SUBROUTINE OPTHAS( LASTISCN, KSCN, ISCN, ADJUST, KEEP, DONE )
C
C     Determine scans to observe with OPTMODE='HASEQ'.  This 
C     mode makes one pass through the schedule assigning the next 
C     scan based on a quality measure.  It is meant to be the first, 
C     and simplest, of the astronomy optimization modes that can 
C     completely reorder the input schedule.
C
C
C     Note we can let the geometry parameters and times be altered  
C     in the input scan to save a lot of shuffling of data.
C
C     Input call arguments:
C       KSCN:   Count calls to the optimization routines
C       ISCN:   Output scan number (provided by SCHOPT)
C     Output call arguments:
C       ADJUST:  True=>later operations are allowed to change the time.
C       KEEP:    Do keep this scan (will always be true here)
C       DONE:    Done making output scans 
C                   (this call won't generate a scan.)
C
      INCLUDE 'sched.inc'
C
      INTEGER    KSCN, ISCN, LASTISCN(MAXSTA)
      LOGICAL    ADJUST, KEEP, DONE
C
      INTEGER    JSCN, LSCN, NOUT, ISRC, ISTA, RSTA, WTMSCN
      INTEGER    SCNUSED(MAXSCN)
      INTEGER    IDUMMY, NSKIP, YEAR, DAY1
      DOUBLE PRECISION  TAPPROX, LASTTIME, TSPACE
      REAL       HAMIN(MAXSCN), HAMAX(MAXSCN)
      REAL       HABEGIN(MAXSCN), HAEND(MAXSCN), HAAVAIL(MAXSCN)
      REAL       SCNWT(MAXSCN), WTMAX
      REAL       OPSLEW, HAP, HAUSE(MAXSCN)
      DOUBLE PRECISION  DURTOT, SDTOT, START
      DOUBLE PRECISION  SKIPTIME, SRCSEP, ATSRC, SKIPINC
      LOGICAL    SKIPWARN, SETRISE(MAXSCN)
      CHARACTER  TFORM*8, CSTART*8, CSKIP*8
C

      INTEGER    SRUSED(MAXSRC), SRLOOK(MAXSCN)
      LOGICAL    LDUMMY
      SAVE       NOUT, SCNUSED, DURTOT, RSTA, HAUSE, SETRISE
      SAVE       HAMIN, HAMAX, HABEGIN, HAEND, HAAVAIL
      SAVE       SRUSED, SRLOOK, SKIPWARN, SKIPTIME, SKIPINC, NSKIP
      SAVE       SDTOT
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'OPTHAS: Starting.' )
C
C     Assume that the number of output scans should be equal to the 
C     number of scans provided by the user.
C
      IF( NOUT .GT. NSCANS ) THEN
         DONE = .TRUE.
         GO TO 999
      END IF
C
      KEEP = .TRUE.
C
C     Initialize many items on first call.
C
      IF( KSCN .EQ. 1 ) THEN
         NOUT = 0
         SKIPWARN = .TRUE.
         SKIPTIME = 0.D0
         NSKIP = 0
         SKIPINC = 5.D0 * 60.D0 / 86400.D0
C
C        Check some requirements.
C        Requre OPDUR so we can do rational things relative to the end
C        of observations.
C
         IF( OPDUR .LE. 0.D0 ) THEN
            CALL ERRLOG( 'OPHAS: Please specify OPDUR for OPTMODE=HAS' )
         END IF
         IF( OPDUR .GT. 1.D0 ) THEN
            CALL ERRLOG( 'OPHAS: This early version of OPTMODE=HAS ' //
     1          'not meant for observations over 24 hours' )
         END IF
C
C        Get the station number for the reference station.
C
         CALL STANO( OPHASTA, IDUMMY, RSTA, LDUMMY )
C
C        Initialize bookkeeping for the accounting of when a source
C        was used.
C
         DO ISRC = 1, NSRC
            SRUSED(ISRC) = 0
         END DO
         DO JSCN = 1, NSCANS
            SRLOOK(JSCN) = 0
         END DO

C
C        Determine how often each source is requested.
C        For each scan, tell which look at the source this was.
C        Initialize SCNUSED.  
C        Accumulate the total in scan time.
C
         DURTOT = 0.D0
         SDTOT = 0.D0
         DO JSCN = 1, NSCANS
            ISRC   = SRCNUM(JSCN)
            SRUSED(ISRC)  = SRUSED(ISRC) + 1
            SRLOOK(JSCN)  = SRUSED(ISRC)
            SCNUSED(JSCN) = 0
            DURTOT = DURTOT + DUR(JSCN)
         END DO
C
C        Compare the total time request with OPDUR.
C
         IF( DURTOT .GT. OPDUR ) THEN
            WRITE( MSGTXT, '( A, F8.3, A  )' )
     1         'OPTHAS: ** The total time in requested scans of', 
     1         DURTOT * 24.D0, ' hours' 
            CALL WLOG( 1, MSGTXT )
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F8.3, A )' )
     1         '           exceeds the experiment duration (OPDUR) of ',
     2         OPDUR * 24.D0, ' hours.'
            CALL WLOG( 1, MSGTXT )
            MSGTXT = ' '
            CALL WLOG( 1, '           Not all scans can be scheduled.' )
         END IF
C
C        For each scan, find the minimum and maximum hour
C        angles for the scan start time to adhere to the minimum 
C        elevation and minimum number of antennas.  
C        This is done on a per-scan basis to allow for different 
C        elevation limits or minimum antennas.
C        Also allows for future imposed limits.
C        Note that HAMAX is the HA of the start of the scan so that
C        the scan will be valid to the end.  This presumes later tests
C        will be based on the start time.
C        Also get the reference station hour angles at the start
C        and end of the experiment to help with later optimizations.
C        And get the total hours the source is available for observing
C
         DO JSCN = 1, NSCANS
C
C           Get the hour angle limits.
C
            CALL HALIM( JSCN, RSTA, HAMIN(JSCN), HAMAX(JSCN) )
C
C           Get the experiment start and stop hour angles.
C
            CALL SCHGEO( JSCN, RSTA, STARTJ(1), HA1(JSCN,RSTA), 
     1             EL1(JSCN,RSTA), AZ1(JSCN,RSTA),
     2             LST1(JSCN,RSTA), PA1(JSCN,RSTA) )
            CALL SCHGEO( JSCN, RSTA, STARTJ(1)+OPDUR, HA2(JSCN,RSTA), 
     1             EL2(JSCN,RSTA), AZ2(JSCN,RSTA),
     2             LST2(JSCN,RSTA), PA2(JSCN,RSTA) )
            HABEGIN(JSCN) = HA1(JSCN,RSTA)
            HAEND(JSCN) = HA2(JSCN,RSTA)
C
C           Get the time each source is available.  Some day be
C           clever and don't call this every time a source is
C           looked at.  For now, don't worry about it.
C
            CALL HAAVAI( HAMIN(JSCN), HAMAX(JSCN), HABEGIN(JSCN), 
     1                   HAEND(JSCN), HAAVAIL(JSCN), HAUSE,
     2                   SETRISE(JSCN), SRUSED(SRCNUM(JSCN)), 
     3                   OPDUR, SIDR )
C
C           Get the desired hour angles.  SRUSED can't be zero from
C           the way it was set (says here!).
C
            IF( OPHA(JSCN) .EQ. 0.0 ) THEN
               OPHA(JSCN) = HAUSE(SRLOOK(JSCN))
            END IF
            IF( OPHAWID(JSCN) .EQ. 0.0 ) THEN
               OPHAWID(JSCN) = HAAVAIL(JSCN) / SRUSED(SRCNUM(JSCN))
            END IF
C
         END DO
C
      END IF
C
C     Jump back to here if no scan could be found be we aren't to
C     the end of the experiment.  SKIPTIME will be set to push the
C     scan time forward.
C
  100 CONTINUE
C
C     Now go through the input scans and pick one.
C
C     Get the nominal start time of the scan for calculating
C     geometry, slews etc.  For the first scan, use the experiment
C     start time and don't allow that to be adjusted later.
C     For later scans, use the last scan stop time and allow 
C     adjustment later.
C
      IF( KSCN .EQ. 1 ) THEN
C         STARTJ(ISCN) = STARTJ(1)
C         STOPJ(ISCN) = STARTJ(1) + DUR(1)
         TAPPROX = STARTJ(1)
         ADJUST = .FALSE.
      ELSE IF( SKIPTIME .NE. 0.D0 ) THEN
         TAPPROX = STOPJ(ISCN-1) + SKIPTIME
         ADJUST = .FALSE.
         IF( TAPPROX .GT. STARTJ(1) + OPDUR ) THEN
            DONE = .TRUE.
            GO TO 999
         END IF
      ELSE
         TAPPROX = STOPJ(ISCN-1)
         ADJUST = .TRUE.
      END IF
      WTMAX = 0.0
      WTMSCN = 0
      DO JSCN = 1, NSCANS
         IF( SCNUSED(JSCN) .EQ. 0 ) THEN
C
C           Note that so far there is no need for SCNWT to be an array.
C
            SCNWT(JSCN) = 0.0
C
C           First see if the source is up at the reference station.
C           If not, we save a lot of work.
C           Note that the elev limits and minimum number of 
C           stations are already incorporated in the HAMIN and HAMAX.
C
            CALL SCHGEO( JSCN, RSTA, TAPPROX, HA1(JSCN,RSTA), 
     1             EL1(JSCN,RSTA), AZ1(JSCN,RSTA),
     2             LST1(JSCN,RSTA), PA1(JSCN,RSTA) )
            IF( HA1(JSCN,RSTA) .GE. HAMIN(JSCN) .AND.
     1          HA1(JSCN,RSTA) .LE. HAMAX(JSCN) ) THEN
C
C              Now get the geometry for all stations so that we can
C              utilize slew times etc.  Also, for the slew time, get
C              the time the last antenna that will be up is on source.
C              Couch the slew time in seconds.
C
               ATSRC = 0.D0
               DO ISTA = 1, NSTA
                  IF( STASCN(JSCN,ISTA) ) THEN
                     CALL OPTGEO( JSCN, ISTA, TAPPROX, LASTISCN(ISTA),
     1                      LASTTIME, TONSRC(JSCN,ISTA) )
                     IF( NOUT .GT. 0 .AND. 
     1                      UP1(JSCN,ISTA) .EQ. ' ' ) THEN
                        ATSRC = MAX( ATSRC, TONSRC(JSCN,ISTA) )
                     END IF
                  END IF
               END DO
               IF( NOUT .EQ. 0 ) THEN
                  OPSLEW = ( ATSRC - STOPJ(ISCN-1) ) * 86400.D0
               ELSE
                  OPSLEW = 0.D0
               END IF
C
C              Make a priority based schedule.
C
C              Get the interval since the last scan on this source.
C              Set to 24.0 hr if this is the first scan on the source.
C              Then don't let sources be too close together.
C
               SRCSEP = 24.D0
               IF( NOUT .GT. 0 ) THEN
                  DO LSCN = SCAN1, SCANL
                     IF( SCNSRC(JSCN) .EQ. SCNSRC(LSCN) ) THEN
                        TSPACE = ( TAPPROX - STOPJ(LSCN) ) * 24.D0
                        SRCSEP = MIN( SRCSEP, TSPACE )
                     END IF
                  END DO
               END IF
C
               IF( SRCSEP .LT. OPMINSEP(JSCN) * OPHAWID(JSCN) ) THEN
                  SCNWT(JSCN) = 0.D0
               ELSE
C
C                 Get a weight for this scan that is low before the
C                 desired hour angle and high after.
C                 Use an arctan to get a nice
C                 functional form that will emphasize the right 
C                 spacing.
C                 The desired separation is the available interval 
C                 devided by the number of scans requested on the 
C                 source.
C                 But insist on a minimum separation.
C                 Note the factor of 1.5 in the ATAN just adjusts the 
C                 shape of the curve.  It is arbitrary.
C
C                 Deal with the special case of a source that sets, then
C                 rises.  The low HA scans belong at the end, not
C                 the beginning.
C
                  IF( OPHA(JSCN) .GT. HAEND(JSCN) .AND.
     1                SETRISE(JSCN) ) THEN
                     SCNWT(JSCN) = 0.0
                  ELSE
                     SCNWT(JSCN) = 0.5 + ( 1.0 / PI ) * 
     1                   ATAN( 1.5 * ( HA1(JSCN,RSTA) - OPHA(JSCN) ) / 
     2                   OPHAWID(JSCN) )
                  END IF
C
  Picking bad scans because they are the only ones available despite
large HA offsets.  Perhaps have a limit on how far off a scan can be.
       write(*,'( A, 2f6.2, I4, 4f6.2, l2, A, A )' )
     a' SCNWT ', ( HA1(JSCN,RSTA) - OPHA(JSCN) ), OPHAWID(JSCN),
     1            JSCN, SRCSEP, SCNWT(JSCN), OPHA(JSCN), HAEND(JSCN),
     2            setrise(jscn), ' ', scnsrc(jscn)

C                 Adjust the weight according to the slew time.  But
C                 don't reward no source change.  Also SCNWT=0 is a flag
C                 that we don't want this scan so don't undo that flag.
C
                  IF( SCNSRC(JSCN) .NE. SCNSRC(ISCN-1) .AND. 
     1                OPSLEWTI(JSCN) .GT. 0.0 ) THEN
                     SCNWT(JSCN) = SCNWT(JSCN) + MAX( 0.0,  
     1                   OPSLEWWT(JSCN) * 
     2                  ( 1.0 - OPSLEW / OPSLEWTI(JSCN) ) )
                  END IF
C
C                Try to get sources near rise and set.
C
                  IF( OPHLIMTI(JSCN) .GT. 0.0 ) THEN
                     SCNWT(JSCN) = SCNWT(JSCN) + 
     1                  MAX( 0.0, OPHLIMWT(JSCN) *
     2                  ( 1.0 - ABS( HA1(JSCN,RSTA) - HAMIN(JSCN) ) /
     3                  OPHLIMTI(JSCN) ) )
                     SCNWT(JSCN) = SCNWT(JSCN) + 
     1                  MAX( 0.0, OPHLIMWT(JSCN) *
     2                  ( 1.0 - ABS( HA1(JSCN,RSTA) - HAMAX(JSCN) ) /
     3                  OPHLIMTI(JSCN) ) )
                  END IF
C
C                 Snare the scan with the highest weight.
C
                  IF( SCNWT(JSCN) .GT. WTMAX ) THEN
                     WTMAX = SCNWT(JSCN)
                     WTMSCN = JSCN
                  END IF
               END IF
            END IF
         END IF
      END DO
C
C     Now keep the scan with the highest weight
C
      IF( WTMSCN .GT. 0 ) THEN
         NOUT = NOUT + 1
         CALL SCNDUP( ISCN, WTMSCN, .FALSE. )
         STARTJ(ISCN) = TAPPROX
         STOPJ(ISCN)  = STARTJ(ISCN) + DUR(ISCN)
         SCNUSED(WTMSCN) = ISCN
         SKIPTIME = 0.D0
         SDTOT = SDTOT + DUR(ISCN)
       write(*,*) 'OPTHAS:', SCNSRC(ISCN), WTMSCN, WTMAX, 
     1      HA1(WTMSCN,RSTA)
C
      ELSE
C
C        If got here, couldn't find a usable scan.
C
         CALL TIMEJ( TAPPROX, YEAR, DAY1, START )
         CSKIP = TFORM( START, 'T', 0, 2, 2, '::@' )
         WRITE( MSGTXT, '( A, F6.2, 3A, I5, A )') 
     1     'OPTHAS:  Skipping ', SKIPINC * 86400.D0 / 60.D0, 
     2     ' minutes at ', CSKIP, ' after scan ', ISCN, 
     3     ' because no scans are available.'
         CALL WLOG( 1, MSGTXT )
         MSGTXT = ' '
         SKIPWARN = .FALSE.
         SKIPTIME = SKIPTIME + SKIPINC
         NSKIP = NSKIP + 1
C
C        Jump back to the beginning of the scan search to try again
C        at the later time.
C
         GO TO 100
C
      END IF
C
C     Test that the scan is in the requested window.
C
      IF( OPDUR .GT. 0.D0 .AND. 
     1    STOPJ(ISCN) .GT. STARTJ(1) + OPDUR ) THEN
         WRITE( MSGTXT, '( )') 
     1       'OPTHAS:  Schedule stopped because requested total ' //
     2       'duration reached'
         CALL WLOG( 1, MSGTXT )
         MSGTXT = ' '
         DONE = .TRUE. 

      END IF
C
  999 CONTINUE
C
C     Write a summary of use of input scans.
C
      IF( DONE ) THEN
C
         CALL WLOG( 0, ' ' )
         WRITE( MSGTXT, '( A )' )
     1       'OPTHAS:  HAS mode schedule made.'
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
         WRITE( MSGTXT, '( A, I5 )' )
     1       '         Requested scans: ', NSCANS
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
         WRITE( MSGTXT, '( A, F7.3, A )' )
     1       '         Sum of requested durations: ', DURTOT * 24.D0, 
     2       ' hr.'
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
         WRITE( MSGTXT, '( A, I5 )' )
     1       '         Scheduled scans: ', NOUT
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
         WRITE( MSGTXT, '( A, F7.3, A )' )
     1       '         Sum of scheduled durations: ', SDTOT * 24.D0, 
     2       ' hr.'
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
         WRITE( MSGTXT, '( A, F7.3, A, I5 )' )
     1       '         Number of ', SKIPINC * 86400.D0 / 60.D0, 
     2       ' minutes skips because of no scan available: ', NSKIP
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
         WRITE( MSGTXT, '( A, F8.3, A )' )
     1       '         Total time in skips: ', 
     2        SKIPINC * NSKIP * 86400.D0 / 60.D0, ' minutes.'
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
         WRITE( MSGTXT, '( A, F7.3, A )' )
     1       '         Total Duration: ', 
     2       ( STOPJ(SCANL) - STARTJ(SCAN1) ) * 24.D0, ' hr.'
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
C        Table header
C
         CALL WLOG( 0, ' ' )
         CALL WLOG( 0, 'OPTHAS:  Input scan information.  ' //
     1             'Columns are:' )
         CALL WLOG( 0, '         Output scan number and time ' )
         CALL WLOG( 0, '         Reference station hour angle.  ' //
     1             'for scheduled scan.' )
         CALL WLOG( 0, '         Allowed reference station hour ' //
     1             'angle range (for scan start).' )
         CALL WLOG( 0, '         Reference station hour ' //
     1              'angles for beginning and end of observations. ' )
         CALL WLOG( 0, '         Total hours this source is ' //
     1              'available.' )
         CALL WLOG( 0, '         The reference station is: ' //
     1              STANAME(RSTA) )
         CALL WLOG( 0, '  Input    Source    Output      Time  '//
     1      '        HA      HA      HA      HA      HA   '//
     2      '   HA  Available')
         CALL WLOG( 0, '   Scan               Scan   Day    hms'//
     1      '       Scan    Opt     Min     Max    Start  '//
     2      '  End    Hours' )
C
C        Write the information for each scan to the log.
C
         DO JSCN = 1, NSCANS
            IF( SCNUSED(JSCN) .NE. 0 ) THEN
               CALL TIMEJ( STARTJ(SCNUSED(JSCN)), YEAR, DAY1, START )
               CSTART = TFORM( START, 'T', 0, 2, 2, '::@' )
               HAP = HA1(SCNUSED(JSCN),RSTA)
            ELSE
               DAY1 = 0
               CSTART = ' '
               HAP = 0.0
            END IF
            WRITE( MSGTXT, '( I5, 4X, A12, I5, I6, 2X, A8, 7F8.3 )' )
     1           JSCN, SCNSRC(JSCN), SCNUSED(JSCN), 
     2           DAY1, CSTART, HAP, OPHA(JSCN),
     3           HAMIN(JSCN), HAMAX(JSCN), 
     4           HABEGIN(JSCN), HAEND(JSCN), HAAVAIL(JSCN)
            CALL WLOG( 0, MSGTXT )
            MSGTXT = ' '
         END DO
         WRITE( MSGTXT, '( A, I5, A, I5, A )' )
     1      'OPTHAS: There were ', NSCANS, ' requested scans and ', 
     2      NOUT, ' Scheduled scans.'
         CALL WLOG( 1, MSGTXT ) 
         MSGTXT = ' '
         CALL WLOG( 1, 'OPTHAS: See the sched.runlog for details.' )
      END IF
C
      RETURN
      END
