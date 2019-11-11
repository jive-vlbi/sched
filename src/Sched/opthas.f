      SUBROUTINE OPTHAS( LASTISCN, KSCN, ISCN, ADJUST, KEEP, DONE )
Cf2py intent(in) LASTISCN, KSCN, ISCN
Cf2py intent(out) ADJUST, KEEP, DONE
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
C     Save a bit of space by making the input scan variables half size.
C     If there are more than MAXSCN/2 input scans, the program can't
C     work anyway since it creates a new version of the schedule in
C     the tail of the scan arrays.
C
      INTEGER    MAXINS
      PARAMETER  ( MAXINS = MAXSCN / 2 )
      INTEGER    KSCN, ISCN, LASTISCN(MAXSTA)
      LOGICAL    ADJUST, KEEP, DONE
C
      INTEGER    JSCN, LSCN, NOUT, ISRC, ISTA, RSTA, WTMSCN
      INTEGER    SCNUSED(MAXINS)
      INTEGER    IDUMMY, NSKIP, YEAR, DAY1, DAY2, DAYR, DAYS
      DOUBLE PRECISION  TAPPROX, LASTTIME, TSPACE
      REAL       HAMIN(MAXINS), HAMAX(MAXINS)
      REAL       HABEGIN(MAXINS), HAEND(MAXINS)
      REAL       SCNWT(MAXINS), WTMAX
      REAL       OPSLEW, HAP, MINDUR
      REAL       TIMEWT, SLEWWT, RISEWT, SETWT
      DOUBLE PRECISION  DURTOT, SDTOT, START, HTSTOP
      DOUBLE PRECISION  TUSE(MAXINS), TAVAIL(MAXINS), OPHAT(MAXINS)
      DOUBLE PRECISION  OPHAWIDT(MAXINS)
      DOUBLE PRECISION  SKIPTIME, SRCSEP, ATSRC, SKIPINC
      DOUBLE PRECISION  THAMIN(MAXINS), THAMAX(MAXINS)
      CHARACTER  TFORM*8, CSTART*8, CSKIP*8, COPT*8, CRISE*8, CSET*8
C
      INTEGER    SRUSED(MAXINS), SRLOOK(MAXINS)
      LOGICAL    LDUMMY
      SAVE       NOUT, SCNUSED, DURTOT, RSTA
      SAVE       HAMIN, HAMAX, HABEGIN, HAEND, THAMIN, THAMAX
      SAVE       TUSE, TAVAIL
      SAVE       SRUSED, SRLOOK, SKIPTIME, SKIPINC, NSKIP
      SAVE       SDTOT
      SAVE       OPHAT, OPHAWIDT
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
         SKIPTIME = 0.D0
         NSKIP = 0
         SKIPINC = 5.D0 * 60.D0 / 86400.D0
C
C        Check some requirements.
C        Requre OPDUR so we can do rational things relative to the end
C        of observations.
C
         IF( NSCANS .GT. MAXINS ) THEN
            WRITE( MSGTXT, '( A, A, I4, A, I4 )' ) 
     1         'OPHAS: Too many requested scans for OPTMODE=HAS.  ',
     2         'Max:', MAXINS, '  Requested:', NSCANS 
            CALL ERRLOG( MSGTXT )
         END IF
         IF( OPDUR .LE. 0.D0 ) THEN
            CALL ERRLOG( 'OPHAS: Please specify OPDUR for OPTMODE=HAS' )
         END IF
C
C        Keep things simpler by not going over 24 hours sidereal in
C        start times.  Allow for durations.
C
         MINDUR = 0.0
         DO JSCN = 1, NSCANS
            MINDUR = MIN( MINDUR, DUR(JSCN) )
         END DO
         IF( OPDUR * SIDR - MINDUR .GT. 1.D0 ) THEN
            CALL ERRLOG( 'OPHAS: OPTMODE=HAS not meant for ' //
     1          'observations over 24 hours sidereal + min scan dur.' )
         END IF
C
C        HTSTOP is the observation stop time.  Most scan setting 
C        calculations are for the scan start time so this will often
C        need to be adjusted by the duration.
C
         HTSTOP = STARTJ(1) + OPDUR
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
            CALL HALIM( JSCN, RSTA, HAMIN(JSCN), HAMAX(JSCN),
     1                  THAMIN(JSCN), THAMAX(JSCN) )
C
C           Get the experiment start and stop hour angles for the
C           start of scan.
C
            CALL SCHGEO( JSCN, RSTA, STARTJ(1), HA1(JSCN,RSTA), 
     1             EL1(JSCN,RSTA), AZ1(JSCN,RSTA),
     2             LST1(JSCN,RSTA), PA1(JSCN,RSTA) )
            CALL SCHGEO( JSCN, RSTA, HTSTOP - DUR(JSCN), 
     1             HA2(JSCN,RSTA), EL2(JSCN,RSTA), AZ2(JSCN,RSTA),
     2             LST2(JSCN,RSTA), PA2(JSCN,RSTA) )
            HABEGIN(JSCN) = HA1(JSCN,RSTA)
            HAEND(JSCN) = HA2(JSCN,RSTA)
C
C           Get the time each source is available and the desired
C           observe time.
C           A possible weakness in this is that if the user gives
C           different opminants, the rise and set for a source will
C           change and the default scan times will not be a clean
C           sequence.
C
            CALL HAAVAI( THAMIN(JSCN), THAMAX(JSCN),
     1                   STARTJ(1), HTSTOP - DUR(JSCN),
     2                   TAVAIL(JSCN), TUSE(JSCN), 
     3                   SRUSED(SRCNUM(JSCN)), SRLOOK(JSCN), 
     4                   SIDR )
C
C           Abort if the source is never up.
C
            IF( TAVAIL(JSCN) .EQ. -1.D0 ) THEN
               CALL WLOG( 1, ' ' )
               MSGTXT = ' ****** ' //SCNSRC(JSCN) // 
     1           ' is never above OPMINEL and the horizon at ' //
     2           'OPMINANT stations at once.'
               CALL WLOG( 1, MSGTXT )
               CALL ERRLOG( 'Leave out the source or change ' //
     1                   'OPMINANT or OPMINEL' )
             END IF
C
C           Get the desired hour angles.  SRUSED can't be zero from
C           the way it was set (says here!).  If OPHA is not 0.0, that  
C           means it was set by the user.
C
            IF( OPHA(JSCN) .EQ. 0.0 ) THEN
               OPHAT(JSCN) = TUSE(JSCN)
            ELSE
               OPHAT(JSCN) = ( OPHA(JSCN) - HABEGIN(JSCN) ) / SIDR +
     1               STARTJ(1)
               IF( OPHAT(JSCN) .LT. STARTJ(1) ) THEN
                  OPHAT(JSCN) = OPHAT(JSCN) + 24.D0 / SIDR
                  IF( OPHAT(JSCN) .GT. STARTJ(1) + OPDUR ) THEN
                     WRITE( MSGTXT, '( A, I5, 2X, A, F7.2 )' )
     1                 'OPHAS: Requested hour angle ',
     2                 'outside range: scan, source, OPHA: ',
     3                 JSCN, SCNSRC(JSCN), OPHA(JSCN)
                     CALL WLOG( 1, MSGTXT )
                     MSGTXT = ' '                     
                  END IF
               END IF
            END IF
            IF( OPHAWID(JSCN) .EQ. 0.0 ) THEN
               OPHAWIDT(JSCN) = 0.7D0 * TAVAIL(JSCN) / 
     1                SRUSED(SRCNUM(JSCN))
            ELSE
               OPHAWIDT(JSCN) = OPHAWID(JSCN) / 86400.D0
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
C     Allow skipping at the start.
C
      IF( KSCN .EQ. 1 ) THEN
C         STARTJ(ISCN) = STARTJ(1)
C         STOPJ(ISCN) = STARTJ(1) + DUR(1)
         TAPPROX = STARTJ(1) + SKIPTIME
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
      IF( OPPRTLEV .GE. 3 ) THEN
         WRITE( MSGTXT, '( A, A, A )' )
     1      '              Scan   Source     New scan time',
     2      ' Opt scan time  TIMEWT  SLEWWT  RISEWT   SETWT',
     3      '   SCNWT     dT      WID  SlewSec'
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
      END IF
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
                     CALL STAGEO( JSCN, ISTA, TAPPROX, LASTISCN(ISTA),
     1                      LASTTIME, TONSRC(JSCN,ISTA), 'OPTHAS' )
                     IF( KSCN .GT. 1 .AND. 
     1                      UP1(JSCN,ISTA) .EQ. ' ' ) THEN
                        ATSRC = MAX( ATSRC, TONSRC(JSCN,ISTA) )
                     END IF
                  END IF
               END DO
               IF( KSCN .GT. 1 ) THEN
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
               SRCSEP = 1.D0
               IF( KSCN .GT. 1 ) THEN
                  DO LSCN = SCAN1, SCANL
                     IF( SCNSRC(JSCN) .EQ. SCNSRC(LSCN) ) THEN
                        TSPACE = TAPPROX - STOPJ(LSCN)
                        SRCSEP = MIN( SRCSEP, TSPACE )
                     END IF
                  END DO
               END IF
C
C              Enforce the minimum separation.  Also ignore scans
C              that are just too far off in time.  Make that max time
C              a user input some day.
C
               IF( SRCSEP .LT. OPMINSEP(JSCN) * OPHAWIDT(JSCN)
     1              .OR. ABS( TAPPROX - OPHAT(JSCN) ) .GT. 
     2              OPHMAXDT(JSCN) / 86400.D0 ) THEN
C
                  SCNWT(JSCN) = 0.D0
C
               ELSE
C
C                 Get a weight for this scan that is low before the
C                 desired hour angle and high after.
C                 Use an arctan to get a nice
C                 functional form that will emphasize the right 
C                 spacing.
C                 The desired separation is the available interval 
C                 devided by the number of scans requested on the 
C                 source.  This was set up in HAAVAI.
C                 Note the factor of 1.5 in the ATAN just adjusts the 
C                 shape of the curve.  It is arbitrary.  I might need
C                 to play with it.
C
C                 NOTE:  the weight equations are in the manual.  Update
C                        the manual if changes are made.
C
                  TIMEWT = OPHAWT(JSCN) * ( 0.5 + ( 1.0 / PI ) * 
     1                 ATAN( 1.5 * ( TAPPROX - OPHAT(JSCN) ) / 
     2                     OPHAWIDT(JSCN) ) )
C
C                 Adjust the weight according to the slew time.  But
C                 don't reward no source change.  Also SCNWT=0 is a flag
C                 that we don't want this scan so don't undo that flag.
C                 Recall OPSLEWTI and OPSLEW are in seconds
C
                  SLEWWT = 0.0
                  IF( SCNSRC(JSCN) .NE. SCNSRC(ISCN-1) .AND. 
     1                OPSLEWTI(JSCN) .GT. 0.0 ) THEN
                     SLEWWT = MAX( 0.0, OPSLEWWT(JSCN) * 
     1                  ( 1.0 - OPSLEW / OPSLEWTI(JSCN) ) )
                  END IF
C
C                 Try to get sources near rise and set.
C                 Recall OPHLIMTI is in seconds at this point.
C                 This is only worrying about rise and set, not
C                 beginning and end of observation.
C
                  RISEWT = 0.0
                  SETWT = 0.0
                  IF( OPHLIMTI(JSCN) .GT. 0.0 ) THEN
                     RISEWT = MAX( 0.0, OPHLIMWT(JSCN) *
     1                  ( 1.0 - ABS( HA1(JSCN,RSTA) - HAMIN(JSCN) ) *
     2                  3600.0 / OPHLIMTI(JSCN) ) )
                     SETWT = MAX( 0.0, OPHLIMWT(JSCN) *
     1                  ( 1.0 - ABS( HA1(JSCN,RSTA) - HAMAX(JSCN) ) *
     2                  3600.0 / OPHLIMTI(JSCN) ) )
                  END IF
C
C                 Snare the scan with the highest weight.
C
                  SCNWT(JSCN) = TIMEWT + SLEWWT + RISEWT + SETWT
                  IF( SCNWT(JSCN) .GT. WTMAX ) THEN
                     WTMAX = SCNWT(JSCN)
                     WTMSCN = JSCN
                  END IF
C
C                 Write out all the weights for debugging.
C
                  IF( OPPRTLEV .GE. 3 ) THEN
                     CALL TIMEJ( TAPPROX, YEAR, DAY1, START ) 
                     CSTART = TFORM( START, 'T', 0, 2, 2, '::@' )
                     CALL TIMEJ( OPHAT(JSCN), YEAR, DAY2, START )
                     COPT = TFORM( START, 'T', 0, 2, 2, '::@' )
                     WRITE( MSGTXT,'( A, I5, 1X, A, I5, 1X, A, I5,'//
     1                   ' 1X, A, 8F8.2 )' )
     2                   ' SCAN WEIGHT ', JSCN, SCNSRC(JSCN), DAY1, 
     3                   CSTART, DAY2, COPT, TIMEWT, SLEWWT, RISEWT, 
     4                   SETWT, SCNWT(JSCN), 
     5                   ( TAPPROX - OPHAT(JSCN) ) * 24.D0, 
     6                   OPHAWIDT(JSCN) * 24.D0, OPSLEW
                     CALL WLOG( 0, MSGTXT )
                     MSGTXT = ' '
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
         CALL SCNDUP( ISCN, WTMSCN, .FALSE., 'OPTHAS' )
         STARTJ(ISCN) = TAPPROX
         STOPJ(ISCN)  = STARTJ(ISCN) + DUR(ISCN)
         SCNUSED(WTMSCN) = ISCN
         SKIPTIME = 0.D0
         SDTOT = SDTOT + DUR(ISCN)
C
C        Tell the user about this scan.
C
         IF( OPPRTLEV .GE. 2 ) THEN
            CALL TIMEJ( TAPPROX, YEAR, DAY1, START ) 
            CSTART = TFORM( START, 'T', 0, 2, 2, '::@' )
            WRITE( MSGTXT,'( A, I5, 1X, A, I5, 1X, A, A, F10.2, A, ' //
     1          ' F10.3 )' )
     2          'USE SCAN ', WTMSCN, SCNSRC(WTMSCN), DAY1, CSTART,
     3          '  HA=', HA1(WTMSCN,RSTA), '  Weight: ', WTMAX
            CALL WLOG( 0, MSGTXT )
            MSGTXT = ' '
          END IF
C
      ELSE
C
C        If got here, couldn't find a usable scan.
C
         SKIPTIME = SKIPTIME + SKIPINC
         NSKIP = NSKIP + 1
         IF( OPPRTLEV .GE. 1 ) THEN
            CALL TIMEJ( TAPPROX, YEAR, DAY1, START )
            CSKIP = TFORM( START, 'T', 0, 2, 2, '::@' )
            WRITE( MSGTXT, '( A, F6.2, 2A, F7.2, A, I5, A )') 
     1        'OPTHAS:  Skipping ', SKIPINC * 86400.D0 / 60.D0, 
     2        ' min at ', CSKIP, SKIPTIME * 86400.D0 / 60.D0,  
     3        ' min after scan ', ISCN, 
     4        ' because no scans are available.'
            CALL WLOG( 0, MSGTXT )
            MSGTXT = ' '
         END IF
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
         WRITE( MSGTXT, '( A )') 
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
         CALL WLOG( 0, ' ' )
         IF( OPPRTLEV .LE. 0 ) THEN
            WRITE( MSGTXT, '( A, I2, A )' )
     1       '         OPPRTLEV=', OPPRTLEV, 
     2       ':  Minimum information printed.'
         ELSE IF( OPPRTLEV .EQ. 1 ) THEN
            WRITE( MSGTXT, '( A, I2, A )' )
     1       '         OPPRTLEV=', OPPRTLEV, 
     2       ':  Include summary of fate of each input scan.'
         ELSE IF( OPPRTLEV .EQ. 2 ) THEN
            WRITE( MSGTXT, '( A, I2, A, A )' )
     1       '         OPPRTLEV=', OPPRTLEV, 
     2       ':  Minimum feedback as scans chosen plus fate ',
     3       'of input scans.'
         ELSE IF( OPPRTLEV .GE. 3 ) THEN
            WRITE( MSGTXT, '( A, I2, A, A )' )
     1       '         OPPRTLEV=', OPPRTLEV, 
     2       ':  Details of each scan choice plus summaries.'
         END IF
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
         WRITE( MSGTXT, '( A, I5 )' )
     1       '         Requested scans: ', NSCANS
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
         WRITE( MSGTXT, '( A, I5 )' )
     1       '         Scheduled scans: ', NOUT
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
         WRITE( MSGTXT, '( A, F7.3, A )' )
     1       '         Sum of requested durations: ', DURTOT * 24.D0, 
     2       ' hr.'
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
         WRITE( MSGTXT, '( A, F7.3, A )' )
     1       '         Sum of scheduled durations: ', SDTOT * 24.D0, 
     2       ' hr.'
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
         CALL TIMEJ( STARTJ(SCAN1), YEAR, DAY1, START )
         CSTART = TFORM( START, 'T', 0, 2, 2, '::@' )
         WRITE( MSGTXT, '( A, I5, 2X, A )' )
     1       '         Start day and time: ', DAY1, CSTART
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
         CALL TIMEJ( STOPJ(SCANL), YEAR, DAY1, START )
         CSTART = TFORM( START, 'T', 0, 2, 2, '::@' )
         WRITE( MSGTXT, '( A, I5, 2X, A, F12.3 )' )
     1       '         Stop day and time and total duration (hr): ',
     2       DAY1, CSTART, ( STOPJ(SCANL) - STARTJ(SCAN1) ) * 24.D0
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
         WRITE( MSGTXT, '( A, F8.2 )' )
     1       '         Requested duration (hours) ', OPDUR * 24.D0
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
         WRITE( MSGTXT, '( A, F8.3 )' )
     1    '         Maximum allowed deviation from optimum time '//
     2    '(hr; scan 1)', OPHMAXDT(1) / 3600.D0
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
     2        SKIPINC * NSKIP * 86400.D0 / 60.D0, 
     3       ' minutes.  (Some may be after last scan)'
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
C
C        Table header
C
         IF( OPPRTLEV .GE. 1 ) THEN
            CALL WLOG( 0, ' ' )
            CALL WLOG( 0, 'OPTHAS:  Input scan information.  ' //
     1                   'Columns are:' )
            CALL WLOG( 0, '         Output scan number and time ' )
            CALL WLOG( 0, '         Reference station hour angle.  ' //
     1                'for scheduled scan.' )
            CALL WLOG( 0, '         Allowed reference station hour ' //
     1                'angle range (for scan start).' )
            CALL WLOG( 0, '         Reference station hour ' //
     1                 'angles for beginning and end of observations.' )
            CALL WLOG( 0, '         Total hours this source is ' //
     1                 'available.' )
            CALL WLOG( 0, '         The reference station is: ' //
     1                 STANAME(RSTA) )
            CALL WLOG( 0, '  Input    Source    Output      Time  '//
     1         '        HA        Time        HA      HA      HA   '//
     2         '   HA  Available     Rise         Set')
            CALL WLOG( 0, '   Scan               Scan   Day    hms'//
     1         '       Scan        Opt       Min     Max    Start  '//
     2      '  End    Hours' )
C
C           Write the information for each scan to the log.
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
               CALL TIMEJ( OPHAT(JSCN), YEAR, DAY2, START )
               COPT = TFORM( START, 'T', 0, 2, 2, '::@' )
               CALL TIMEJ( THAMIN(JSCN), YEAR, DAYR, START )
               CRISE = TFORM( START, 'T', 0, 2, 2, '::@' )
               CALL TIMEJ( THAMAX(JSCN), YEAR, DAYS, START )
               CSET = TFORM( START, 'T', 0, 2, 2, '::@' )
               WRITE( MSGTXT, '( I5, 4X, A12, I5, I6, 2X, A8, '//
     1           'F8.3, I5, 2X, A, 5F8.3, I5, 1X, A, I5, 1X, A )' )
     1           JSCN, SCNSRC(JSCN), SCNUSED(JSCN), 
     2           DAY1, CSTART, HAP, DAY2, COPT,
     3           HAMIN(JSCN), HAMAX(JSCN), 
     4           HABEGIN(JSCN), HAEND(JSCN), TAVAIL(JSCN) * 24.D0,
     5           DAYR, CRISE, DAYS, CSET
               CALL WLOG( 0, MSGTXT )
               MSGTXT = ' '
            END DO
         END IF
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
