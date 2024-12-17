      SUBROUTINE SATTIM( ISCN, START, STOP, DAY, YEAR )
C
C     Routine to test input times and process them to form Julian
C     start and stop times which will be used throughout the rest of
C     the SCHED.  All times are fractional days.
C
C     Note that this routine is not where dwell time scheduling, or other
C     optimization is done and the times can be quite far off.  Scan 
C     times should not be used for anything that matters until after
C     SCHOPT is called.
C     
C     This was called SETTIM, but the MicroSoft fortran library also has
C     a routine of that name.
C
      INCLUDE 'sched.inc'
C
      DOUBLE PRECISION   START, STOP, RJDAY
      DOUBLE PRECISION   INSTOP, UT, INDUR, INGAP
      INTEGER            DAY, YEAR, ISCN, LASTDAY, ERR
      INTEGER            RDAY, INDAY, INYEAR
      LOGICAL            WARN1, WARN2
      SAVE               LASTDAY, WARN1, WARN2
C ---------------------------------------------------------------------
C     First, do a series of checks that the inputs are appropriate.
C
C     Test for required time information in first scan.
C
      IF( ISCN .EQ. 1 ) THEN
         IF( START .EQ. UNSET ) 
     1       CALL ERRLOG( 
     2           'SATTIM: Start time required for first scan.' )
         IF( DAY .EQ. 0 ) 
     1       CALL ERRLOG( 'SATTIM: Day required for first scan.' )
         IF( YEAR .EQ. 0 .AND. .NOT. ( LST .AND. DAY .GT. 366 ) ) 
     1       CALL ERRLOG( 'SATTIM: Year required for first scan.' )
         LASTDAY = DAY
         WARN1 = .TRUE.
         WARN2 = .TRUE.
         TFIRST = 999.D9
      END IF
C
C     Check for inappropriate specification of new day with duration
C     scheduling.
C
      IF( DAY .NE. 0 ) THEN
         IF( START .EQ. UNSET .AND. STOP .EQ. UNSET )  THEN
            IF( DAY .NE. LASTDAY ) THEN
               CALL ERRLOG('SATTIM: Day not allowed to change without'//
     1           '  new START or STOP ' )
            END IF
         END IF
         LASTDAY = DAY
      END IF
C
C     Make sure we know how long the scan should be.
C
      IF( STOP .EQ. UNSET .AND. DUR(ISCN) .LE. 0.D0 ) THEN
         WRITE( MSGTXT, '( A, I5 )' )
     1     'SATTIM: No STOP time or DURation available for scan ', ISCN
         CALL ERRLOG( MSGTXT )
      END IF
C
C     Warn if both GAP and START were specified.  Don't worry about
C     first scan - might be valid spec, especially if optimizing.
C
      IF( START .NE. UNSET .AND. GAP(ISCN) .NE. 0.D0 .AND. 
     1    ISCN .NE. 1 .AND. WARN1 ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, A, I5 )' ) 
     1     'SATTIM: WARNING - Both GAP and START specified.',
     2     '  Will use START.  Scan: ', ISCN
         CALL WLOG( 1, MSGTXT )
         WARN1 = .FALSE. 
      END IF
C
C     If START and DWELL were specified, warn of what will happen.
C     Don't worry about first scan - might be valid spec, especially 
C     if optimizing.
C
      IF( START .NE. UNSET .AND. DWELL(ISCN) .AND. ISCN .NE. 1 .AND.
     1    WARN2 ) THEN
         CALL WLOG( 1, 'SATTIM: WARNING, both START and DWELL given.'//
     1        '  Will use START.' )
         WARN2 = .FALSE.
      END IF
C
C     Now use the inputs to establish the first cut at scan times.
C     These may be modified later if there is any optimization.
C     Avoid dealing with the start time day number - confusing with
C     LST options.
C
C     Transfer input parameters to local variables in preparation for
C     possible changes if a day boundary was crossed or if LST was
C     specified.
C
      INSTOP  = STOP
      INDUR   = DUR(ISCN)
      INGAP   = GAP(ISCN)
      INYEAR  = YEAR
      INDAY   = DAY
C
C     Above we required that STOP or DUR be set.  If STOP was not
C     set, and START was, set INSTOP now.  Later, once we have
C     STOPJ, we will reconstruct STARTJ from DUR.  This avoids
C     a lot of hoop jumping concerning the start day number.
C     If START, STOP, and DUR were given, require consistency to
C     better than 1 second.
C     Also add a day to DUR if start and stop on different days.
C     Warn the user when this happens.
C
      IF( START .NE. UNSET .AND. INSTOP .EQ. UNSET ) THEN
         IF( START + INDUR .GT. 1.D0 - ONESEC ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I4, 2A )' ) 
     1           'SATTIM: ** WARNING:  Scan ', ISCN, ' has a specified',
     2        ' start time and crosses midnight (or almost).'
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, '        Remember that YEAR, MONTH, and DAY'//
     1        ' apply to the scan stop time.' )
            CALL WLOG( 1, '        Also if the stop was right at 24hr,'
     1        // ' it might be adjusted to 23:59:59 after application'
     2        // ' of the day.' )
            CALL WLOG( 1, '        Check your scan dates carefully.' )
         END IF
         INSTOP = MOD( START + INDUR, 1.D0 )
      END IF
      IF( START .NE. UNSET .AND. INSTOP .NE. UNSET ) THEN
         INDUR = INSTOP - START
         IF( INDUR .LT. 0.D0 ) INDUR = INDUR + 1.D0
         IF( DUR(ISCN) .NE. 0.D0 .AND. 
     1       ABS( DUR(ISCN) - INDUR ) .GT. 1.D0 / 86400.D0 ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I5 )' ) 
     1         'SATTIM: START, STOP, and DURATION all given for scan ', 
     2         ISCN
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, '        They are not consistent. ' )
            CALL ERRLOG( ' Please fix times, or remove one.' )
         END IF
      END IF
C
C     If LST was specified, convert times to UT.
C     START, STOP, DUR, DWELL, and GAP are assumed to be sidereal times.
C     The day can be specified in one of two ways.  If DAY is greater
C     than 366, it is assumed to be the Local Sidereal Day as found on
C     VLA schedules and YEAR and MONTH are ignored.  If it is less than
C     366, YEAR, MONTH, and DAY are assumed to specify a UT day.
C     The following converts INYEAR, INDAY, INSTOP, INDUR, and INGAP
C     to UT times.
C
      IF( LST ) THEN
C
         IF( INSTOP .NE. UNSET ) THEN
            CALL GLSTDAY( ISCN, INYEAR, INDAY, INSTOP, RDAY )
C
            CALL LST2UT( LONG(LSTSTA), RDAY, INSTOP,
     1                   INYEAR, INDAY, UT )
            INSTOP = UT
         END IF
C
C        Convert sidereal duration to UT duration.
C
         INDUR = INDUR / SIDR 
         INGAP = INGAP / SIDR
C
      END IF
C
C     Check for reasonable year.
C
      IF( ( INSTOP .NE. UNSET .AND.
     1    ( INYEAR .LT. 1900 .OR. INYEAR .GT. 2100 ) ) ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I6 )' ) 
     1      ' **** WARNING: Unlikely year: ', INYEAR
         CALL WLOG( 1, MSGTXT )
      END IF
C
C     Determine STOPJ(ISCN), which is a Modified Julian day number, 
C     if we have INSTOP. This form of time is used to avoid 24 hr 
C     ambiguities.
C
      IF( INSTOP .NE. UNSET )  THEN
         CALL SLA_CLDJ( INYEAR, 1, INDAY, RJDAY, ERR )
         IF( ERR .NE. 0 .AND. ERR .NE. 3 ) 
     1         CALL ERRLOG( 'Problem converting to MJD' )
         STOPJ(ISCN) = INSTOP + RJDAY
      END IF
C
C     Now get STARTJ(ISCN), either from the stop of this scan, if
C     we have INDUR, or from the previous scan stop time.
C
      IF( INSTOP .NE. UNSET .AND. INDUR .NE. 0.D0 ) THEN
         STARTJ(ISCN) = STOPJ(ISCN) - INDUR
      ELSE 
         STARTJ(ISCN) = STOPJ(ISCN-1) + INGAP
      END IF
C
C     Now get STOPJ(ISCN) if we don't already have it.
C
      IF( INSTOP .EQ. UNSET ) THEN
         STOPJ(ISCN) = STARTJ(ISCN) + INDUR
      END IF
C
C     Set DUR(ISCN) and GAP(ISCN) according to above adjustments.
C     This also accomplishes the LST to UT conversion.
C     Note GAP is not set to STARTJ(ISCN) - STOPJ(ISCN-1) because
C     that could do odd things when START was specified.
C
      DUR(ISCN) = STOPJ(ISCN) - STARTJ(ISCN)
      GAP(ISCN) = INGAP
C
C     Adjustments for PRESCAN will be made in OPTTIM called by SCHOPT.
C
C     Find the earliest start time of the experiment.  This used
C     to be done after most schedule optimization, but is needed
C     for some tape handling options earlier than that.  Adjust
C     for PRESCAN assuming that the full PRESCAN will be used
C     on the first scan (no concern about not overlapping an
C     earlier scan.
C
      TFIRST = MIN( STARTJ(ISCN) + PRESCAN(ISCN), TFIRST )
C
      RETURN
      END

