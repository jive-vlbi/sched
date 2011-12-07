      SUBROUTINE GETTIM( ISCN, VALUE, KC, KI, START, STOP, DAY, YEAR,
     1                   MJD1 )
C
C     Routine for SCHED called by SCHIN to extract time information
C     from the input for each scan.
C
C     The schedule timing parameters are passed to other routines 
C     through sched.inc are  DUR, GAP, PRESCAN, and DWELL.
C
C     This routine collects the scan inputs as the scans go by.
C     SATTIM will be called later by TIMES (called by SCHIN) to set 
C     the STARTJ etc based on the START, STOP, DAY, and YEAR 
C     parameters read here.
C
C     MJD1 is the MJD of the first scan and is used for selection
C     of locations file entries when there is episodic motion.
C     It might be off by a day, but that shouldn't matter.
C
      INCLUDE 'sched.inc'
C
      INTEGER           ISCN, KI(*), MONTH, DOY, YR, JERR
      INTEGER           I1, I2, KEYPTR, TNOWAIT
      INTEGER           YEAR(*), DAY(*), MYEAR, MDAY
      DOUBLE PRECISION  STOP(*), START(*), MUT
      DOUBLE PRECISION  VALUE(*), TDWELL, TDUR, TMINDW, MJD1
      CHARACTER         KC(*)*(*)
C -------------------------------------------------------------------
C     Month defaults to 1 for day = day of year.
C     Prevent use of optional 2 digit YEAR in SLA_CALYD.
C
      DAY(ISCN)  = VALUE( KEYPTR( 'DAY', KC, KI ) )
      MONTH      = VALUE( KEYPTR( 'MONTH', KC, KI ) )
      YEAR(ISCN) = VALUE( KEYPTR( 'YEAR', KC, KI ) )
      IF( YEAR(ISCN) .LT. 100 .AND. YEAR(ISCN) .NE. 0 ) THEN
         CALL ERRLOG( 'GETTIM:  Don''t use 2 digit year!' )
      END IF
      IF( DAY(ISCN) .NE. 0 .AND. MONTH .GT. 1 ) THEN
         CALL SLA_CALYD( YEAR(ISCN), MONTH, DAY(ISCN), YR, DOY, JERR )
         IF( JERR .EQ. 2 )  CALL ERRLOG( 'GETTIM: Bad month ' )
         IF( JERR .EQ. -1 ) CALL ERRLOG( 'GETTIM: Unlikely year ' )
         DAY(ISCN)  = DOY
         YEAR(ISCN) = YR
      END IF
      I1 = KEYPTR( 'START', KC, KI )
      I2 = KEYPTR( 'STOP', KC, KI )
      START(ISCN) = VALUE(I1) / 86400.0D0
      STOP(ISCN)  = VALUE(I2) / 86400.0D0
      IF( VALUE(I1) .EQ. UNSET )  START(ISCN) = UNSET
      IF( VALUE(I2) .EQ. UNSET )  STOP(ISCN)  = UNSET
      PRESCAN(ISCN) = VALUE( KEYPTR( 'PRESCAN', KC, KI ) ) / 86400.D0
      GAP(ISCN) = VALUE( KEYPTR( 'GAP', KC, KI ) ) / 86400.D0
C
C     Get MJD1 based just on the day.  There are lots of ways to 
C     specify the sub-day timing and that will only be straightened
C     out later.  We need MJD1 now because the stations catalog is
C     about to be read.  Being off by a day is no big deal as 
C     typically observations at the time of the shift are unlikely, or
C     you don't want to buck them out.
C
      IF( ISCN .EQ. 1 ) THEN
         IF( DAY(ISCN) .GT. 366 ) THEN
C
C           Deal with a date specified as an LST day.  This is
C           approximate because we will not be using actual start
C           times and we don't know a longitude.  Assume 0 for
C           unknown information.
C
            CALL LST2UT( 0.D0, DAY(ISCN), 0.D0, MYEAR, MDAY, MUT )
            CALL SLA_CLDJ( MYEAR, 1, MDAY, MJD1, JERR )
            IF( JERR .NE. 0 .AND. JERR .NE. 3 ) 
     1          CALL ERRLOG( 'GETTIM: Problem converting day to MJD' )
C            
         ELSE
C
C           Deal with a traditional day/year.
C
            CALL SLA_CLDJ( YEAR(ISCN), 1, DAY(ISCN), MJD1, JERR )
            IF( JERR .NE. 0 .AND. JERR .NE. 3 ) 
     1         CALL ERRLOG( 'GETTIM: Problem getting MJD' //
     2             ' of first scan before station catalog read.' )
         END IF
      END IF
C
C     Process the duration/dwell request.  Here the input has been
C     reset before the scan so that we can detect any changes.
C     However DWELL(2) (will go to NOWAIT) has not been reset.
C
      TDUR   = VALUE( KEYPTR( 'DURation', KC, KI ) )
      TDWELL = VALUE( KEYPTR( 'DWELL', KC, KI ) )
C
      TNOWAIT = VALUE( KEYPTR( 'DWELL', KC, KI ) + 1 )
      TMINDW  = VALUE( KEYPTR( 'DWELL', KC, KI ) + 2 ) / 86400.D0
C
      IF( TDWELL .NE. UNSET .AND. TDUR .NE. UNSET ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I5 )' ) 
     1     'GETTIM: Don''t specify both DUR and DWELL!  Input scan: ',
     2     ISCN
         CALL ERRLOG( MSGTXT )
      ELSE IF( TDWELL .NE. UNSET ) THEN
         DWELL(ISCN) = .TRUE.
         DUR(ISCN) = TDWELL / 86400.D0
         NOWAIT(ISCN) = TNOWAIT
         MINDW(ISCN) = TMINDW
      ELSE IF( TDUR .NE. UNSET ) THEN
         DWELL(ISCN) = .FALSE.
         DUR(ISCN) = TDUR / 86400.D0
         NOWAIT(ISCN) = 0
         MINDW(ISCN) = 0.D0
      ELSE
         IF( ISCN .LE. 1 ) THEN
            DWELL(ISCN) = .FALSE.
            DUR(ISCN) = 0.D0
            NOWAIT(ISCN) = 0
            MINDW(ISCN) = 0.D0
         ELSE
            DWELL(ISCN) = DWELL(ISCN-1)
            DUR(ISCN) = DUR(ISCN-1)
            NOWAIT(ISCN) = NOWAIT(ISCN-1)
            MINDW(ISCN) = MINDW(ISCN-1) 
         END IF
      END IF
      DWELLS = DWELLS .OR. DWELL(ISCN)
C
      RETURN
      END
