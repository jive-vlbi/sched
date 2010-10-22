      SUBROUTINE GETTIM( ISCN, VALUE, KC, KI, START, STOP, DAY, YEAR )
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
      INCLUDE 'sched.inc'
C
      INTEGER           ISCN, KI(*), MONTH, DOY, YR, JERR
      INTEGER           I1, I2, KEYPTR
      INTEGER           YEAR(*), DAY(*)
      DOUBLE PRECISION  STOP(*), START(*)
      DOUBLE PRECISION  VALUE(*), TDWELL, TDUR
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
C     Process the duration/dwell request.  Here the input has been
C     reset before the scan so that we can detect any changes.
C     However DWELL(2) (will go to NOWAIT) has not been reset.
C
      TDUR   = VALUE( KEYPTR( 'DURation', KC, KI ) )
      TDWELL = VALUE( KEYPTR( 'DWELL', KC, KI ) )
      NOWAIT(ISCN) = VALUE( KEYPTR( 'DWELL', KC, KI ) + 1 )
      IF( TDWELL .NE. UNSET .AND. TDUR .NE. UNSET ) THEN
         CALL ERRLOG( 'GETTIM: Don''t specify both DUR and DWELL! ' )
      ELSE IF( TDWELL .NE. UNSET ) THEN
         DWELL(ISCN) = .TRUE.
         DUR(ISCN) = TDWELL / 86400.D0
      ELSE IF( TDUR .NE. UNSET ) THEN
         DWELL(ISCN) = .FALSE.
         DUR(ISCN) = TDUR / 86400.D0
      ELSE
         IF( ISCN .LE. 1 ) THEN
            DWELL(ISCN) = .FALSE.
            DUR(ISCN) = 0.D0
         ELSE
            DWELL(ISCN) = DWELL(ISCN-1)
            DUR(ISCN) = DUR(ISCN-1)
         END IF
      END IF
      DWELLS = DWELLS .OR. DWELL(ISCN)
C
      RETURN
      END
