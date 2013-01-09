      SUBROUTINE INVLA( CASE, ISCN, VALUE, KC, KI )
C
C     Routine for SCHED called by SCHIN to get VLA information from
C     the main schedule.
C
C     Case 1 is the call from the scan loop.
C     Case 2 is after all scans are in to get experiment wide parms.
C
C     Jump through a few hoops to use toggle on a logical that does
C     not have an entry for each scan.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      DOUBLE PRECISION  VALUE(*)
      INTEGER           CASE, ISCN, KI(*), KEYPTR, NGET, NVLA, ISTA
      LOGICAL           WARN, VNTSY(2)
      CHARACTER         KC(*)*(*), KCHAR*256, INBW*4, INBND*2
      SAVE              WARN, VNTSY
      DATA              WARN  / .TRUE. /
C -------------------------------------------------------------------
      IF( DEBUG .AND. ( ISCN .LT. 3 .OR. CASE .NE. 1 ) ) THEN
         WRITE( MSGTXT, '( A, I3 )' ) 
     1       'INVLA: Starting with case: ', CASE
         CALL WLOG( 0, MSGTXT )
      END IF
C
      IF( CASE .EQ. 1 ) THEN
C
C        Reading each scan.
C
C        The observing mode.
C
         VLAMODE(ISCN) = KCHAR( 'VLAMODE', 2, .TRUE., VALUE, KC, KI )
C
C        On-line Tsys corrections.  TOGGLE only uses the current
C        and previous elements of the logical input array.  Use the
C        two elements of VNTSY to hold these two and keep rolling them
C        down.  The default should be to make the corrections.
C
         IF( ISCN .EQ. 1 ) THEN
            NGET = 1
         ELSE IF( ISCN .EQ. 2 ) THEN
            NGET = 2
         ELSE
            VNTSY(1) = VNTSY(2)
            NGET = 2
         END IF
         CALL TOGGLE( VNTSY, NGET, 'VLANTSYS', 'VLATSYS', UNSET,
     1                VALUE, KC, KI )
         IF( VNTSY(NGET) ) THEN
            VLATSYS(ISCN) = 'T'
         ELSE
            VLATSYS(ISCN) = ' '
         END IF
C
C        Pointing.
C
         VLAPEAK(ISCN) = KCHAR( 'VLAPEAK', 9, .TRUE., VALUE, KC, KI )
C
C        VLA phasing source with its default.
C
         VLAPHS(ISCN) = KCHAR( 'VLAPSRC', 12, .TRUE., VALUE, KC, KI )
         IF( VLAPHS(ISCN)(1:1) .EQ. ' ' ) VLAPHS(ISCN) = SCNSRC(ISCN)
C
C        VLABAND and VLABW have been moved to the setup files.  
C        For partial backward compatability, allow them to be in the
C        schedule, but warn that they are needed in the setups.
C
         INBW = KCHAR( 'VLABW', 4, .FALSE., VALUE, KC, KI )
         INBND = KCHAR( 'VLABAND', 2, .TRUE., VALUE, KC, KI )
         IF( WARN .AND. ( INBW .NE. 'ZZZZ' .OR. INBND .NE. 'ZZ' ) )
     1       THEN
            CALL WLOG( 1, 'INVLA: -- WARNING -- VLABW or VLABAND ' //
     1           ' specified in main schedule.' )
            CALL WLOG( 1, 'INVLA:               They will be ignored.' )
            CALL WLOG( 1, 'INVLA:               Specify them in the ' //
     1           'setup files.' )
            WARN = .FALSE.
         END IF
C
C        Get the integration time.  Can speed phasing.
C
         VLAINTEG(ISCN) = VALUE( KEYPTR( 'VLAINTEG', KC, KI ) )
C
C
C        Get the time for EVLA phasing subscans.  This is a new EVLA
C        parameter not used for the old VLA.
C
         VLAPTIME(ISCN) = VALUE( KEYPTR( 'VLAPTIME', KC, KI ) )
C
      ELSE IF( CASE .EQ. 2 ) THEN
C
C        Schedule wide parameters.
C
         VLATYPE  = KCHAR( 'VLATYPE', 9, .TRUE., VALUE, KC, KI )
         VLAUSERN = VALUE( KEYPTR( 'VLAUSERN', KC, KI ) ) 
         IATUTC   = VALUE( KEYPTR( 'IATUTC', KC, KI ) )
C
C        Check that there aren't more than one VLA station for 
C        regular scheduling.
C
         IF( .NOT. NOSET ) THEN
            NVLA = 0
            DO ISTA = 1, NSTA
               IF( STANAME(ISTA)(1:3) .EQ. 'VLA' ) NVLA = NVLA + 1
            END DO
            IF( NVLA .GE. 2 ) CALL WRTMSG( 1, 'INVLA', 'multipleVLA' )
         END IF
      ELSE
         CALL ERRLOG( 'INVLA: Bad case.' )
      END IF
C
C     Get the reference antenna.
C
      VLARFANT = VALUE( KEYPTR( 'VLARFANT', KC, KI ) )
C
      RETURN
      END
