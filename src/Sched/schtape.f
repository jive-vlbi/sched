      SUBROUTINE SCHTAPE( ISCN, LASTISCN )
C
C     Obsolete - only called if tapes in use.
C
C     Subroutine for SCHED, called by SCHOPT, that deals with tape
C     handling issues for a scan.
C
      INCLUDE      'sched.inc'
C
      INTEGER      ISCN, LASTISCN(MAXSTA), ISTA
      LOGICAL      DOINGCHG, TAPCHG, BEGSCN, USEWARN
      SAVE         DOINGCHG, BEGSCN, USEWARN
      DATA         DOINGCHG  / .FALSE. /
      DATA         BEGSCN / .TRUE. /
      DATA         USEWARN / .TRUE. /
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
C     This routine and everything it calls should eventually be removed.
C     Meanwhile, get a warning if anyone uses it.
C
      IF( USEWARN ) THEN
         CALL WLOG(1,'Subroutine SCHTAPE: This part of SCHED will be')
         CALL WLOG( 1, '   removed some day.  Please report to Craig' )
         CALL WLOG( 1, '   Walker if you see this message.' )
         USEWARN = .FALSE.
      END IF
C
C ----------------------------------------------------------------------
C ----------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LT. SCAN1 + 5 ) 
     1      CALL WLOG( 0, 'SCHTAPE starting' )
C
      IF( BEGSCN ) THEN
         DOINGCHG = .FALSE.
         BEGSCN = .FALSE.
      END IF
C
      TAPCHG = .FALSE.
      DO ISTA = 1, NSTA
         IF( STASCN(ISCN,ISTA) ) THEN
C     
            IF( .NOT. NOSET .AND. ( USETAPE(ISTA) .OR. MARK2 ) ) THEN
C     
C              Get the tape information for VLBA and Mark III.
C     
               CALL TPTAPE( ISCN, ISTA, LASTISCN )
C     
C              Sense if there is a tape change.
C     
               IF( USETAPE(ISTA) .AND.
     1             MOD( TPDAT(1,ISCN,ISTA), 10 ) .GT. 0 .AND.
     2             .NOT. DOINGCHG ) TAPCHG = .TRUE.
C
            END IF
         END IF
      END DO
C     
C     Deal with situations where tape actions must be
C     coordinated between stations.  For now, this is just
C     tape changes.
C     
      CALL TSYNC( ISCN, LASTISCN, TAPCHG, DOINGCHG )
C
      RETURN
      END

