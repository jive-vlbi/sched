      SUBROUTINE SCHTAPE( ISCN, LASTISCN )
C
C     Subroutine for SCHED, called by SCHOPT, that deals with tape
C     handling issues for a scan.
C
      INCLUDE      'sched.inc'
C
      INTEGER      ISCN, LASTISCN(MAXSTA), ISTA
      LOGICAL      DOINGCHG, TAPCHG, BEGSCN
      SAVE         DOINGCHG, BEGSCN
      DATA         DOINGCHG  / .FALSE. /
      DATA         BEGSCN / .TRUE. /
C ----------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LT. SCAN1 + 5 ) 
     1      CALL WLOG( 0, 'SCHTAPE starting' )
C
      IF( BEGSCN ) THEN
         DOINGCHG = .FALSE.
         BEGSCN = .FALSE.
      END IF
C
C     Set TPSTART for all stations.  Done here so stations
C     can know about each other.  If DOVEX, don't adjust
C     later.
C     
      CALL SETTPS( ISCN, LASTISCN )
C     
      TAPCHG = .FALSE.
      DO ISTA = 1, NSTA
         IF( STASCN(ISCN,ISTA) ) THEN
C     
            IF( .NOT. NOSET ) THEN
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
C              Get the disk information.
C
               IF( VLBITP .AND. USEDISK(ISTA) ) THEN
                  CALL DISKPOS( ISCN, ISTA, LASTISCN )
               END IF
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

