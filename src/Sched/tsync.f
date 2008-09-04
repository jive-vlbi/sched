      SUBROUTINE TSYNC( ISCN, LASTISCN, TAPCHG, DOINGCHG )
C
C     Obsolete - only called if tapes in use.
C
C     Routine for SCHED, called by SCHOPT, that deals with situations
C     when tape handling must be synchronized between stations.  This
C     does not match well with the model mostly used by SCHED where
C     the tape handling can be independent at each station.
C
      INCLUDE   'sched.inc'
C
      INTEGER   ISCN, ISTA, LASTISCN(MAXSTA)
      LOGICAL   TAPCHG, DOINGCHG, DOCHG(MAXSTA)
      SAVE      DOCHG
C ----------------------------------------------------------------------
C
C     Synchronize tape changes if requested.  With
C     subarrays, this may have to be done over a few
C     scans. 
C     
C     Note that the following code will do strange things
C     when the scans are not in close to time order.
C     
      IF( TAPESYNC .AND. .NOT. NOSET ) THEN
C     
C        If any station had a tape change, and we are not
C        in the middle of a previously triggered change,
C        flag all stations to do a change asap.
C     
         IF( TAPCHG .AND. .NOT. DOINGCHG ) THEN
            DOINGCHG = .TRUE.
            DO ISTA = 1, NSTA
               DOCHG(ISTA) = .TRUE.
            END DO
         END IF
C     
C        If this scan is one for which a tape change is
C        needed, and this station is in the scan and is
C        not already slated for a change, 
C        set TAPE and rerun TPTAPE.
C     
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) .AND. DOCHG(ISTA) ) THEN
               IF( MOD( TPDAT(1,ISCN,ISTA), 10 ) .EQ. 0 ) THEN
                  TAPE(ISCN,ISTA) = .TRUE.
                  CALL TPTAPE( ISCN, ISTA, LASTISCN )
               END IF
               DOCHG(ISTA) = .FALSE.
            END IF
         END DO                   
C     
C        If all stations have done the change, turn off
C        the flag that indicates that the change is in 
C        progress.
C     
         DOINGCHG = .FALSE.
         DO ISTA = 1, NSTA
            IF( DOCHG(ISTA) ) DOINGCHG = .TRUE.
         END DO
C     
      END IF
C
C
      RETURN
      END


