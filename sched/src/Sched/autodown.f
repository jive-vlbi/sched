      SUBROUTINE AUTODOWN( ISCN )
C
C     Subroutine for SCHED that removes any stations using 
C     disk recording from a scan in which the 
C     source is not up. This helps save media, especially at MK, 
C     and can prevent excessive trips to the site by the site tech.
C     It also helps Mark5 stations avoid slews to unnecessary scans.
C
      INCLUDE   'sched.inc'
C
      INTEGER   ISCN, ISTA
      LOGICAL   DOWNWARN
      DATA      DOWNWARN   / .TRUE. /
      SAVE      DOWNWARN
C ---------------------------------------------------------------------
      DO ISTA = 1, NSTA
         IF( STASCN(ISCN,ISTA) ) THEN
            IF( .NOT. DODOWN .AND. ( USEDISK(ISTA) .AND.
     1           ( UP1(ISCN,ISTA) .EQ. 'D' .AND.
     2             UP2(ISCN,ISTA) .EQ. 'D' ) 
     3           ) ) THEN
               STASCN(ISCN,ISTA) = .FALSE.
               IF( DOWNWARN ) THEN
                  CALL WRTMSG( 0, 'AUTODOWN', 'scanremoval' )
                  DOWNWARN = .FALSE.
               END IF
            END IF
         END IF
      END DO
C
      RETURN
      END
