      SUBROUTINE AUTODOWN( LASTISCN, ISCN, NGOOD )
Cf2py intent(in) LASTISCN, ISCN
Cf2py intent(out) NGOOD
C
C     Subroutine for SCHED that removes any stations using 
C     disk recording from a scan in which the 
C     source is not up. This helps save media, especially at MK, 
C     and can prevent excessive trips to the site by the site tech.
C     It also helps Mark5 stations avoid slews to unnecessary scans.
C
C     Recalculate NGOOD since that can be changed here.
C
      INCLUDE   'sched.inc'
C
      INTEGER   ISCN, ISTA, NGOOD, KSTA, LASTISCN(MAXSTA)
      INTEGER   DROPSTA(MAXSTA), IDROP, NDROP, NKEEP
      REAL      SLEWD
      LOGICAL   DOWNWARN, SLEWWARN
      DATA      DOWNWARN   / .TRUE. /
      DATA      SLEWWARN   / .TRUE. /
      SAVE      DOWNWARN, SLEWWARN
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'AUTODOWN starting. ' )
C
      NGOOD = 0
C
      NDROP = 0
      DO ISTA = 1, NSTA
C
C        Take out stations that are down.
C
         IF( STASCN(ISCN,ISTA) ) THEN
            IF( .NOT. DODOWN(ISCN) .AND. ( USEDISK(ISTA) .AND.
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
C
C        Now take out any station that just can't get there
C        before the end of the scan because of a slow slew.
C        
C        Make an exception for a wrap.  The Verify schedules 
C        have found several cases when phase referencing when
C        a wrap is needed for one well before the other.  
C        The antenna is left on the second source for several
C        scan pairs where, if it had just gone ahead, it could
C        get there, perhaps after missing the scan, but then
C        go back and forth at the new wrap.  So, do not take
C        the source out if it is an altaz antenna and the 
C        azimuth slew is more than 315 degrees (limit is
C        arbitrary, but probably ok).  Still go ahead and put
C        a W beside such scans.
C
C        Make another exception - don't take all stations out
C        of a scan because of the slew.  One of the examples 
C        had a lot of short phase referencing scans, scheduled
C        with duration, that followed a long slew for which the
C        duration was not adequate for any antenna to get there.
C        All of those short scans got sucked up against the long
C        slew and dropped.
C
         IF( STASCN(ISCN,ISTA) ) THEN
            IF( USEDISK(ISTA) .AND.
     1            TONSRC(ISCN,ISTA) .GE. STOPJ(ISCN) ) THEN
               IF( .NOT. DODOWN(ISCN) ) THEN
                  KSTA = STANUM(ISTA)
                  SLEWD = ABS( AZ1(ISCN,ISTA) - 
     1                         AZ2(LASTISCN(ISTA),ISTA) )
                  IF( MOUNT(KSTA) .NE. 'ALTAZ' .OR. 
     1                 SLEWD .LT. 315.0 ) THEN
                     STASCN(ISCN,ISTA) = .FALSE.
                     NDROP = NDROP + 1
                     DROPSTA(NDROP) = ISTA
                  END IF
               END IF
               
C
C              Change the up/down warning to a slew warning.
C              Keep these even if keeping the scan to indicate
C              it won't get there.
C
               UP1(ISCN,ISTA) = 'W'
               UP2(ISCN,ISTA) = 'W'
               IF( SLEWWARN ) THEN
                  CALL WRTMSG( 0, 'AUTODOWN', 'slewremoval' )
                  SLEWWARN = .FALSE.
               END IF
C
            END IF
         END IF
C
      END DO
C
C     See how many stations got kept.
C
      NKEEP = 0
      DO ISTA = 1, NSTA
         IF( STASCN(ISCN,ISTA) ) THEN
            NKEEP = NKEEP + 1
         END IF
      END DO
C
C     If NKEEP is zero, and some were dropped only because of 
C     slew, put them back to avoid never getting to a DUR scheduled
C     phase referencing sequence.
C
      IF( NKEEP .EQ. 0 .AND. NDROP .GE. 1 ) THEN
         DO IDROP = 1, NDROP
            STASCN(ISCN,DROPSTA(IDROP)) = .TRUE.
         END DO
      END IF      
C
C     Get NGOOD after the above adjustments.
C
      DO ISTA = 1, NSTA
         IF( STASCN(ISCN,ISTA) .AND.
     1       ( EL1(ISCN,ISTA) .GE. OPMINEL(ISCN) .AND. 
     2         EL2(ISCN,ISTA) .GE. OPMINEL(ISCN) .AND.
     3         UP1(ISCN,ISTA) .EQ. ' ' .AND. 
     4         UP2(ISCN,ISTA) .EQ. ' ' ) ) THEN
             NGOOD = NGOOD + 1
         END IF
      END DO
C
      RETURN
      END
