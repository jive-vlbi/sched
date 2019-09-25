      SUBROUTINE OPTHIEL( LASTISCN, KSCN, ISCN, ADJUST, KEEP, DONE )
Cf2py intent(in) LASTISCN, ISCN
Cf2py intent(in, out) KSCN
Cf2py intent(out) ADJUST, KEEP, DONE
C
C     Routine for optimization mode of sched that decides what to
C     do with the next input scan.  It is used for OPTMODE=HIGHEL
C     It will construct a new scan list (ISCN > NSCANS) using a 
C     simple scheme.  Each time it encounters a scan with the
C     parameter HIGROUP set greater than 1, it will consider the
C     next HIGROUP scans and select the one with the highest
C     minimum elevation across the array.  It will only use that
C     one scan of the group, and will push KSCN ahead so that the
C     next scan is the one beyond the group.
C
      INCLUDE 'sched.inc'
C
      INTEGER           LASTISCN(MAXSTA), KSCN, ISCN
      LOGICAL           KEEP, ADJUST, DONE
C
      INTEGER           NGOOD, NPRIO, SKIP(MAXSRC), ISTA, ISRC
      INTEGER           MISSED, JKSCN, JISCN, MSCN
      REAL              SCNLO, GRPMXLO, ELA
      DOUBLE PRECISION  TAPPROX, TBEGIN, T_AVAIL, LASTTIME
      LOGICAL           OKSTA(MAXSTA), USESTA
      SAVE              SKIP, MISSED
C ---------------------------------------------------------------------
      DONE = .FALSE.
C
C     Do not allow fixed times (too confusing).
C
      ADJUST = .TRUE.
C
C     KSCN is the count of input scans for this optimization mode.
C     Quit when all have been processed.
C
      IF( KSCN .GT. NSCANS ) THEN
         DONE = .TRUE.
         GO TO 999
      END IF
C
C     Make sure HIGROUP is at least 1.  Also be sure it is not
C     large enough to go past the input scans.
C
      HIGROUP(KSCN) = MAX( HIGROUP(KSCN), 1 )
      IF( KSCN + HIGROUP(KSCN) - 1 .GT. NSCANS ) THEN
         CALL ERRLOG( 'OPTHIEL: HIGROUP in last scan too large.' )
      END IF
C
C     Just copy the input to the output if HIGROUP(KSCN) = 1
C
      IF( HIGROUP(KSCN) .EQ. 1 ) THEN
         CALL SCNDUP( ISCN, KSCN, .TRUE., 'OPTHIEL' )
         GO TO 999
      END IF
C
C     Get the time to use for the geometric calculations.
C     Use the most recent stop time for any antenna scheduled to be 
C     in any of the HIGROUP scans.
C
C     Use the nominal start time if this is the first scan for
C     all participating stations.  Also, make sure if only some 
C     antennas are new, they don't set the start time.
C
      TAPPROX = TFIRST
      DO ISTA = 1, NSTA
         USESTA = .FALSE.
         DO JKSCN = KSCN, KSCN + HIGROUP(KSCN) - 1
            IF( STASCN(JKSCN,ISTA) ) USESTA = .TRUE.
         END DO
         IF( USESTA ) THEN
            IF( LASTISCN(ISTA) .NE. 0 ) THEN
               TAPPROX = MAX( TAPPROX, 
     1              STOPJ(LASTISCN(ISTA)) + GAP(ISCN) )
            END IF
         END IF
      END DO
C
C     Copy the next HIGROUP input scans to the output scans and
C     get their geometries. Ultimately all but one will be overwritten.
C
      GRPMXLO = -100.0
      DO JKSCN = KSCN, KSCN + HIGROUP(KSCN) - 1
         JISCN = ISCN + JKSCN - KSCN
         CALL SCNDUP( JISCN, JKSCN, .TRUE., 'OPTHIEL' )
         SCNLO = 100.0
         DO ISTA = 1, NSTA
            IF( STASCN(JISCN,ISTA) ) THEN
               CALL STAGEO( JISCN, ISTA, TAPPROX, LASTISCN(ISTA),  
     1          LASTTIME, T_AVAIL, 'OPTSKD' )
               ELA = ( EL1(JISCN,ISTA) + EL2(JISCN,ISTA) ) / 2.0
               SCNLO = MIN( SCNLO, ELA )
            END IF
         END DO
         IF( SCNLO .GT. GRPMXLO ) THEN
            GRPMXLO = SCNLO
            MSCN = JISCN            
         END IF
      END DO
C
C     So scan MSCN is the one with the highest minimum elevation.
C
C     Move it to output scan ISCN.
C
      CALL SCNDUP( ISCN, MSCN, .TRUE., 'OPTHIEL' )
C
C     Adjust KSCN so we don't look again at the scans just considered.
C     Should end up here being one less than the input scan we
C     wish to consider next.
C
      KSCN = KSCN + HIGROUP(KSCN) - 1
C
C     Jump here when done.
C
  999 CONTINUE
C
C     Set KEEP.  I don't think this routine should ever want a scan
C     not kept.
C
      KEEP = .TRUE.
      RETURN
      END
