      SUBROUTINE OPTNONE( KSCN, ISCN, ADJUST, KEEP, DONE )
C
C     Routine for sched routine schopt that picks next scan in the
C     case of no optimization.  This just returns the current input
C     scan with appropriate flags set.  It copies it to a new slot
C     if requested, which might be needed when inserting peaking scans.
C
C     In the case where some scans have dwell time scheduling, the 
C     fixed scans will use this routine and the dwell scans will 
C     use OPTSKD.
C
      INCLUDE 'sched.inc'
C
      INTEGER    ISCN, KSCN
      LOGICAL    ADJUST, KEEP, DONE
C ---------------------------------------------------------------------
      IF( DEBUG .AND. KSCN .LE. 3 ) CALL WLOG( 0, 'OPTNONE: Starting.' )
C
C     Set output flags so no adjustments will be made later.
C
      ADJUST = .FALSE.
      KEEP   = .TRUE.
      DONE   = KSCN .GT. NSCANS
      IF( .NOT. DONE ) THEN
C
C        Copy the input scan to the output if necessary.
C
         IF( KSCN .NE. ISCN ) THEN
            CALL SCNDUP( ISCN, KSCN, .TRUE. )
         END IF
C
      END IF
C
      RETURN
      END

