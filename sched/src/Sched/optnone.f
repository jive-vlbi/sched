      SUBROUTINE OPTNONE( KSCN, ISCN, ADJUST, KEEP, DONE )
C
C     Routine for sched routine schopt that picks next scan in the
C     case of no optimization.  This just returns the current input
C     scan with appropriate flags set.  It copies it to a new slot
C     if requested, which might be needed when inserting peaking scans.
C
C     Dwell scans used to be handled in OPTDWELL, but that was mostly
C     redundant with OPTTIM that is called later, so was removed.
C     In the process, the setting of ADJUST got a little more 
C     complicated here.
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
      KEEP   = .TRUE.
      DONE   = KSCN .GT. NSCANS
      IF( .NOT. DONE ) THEN
C
C        Copy the input scan to the output if necessary.
C
         IF( KSCN .NE. ISCN ) THEN
            CALL SCNDUP( ISCN, KSCN, .TRUE., 'OPTNONE' )
         END IF
C
      END IF
C
C     Set whether to allow start time adjustments.  Note that, if
C     stop times only were specified (DURONLY(ISCN)=4), OPTTIM will not
C     adjust the stop time regardless of the setting of ADJUST.
C
      ADJUST = ( DWELLS .AND. ISCN .GT. SCAN1 ) .AND. 
     1         ( DURONLY(ISCN) .EQ. 1 .OR. DURONLY(ISCN) .EQ. 4 ) 
C
      RETURN
      END

