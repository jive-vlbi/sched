      SUBROUTINE OPTNONE( KSCN, ISCN, ADJUST, KEEP, DONE )
Cf2py intent(in) KSCN, ISCN
Cf2py intent(out) ADJUST, KEEP, DONE
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



C     Some notes about my thinking dealing with the Feb. 2014 GMVA run.

C     Nasty case:  There are DWELL scans in the project so ADJUST gets
C     set to true.  But there are START times on later scans, followed
C     by only DUR scheduling.  So DWELL should be false.  If there
C     are inserted pointing scans, GAP gets set to zero.
C
C     Does gap apply to the gap from the inserted scan or from the previous
C     recording scan?  It depends on what you are trying to use GAP for.
C
C     Do I need OPTNONE to be scan specific about ADJUST, being sensitive to
C     the history since the last START or STOP?
C
C     In the end, I added ORIGEN and LASTSSCN to deal with these situations.
C     I'm not all that confident of the results yet.
