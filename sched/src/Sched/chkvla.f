      SUBROUTINE CHKVLA( KS, ERRS )
C
C     Subroutine for SCHED called by CHKSET that checks some
C     VLA specific setup file parameters.  It also checks some
C     VLA specific scan parameters.
C
C     If ERRS is returned as TRUE, CHKSET will give the setup
C     file name and then crash the program.
C
C     The VLA routines are getting major surgery for the switch from
C     the old VLA system to the JVLA.  The VLA now uses VEX for 
C     control and WIDAR for the DAR.  There should not be much that
C     is different from the other VEX stations, but keep these 
C     routines available in case some are found.  Process started
C     May 9, 2012 RCW.  For the old code, look for versions of 
C     SCHED 10.1 or earlier.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER     KS
      LOGICAL     ERRS
C  --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKVLA: Starting.' )
C
C     It is not wise to make VLA schedules longer than 24 hours.
C     There is no day number on the scans so, if there is a restart,
C     they can end up on the wrong day.
C
C     Still current for JVLA?  Leave it just in case.
C
      IF( TEND - TFIRST .GT. 1.D0 ) THEN
         CALL WRTMSG( 0, 'CHKVLA', 'vlaover24' )
      END IF
C
C     Check VLARFANT.  Not sure if this will do anything for JVLA.
C
      IF( VLARFANT .LT. 1 .OR. VLARFANT .GT. 28 ) THEN
         SETMSG = ' '
         WRITE( SETMSG, '( A, I7, A )' ) 'CHKVLA: VLARFANT ', 
     1      VLARFANT, ' out of range 1 to 28.'
         CALL WLOG( 1, SETMSG )
         ERRS = .TRUE.
      END IF
C
      RETURN
      END
