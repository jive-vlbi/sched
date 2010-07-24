      SUBROUTINE VLBAEND( ISTA, LASTDY, LSCN )
C
C     Routine for SCHED called by VLBA to finish off the VLBA control
C     file.  This was much more complicated in the tape days when 
C     postpasses were an issue.
C
C     Note that we are depending on TPCDRIV retaining the value
C     it had at the end of the last valid scan.  Hence its presence
C     among the SAVEd variables in routine VLBA.
C
      INCLUDE  'sched.inc'
C
      INTEGER           LASTDY, LSCN, ISTA
      CHARACTER         TSTOP*9, VLBASTOP*9
      DOUBLE PRECISION  OFFSET
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'VLBAEND: Starting.' )
C
C     Deal with wrapping up the recordings if any were being made.
C
      IF( VLBITP ) THEN
C
C        Turn off the disk, but let tape determine the other details.
C
         IF( USEDISK(ISTA) ) THEN
            WRITE( IUVBA, '( A )' ) 'disk=off'
         END IF
C
C        Do a final short recording off scan then quit.  This final 
C        scan is probably not needed, but was there from the tape
C        days and I won't perturb things.  Keep the 5 seconds total
C        that was used before for now.
C
         OFFSET = 5.D0
         TSTOP = VLBASTOP( STOPJ(LSCN), OFFSET, LASTDY, TWOPI, IUVBA )
         WRITE( IUVBA, '( A, A9, A )' )
     1       'stop=', TSTOP, '   !NEXT!'
C
      END IF
C
C     Finally really quit the program for all cases.
C
      WRITE( IUVBA, '(A)' ) '     !QUIT! '
C
      RETURN
      END
