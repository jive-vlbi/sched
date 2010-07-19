      SUBROUTINE VLBADK( ISCN, ISTA, SETSC )
C
C     Routine for SCHED to write disk commands into telescope 
C     schedule.  This is pretty simple so far.  Let's hope it 
C     stays that way, but put it here to keep it isolated.
C
      INCLUDE  'sched.inc'
      INCLUDE   'schset.inc'
      INTEGER  ISCN, ISTA
      LOGICAL  SETSC
C ---------------------------------------------------------------------
C     Write disk=on if recording, else disk=off.  Assume FORMAT=NONE
C     means not recording.  That might happen without NOREC for 
C     RDBE schedules where the recording is happening outside the 
C     control of the crd files.
C
      IF( USEDISK(ISTA) ) THEN
         IF( NOREC(ISCN) .OR. SETSC .OR. FORMAT(LS) .EQ. 'NONE' ) THEN
            WRITE( IUVBA, '( A )' ) 'disk=off'
         ELSE
            WRITE( IUVBA, '( A )' ) 'disk=on'
         END IF
      END IF
C
      RETURN
      END

