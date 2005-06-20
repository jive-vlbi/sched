      SUBROUTINE VLBADK( ISCN, ISTA, SETSC )
C
C     Routine for SCHED to write disk commands into telescope 
C     schedule.  This is pretty simple so far.  Let's hope it 
C     stays that way, but put it here to keep it isolated.
C
      INCLUDE  'sched.inc'
      INTEGER  ISCN, ISTA
      LOGICAL  SETSC
C ---------------------------------------------------------------------
C     Write disk=on if recording, else disk=off.
C
      IF( USEDISK(ISTA) ) THEN
         IF( NOREC(ISCN) .OR. SETSC ) THEN
            WRITE( IUVBA, '( A )' ) 'disk=off'
         ELSE
            WRITE( IUVBA, '( A )' ) 'disk=on'
         END IF
      END IF
C
      RETURN
      END

