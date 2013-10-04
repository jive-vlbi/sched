      SUBROUTINE VLBADK( ISCN, ISTA, SETSC )
C
C     Routine for SCHED to write disk commands into telescope 
C     schedule.  This is pretty simple so far.  Let's hope it 
C     stays that way, but put it here to keep it isolated.
C
C     SETSC indicates this is a setup scan.  It is hardwired in
C     the calling routine vlba.f in each of the 2 calls.
C
      INCLUDE  'sched.inc'
      INCLUDE   'schset.inc'
      INTEGER  ISCN, ISTA, KS
      LOGICAL  SETSC
C ---------------------------------------------------------------------
C     Write disk=on if recording, else disk=off.  Assume FORMAT=NONE
C     means not recording.  That might happen without NOREC for 
C     RDBE schedules where the recording is happening outside the 
C     control of the crd files.
C
C
      KS = NSETUP(ISCN,ISTA)
      IF( USEDISK(ISTA) ) THEN
         IF( NOREC(ISCN) .OR. SETSC .OR. FORMAT(KS) .EQ. 'NONE' .OR.
     1       ( DAR(STANUM(ISTA))(1:4) .EQ. 'RDBE' .AND. 
     2       .NOT. DOMKA ) ) THEN
            WRITE( IUVBA, '( A )' ) 'disk=off'
         ELSE
            WRITE( IUVBA, '( A )' ) 'disk=on'
         END IF
      END IF
C
      RETURN
      END

