      INTEGER FUNCTION PSETI( ISCN, ISTA )
C
C     Get the pulse cal set to use for a scan/station.
C
      INCLUDE     'sched.inc'
      INCLUDE     'schset.inc'
C
      INTEGER       ISCN, ISTA, KF, KS, IPC
      CHARACTER     CPCAL*4
C ----------------------------------------------------------------------
C     First get the frequency set to use for this scan/station.
C
      KF = FSETI( ISCN, ISTA )
      KS = FSETKS(KF)
C
C     Get the pcal setting for this scan/station.
C
      IF( PCAL(ISCN) .EQ. ' ' ) THEN 
         CPCAL = SPCAL(KS)
      ELSE 
         CPCAL = PCAL(ISCN)
      END IF
      CALL UPCASE( CPCAL )
C
      IF( CPCAL .EQ. 'OFF' ) THEN
         IPC = 1
      ELSE IF( CPCAL .EQ. '1MHZ' ) THEN
         IPC = 2
      ELSE IF( CPCAL .EQ. '5MHZ' ) THEN
         IPC = 3
      ELSE
         CALL ERRLOG( 'PSETI: Programming problem with PCAL' // CPCAL )
      END IF
C
      PSETI = FSETPS(IPC,KF)
C
      IF( PSETI .LT. 1 .OR. PSETI .GT. NPSET ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( 2I5, 2A, 2I5 )' ) NPSET, IPC, ' ', 
     1       CPCAL, KF, KS
         CALL WLOG( 1, MSGTXT )
         WRITE( MSGTXT, '( A, I4 )' ) 
     1      'PSETI: Programming problem with getting pcal set', PSETI
         CALL ERRLOG( MSGTXT )
      END IF
C
      RETURN
      END
