      SUBROUTINE CHKFIRM
C
C     Routine called by DEFSET that checks that the firmware
C     specified by DBE in the setup file and the DAR
C     specified in the station catalog are compatible. 
C
C     Might some FORMAT checks go here too?
C
C     This routine used to set the DBE if needed.  But that violates
C     the overall SCHED logic of setting everything possible before 
C     making the checks.  So I split that out to SETFIRM on Oct. 
C     31, 2013.  RCW.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER   KS, KSTA, LEN1
      LOGICAL   GOTERR
C ----------------------------------------------------------------
C
C     Loop over the setup files.
C
      GOTERR = .FALSE.
      DO KS = 1, NSET
         KSTA = ISETSTA(KS)
C
C        Note that all VLBA stations (all RDBE stations?) will be
C        RDBE2.  Somewhere else, protect against using the PFB with
C        more than one RDBE (the fixed output bit rate doesn't allow
C        that.
C
         IF( DAR(KSTA)(1:4) .EQ. 'RDBE' ) THEN
            IF( DBE(KS) .NE. 'RDBE_PFB' .AND.
     1          ( DBE(KS) .NE. 'RDBE_DDC' .AND. NCHAN(KS) .LE. 8 )) THEN
               GOTERR = .TRUE.
            END IF
C
         ELSE IF( DAR(KSTA) .EQ. 'DBBC' ) THEN
            IF( DBE(KS) .NE. 'DBBC_PFB' .AND. 
     1          DBE(KS) .NE. 'DBBC_DDC' ) THEN
               GOTERR = .TRUE.
            END IF
         ELSE IF( DAR(KSTA) .EQ. 'WIDAR' ) THEN
            IF( DBE(KS) .NE. 'WIDAR' ) THEN
               GOTERR = .TRUE.
            END IF
         ELSE
            IF( DBE(KS) .NE. ' ' ) THEN
               GOTERR = .TRUE.
            END IF
         END IF
C
C        Write the error message and die.
C
         IF( GOTERR ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( 5A, I4, 2A )' ) 'CHKFIRM:  Invalid DBE (',
     1         DBE(KS)(1:LEN1(DBE(KS))),
     2         ') specified for DAR = ', 
     3         DAR(KSTA)(1:LEN1(DAR(KSTA))), 
     4         ' and NCHAN=', NCHAN(KS), ' at station ', 
     5         SETSTA(1,KS)(1:LEN1(SETSTA(1,KS)))
            CALL WLOG( 1, MSGTXT )
            MSGTXT = ' '
            MSGTXT = 
     1         '          Are you using the right station catalog?' 
            CALL ERRLOG( MSGTXT )
         END IF
      END DO
      RETURN
      END
