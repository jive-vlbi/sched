      SUBROUTINE CHKFIRM
C
C     Routine called by DEFSET that checks that the firmware
C     specified by DBE in the setup file and the DAR
C     specified in the station catalog are compatible. 
C     Set DBE defaults if needed.
C
C     Might some FORMAT chanceks go here too?
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER   KS, KSTA, LEN1
      LOGICAL   GOTERR
C ----------------------------------------------------------------
C
C     Loop over the setup files.  Now that there is one per station,
C     this process should be straight foreward.
C
      GOTERR = .FALSE.
      DO KS = 1, NSET
         KSTA = ISETSTA(KS)
         IF( DAR(KSTA) .EQ. 'RDBE' ) THEN
C
            IF( DBE(KS) .EQ. ' ' ) THEN
               DBE(KS) = 'RDBE_PFB'
            ELSE IF( DBE(KS) .NE. 'RDBE_PFB' .AND.
     1               DBE(KS) .NE. 'RDBE_DDC' ) THEN
               GOTERR = .TRUE.
            END IF
         ELSE IF( DAR(KSTA) .EQ. 'RDBE2' ) THEN
C
            IF( DBE(KS) .EQ. ' ' ) THEN
               DBE(KS) = 'RDBE_DDC'
            ELSE IF( DBE(KS) .NE. 'RDBE_DDC' ) THEN
               GOTERR = .TRUE.
            END IF
         ELSE IF( DAR(KSTA) .EQ. 'DBBC' ) THEN
C           CR: DBBC defaults to DDC personality (PFB will probably
C           never be used)
            IF( DBE(KS) .EQ. ' ' ) THEN
               DBE(KS) = 'DBBC_DDC'
            ELSE IF( DBE(KS) .NE. 'DBBC_PFB' .AND. 
     1               DBE(KS) .NE. 'DBBC_DDC' ) THEN
               GOTERR = .TRUE.
            END IF
         ELSE IF( DAR(KSTA) .EQ. 'WIDAR' ) THEN
            IF( DBE(KS) .EQ. ' ' ) THEN
               DBE(KS) = 'WIDAR'
            ELSE IF( DBE(KS) .NE. 'WIDAR' ) THEN
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
            WRITE( MSGTXT, '( 6A )' ) 'CHKFIRM:  Invalid DBE (',
     1         DBE(KS)(1:LEN1(DBE(KS))),
     2         ') specified for DAR = ', 
     3         DAR(KSTA)(1:LEN1(DAR(KSTA))), 
     4         ' at station ', 
     5         SETSTA(1,KS)(1:LEN1(SETSTA(1,KS)))
            CALL ERRLOG( MSGTXT )
         END IF
      END DO
      RETURN
      END
