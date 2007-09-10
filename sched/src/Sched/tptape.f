      SUBROUTINE TPTAPE( ISCN, ISTA, LASTISCN )
C
C     Routine for SCHED called by SCHTAPE and TSYNC that deals with tape
C     handling.  Actually this routine just selects which tape
C     handling routine to call based on the recording system type.
C     The real work is done in the next layer of subroutines.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER  ISCN, ISTA, LASTISCN(MAXSTA)
C
C  --------------------------------------------------------------------
C     MarkII.
C
      IF( MARK2 ) THEN
         IF( STASCN(ISCN,ISTA) ) THEN
            CALL TPMK2( ISCN, ISTA, LASTISCN )
            LASTISCN(ISTA) = ISCN
         END IF
      END IF
C
C     VLBA and Mark IV and S2
C
      IF( VLBITP .AND. USETAPE(ISTA) ) THEN
C
         IF( RECORDER(STANUM(ISTA)) .EQ. 'S2' ) THEN
C
C           S2 recording system.
C
            CALL TPS2( ISCN, ISTA, LASTISCN )
C
         ELSE
C
C           VLBA or Mark IV recording system.
C
            CALL TPSCH( ISCN, ISTA, LASTISCN )
C
         END IF
C
      ELSE
C
C        Not a tape handling experiment.  Set defaults for the
C        tape information.  Probably won't get here because of 
C        controls on the call.
C
         CALL TPPACK( 'PACK', TPDAT(1,ISCN,ISTA),
     1             .FALSE., .FALSE., .FALSE., 1, 1, 1, 1, 1 )
C
      END IF
C
      RETURN
      END
