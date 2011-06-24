      SUBROUTINE SCHOPEN( INSCH, IUSCH, KD, KC, KI )
C
C     Routine that opens a KEYIN input file after use of the SCHEDULE
C     input parameter.
C
C     This will often be associated with the use of RESTART in the
C     plotting section.
C
C     In all cases, this will not be the very first read (that got
C     SCHEDULE) so the input KEYS should already be established.
C
      DOUBLE PRECISION  KD(*)
      CHARACTER         KC(*)*(*), SCHEDULE*80, KCHAR*256
      CHARACTER         RESULT*256
      INTEGER           KI(*), INSCH, IER, VLBOPE, IUSCH
C --------------------------------------------------------------------
      SCHEDULE = KCHAR( 'SCHedule', 80, .FALSE., KD, KC, KI )
C
      IF( SCHEDULE .NE. ' ' ) THEN
         INSCH = IUSCH
         IER = VLBOPE( INSCH, SCHEDULE, 'TEXT', 'OLD', RESULT )
         IF( IER .NE. 1 ) THEN
            CALL WLOG( 1, RESULT )
            CALL ERRLOG( 'SCHFILES: Problem opening schedule file '//
     1                SCHEDULE )
         END IF
      END IF
C
      RETURN
      END
