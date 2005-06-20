      SUBROUTINE GETPTLO( PTLO, ISCN )
C
C     Routine for SCHED called by CRDVLA that gets the PT first LO 
C     to put on the VLA LO card in Pie Town link observations.
C
      INCLUDE      'sched.inc'
      INCLUDE      'schset.inc'
C
      CHARACTER        PTLO*9, CAPCOD*2
      INTEGER          ISCN, IPT, ISET, ISTA
      REAL             RPTLO
      SAVE             RPTLO
      DATA             RPTLO / 0.0 /
C ---------------------------------------------------------------------
      IF( PTLINK ) THEN
         
C
C        Get which station is PT.
C
         IPT = 0
         DO ISTA = 1, NSTA
            CAPCOD = STCODE(STANUM(ISTA))
            CALL UPCASE( CAPCOD )
            IF( CAPCOD .EQ. 'PT' ) IPT = ISTA
         END DO
C
         IF( IPT .EQ. 0 ) THEN
            CALL ERRLOG( 'GETPTLO:  PT not in a PTLINK observation??' )
         END IF
C
C        If PT is in the scan, get the current firstlo.  If not, use
C        the last one found.
C
         IF( STASCN(ISCN,IPT) ) THEN
            ISET = NSETUP( ISCN, IPT )
            RPTLO = FIRSTLO(1,ISET) / 1000.0
         END IF
C
         WRITE( PTLO, '( F9.1 )' ) RPTLO
C
      ELSE
C
C        Not a link experiment.  Put blank on the card.
C
         PTLO = ' '
C
      END IF
C
      RETURN
      END
