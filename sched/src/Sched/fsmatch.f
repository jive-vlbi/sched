      LOGICAL FUNCTION FSMATCH( KF, JF )
C
C     Check if two frequency sets are the same.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
C
      INTEGER   KF, JF
      INTEGER   ICH, KS, JS, NNCHAN
      LOGICAL   SAMESET, PSMATCH, FQMATCH
      DOUBLE PRECISION   KBBCFREQ(MCHAN), KBBCBW(MCHAN)
      DOUBLE PRECISION   KLOSUM(MCHAN)
      DOUBLE PRECISION   JBBCFREQ(MCHAN), JBBCBW(MCHAN)
      DOUBLE PRECISION   JLOSUM(MCHAN)
C -------------------------------------------------------------------
C     Get the setup group numbers these are from.
C
      KS = FSETKS(KF)
      JS = FSETKS(JF)
C
C     Initialize the output.
C
      FSMATCH = SAMESET( KS, JS )
C
C     Check for same setup file (not group), including lots
C     of details.
C
      FQMATCH = .TRUE.
      IF( FSMATCH ) THEN
         CALL FSFREQ( KF, KLOSUM, KBBCFREQ, KBBCBW )
         CALL FSFREQ( JF, JLOSUM, JBBCFREQ, JBBCBW )
         NNCHAN = NCHAN(KS)
         DO ICH = 1, NNCHAN
            IF( ABS( KLOSUM(ICH) - JLOSUM(ICH) ) .GT. 1.0D-6 .OR.
     1          ABS( KBBCFREQ(ICH) - JBBCFREQ(ICH) ) .GT. 1.0D-6 .OR.
     2          ABS( KBBCBW(ICH) - JBBCBW(ICH) ) .GT. 1.0D-6 ) THEN
               FQMATCH = .FALSE.
            END IF
         END DO
      END IF
C
C        Check for same pcal set.
C
      PSMATCH = .TRUE.
      IF( FSMATCH ) THEN
         IF( FSETPS(1,KF) .NE. FSETPS(1,JF) .OR.
     1       FSETPS(2,KF) .NE. FSETPS(2,JF) .OR.
     2       FSETPS(3,KF) .NE. FSETPS(3,JF) ) PSMATCH = .FALSE.
      END IF
C
      FSMATCH = FSMATCH .AND. FQMATCH .AND. PSMATCH
C
      RETURN
      END
