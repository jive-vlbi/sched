      LOGICAL FUNCTION VXCFBB( ISET, JSET )
C
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 020596 
C     returns true if BB block in 2 SCHED sets are identical
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER ISET, JSET, NBBCI, NBBCJ, ICH, JCH
      INTEGER IBBC(MAXCHN), JBBC(MAXCHN)
      CHARACTER IIF(MAXCHN)*2, JIF(MAXCHN)*2
      LOGICAL IDENT, NEWFND
C ----------------------------------------------------------------------
C
      IDENT = .TRUE.      
C
C     BBC section, depends on NBBC but also on which IF they connect
C     and actually also on which BBC/VCs are used
C
      NBBCI = 0
      DO ICH= 1, NCHAN(ISET)
         NEWFND = .TRUE.
         IF( ICH .GT. 1 ) THEN
            DO JCH = 1, ICH-1
               IF( BBC(ICH,ISET) .EQ. BBC(JCH,ISET)) NEWFND = .FALSE.
            END DO
         END IF
         IF( NEWFND ) THEN
            NBBCI = NBBCI + 1
            IIF(NBBCI) = IFCHAN(ICH,ISET)
            IBBC(NBBCI) = BBC(ICH,ISET)
         END IF
      END DO
C
      NBBCJ = 0
      DO ICH=1,NCHAN(JSET)
         NEWFND = .TRUE.
         IF( ICH .GT. 1 ) THEN
            DO JCH = 1, ICH-1
               IF( BBC(ICH,JSET) .EQ. BBC(JCH,JSET)) NEWFND = .FALSE.
            END DO
         END IF
         IF( NEWFND ) THEN
            NBBCJ = NBBCJ + 1
            JIF(NBBCJ) = IFCHAN(ICH,JSET)
            JBBC(NBBCJ) = BBC(ICH,JSET)
         END IF
      END DO
C
C     Now we can compare directly
C
      IF( NBBCI .NE. NBBCJ ) THEN
         IDENT = .FALSE.
      ELSE
         DO ICH = 1, NBBCI
            IF( IIF(ICH) .NE. JIF(ICH) .OR.
     1          IBBC(ICH) .NE. JBBC(ICH) )
     2          IDENT = .FALSE.
         END DO
      END IF
C
      VXCFBB = IDENT 
C
      RETURN
      END
