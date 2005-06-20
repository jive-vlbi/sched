      LOGICAL FUNCTION VXCFRL( ISET, JSET )
C 
C     Routine specific for the VEX extension of SCHED. 
C     returns true if RL block in 2 SCHED sets are identical
C     for now it is all 14 track, so depends on BARREL and TAPEMODE
C     By H.J. van Langevelde, JIVE, 300496 
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER ISET, JSET, IROLL, JROLL
      LOGICAL IDENT
C ----------------------------------------------------------------------
C
      IDENT = .TRUE.      
C
C     Find out both rolls
C
      IROLL = -1
      IF( BARREL(ISET) .EQ. 'roll_off' ) IROLL = 0
      IF( BARREL(ISET) .EQ. 'roll_16' ) IROLL = 16
      IF( BARREL(ISET) .EQ. 'roll_8' ) IROLL = 8
      IF( BARREL(ISET) .EQ. 'roll_auto' ) THEN
         IF( FORMAT(ISET)(1:7) .EQ. 'MARKIII' ) THEN
            IROLL = 0
         ELSE
            IF( TAPEMODE(ISET) .EQ. 1 ) IROLL = 16
            IF( TAPEMODE(ISET) .EQ. 2 ) IROLL = 16
            IF( TAPEMODE(ISET) .EQ. 4 ) IROLL = 8
            IF( TAPEMODE(ISET) .EQ. 0 ) IROLL = 0
         END IF
      END IF
C
      JROLL = -1
      IF( BARREL(JSET) .EQ. 'roll_off' ) JROLL = 0
      IF( BARREL(JSET) .EQ. 'roll_16' ) JROLL = 16
      IF( BARREL(JSET) .EQ. 'roll_8' ) JROLL = 8
      IF( BARREL(JSET) .EQ. 'roll_auto' ) THEN
         IF( FORMAT(JSET)(1:7) .EQ. 'MARKIII' ) THEN
               JROLL = 0
         ELSE
            IF( TAPEMODE(JSET) .EQ. 1 ) JROLL = 16
            IF( TAPEMODE(JSET) .EQ. 2 ) JROLL = 16
            IF( TAPEMODE(JSET) .EQ. 4 ) JROLL = 8
            IF( TAPEMODE(JSET) .EQ. 0 ) JROLL = 0
         END IF
      END IF
C
      IF( IROLL .NE. JROLL ) IDENT = .FALSE.
C
      VXCFRL = IDENT 
C
      RETURN
      END
