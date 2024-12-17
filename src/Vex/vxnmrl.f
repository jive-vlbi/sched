      CHARACTER*32 FUNCTION VXNMRL( IXX )
C
C     Routine specific for the VEX extension of SCHED. 
C     function generates a name for ROLL block IXX
C     By H.J. van Langevelde, JIVE, 300496 
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      INTEGER IXX, KS, ROLLBY
C ----------------------------------------------------------------------
C     
C     Depends on Barrel Roll...
C
      KS = RLISSET(IXX)
      ROLLBY = -1
      IF( BARREL(KS) .EQ. 'roll_off' ) ROLLBY = 0
      IF( BARREL(KS) .EQ. 'roll_16' ) ROLLBY = 16
      IF( BARREL(KS) .EQ. 'roll_8' ) ROLLBY = 8
      IF( BARREL(KS) .EQ. 'roll_auto' ) THEN
         IF( FORMAT(KS)(1:7) .EQ. 'MARKIII' ) THEN
            ROLLBY = 0
         ELSE
            IF( TAPEMODE(KS) .EQ. 1 ) ROLLBY = 16
            IF( TAPEMODE(KS) .EQ. 2 ) ROLLBY = 16
            IF( TAPEMODE(KS) .EQ. 4 ) ROLLBY = 8
            IF( TAPEMODE(KS) .EQ. 0 ) ROLLBY = 0
         END IF
      END IF
C
C     so an appropriate name is
C        
      VXNMRL = 'RandomScramble' 
      IF( ROLLBY .EQ. 16) VXNMRL = 'StndRoll/16'
      IF( ROLLBY .EQ. 8) VXNMRL = 'StndRoll/8'
      IF( ROLLBY .EQ. 0) VXNMRL = 'NoRoll'
C
      RETURN
      END
