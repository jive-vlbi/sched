      CHARACTER*32 FUNCTION VXNMPR2( IXX )
C
C     Routine specific for the VEX extension of SCHED
C     function generates a name for PR block IXX
C     NOTE: slightly less general then other "vxnm" routines
C     By H.J. van Langevelde, JIVE, 010996
C
      INCLUDE 'sched2.inc'
      INCLUDE 'schset2.inc'
      INCLUDE 'vxlink2.inc'
      INTEGER IXX
C ----------------------------------------------------------------------
C
C     Pass order depends on TAPEMODE
C
      WRITE( VXNMPR2, '( A, I2.2 )' ) 'Mode_', IXX
C
      RETURN
      END
