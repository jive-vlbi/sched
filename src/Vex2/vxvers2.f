      SUBROUTINE VXVERS2( VERVX, VERVEX )
C
C     Return the version of the VEX standard to which the current code  
C     applies
C
      INCLUDE    'sched2.inc'
      INCLUDE    'schset2.inc'
      INCLUDE    'vxlink2.inc'
C
      REAL       VERVX
      REAL       VERVEX
      CHARACTER  TEST*3
C ----------------------------------------------------------------------
      TEST = VXSOFT
      READ( TEST, '( F3.2)' ) VERVX
C
      TEST = VEXVER
      READ( TEST, '( F3.1)' ) VERVEX
C
      RETURN
      END

