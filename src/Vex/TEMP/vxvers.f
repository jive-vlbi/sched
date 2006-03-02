      SUBROUTINE VXVERS( VERVX, VERVEX )
C
C     Return the version of the VEX standard to which the current code  
C     applies
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
      INCLUDE    'vxlink.inc'
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

