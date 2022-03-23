      CHARACTER*32 FUNCTION VXNMSI2( IXX )
C
C     Routine specific for the VEX extension of SCHED. 
C     function generates a name for SI block IXX
C     By H.J. van Langevelde, JIVE, 300496 
C
      INCLUDE 'sched2.inc'
      INCLUDE 'schset2.inc'
      INCLUDE 'vxlink2.inc'
C
      INTEGER IXX, KS, ISTA, ISCAT
      CHARACTER NAME*32
C ----------------------------------------------------------------------
C     
C     Find a station that uses this SIS def
C
      KS = -1
      DO ISTA = 1, NSTA
         IF( ISTASI(ISTA) .EQ. IXX ) KS = ISTA
      END DO
C
      IF( KS .LT. 0 ) CALL ERRLOG(' VXNMSI2: no station for SITE def ')
C
C     name is simply the station's name
C
      ISCAT = STANUM(KS)
      NAME = STATION(ISCAT)
C        
      VXNMSI2 = NAME
C
      RETURN
      END
