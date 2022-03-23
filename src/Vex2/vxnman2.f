      CHARACTER*32 FUNCTION VXNMAN2( IXX )
C
C     Routine specific for the VEX extension of SCHED. 
C     function generates a name for AN block IXX
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
C     Find a station that uses this DAS def
C
      KS = -1
      DO ISTA = 1, NSTA
         IF( ISTAAN(ISTA) .EQ. IXX ) KS = ISTA
      END DO
C
      IF( KS .LT. 0 ) CALL ERRLOG(' VXNMAN: no station for Antenna def')
C
C     name is simply
C
      ISCAT = STANUM(KS)
      NAME = STATION(ISCAT)
C
      VXNMAN2 = NAME
C
      RETURN
      END
