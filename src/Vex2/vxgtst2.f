      INTEGER FUNCTION VXGTST2( IMODE )
C
C     Routine specific for the VEX extension of SCHED. 
C     function returns the SCHED setup corresponding to the given VEX mode
C     By C. Reynolds, JIVE, 050802
C
      INCLUDE 'sched2.inc'
      INCLUDE 'schset2.inc'
      INCLUDE 'vxlink2.inc'
C
      INTEGER ISET, ISTA, IMODE
C ----------------------------------------------------------------------
C     
C     Find a station that uses this setup and get the sched setup from MODSET 
C
      ISET = 0
      DO ISTA = 1, NSTA
         IF( MODSET(ISTA,IMODE) .NE. 0 ) THEN  
           ISET = MODSET(ISTA,IMODE)
         END IF
      END DO
      VXGTST2 = ISET
      RETURN
      END
