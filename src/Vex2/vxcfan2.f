      LOGICAL FUNCTION VXCFAN2( ISTA, JSTA )
C 
C     Routine specific for the VEX extension of SCHED. 
C     returns true if AN block in 2 SCHED STAs are identical
C     but maybe could have generic telescopes, like VLBA
C     By H.J. van Langevelde, JIVE, 300496 
C
      INCLUDE 'sched2.inc'
      INCLUDE 'schset2.inc'
C
      INTEGER ISTA, JSTA
      LOGICAL IDENT
C ----------------------------------------------------------------------
C
      IDENT = .TRUE.      
C
C     Find out both rolls
C
      IF( ISTA .NE. JSTA ) IDENT = .FALSE.
C
      VXCFAN2 = IDENT 
C
      RETURN
      END
