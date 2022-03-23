      LOGICAL FUNCTION VXCFSI( ISTA, JSTA )
C 
C     Routine specific for the VEX extension of SCHED. 
C     returns true if SI block in 2 SCHED STAs are identical
C     for now it always when ISTA and JSTA are identical
C     By H.J. van Langevelde, JIVE, 300496 
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER ISTA, JSTA
      LOGICAL IDENT
C ----------------------------------------------------------------------
C
      IDENT = .TRUE.      
C
C     Find out both the same station or not...
C
      IF( ISTA .NE. JSTA) IDENT = .FALSE.
C
      VXCFSI = IDENT 
C
      RETURN
      END
