      LOGICAL FUNCTION VXCFDA( ISTA, JSTA )
C
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 020596 
C     returns true if DA block in 2 SCHED STAs are identical
C     cf high or low density, thick or thin, n_heads etc
C     Updated for 1.4 220796 Huib
C
C     CR: 8 Nov 2005. Note we have a problem here. If there is a station
C      in the schedule that never appears in a scan (possible when AUTOTAPE is
C      being used), then the tests for ISET and JSET can return
C      undefined values which result in a segmentation error. Need to
C      think how to deal with this - may affect other parts of the VEX
C      writer too.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER ISTA, JSTA, ISET, JSET, ISCAT, JSCAT, I
      LOGICAL IDENT
C ----------------------------------------------------------------------
C
      IDENT = .TRUE.      
C
C     Find out both densities and lengths and recorder
C
      IF( DENSITY(ISTA) .NE. DENSITY(JSTA) ) 
     1     IDENT = .FALSE.
      IF( TPLENG(ISTA) .NE. TPLENG(JSTA) ) 
     1     IDENT = .FALSE.
C
      IF( STNDRIV(STANUM(ISTA)) .NE. STNDRIV(STANUM(JSTA)) )
     1     IDENT = .FALSE.
      IF( NHEADS(STANUM(ISTA)) .NE. NHEADS(STANUM(JSTA)) )
     1     IDENT = .FALSE.
C
C     Can also be a lot of things between the format & control
C     find a typical Set for each telescope
C
      ISCAT = STANUM(ISTA)
      DO I = 1, NSET
         IF ( USED(I) .AND. ( 
     1       STATION(ISCAT) .EQ. SETSTA(1,I) )) ISET = I
      END DO
C
      JSCAT = STANUM(JSTA)
      DO I = 1, NSET
         IF ( USED(I) .AND. (
     1       STATION(JSCAT) .EQ. SETSTA(1,I) )) JSET = I
      END DO
C
      IF( CONTROL(ISCAT) .NE. CONTROL(JSCAT) ) IDENT = .FALSE.
      IF( DAR(ISCAT) .NE. DAR(JSCAT) ) IDENT = .FALSE.
      IF( RECORDER(ISCAT) .NE. RECORDER(JSCAT) ) IDENT = .FALSE.
      IF( USETAPE(ISTA) .NEQV. USETAPE(JSTA) ) IDENT = .FALSE.
C
C     This could be made more elegant, as the fanout may be omitted 
C     but no harm can be done, only an extra def section
C
      IF( FORMAT(ISET) .NE. FORMAT(JSET) ) IDENT = .FALSE.
      IF( FANOUT(ISET) .NE. FANOUT(JSET) ) IDENT = .FALSE.
C
      VXCFDA = IDENT 
C
      RETURN
      END
