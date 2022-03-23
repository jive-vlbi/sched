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
C     As part of removing tape, I have commented out some of the tests
C     here as they are tape specific.  July 22, 2010  RCW
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
C      IF( DENSITY(ISTA) .NE. DENSITY(JSTA) ) 
C     1     IDENT = .FALSE.
C      IF( TPLENG(ISTA) .NE. TPLENG(JSTA) ) 
C     1     IDENT = .FALSE.
C
C     Note STNDRIV is not given in the current station catalogs (NDRIVES)
C     in the Mark5A era.  It might return if stations get more than one
C     disk system but that is not yet supported.  The default for 
C     STNDRIV is 2, which was appropriate for tapes at the VLBA, but not
C     for any current system.  That should not have any effect.  The same
C     comments go for NHEADS.  In fact, I don't see where that one will
C     return so I have commented it out (RCW).
C
      IF( STNDRIV(STANUM(ISTA)) .NE. STNDRIV(STANUM(JSTA)) )
     1     IDENT = .FALSE.
C      IF( NHEADS(STANUM(ISTA)) .NE. NHEADS(STANUM(JSTA)) )
C     1     IDENT = .FALSE.
C
C     Can also be a lot of things between the format & control
C     find a typical Set for each telescope
C
      ISET = 0
      ISCAT = STANUM(ISTA)
      DO I = 1, NSET
         IF ( USED(I) .AND. ( 
     1       STATION(ISCAT) .EQ. SETSTA(1,I) )) ISET = I
      END DO
      IF ( ISET .EQ. 0 ) THEN
         CALL WLOG (1, 'VXCFDA: ' // STATION(ISCAT) // ' does ' //
     1   'not appear in any scans!' )
      END IF
C
      JSET = 0
      JSCAT = STANUM(JSTA)
      DO I = 1, NSET
         IF ( USED(I) .AND. (
     1       STATION(JSCAT) .EQ. SETSTA(1,I) )) JSET = I
      END DO
C
C     Recorder and USETAPE are obsolete RCW.
C        but add back in DISK to be able to separate Mark5A & 5B stations
C        into different $DAS-section blocks -- rmc (16sep2011)
C
      IF( CONTROL(ISCAT) .NE. CONTROL(JSCAT) ) IDENT = .FALSE.
      IF( DAR(ISCAT) .NE. DAR(JSCAT) ) IDENT = .FALSE.
      IF( DISK(ISCAT) .NE. DISK(JSCAT) ) IDENT = .FALSE.
C      IF( RECORDER(ISCAT) .NE. RECORDER(JSCAT) ) IDENT = .FALSE.
C      IF( USETAPE(ISTA) .NEQV. USETAPE(JSTA) ) IDENT = .FALSE.
C
C     If station appears in no scans, then ISET and JSET can still be 0
      IF( ISET .GT. 0 .AND. JSET .GT. 0 ) THEN
C
C        This could be made more elegant, as the fanout may be omitted 
C        but no harm can be done, only an extra def section
C        Actually, for cases with observations with different formats
C        in different scans, this could falsely claim stations are
C        different.
C
         IF( FORMAT(ISET) .NE. FORMAT(JSET) ) IDENT = .FALSE.
         IF( FANOUT(ISET) .NE. FANOUT(JSET) ) IDENT = .FALSE.
      END IF
C
      VXCFDA = IDENT 
C
      RETURN
      END
