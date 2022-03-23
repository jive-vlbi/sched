      LOGICAL FUNCTION VXCFTR( ISET, JSET )
C
C     Routine specific for the VEX extension of SCHED.
C     returns true if TR block in 2 SCHED sets are identical
C     for now it is mostly 14 pos, so depends on fanout, nchan and bits
C     and format
C     By H.J. van Langevelde, JIVE, 010996
C
C     Added comparison of DBE and FIRMFILE  Feb. 13, 2013  RCW.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      INTEGER ISET, JSET
      LOGICAL IDENT
      INTEGER ISTA, ISCAT, JSCAT, ICH, IP, STAI, STAJ
C ----------------------------------------------------------------------
C
      IDENT = .TRUE.      
C
C     TRACK dependendence on SUBPASS section trivial
C
      IF( TAPEMODE(ISET) .NE. TAPEMODE(JSET) ) IDENT = .FALSE.
      IF( NCHAN(ISET) .NE. NCHAN(JSET) ) IDENT = .FALSE.
      IF( SAMPRATE(ISET) .NE. SAMPRATE(JSET) ) IDENT = .FALSE.
      IF( BITS(1,ISET) .NE. BITS(1,JSET) ) IDENT = .FALSE.
C
C     compare the format, and the fanout
C
      IF( FORMAT(ISET) .NE. FORMAT(JSET) ) IDENT = .FALSE.
      IF( FANOUT(ISET) .NE. FANOUT(JSET) ) IDENT = .FALSE.
C
C     Compare the DEB and FIRMFILE settings.
C
      IF( DBE(ISET) .NE. DBE(JSET) ) IDENT = .FALSE.
      IF( FIRMFILE(ISET) .NE. FIRMFILE(JSET) ) IDENT = .FALSE.
C
C     because of the S2 modes depend on cabling, S2 are not identical
C     for different DAR
C
      IF( IDENT .AND. FORMAT(ISET) .EQ. 'S2' ) THEN
C
C        find stations in order to compare DAR
C
         DO ISTA = 1, NSTA
            IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,ISET) )
     4          ISCAT = STANUM(ISTA)
         END DO
         DO ISTA = 1, NSTA
            IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,JSET) )
     4          JSCAT = STANUM(ISTA)
         END DO
         IF( DAR(ISCAT) .NE. DAR(JSCAT) ) IDENT = .FALSE.
      END IF
C
C     compare MEDIA - no subpass for disks
C
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,ISET) )
     4       STAI = ISTA
      END DO
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,JSET) )
     3       STAJ = ISTA
      END DO
      IF( USETAPE(STAI) .NEQV. USETAPE(STAJ) ) IDENT = .FALSE.
C
C     compare details of TRACKS
C
      IF( IDENT ) THEN
         DO ICH = 1, NCHAN(ISET)
            DO IP = 1, TAPEMODE(ISET)
               IF( TRACK(ICH,IP,ISET) .NE. TRACK(ICH,IP,JSET) )
     1             IDENT = .FALSE.
            END DO
         END DO
      END IF
C
      VXCFTR = IDENT 
C
      RETURN
      END
