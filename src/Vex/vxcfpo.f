      LOGICAL FUNCTION VXCFPO( ISET, JSET )
C
C     Routine specific for the VEX extension of SCHED.
C     returns true if PO block in 2 SCHED sets are identical
C     Depends on TPMODE only, and whether one is S2
C     By H.J. van Langevelde, JIVE, 010996
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      INTEGER ISET, JSET, ISTA, STAI, STAJ, FNDHPOS
      INTEGER JSCAT, ISCAT
      LOGICAL IDENT
C ----------------------------------------------------------------------
C
      IDENT = .TRUE.      
C
C     PassOrder section easy, depends on TAPEMODE only, or if one S2
C     CR (25 Oct 2004): I don't think this is complete, as stations may
C       have different subpasses, if they are observing different
C       numbers of subbands. However, as subpasses will cease to exist
C       with the adoption of disks, I want try to fix this now.
C
      IF( (FORMAT(ISET) .EQ. 'S2' .OR. FORMAT(JSET) .EQ. 'S2')
     1    .AND. FORMAT(ISET) .NE. FORMAT(JSET) ) IDENT = .FALSE.
      IF( (FORMAT(ISET) .EQ. 'LBA' .OR. FORMAT(JSET) .EQ. 'LBA')
     1    .AND. FORMAT(ISET) .NE. FORMAT(JSET) ) IDENT = .FALSE.
      IF( TAPEMODE(ISET) .NE. TAPEMODE(JSET) ) IDENT = .FALSE.
C
C     Need to check the number of head positions
C
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,ISET) ) 
     4       STAI = ISTA
      END DO
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,JSET) ) 
     4       STAJ = ISTA
      END DO
C
C     Make sure stations in a set don't appear with different nhdpos
C
      FNDHPOS = 0
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,ISET) ) THEN
            IF( FNDHPOS .EQ. 0 ) FNDHPOS = NHDPOS(ISTA)
            IF( FNDHPOS .NE. NHDPOS(ISTA) ) THEN
               WRITE( MSGTXT, '( A, A, A )' ) 
     1             'VXCFHP: VEX cannot deal with a station',
     2             ' with discrepant tape mode: ',
     3             STATION(STANUM(ISTA))
               CALL ERRLOG( MSGTXT )
            END IF
         END IF
      END DO
C
      FNDHPOS = 0
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,JSET) ) THEN
            IF( FNDHPOS .EQ. 0 ) FNDHPOS = NHDPOS(ISTA)
            IF( FNDHPOS .NE. NHDPOS(ISTA) ) THEN
               WRITE( MSGTXT, '( A, A, A )' ) 
     1             'VXCFHP: VEX cannot deal with a station',
     2             ' with discrepant tape mode: ',
     3             STATION(STANUM(ISTA))
               CALL ERRLOG( MSGTXT )
            END IF
         END IF
      END DO

      IF ( IDENT ) IDENT = ( NHDPOS(STAI) .EQ. NHDPOS(STAJ) )
C
C     compare MEDIA - no passorder for disks
C
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,ISET) )
     4       ISCAT = ISTA
      END DO
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,JSET) ) 
     4       JSCAT = ISTA
      END DO

      IF( USETAPE(ISCAT) .NEQV. USETAPE(JSCAT) ) IDENT = .FALSE.
C
      VXCFPO = IDENT 
C
      RETURN
      END
