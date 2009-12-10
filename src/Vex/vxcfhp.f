      LOGICAL FUNCTION VXCFHP( ISET, JSET )
C
C     Routine specific for the VEX extension of SCHED
C     returns true if HP block in 2 SCHED sets are identical
C     for now it is all 14 track, except for S2
C     By H.J. van Langevelde, JIVE, 010996
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      INTEGER ISET, JSET, ISTA, STAI, STAJ, FNDHPOS
      INTEGER ISCAT, JSCAT
      LOGICAL IDENT
C ----------------------------------------------------------------------
C
      IDENT = .TRUE.      
C
C     Check if one is S2, then it is different
C
      IF( (FORMAT(ISET) .EQ. 'S2' .OR. FORMAT(JSET) .EQ. 'S2')
     1    .AND. FORMAT(ISET) .NE. FORMAT(JSET) ) IDENT = .FALSE.
C
C     Check if one is LBA, then it is different
C
      IF( (FORMAT(ISET) .EQ. 'LBA' .OR. FORMAT(JSET) .EQ. 'LBA')
     1    .AND. FORMAT(ISET) .NE. FORMAT(JSET) ) IDENT = .FALSE.
C
C     Need to check the number of head positions
C
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,ISET) )  STAI = ISTA
      END DO
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,JSET) ) 
     3       STAJ = ISTA
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

      FNDHPOS = 0
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,JSET)  ) THEN
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

      IF( IDENT ) IDENT = ( NHDPOS(STAI) .EQ. NHDPOS(STAJ) )
C
C     compare MEDIA - no headpos for disks
C
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,ISET) )
     3       ISCAT = ISTA
      END DO
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,JSET) )
     3       JSCAT = ISTA
      END DO

      IF( USETAPE(ISCAT) .NEQV. USETAPE(JSCAT) ) IDENT = .FALSE.
C
      VXCFHP = IDENT 
C
      RETURN
      END
