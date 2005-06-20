      CHARACTER*32 FUNCTION VXNMHP( IXX )
C
C     Routine specific for the VEX extension of SCHED
C     function generates a name for HP block IXX
C     for now it is all 14 track, except for S2
C     By H.J. van Langevelde, JIVE, 010996
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C      
      INTEGER IXX, ASTAT
      INTEGER KS, ISTA, VXNHDS
      LOGICAL TWOSTACK
C ----------------------------------------------------------------------
C     
C     Different for S2 (meaningless)
C
      KS = HPISSET(IXX)
      IF( FORMAT(KS) .EQ. 'S2' ) THEN
         VXNMHP = 'S2Void'
      ELSE
C
C     Check if two heads applies
C
         DO ISTA = 1, NSTA
            IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,KS) .OR. 
     1          (STATION(STANUM(ISTA))(1:4) .EQ. 'VLBA' .AND.
     2          SETSTA(1,KS)(1:4) .EQ. 'VLBA' .AND.
     3          .NOT. (VLBASSTA(ISTA,KS)) ) )
     4            ASTAT = ISTA
         END DO
C
         IF( USETAPE(ASTAT) ) THEN
            TWOSTACK = .FALSE.
            IF ( VXNHDS( KS ) .GT. 1 ) THEN 
               TWOSTACK = .TRUE.
C
C              Uses more than 33 tracks, by 2 heads or 2 recorders?
C
               IF ( TWOSTACK ) THEN
                  IF( .NOT. NHEADS(STANUM(ASTAT)) .GT. 1 ) THEN 
                     IF( STNDRIV(STANUM(ASTAT)) .GT. 1 ) THEN
                        TWOSTACK = .FALSE. 
                     ELSE
            CALL ERRLOG('VXNMHP: More than 32 tracks with 1 head?')
                     END IF
                  END IF
               END IF
            END IF
            
            
            VXNMHP = 'Stnd14Pos'
            IF (TWOSTACK) VXNMHP = 'MkIV2head6pos'
         ELSE IF( USEDISK(ASTAT) ) THEN
            VXNMHP = 'DiskVoid'
         ELSE
            CALL ERRLOG('VXNMHP: Not using disks or tapes at '//
     1           STATION(STANUM(ASTAT))// '!' )
         END IF
      END IF
C
      RETURN
      END
