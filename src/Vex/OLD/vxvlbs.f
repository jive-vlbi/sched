      SUBROUTINE VXVLBS
C
C     Routine for the VEX extension of SCHED.
C     CR: 19 Oct 2004. Usually the setups for the VLBA stations appear
C     under the 'VLBA' setup name. However, if one or more VLBA stations
C     have different frequency setups, then these will be referenced by
C     that station's own name. Note the 'different' stations in the array
C     VLBASSTA and the 'different' setups in the array VLBASSET.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
C     variables
C     
      INTEGER   ISETFL, ISET, ISTA, ISCAT
      INTEGER   LEN1, J
C ----------------------------------------------------------------------
C
      IF( DEBUG ) CALL WLOG (1, 'VXVLBS: checking for special ' //
     1                  'VLBA setups')
C
      DO ISET = 1, NSET
        DO ISTA = 1, NSTA
          VLBASSTA(ISTA, ISET) = .FALSE.
          VLBASSET(ISET) = .FALSE.
        END DO
      END DO
      DO ISETFL = 1, NSETF
        DO ISET = 1, NSET
          IF( USED(ISET) ) THEN 
C           Only if this setup was used in this setup file
            IF( SETNAME(ISET) .EQ. SETFILE(ISETFL) ) THEN
              DO ISTA = 1, NSTA
                ISCAT = STANUM(ISTA)
                IF( STATION(ISCAT)(1:4) .EQ. 'VLBA' .AND.
     1             SETSTA(1,ISET)(1:4) .EQ. 'VLBA') THEN
C                  If the station name matches the setup name exactly
C                  then it's a special VLBA setup.
                   IF ( STATION(ISCAT) .EQ. SETSTA(1,ISET) ) THEN
                     VLBASSET(ISET) = .TRUE.
                     WRITE( MSGTXT, '( A, A, A)' )
     2          'VXMODE: Setup ', SETNAME(ISET)(1:LEN1(SETNAME(ISET))), 
     3          ' is not the same for all VLBA stations '
                     CALL WLOG( 1, MSGTXT )
                   END IF
C                  Loop through all the setups corresponding to
C                  this setup file to check if this VLBA station has a
C                  setup of its own.
                   DO J = 1, NSET
                     IF( SETNAME(J) .EQ. SETFILE(ISETFL) ) THEN
                       IF ( J .NE. ISET ) THEN
                         IF ( STATION(ISCAT) .EQ. SETSTA(1,J)) THEN
                           VLBASSTA(ISTA, ISET) = .TRUE.
                           WRITE( MSGTXT, '( A, A, A, A, A,A)' )
     2          'VXMODE: Station ', STATION(ISCAT), ' has a setup',
     3          ' which differs from other VLBA stations in',
     4          ' setup ', SETNAME(ISET)
                           CALL WLOG( 1, MSGTXT )
                         END IF
                       END IF
                     END IF
                  END DO
                END IF
              END DO
            END IF
          END IF
        END DO
      END DO

      RETURN 
      END
