      SUBROUTINE VXTELS( BLOCK, NXXVEX, STAXX, XXLINK )
C
C     Routine for the VEX extension of SCHED.
C     Finds out how many sections of type XX (e.g. DA) etc there will be.
C     Where the def sections are referenced by Station, not by MODE.
C     H.J.van Langevelde JIVE, 130596C
C
C     Input:
C        BLOCK = Two letter code, denoting which $BLOCK we are sorting
C     Output:
C        NXXVEX = Number of defs in $BLOCK
C        STAXX = List of stations, giving which def XX they use
C        XXLINK(IXX) = Name of link IXX.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      CHARACTER BLOCK*2, XXLINK(MAXSTA)*32
      INTEGER NXXVEX
      INTEGER STAXX(MAXSTA)
C
      INTEGER ISTA, JSTA
      LOGICAL NEWFND, VXCFDA, VXCFSI, VXCFAN
C ----------------------------------------------------------------------
C
      WRITE( MSGTXT, '( A, A, A )' ) 
     1    'VXTELS: Start sorting ', BLOCK, ' definition'
      IF( DEBUG ) CALL WLOG( 1, MSGTXT )
      NXXVEX = 0
      DO ISTA = 1, NSTA
C
C        Obviously a new block it's the first
C
         IF( ISTA .EQ. 1 ) THEN
            NEWFND = .TRUE.
         ELSE
C
C           Find if any tel JSTA has same chars ISTA = 1 to JSTA-1 
C
            NEWFND = .TRUE.
            DO JSTA = 1, (ISTA - 1)
C
C              Check Data Acquisition system
C
               IF( BLOCK .EQ. 'DA' ) THEN
                  IF( VXCFDA( ISTA, JSTA ) ) THEN
                     NEWFND = .FALSE.
C
C                    Same as old station, so use same def
C
                     STAXX(ISTA) = STAXX(JSTA)
                  END IF
C
C              Check SITE (a trivial comparison)
C
               ELSE IF( BLOCK .EQ. 'SI' ) THEN
                  IF( VXCFSI( ISTA, JSTA ) ) THEN
                     NEWFND = .FALSE.
C
C                    Same as old station, so use same def
C
                     STAXX(ISTA) = STAXX(JSTA)
                  END IF
C
C              Check station
C
               ELSE IF( BLOCK .EQ. 'AN' ) THEN
                  IF( VXCFAN( ISTA, JSTA ) ) THEN
                     NEWFND = .FALSE.
C
C                    Same as old station, so use same def
C
                     STAXX(ISTA) = STAXX(JSTA)
                  END IF
               ELSE
                  CALL ERRLOG(' VXSORT: unsupported BLOCK def ')
               END IF
            END DO
         END IF
         IF( NEWFND ) THEN
C
C           fill a new block and give it a name
C
            NXXVEX = NXXVEX + 1
            IF( NXXVEX .GT. MAXMOD ) CALL ERRLOG(
     1                 'VXTELS: Number of '//BLOCK//' defs '//
     2                 'exceeding MAXMOD, need to re-compile...')
C
C           store which def this station uses:
C
            STAXX(ISTA) = NXXVEX
C
            CALL VXNMXX( BLOCK, NXXVEX, XXLINK )
C
         END IF
      END DO
C
C     no further complications, done
C
      RETURN
      END


