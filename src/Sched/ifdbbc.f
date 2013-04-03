      SUBROUTINE IFDBBC( MYDBBCVER, MAXBBC, MAXIF, IFBBC, MIF )
C
C     CR: 26 March, 2013
C     Routine for Sched called by BBCDBBC and CHKDBBC which gives the
C     allowed IF-> BBC mappings for the different flavours of DBBC.
C
      INCLUDE    'sched.inc'
C
      CHARACTER   MYDBBCVER*(*)
      INTEGER     MAXBBC, MAXIF, I
      INTEGER     IFBBC(MAXBBC, MAXIF), MIF, IIF, IBBC
C
C  -----------------------------------------------------------
C     Software check:
C
      IF( DEBUG ) CALL WLOG( 1, 'IFDBBC: Starting' )
      call wlog(1, 'In ifdbbc.f')
C
C     Initialise the IF->BBC lists. IFBBC(IBBC, IIF) = 0 means IBBC not
C     accessible from IIF. 
C
      DO IBBC = 1, MAXBBC
         DO IIF = 1, MAXIF
            IFBBC(IBBC, IIF) = 0
         END DO
      END DO
      IF( MYDBBCVER .EQ. 'ASTRO' ) THEN
C        'Astro' version
         MIF = 4
         DO IBBC = 1, 4
            IFBBC(IBBC, 1) = 1
            IFBBC(IBBC+4, 2) = 1
            IFBBC(IBBC+8, 3) = 1
            IFBBC(IBBC+12, 4) = 1
         END DO
      ELSE IF( MYDBBCVER .EQ. 'GEO' ) THEN
C        'geo' version
         MIF = 2
         DO IBBC = 1, 8
            IFBBC(IBBC, 1) = 1
            IFBBC(IBBC+8, 2) = 1
         END DO
      ELSE IF( MYDBBCVER .EQ. 'HYBRID' ) THEN
C        'hybrid' version
         MIF = 3
         DO IBBC = 1, 4
            IFBBC(IBBC, 1) = 1
            IFBBC(IBBC+4, 2) = 1
         END DO
         DO IBBC = 1, 8
            IFBBC(IBBC+8, 3) = 1
         END DO
      END IF
C      DO IBBC=1,MAXBBC
C         print*, (IFBBC(IBBC,IIF), IIF=1,MAXIF)
C      END DO
      
      call wlog(1, 'leaving ifdbbc.f')

      RETURN
      END
