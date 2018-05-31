      SUBROUTINE IFDBBC( MYDBBCVER, MAXBBC, MAXIF, IFBBC, MIF )
Cf2py intent(out) IFBBC, MIF
C
C     CR: 26 March, 2013
C     Routine for Sched called by BBCDBBC and CHKDBBC which gives the
C     allowed IF-> BBC mappings for the different flavours of DBBC.
C
      INCLUDE    'sched.inc'
C
      CHARACTER   MYDBBCVER*(*)
      INTEGER     MAXBBC, MAXIF
      INTEGER     IFBBC(MAXBBC, MAXIF), MIF, IIF, IBBC
C
C  -----------------------------------------------------------
C     Software check:
C
      IF( DEBUG ) CALL WLOG( 1, 'IFDBBC: Starting' )
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
C        'Astro' version, 4 IFs with 4 BBCs each
         MIF = 4
         DO IBBC = 1, 4
            IFBBC(IBBC, 1) = 1
            IFBBC(IBBC+4, 2) = 1
            IFBBC(IBBC+8, 3) = 1
            IFBBC(IBBC+12, 4) = 1
         END DO
      ELSE IF( MYDBBCVER .EQ. 'GEO' ) THEN
C        'geo' version, 2 IFs with 8 BBCs each
         MIF = 2
         DO IBBC = 1, 8
            IFBBC(IBBC, 1) = 1
            IFBBC(IBBC+8, 2) = 1
         END DO
      ELSE IF( MYDBBCVER .EQ. 'HYBRID' ) THEN
C        'hybrid' version, 3 IFs with 2x4 and 1x8 BBCs
         MIF = 3
         DO IBBC = 1, 4
            IFBBC(IBBC, 1) = 1
            IFBBC(IBBC+4, 2) = 1
         END DO
         DO IBBC = 1, 8
            IFBBC(IBBC+8, 3) = 1
         END DO
      ELSE
         WRITE( MSGTXT, '( A, A )' )
     1       'IFDBBC: DBBCVER not recognised:', MYDBBCVER
         CALL WLOG( 1, MSGTXT )
         CALL ERRLOG( 'catalog error' )
C
      END IF
C
      RETURN
      END
