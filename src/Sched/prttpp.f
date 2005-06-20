      SUBROUTINE PRTTPP( KS, IUNIT )
C
C     Routine to write the time per tape pass into the setup summary
C     in the summary file.  For SCHED, called by PRTSET.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER    KS, IG, NGOT, ISTA, IUNIT
      INTEGER    GTPLEN(MAXSTA), LEN1
      DOUBLE PRECISION  RADTIME
      LOGICAL    NEWCOMB
      CHARACTER  GDEN(MAXSTA)*4, TTIME*8, TFORM*8
C ---------------------------------------------------------------------
      NGOT = 0
C
C     Go through the stations.  If this setup might be used for the
C     station, write the appropriate tape times.  However, do not
C     repeat length/density combinations.  Most of this hoop jumping
C     would only be useful if, for example, 2 VLBA stations were
C     set up with different length tapes.
C
      DO ISTA = 1, NSTA
         NEWCOMB = .TRUE.
         IF( STANAME(ISTA) .EQ. SETSTA(1,KS) .OR.
     1       ( STANAME(ISTA)(1:4) .EQ. 'VLBA' .AND. 
     2         SETSTA(1,KS) .EQ. 'VLBA' ) .OR. 
     3       ( STANAME(ISTA)(1:3) .EQ. 'VLA' .AND. 
     4         SETSTA(1,KS) .EQ. 'VLA' ) ) THEN 
             IF( NGOT .GE. 1 ) THEN
                DO IG = 1, NGOT
                   IF( GDEN(IG)(1:1) .EQ. DENSITY(ISTA) .AND. 
     1                 GTPLEN(IG) .EQ. TPLENG(ISTA) ) THEN
                      NEWCOMB = .FALSE.
                   END IF
                END DO
             END IF
C
C            Save the new combination.  Note that invalid densities
C            were precluded in STALST for normal processing, but
C            we need to protect against problems when this routine
C            is used in error messages.
C
             IF( NEWCOMB ) THEN
                NGOT = NGOT + 1
                GTPLEN(NGOT) = TPLENG(ISTA)
                IF( DENSITY(ISTA) .EQ. 'H' ) THEN
                   GDEN(NGOT) = 'High'
                   IF( SPEEDH(KS) .EQ. 0.0 ) THEN
                      RADTIME = 0.0D0
                   ELSE
                      RADTIME = GTPLEN(NGOT) * 12.0 / SPEEDH(KS)
                   END IF
                ELSE
                   GDEN(NGOT) = 'Low'
                   IF( SPEEDL(KS) .EQ. 0.0 ) THEN
                      RADTIME = 0.0D0
                   ELSE
                      RADTIME = GTPLEN(NGOT) * 12.0 / SPEEDL(KS)
                   END IF
                END IF
                RADTIME = RADTIME * RADHR / 3600.D0
                TTIME = TFORM( RADTIME, 'T', 0, 2, 2, ':: ' )
C
C               Write the information.
C
                WRITE( IUNIT, '( A, I6, A, A, A, A )' )
     1             '   Time per pass for ', GTPLEN(NGOT), 
     2             ' ft, ', GDEN(NGOT)(1:LEN1(GDEN(NGOT))), 
     2             ' density tapes is: ', TTIME
C
             END IF
         END IF
      END DO
C
      RETURN
      END


