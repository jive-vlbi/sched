      SUBROUTINE HAAVAI( HAMIN, HAMAX, HABEGIN, HAEND, HAAVAIL, 
     1                   HAUSE, SETRISE, NHA, OPDUR, SIDR )
C
C     Subroutine for OPTHAS that gets the total time the source is 
C     available.  Allow 10s tolerance on some tests.
C
C     Also determine the default hour angles (HAUSE) to use for even 
C     spacing of NHA scans.
C
      INTEGER           NHA, I
      REAL              HAMIN, HAMAX, HABEGIN, HAEND, HAAVAIL
      REAL              HAUSE(*)
      LOGICAL           SETRISE
      DOUBLE PRECISION  OPDUR, SIDR
C ----------------------------------------------------------------------
C     First the cases where the source is up at the start.
C     Note that we know the source is up sometime or HALIM would
C     have stopped.
C
      SETRISE = .FALSE.
      IF( HABEGIN .GT. HAMIN .AND. HABEGIN .LT. HAMAX ) THEN
C
C        Is it still up at the end?  The 10 second adjustment gives a   
C        bit of tolerance in tight cases.
C
         IF( OPDUR * 24.D0 * SIDR - 10.0 / 3600.D0 .LE. 
     1          HAMAX - HABEGIN ) THEN
            HAAVAIL = OPDUR * 24.D0 * SIDR
            DO I = 1, NHA
               HAUSE(I) = HABEGIN + HAAVAIL * ( I - 0.5 ) / NHA
            END DO
         ELSE
C
C           If not, add the up time at the start, and test 
C           if it rises again.  If so, add that time.  This
C           test only works properly if we know the source
C           has not been up the whole time.
C
            HAAVAIL = HAMAX - HABEGIN
            IF( HAEND .GE. HAMIN ) THEN
               HAAVAIL = HAAVAIL + HAEND - HAMIN 
               SETRISE = .TRUE.
               DO I = 1, NHA
                  HAUSE(I) = HABEGIN + HAAVAIL * ( I - 0.5 ) / NHA
                  IF( HAUSE(I) .GT. HAMAX ) THEN
                     HAUSE(I) = HAMIN + HAAVAIL * ( I - 0.5 ) / NHA 
     1                     - ( HAMAX - HABEGIN )
                  END IF
               END DO
            ELSE
               DO I = 1, NHA
                  HAUSE(I) = HABEGIN + HAAVAIL * ( I - 0.5 ) / NHA
               END DO
            END IF
         END IF
      ELSE
C
C        Now for the two cases where it rises during the obs.
C
         IF( HAEND .GE. HAMIN .AND.
     1       HAEND .LE. HAMAX ) THEN
            HAAVAIL = HAEND - HAMIN
         ELSE
            HAAVAIL = HAMAX - HAMIN
         END IF
         DO I = 1, NHA
            HAUSE(I) = HAMIN + HAAVAIL * ( I - 0.5 ) / NHA
         END DO
      END IF
      RETURN
      END
