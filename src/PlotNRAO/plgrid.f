      SUBROUTINE PLGRID( DECSRC )
C
C     Plot the grid used for UV optimization on a UV plot.
C
C     The radial spacing is like a logarithmic spacing with a
C     constant added.
C
      INCLUDE     'sched.inc'
      INCLUDE     'plot.inc'
C
      INTEGER     I, ML, IC
      PARAMETER   (ML=1000)
      DOUBLE PRECISION DECSRC
      REAL        RADIUS, LATCEN, VX, ANG
      REAL        LRINC
      REAL        X(ML), Y(ML)
      REAL        FMIN, FMAX, LFMIN, LFMAX, FRAD, LFRAD
C ----------------------------------------------------------------------
C
C     Get the factor for V scaling to take into account 
C     foreshortening.
C
      LATCEN = 34.0 * RADDEG
      VX = COS( LATCEN - DECSRC ) / 0.9
C
      CALL PGSCR( 21, 0.7, 0.7, 0.7 ) 
C      CALL PGSCR( 21, 1.0, 1.0, 1.0 )
      CALL PGSLW( 1 )
      CALL PGSCI( 21 )
      CALL PGSFS( 2 )
C
C     Draw the circles.
C
      FMIN = ( GRIDMIN + SQRT( GRIDMIN**2 + GRIDW0**2 ) )
      LFMIN = LOG( FMIN )
      FMAX = ( GRIDMAX + SQRT( GRIDMAX**2 + GRIDW0**2 ) )
      LFMAX = LOG( FMAX )
      LRINC = ( LFMAX  - LFMIN ) / GRIDNR
C
C     To get the radius for the cell boundary, we must solve
C     and equation of the form ax**2 + bx + c = 0 with a=1.
C
      DO I = 1, GRIDNR + 1
         LFRAD = LFMIN + ( I - 1 ) * LRINC
         FRAD = EXP( LFRAD )
         RADIUS = 0.5 * ( FRAD - GRIDW0**2 / FRAD )
         DO IC = 1, ML
            ANG = TWOPI * IC / ( ML - 1 )
            X(IC) = RADIUS * COS( ANG )
            Y(IC) = VX * RADIUS * SIN( ANG )
         END DO
         CALL PGLINE( ML, X, Y )
      END DO
C
C     Draw the radial lines.
C
      DO I = 1, GRIDNT
         X(1) = GRIDMIN * COS( RADDEG * 360.0 * I / GRIDNT )
         X(2) = GRIDMAX * COS( RADDEG * 360.0 * I / GRIDNT )
         Y(1) = VX * GRIDMIN * SIN( RADDEG * 360.0 * I / GRIDNT )
         Y(2) = VX * GRIDMAX * SIN( RADDEG * 360.0 * I / GRIDNT )
         CALL PGLINE( 2, X, Y )
      END DO
C
      CALL PGSFS( 1 )
C
      RETURN
      END
