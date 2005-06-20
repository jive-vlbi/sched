      DOUBLE PRECISION FUNCTION sla_DSEP (A1, B1, A2, B2)
*+
*     - - - - -
*      D S E P
*     - - - - -
*
*  Angle between two points on a sphere (double precision)
*
*  Given:
*     A1,B1    dp    spherical coordinates of one point
*     A2,B2    dp    spherical coordinates of the other point
*
*  (The spherical coordinates are RA,Dec, Long,Lat etc, in radians.)
*
*  The result is the angle, in radians, between the two points.  It
*  is always positive.
*
*  Called:  sla_DCS2C
*
*  P.T.Wallace   Starlink   April 1985
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION A1,B1,A2,B2

      INTEGER I
      DOUBLE PRECISION V1(3),V2(3),W



*  Convert coordinates from spherical to Cartesian
      CALL sla_DCS2C(A1,B1,V1)
      CALL sla_DCS2C(A2,B2,V2)

*  Modulus squared of half the difference vector
      W=0D0
      DO I=1,3
         W=W+(V1(I)-V2(I))**2
      END DO
      W=W/4D0

*  Angle between the vectors
      sla_DSEP=2D0*ATAN2(SQRT(W),SQRT(MAX(0D0,1D0-W)))

      END
