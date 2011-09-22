C*PGARC -- draw an arc of a circle
C%void cpgarc(float xcent, float ycent, float radius, float angl1, \
C% float angl2);
C+
      SUBROUTINE PGARC (XCENT, YCENT, RADIUS, ANGL1, ANGL2)
      REAL XCENT, YCENT, RADIUS, ANGL1, ANGL2
C
C Draw an arc of a circle (in world-coordinate space), using current
C attributes. The arc is specified by the coordinates of the center
C of the circle, the radius, and two angles; ANG1 is the angle between
C the x-axis and the vector from the center of the circle to the starting
C point of the arc, and ANG2 is the angle between the x-axis and the vector
C from the center of the circle to the end point of the arc. To draw
C a complete circle, set ANG2 = ANG1 + 360.0.
C
C Arguments:
C  XCENT  (input)  : world x-coordinate of the center of the circle.
C  YCENT  (input)  : world y-coordinate of the center of the circle.
C  RADIUS (input)  : radius of circle (world coordinates).
C  ANGL1  (input)  : starting angle (degrees), measured counterclockwise
C                    from the x-axis
C  ANGL2  (input)  : ending angle, measured similarly.
C--
C 11-Feb-1999 - [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INTEGER MAXPTS
      PARAMETER (MAXPTS=180)
C
      INTEGER NPTS, I, RADPIX
      REAL ANGLE, DA
      REAL X(MAXPTS), Y(MAXPTS)
C
      RADPIX = NINT(ABS(RADIUS*MAX(PGXSCL(PGID), PGYSCL(PGID))))
      NPTS = MAX(8, MIN(MAXPTS, RADPIX))
      DA = (ANGL2-ANGL1)/REAL(NPTS-1)
      DO 10 I=1,NPTS
         ANGLE = (ANGL1 + (I-1)*DA)/57.3
         X(I) = XCENT + RADIUS*COS(ANGLE)
         Y(I) = YCENT + RADIUS*SIN(ANGLE)
   10 CONTINUE
      CALL PGLINE(NPTS, X, Y)
C-----------------------------------------------------------------------
      END
