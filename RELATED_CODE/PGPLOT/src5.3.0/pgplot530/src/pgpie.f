C*PGPIE -- fill a sector of a circle
C%void cpgpie(float xcent, float ycent, float radius, float angl1, \
C% float angl2);
C+
      SUBROUTINE PGPIE (XCENT, YCENT, RADIUS, ANGL1, ANGL2)
      REAL XCENT, YCENT, RADIUS, ANGL1, ANGL2
C
C Fill a sector of a circle (in world-coordinate space), using current
C fill-area attributes. The sector is bounded by two radii and the
C included arc of a circle, and is specified by the coordinates of the
C center of the circle, the radius, and two angles; ANG1 is the angle
C between the x-axis and the radius to the starting point of the arc,
C and ANG2 is the angle between the x-axis and the radius to the end
C point of the arc. To draw  a complete circle, set ANG2 = ANG1 + 360.0,
C or use PGCIRC.
C
C Arguments:
C  XCENT  (input)  : world x-coordinate of the center of the circle.
C  YCENT  (input)  : world y-coordinate of the center of the circle.
C  RADIUS (input)  : radius of circle (world coordinates).
C  ANGL1  (input)  : starting angle (degrees), measured counterclockwise
C                    from the x-axis
C  ANGL2  (input)  : ending angle, measured similarly.
C--
C 10-Feb-1999 - [TJP].
C-----------------------------------------------------------------------
      INCLUDE 'pgplot.inc'
      INTEGER MAXPTS
      PARAMETER (MAXPTS=181)
C
      INTEGER NPTS, I, RADPIX
      REAL ANGLE, DA
      REAL X(MAXPTS), Y(MAXPTS)
C
      RADPIX = NINT(ABS(RADIUS*MAX(PGXSCL(PGID), PGYSCL(PGID))))
      NPTS = MAX(8, MIN(MAXPTS-1, RADPIX))
      DA = (ANGL2-ANGL1)/REAL(NPTS-1)
      DO 10 I=1,NPTS
         ANGLE = (ANGL1 + (I-1)*DA)/57.3
         X(I) = XCENT + RADIUS*COS(ANGLE)
         Y(I) = YCENT + RADIUS*SIN(ANGLE)
   10 CONTINUE
      NPTS = NPTS+1
      X(NPTS) = XCENT
      Y(NPTS) = YCENT
      CALL PGPOLY(NPTS, X, Y)
C-----------------------------------------------------------------------
      END
