C*PGSHD1 -- shade area under a curve
C%void cpgshd1(int n, const float *x, const float *y, float base);
C+
      SUBROUTINE PGSHD1 (N, X, Y, BASE)
      INTEGER  N
      REAL     X(*), Y(*), BASE
C
C Shade the area between the curve y(x) and the horizontal baseline
C y = BASE. The curve y(x) is defined by a set of straight segments,
C as in PGLINE. The area is shaded using current color index and 
C fill-area attributes.
C
C Arguments:
C  N      (input)  : number of points defining the line; the line
C                    consists of (N-1) straight-line segments.
C                    N should be greater than 1 (if it is 1 or less,
C                    nothing will be drawn).
C  X      (input)  : world x-coordinates of the points.
C  Y      (input)  : world y-coordinates of the points.
C  BASE   (input)  : the ordinate (y-value) of the baseline.
C
C The dimension of arrays X and Y must be greater than or equal
C to N.
C--
C 26-Jun-1997: new routine (TJP).
C-----------------------------------------------------------------------
      INTEGER NP
      PARAMETER (NP=100)
      REAL XP(NP), YP(NP)
      INTEGER  I, I1, I2, K
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGSHD1')) RETURN
      IF (N.LT.2) RETURN
C
      CALL PGBBUF
C
C Split the curve into segments to fit in the work space.
C
      I2 = 1
 10   CONTINUE
      I1 = I2
      I2 = MIN(I1-1+NP-2, N)
      K = 0
C
C Fill polygon array for one segment.
C
      DO 20 I=I1,I2
         K = K+1
         XP(K) = X(I)
         YP(K) = Y(I)
 20   CONTINUE
      XP(K+1) = X(I2)
      XP(K+2) = X(I1)
      YP(K+1) = BASE
      YP(K+2) = BASE
      K = K+2
C
C Draw this segment.
C
      CALL PGPOLY(K, XP, YP)
C
C Check whether more segments are required,
C
      IF (I2.LT.N) GOTO 10
C
      CALL PGEBUF
      END
