C*PGSHD2 -- shade area between two curves
C%void cpgshd2(int n, const float *x, const float *y1, const float*y2);
C+
      SUBROUTINE PGSHD2 (N, X, Y1, Y2)
      INTEGER  N
      REAL     X(*), Y1(*), Y2(*)
C
C Shade the area between two curves, y1(x) and y2(x), both defined
C by polylines. The curves are defined by their values y1(i) and y2(i)
C at abscissae x(i), which are the same for both curves. The area
C is shaded using current color index and fill-area attributes.
C
C Arguments:
C  N      (input)  : number of points defining the curves; the curves
C                    each consist of (N-1) straight-line segments.
C                    N should be greater than 1 (if it is 1 or less,
C                    nothing will be drawn).
C  X      (input)  : world x-coordinates of the points.
C  Y1     (input)  : world y-coordinates of the points defining the
C                    first curve.
C  Y2     (input)  : world y-coordinates of the points defining the
C                    second curve.
C
C The dimension of arrays X, Y1 and Y2 must be greater than or equal
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
      IF (PGNOTO('PGSHD2')) RETURN
      IF (N.LT.2) RETURN
C
      CALL PGBBUF
C
C Split the curves into segments to fit in the work space.
C
      I2 = 1
 10   I1 = I2
      I2 = MIN(I1-1+NP/2, N)
      K = 0
C
C Fill polygon array for one segment.
C
      DO 20 I=I1,I2
         K = K+1
         XP(K) = X(I)
         YP(K) = Y1(I)
 20   CONTINUE
      DO 30 I=I2,I1,-1
         K = K+1
         XP(K) = X(I)
         YP(K) = Y2(I)
 30   CONTINUE
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
