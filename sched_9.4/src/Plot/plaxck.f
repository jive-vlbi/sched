      SUBROUTINE PLAXCK( X1, X2, Y1, Y2, AX, EL, CH )
C
C     Routine for sched that update an element of
C     a coordinate object
C
      INCLUDE 'plot.inc'
C
      CHARACTER     CH, STRVAL*14
      INTEGER       LMIN, LMAX
      INTEGER       I, AX, EL, K, OFS
      REAL          X1, X2, Y1, Y2
C ----------------------------------------------------------------------
C
C     Set check limits
C
      K = PXYBCK(1)
      IF( AX .GT. 2 ) K = PXYBCK(2)
      LMIN = 0
      LMAX = ABS( PXSLIM(K,2) )
      IF( PXYTYP(K) .EQ. 'Wv' ) THEN
         LMAX = ( LMAX * 10**3 ) / 10**PXYWLE
      END IF
C
C     Set Value Dependency
C
      OFS = 0
      IF( EL .EQ. 1 ) THEN
         IF( PXSVAL(K, AX, 2) .GT. 0 .OR.
     1       PXSVAL(K, AX, 3) .GT. 0 ) OFS = 1
      END IF
C
C     Set the new Value of Axis element selected
C
      CALL PLVLCK( X1, X2, Y1, Y2, PXSBXM(1,AX), PXSBXM(2,AX),
     1             PXSBXM(3,AX), PXSBXM(4,AX), PXSVAL(K,AX,EL),
     2             PXSVAL(K,AX,1), LMIN, LMAX, PXSEXP(AX),
     3             PXYEXP, 1, OFS, EL, CH, STRVAL )
C
      IF( PXYTYP(K) .EQ. 'Sec' ) THEN
         PXSVAL(K,AX,1) = PXSVAL(K,AX,EL)
         PXYSEC = .FALSE.
      END IF
C
      IF( POPTYP(POPBCK) .EQ. 'UV' .AND. PLOVAL ) THEN
         DO 10 I=1,4
            K = PXYBCK(1)
            IF( I .GT. 2 ) K = PXYBCK(2)
            PXSVAL(K,I,EL) = PXSVAL(K,AX,EL)
 10      CONTINUE
         CALL PLSTXT( X1, X2, PXLTXT(3,1), PXLTXT(4,1),
     1                STRVAL, 1, .FALSE. )
         CALL PLSTXT( X1, X2, PXRTXT(3,1), PXRTXT(4,1),
     1                STRVAL, 1, .FALSE. )
         CALL PLSTXT( X1, X2, PYBTXT(3,1), PYBTXT(4,1),
     1                STRVAL, 1, .FALSE. )
         CALL PLSTXT( X1, X2, PYTTXT(3,1), PYTTXT(4,1),
     1                STRVAL, 1, .FALSE. )
      ENDIF
      
      RETURN
      END
