      SUBROUTINE PLQUAL( QUALITY, NLONG, NLAT, QLIMS, SCREEN )
C
C     Plot coutours of the quality measure when it has been
C     measured on a grid around a source (in SUVOPT).
C
      INTEGER    NLONG, NLAT
      REAL       QUALITY(NLONG,NLAT), QLIMS(4)
      REAL       QLONMIN, QLONMAX, QLATMIN, QLATMAX
      LOGICAL    SCREEN
C
      INTEGER    I, J, NLEVS
      PARAMETER  ( NLEVS = 6 )
      REAL       LEVS(NLEVS), LEVINC
      REAL       TR(6)
      REAL       QMIN, QMAX
      INTEGER    INTVAL, MININT, IMLEV
      CHARACTER  CLABEL*6
C ----------------------------------------------------------------------
      INTVAL = 40
      MININT = 10
C
      QLONMIN = QLIMS(1)
      QLONMAX = QLIMS(2)
      QLATMIN = QLIMS(3)
      QLATMAX = QLIMS(4)
C
C     Get the transform from cell to world coordinates.
C
      TR(2) = ( QLONMIN - QLONMAX ) / ( NLONG - 1 )
      TR(1) = QLONMAX - TR(2)
      TR(3) = 0.0
      TR(6) = ( QLATMIN - QLATMAX ) / ( NLAT - 1 )
      TR(4) = QLATMAX - TR(6)
      TR(5) = 0.0
C
C     Determine levels.  Try for even tenths starting just below
C     the maximum.
C
      QMAX = 0.0
      QMIN = 1.E10
      DO J = 1, NLAT
         DO I = 1, NLONG
            QMAX = MAX( QMAX, QUALITY(I,J) )
            QMIN = MIN( QMIN, QUALITY(I,J) )
         END DO
      END DO
C
      LEVINC = 0.02
      IMLEV = QMAX / LEVINC
      LEVS(NLEVS) = IMLEV * LEVINC
      DO I = NLEVS-1, 1, -1
         LEVS(I) = LEVS(I+1) - LEVINC
      END DO
      write(*,'(a,20f6.2)') 'plqual levs ', nlevs, levs
C
      CALL PGSCI( 14 )
      CALL PGCONT( QUALITY, NLONG, NLAT, 1, NLONG, 1, NLAT, LEVS,
     1             NLEVS, TR )
C
C     Label some contours:
C
      CALL PGSCH( 0.55 )
      IF( SCREEN ) THEN
         CALL PGSCI( 7 )
      ELSE
         CALL PGSCI( 14 )
      END IF
      DO I = 1, NLEVS
         WRITE( CLABEL, '( F4.2 )' ) LEVS(I)
         CALL PGCONL( QUALITY, NLONG, NLAT, 1, NLONG, 1, NLAT, LEVS(I),
     1             TR, CLABEL, INTVAL, MININT  )
      END DO
C
      RETURN
      END
