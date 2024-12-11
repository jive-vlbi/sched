      SUBROUTINE PLCELS( XMIN, XMAX, YMIN, YMAX, X1, X2, Y1, Y2 )
C
C     Routine for plot that make the axis value
C
C
      INCLUDE 'sched.inc'
C
      REAL              XMIN, XMAX, YMIN, YMAX, X1, X2, Y1, Y2
      REAL              DMIN, DMAX, RMIN, RMAX, DECM
      REAL              XW1, XW2, YW1, YW2, XLIM, YLIM
      REAL              XUNIT, YUNIT, XYSCALE, RDSCALE
C ----------------------------------------------------------------------
C
C     Rescale RA and DEC to original values
C
      DMIN = YMIN / 3600.D0
      DMAX = YMAX / 3600.D0
      RMIN = XMIN / 3600.D0
      RMAX = XMAX / 3600.D0
C
C     RA and DEC in Sky scale ( Degree )
C
      DECM =  ( ( DMIN + DMAX ) / 2.0 ) * RADDEG
      XUNIT = ABS( RMAX - RMIN ) * 15 * COS( DECM )
      YUNIT = ABS( DMAX - DMIN )
      RDSCALE = XUNIT / YUNIT
C
C     Set the plot viewport
C
      CALL PGQVSZ( 1, XW1, XW2, YW1, YW2 )
C
      XYSCALE = XW2 / YW2
      XLIM = 0.88
      YLIM = XLIM / XYSCALE
C
      X2 = XLIM
      Y2 = ( X2 * XYSCALE ) / RDSCALE 
C
C     Check for out of limit in Y dimension
C
      IF( Y2 .GT. YLIM ) THEN
        Y2 = YLIM
        X2 = Y2 / XYSCALE * RDSCALE
      ENDIF
C
      X1 = ( 1.0 - X2 ) / 2.0
      IF( X1 .LT. 0.08 ) X1 = 0.08
      X2 = X2 + X1
C
      Y1 = ( ( 1.0 - Y2 ) / 2.0 ) + 0.05
      Y2 = Y2 + Y1
C
      RETURN
      END
