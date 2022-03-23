      SUBROUTINE PLTMCK( XAXIS, RXMIN, RXMAX  )
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
C     Routine for plot that check if the default UT or LST axis
C     limit as changed. If the limits as not changed add TBORDER
C     to UT and check the consistence of min max value to plot
C     right limits.
C
      CHARACTER        XAXIS*(*)
      DOUBLE PRECISION RXMIN, RXMAX, TBORDER
      DOUBLE PRECISION HXMIN, HXMAX, HXDEF, CXMIN, CXMAX
C ----------------------------------------------------------------------
C
C     Make original limits to see if these as changed
C
      IF( XAXIS .EQ. 'UT' ) THEN
         CXMIN = ( 3600.D0 * PUTVAL(1,1) ) +
     1           (   60.D0 * PUTVAL(1,2) ) + PUTVAL(1,3)
         CXMAX = ( 3600.D0 * PUTVAL(2,1) ) +
     1           (   60.D0 * PUTVAL(2,2) ) + PUTVAL(2,3)
         HXMIN = PADAYS(1,1) * 86400.D0
         HXMAX = PADAYS(1,2) * 86400.D0
         HXDEF = PADAYS(1,3) * 86400.D0
C
      ELSE IF( XAXIS .EQ. 'LST' ) THEN
         CXMIN = ( 3600.D0 * PLSVAL(1,1) ) +
     1           (   60.D0 * PLSVAL(1,2) ) + PLSVAL(1,3)
         CXMAX = ( 3600.D0 * PLSVAL(2,1) ) +
     1           (   60.D0 * PLSVAL(2,2) ) + PLSVAL(2,3)
         HXMIN = PADAYS(3,1) * 86400.D0
         HXMAX = PADAYS(3,2) * 86400.D0
         HXDEF = PADAYS(3,3) * 86400.D0
C
      ELSE
         CXMIN = ( 3600.D0 * PSGVAL(1,1) ) +
     1           (   60.D0 * PSGVAL(1,2) ) + PSGVAL(1,3)
         CXMAX = ( 3600.D0 * PSGVAL(2,1) ) +
     1           (   60.D0 * PSGVAL(2,2) ) + PSGVAL(2,3)
         HXMIN = PADAYS(2,1) * 86400.D0
         HXMAX = PADAYS(2,2) * 86400.D0
         HXDEF = PADAYS(2,3) * 86400.D0
      END IF
C
C     Add Time Border if the limits as not changed
C     Also save in PXINI and PXEND the original value
C     to plot the vertical line mark of the start/end experiment
C
      IF( CXMIN .EQ. RXMIN .AND. CXMAX .EQ. RXMAX .AND.
     1    HXMIN .EQ. 0.D0  .AND. HXMAX .EQ. HXDEF ) THEN
         TBORDER = (( TEND - TFIRST ) / 20.D0 ) * 86400.D0
         PXINI  = RXMIN
         RXMIN  = RXMIN - TBORDER
         PXEND  = RXMAX + HXDEF
         RXMAX  = PXEND + TBORDER
      ELSE
         PXINI  = CXMIN
         PXEND  = CXMAX + HXDEF
         RXMIN  = RXMIN + HXMIN
         RXMAX  = RXMAX + HXMAX
      END IF
C
      RETURN
      END
