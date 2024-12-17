      SUBROUTINE PLRDWN( XW, YW, RXMIN, RXMAX, RYMIN, RYMAX )
C
      INCLUDE 'plot.inc'
C
C     Routine for SCHED that return the limits of a selected
C     window in the RA - DEC viewport.
C
      REAL              XW(*), YW(*), RXX(2), XSTEP, YSTEP, DUMMY
      DOUBLE PRECISION  RXMIN, RXMAX, RYMIN, RYMAX
C -------------------------------------------------------------------
C
C     X and Y step size
C
      XSTEP = ( RXMAX - RXMIN ) / ( PZMWIN(2) - PZMWIN(1) )
      YSTEP = ( RYMAX - RYMIN ) / ( PZMWIN(4) - PZMWIN(3) )
C
C     check new X Y coordinate
C
      RXX(1) = ( PZMWIN(1) + PZMWIN(2) ) - XW(1)
      RXX(2) = ( PZMWIN(1) + PZMWIN(2) ) - XW(2)
      IF( RXX(1) .GT. RXX(2) ) THEN
         DUMMY = RXX(1)
         RXX(1) = RXX(2)
         RXX(2) = DUMMY
      END IF
C
      IF( YW(1) .GT. YW(2) ) THEN
         DUMMY = YW(1)
         YW(1) = YW(2)
         YW(2) = DUMMY
      END IF
C
C     New window limit in X and Y
C
      RXMIN = RXMIN + ( XSTEP * ( RXX(1) - PZMWIN(1) ) )
      RXMAX = RXMAX - ( XSTEP * ( PZMWIN(2) - RXX(2) ) )
C
      RYMIN = RYMIN + ( YSTEP * ( YW(1) - PZMWIN(3) ) )
      RYMAX = RYMAX - ( YSTEP * ( PZMWIN(4) - YW(2) ) )
C
      RETURN
      END
