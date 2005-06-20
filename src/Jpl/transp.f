      SUBROUTINE TRANSP (MATX)
c
c in-place transpose
c
      DOUBLE PRECISION MATX(3,3), SWAPD

      SWAPD = MATX(2,1)
      MATX(2,1) = MATX(1,2)
      MATX(1,2) = SWAPD
      SWAPD = MATX(3,1)
      MATX(3,1) = MATX(1,3)
      MATX(1,3) = SWAPD
      SWAPD = MATX(3,2)
      MATX(3,2) = MATX(2,3)
      MATX(2,3) = SWAPD
      RETURN
      END
