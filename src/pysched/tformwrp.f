      SUBROUTINE TFORMWRP ( OUTCH, RAD, TYPE, NSIGN, NDEG, NSEC, DELIM )
Cf2py intent(out) OUTCH
C
C     Wrapper function, to force f2py to generate a function which has 
C     memory space for enough output characters
C     
      CHARACTER  TYPE*1, DELIM*3, OUTCH*28, TFORM*28
      DOUBLE PRECISION  RAD
      INTEGER    NSIGN, NDEG, NSEC
      OUTCH = TFORM(RAD, TYPE, NSIGN, NDEG, NSEC, DELIM)
      RETURN
      END
