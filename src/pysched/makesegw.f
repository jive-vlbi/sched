      SUBROUTINE MAKESEGW( JSCN, ISCN, LASTISCN, 
     1                     OKGEO, USEGEO, SEGELEV, STARTB, TGEOEND, 
     2                     LSCN, NTSEG, TSRC, IDUM, SIGMA, SELTYPE,
     3                     WSTA )
Cf2py intent(in) JSCN, ISCN, LASTISCN, OKGEO, USEGEO, SEGELEV, STARTB
Cf2py intent(in) TGEOEND, SIGMA
Cf2py intent(out) LSCN, NTSEG, TSRC, SELTYPE, WSTA
Cf2py intent(in, out) IDUM
      INCLUDE 'sched.inc'
      INTEGER            JSCN, ISCN, LSCN, LASTISCN(*)
      INTEGER            TSRC(MSEG), NTSEG, USEGEO(*), IDUM
      LOGICAL            OKGEO(*)
      REAL               SEGELEV(MAXSTA,MGEO)
      DOUBLE PRECISION   STARTB, TGEOEND, SIGMA(*)
      CHARACTER          SELTYPE(MSEG)*3, WSTA(MSEG)*3
      CALL MAKESEG( JSCN, ISCN, LASTISCN, 
     1              OKGEO, USEGEO, SEGELEV, STARTB, TGEOEND, 
     2              LSCN, NTSEG, TSRC, IDUM, SIGMA, SELTYPE,
     3              WSTA )
      PRINT *, "IGNORE THIS"
      RETURN
      END
