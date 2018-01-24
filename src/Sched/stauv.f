      SUBROUTINE STAUV( ISCN, ISTA, U1, U2, V1, V2 )
C
C     Subroutine for uv plotting in SCHED that gets the station
C     UV for data about to be plotted.
C
C     Plot the UVs that would be used for mapping.  This has 
C     opposite sign of U from what would be used to imagine
C     easily where stations are on the ground (HAZININE convention).
C
C     Note that these U and V are in kilometers.
C
C     The results here might be thought of as for a baseline from the
C     center of the Earth to the station.  Actual baseline parameters
C     can be calculated from differences.
C
      INCLUDE 'sched.inc'
C
      INTEGER     ISCN, ISTA, CATSTA, CATSRC
      REAL        U1, U2, V1, V2
Cf2py intent(out) U1
Cf2py intent(out) U2
Cf2py intent(out) V1
Cf2py intent(out) V2
      REAL        HAR, CH, SH, CD, SD, UXY
C -------------------------------------------------------------------
C     Get station number in catalog.
C
      CATSTA = STANUM(ISTA)
      CATSRC = SRCNUM(ISCN)
C
C     Get the trig terms.
C
      CD = COS( DECP(CATSRC) )
      SD = SIN( DECP(CATSRC) )
      UXY = SQRT( XPOS(CATSTA)**2 + YPOS(CATSTA)**2 )
C
C     UV at scan start.
C
      HAR = HA1(ISCN,ISTA) * RADHR
      CH = COS( HAR )
      SH = SIN( HAR )
      U1 =  (-UXY) * SH / 1000.0 
      V1 = ( ZPOS(CATSTA) * CD - UXY * SD * CH ) / 1000.0
C
C     UV at scan stop.
C
      HAR = HA2(ISCN,ISTA) * RADHR
      CH = COS( HAR )
      SH = SIN( HAR )
      U2 = (-UXY) * SH / 1000.0
      V2 = (  ZPOS(CATSTA) * CD - UXY * SD * CH) / 1000.0
C
      RETURN
      END
