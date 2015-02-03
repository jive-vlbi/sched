C$Procedure      ZZWAHR ( SPICELIB private version of Newhalls' WAHR )
 
      SUBROUTINE ZZWAHR ( ET, DVNUT )
 
C$ Abstract
C
C     Calculates nutation angles delta psi and delta epsilon,  and
C     their rates of change, referred to the ecliptic of date, from
C     the wahr series (Table 1,'Proposal to the IAU Working Group
C     on Nutation', John M. Wahr and Martin L. Smith 1979)
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     NUTATIONS
C
C$ Declarations
 
      IMPLICIT NONE
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      DVNUT(4)
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris Time for which nutations are sought
C     DVNUT      O   Nutation angles and their rates.
C
C$ Detailed_Input
C
C     ET         is the epoch for which nutation angles are being
C                requested expressed in TDB seconds past the epoch
C                of J2000.
C
C$ Detailed_Output
C
C     DVNUT      are the nutation angles and their derivatives.
C                Following the notation on page 112 of the
C                Explanatory Supplement to the Astronomical
C                Almanac we have
C
C                DVNUT(1) = Psi------nutation in longitude (radians)
C                DVNUT(2) = Epsilon--nutation in obliquity (radians)
C                DVNUT(3) = dPsi/dt     (radians/second)
C                DVNUT(4) = dEpsilon/dt (radians/second)
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routine computes the angles required for computing the
C     transformation from the mean of date frame for the earth
C     to the true of date frame of the earth.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     Explanatory Supplement to the Astronomical Almanac edited
C     by P. Kenneth Siedelmann. (1992) (University Science
C     Books, Mill Valley CA) pp. 111-116
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 15-JUL-1997 (WLT)
C
C        This routine was adapted from a routine provided by
C        Skip Newhall.  Skip's notes indicate that he obtained this
C        from Jay Lieske and Mylse Standish.  The actual notes
C        from the original routine WAHR are given here.
C
C           Lieske 3/91.  NUTATION in the IAU J2000 system.  Univac
C           version obtained from Myles Standish, (subroutine WAHR)
C           who had obtained it from USNO.  Re-ordered terms to match
C           Astronomical Almanac 1984 table S23-S25 and corrected
C           the rate for dPsi in the 0 0 2 -2 2 term.  Eliminated
C           the equivalences, common block and added necessary SAVEs.
C           Corrected the fundamental angles (L, L', F, D, Node) to
C           match Almanac.
C
C        In the current routine the various angles L, L', F, D, and
C        Node (MG) are computed using the actual values given
C        in the Explanatory Supplement.
C
C        Note that there is an error in the Explanatory supplement
C        for the Node term.  The Explanatory Supplement (page 114) has
C
C          OMEGA = 135 degrees 2 minutes 40.280 seconds
C                +  etc.
C
C        The correct formulation should be:
C
C          OMEGA = 125 degrees 2 minutes 40.280 seconds
C                +  etc.
C
C        This is the value used in this routine.  The verification of
C        this error is courtesy of Myles Standish.
C
C
C-&
C
C     SPICELIB Functions
C
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      SPD
 
C
C     Parameters
C
C     NTERM is the number of SIN and COSINE terms used in the
C     computation of Delta Psi and Delta epsilon
C
      INTEGER               NTERM
      PARAMETER           ( NTERM = 106 )
 
      DOUBLE PRECISION      ZERO
      PARAMETER           ( ZERO  = 0.0D0 )
 
C
C     The parameters below stand for
C
C        revolutions
C        degrees
C        minutes
C        seconds
C        julian century
C        julian century ** 2
C        julian century ** 3
C
C     These parameters are needed for converting the quantities
C     on page 114 of the Explanatory supplement from revolutions,
C     degrees, minutes and seconds / century, century**2 and century**3
C     to degrees, degrees/day, degrees/(0.0001 days)**2 and
C     degress/(0.0001 days)**3.
C
      DOUBLE PRECISION      REV
      PARAMETER           ( REV   = 360.0D0           )
 
      DOUBLE PRECISION      DEG
      PARAMETER           ( DEG   = 1.0D0             )
 
      DOUBLE PRECISION      MIN
      PARAMETER           ( MIN   = 1.0D0 / 60.0D0    )
 
      DOUBLE PRECISION      SEC
      PARAMETER           ( SEC   = 1.0D0 / 3600.0D0  )
 
      DOUBLE PRECISION      CENT1
      PARAMETER           ( CENT1 = 36525.0D0         )
 
      DOUBLE PRECISION      CENT2
      PARAMETER           ( CENT2 = CENT1*CENT1*1.0D-8 )
 
      DOUBLE PRECISION      CENT3
      PARAMETER           ( CENT3 = CENT1*CENT2*1.0D-4 )
 
 
C
C     The next set of parameters is an enumeration of the various
C     angles needed in the computation of nutations.
C
      INTEGER               L
      PARAMETER           ( L      = 1 )
 
      INTEGER               LP
      PARAMETER           ( LP     = L      + 1 )
 
      INTEGER               F
      PARAMETER           ( F      = LP     + 1 )
 
      INTEGER               D
      PARAMETER           ( D      = F      + 1 )
 
      INTEGER               MG
      PARAMETER           ( MG     = D      + 1 )
 
C
C     Local Variables.
C
      DOUBLE PRECISION      CE
      DOUBLE PRECISION      CL
      DOUBLE PRECISION      COSANG
      DOUBLE PRECISION      ANGLE(5)
      DOUBLE PRECISION      ANGRT(5)
      DOUBLE PRECISION      ARG
      DOUBLE PRECISION      ARGRT
      DOUBLE PRECISION      DD
      DOUBLE PRECISION      DD2
      DOUBLE PRECISION      DDDJ
      DOUBLE PRECISION      DJ
      DOUBLE PRECISION      DPI
      DOUBLE PRECISION      DTWOPI
      DOUBLE PRECISION      FACTR
      DOUBLE PRECISION      ONEDAY
      DOUBLE PRECISION      RADIAN
      DOUBLE PRECISION      RASEC
      DOUBLE PRECISION      SINANG
      DOUBLE PRECISION      T
 
      DOUBLE PRECISION      D0
      DOUBLE PRECISION      D1
      DOUBLE PRECISION      D2
      DOUBLE PRECISION      D3
      DOUBLE PRECISION      F0
      DOUBLE PRECISION      F1
      DOUBLE PRECISION      F2
      DOUBLE PRECISION      F3
      DOUBLE PRECISION      L0
      DOUBLE PRECISION      L1
      DOUBLE PRECISION      L2
      DOUBLE PRECISION      L3
      DOUBLE PRECISION      LP0
      DOUBLE PRECISION      LP1
      DOUBLE PRECISION      LP2
      DOUBLE PRECISION      LP3
      DOUBLE PRECISION      MG0
      DOUBLE PRECISION      MG1
      DOUBLE PRECISION      MG2
      DOUBLE PRECISION      MG3
 
      INTEGER               I
      INTEGER               J
      INTEGER               MATRIX(9,NTERM)
 
      LOGICAL               FIRST
      SAVE
 
      DATA                  FIRST / .TRUE. /
C
C     Below are the coefficients for the various periods of the
C     nutation model.  There does not appear to be any particular reason
C     for the ordering selected.  The n'th row corresponds to the n'th
C     period listed above each data statement.
C
C>> Periods: 6798.4, 3399.2, 1305.5, 1095.2, 1615.7, 3232.9, 6786.3,
C             943.2,  182.6,  365.3,  121.7,  365.2,  177.8,  205.9,
C             173.3,  182.6,  386.0,   91.3,  346.6
C
      DATA ((MATRIX(I,J),I=1,9),J=1,19)/
     .      0,     0,     0,     0,     1, -171996, -1742, 92025,    89,
     .      0,     0,     0,     0,     2,    2062,     2,  -895,     5,
     .     -2,     0,     2,     0,     1,      46,     0,   -24,     0,
     .      2,     0,    -2,     0,     0,      11,     0,     0,     0,
     .     -2,     0,     2,     0,     2,      -3,     0,     1,     0,
     .      1,    -1,     0,    -1,     0,      -3,     0,     0,     0,
     .      0,    -2,     2,    -2,     1,      -2,     0,     1,     0,
     .      2,     0,    -2,     0,     1,       1,     0,     0,     0,
     .      0,     0,     2,    -2,     2,  -13187,   -16,  5736,   -31,
     .      0,     1,     0,     0,     0,    1426,   -34,    54,    -1,
     .      0,     1,     2,    -2,     2,    -517,    12,   224,    -6,
     .      0,    -1,     2,    -2,     2,     217,    -5,   -95,     3,
     .      0,     0,     2,    -2,     1,     129,     1,   -70,     0,
     .      2,     0,     0,    -2,     0,      48,     0,     1,     0,
     .      0,     0,     2,    -2,     0,     -22,     0,     0,     0,
     .      0,     2,     0,     0,     0,      17,    -1,     0,     0,
     .      0,     1,     0,     0,     1,     -15,     0,     9,     0,
     .      0,     2,     2,    -2,     2,     -16,     1,     7,     0,
     .      0,    -1,     0,     0,     1,     -12,     0,     6,     0/
 
C
C     Periods: 199.8, 346.6, 212.3, 119.6, 411.8, 131.7, 169.0, 329.8,
C              409.2, 388.3, 117.5,  13.7,  27.6,  13.6,   9.1,  31.8,
C               27.1,  14.8,  27.7
C
      DATA ((MATRIX(I,J),I=1,9),J=20,38)/
     .     -2,     0,     0,     2,     1,      -6,     0,     3,     0,
     .      0,    -1,     2,    -2,     1,      -5,     0,     3,     0,
     .      2,     0,     0,    -2,     1,       4,     0,    -2,     0,
     .      0,     1,     2,    -2,     1,       4,     0,    -2,     0,
     .      1,     0,     0,    -1,     0,      -4,     0,     0,     0,
     .      2,     1,     0,    -2,     0,       1,     0,     0,     0,
     .      0,     0,    -2,     2,     1,       1,     0,     0,     0,
     .      0,     1,    -2,     2,     0,      -1,     0,     0,     0,
     .      0,     1,     0,     0,     2,       1,     0,     0,     0,
     .     -1,     0,     0,     1,     1,       1,     0,     0,     0,
     .      0,     1,     2,    -2,     0,      -1,     0,     0,     0,
     .      0,     0,     2,     0,     2,   -2274,    -2,   977,    -5,
     .      1,     0,     0,     0,     0,     712,     1,    -7,     0,
     .      0,     0,     2,     0,     1,    -386,    -4,   200,     0,
     .      1,     0,     2,     0,     2,    -301,     0,   129,    -1,
     .      1,     0,     0,    -2,     0,    -158,     0,    -1,     0,
     .     -1,     0,     2,     0,     2,     123,     0,   -53,     0,
     .      0,     0,     0,     2,     0,      63,     0,    -2,     0,
     .      1,     0,     0,     0,     1,      63,     1,   -33,     0/
C
C     Periods: 27.4, 9.6,  9.1,  7.1, 13.8, 23.9, 6.9, 13.6, 27.0, 32.0,
C              31.7, 9.5, 34.8, 13.2, 14.2,  5.6, 9.6, 12.8, 14.8
C
      DATA ((MATRIX(I,J),I=1,9),J=39,57)/
     .     -1,     0,     0,     0,     1,     -58,    -1,    32,     0,
     .     -1,     0,     2,     2,     2,     -59,     0,    26,     0,
     .      1,     0,     2,     0,     1,     -51,     0,    27,     0,
     .      0,     0,     2,     2,     2,     -38,     0,    16,     0,
     .      2,     0,     0,     0,     0,      29,     0,    -1,     0,
     .      1,     0,     2,    -2,     2,      29,     0,   -12,     0,
     .      2,     0,     2,     0,     2,     -31,     0,    13,     0,
     .      0,     0,     2,     0,     0,      26,     0,    -1,     0,
     .     -1,     0,     2,     0,     1,      21,     0,   -10,     0,
     .     -1,     0,     0,     2,     1,      16,     0,    -8,     0,
     .      1,     0,     0,    -2,     1,     -13,     0,     7,     0,
     .     -1,     0,     2,     2,     1,     -10,     0,     5,     0,
     .      1,     1,     0,    -2,     0,      -7,     0,     0,     0,
     .      0,     1,     2,     0,     2,       7,     0,    -3,     0,
     .      0,    -1,     2,     0,     2,      -7,     0,     3,     0,
     .      1,     0,     2,     2,     2,      -8,     0,     3,     0,
     .      1,     0,     0,     2,     0,       6,     0,     0,     0,
     .      2,     0,     2,    -2,     2,       6,     0,    -3,     0,
     .      0,     0,     0,     2,     1,      -6,     0,     3,     0/
 
C
C     Periods: 7.1, 23.9, 14.7, 29.8, 6.9, 15.4, 26.9, 29.5, 25.6, 9.1,
C              9.4,  9.8, 13.7,  5.5, 7.2,  8.9, 32.6, 13.8, 27.8
 
      DATA ((MATRIX(I,J),I=1,9),J=58,76)/
     .      0,     0,     2,     2,     1,      -7,     0,     3,     0,
     .      1,     0,     2,    -2,     1,       6,     0,    -3,     0,
     .      0,     0,     0,    -2,     1,      -5,     0,     3,     0,
     .      1,    -1,     0,     0,     0,       5,     0,     0,     0,
     .      2,     0,     2,     0,     1,      -5,     0,     3,     0,
     .      0,     1,     0,    -2,     0,      -4,     0,     0,     0,
     .      1,     0,    -2,     0,     0,       4,     0,     0,     0,
     .      0,     0,     0,     1,     0,      -4,     0,     0,     0,
     .      1,     1,     0,     0,     0,      -3,     0,     0,     0,
     .      1,     0,     2,     0,     0,       3,     0,     0,     0,
     .      1,    -1,     2,     0,     2,      -3,     0,     1,     0,
     .     -1,    -1,     2,     2,     2,      -3,     0,     1,     0,
     .     -2,     0,     0,     0,     1,      -2,     0,     1,     0,
     .      3,     0,     2,     0,     2,      -3,     0,     1,     0,
     .      0,    -1,     2,     2,     2,      -3,     0,     1,     0,
     .      1,     1,     2,     0,     2,       2,     0,    -1,     0,
     .     -1,     0,     2,    -2,     1,      -2,     0,     1,     0,
     .      2,     0,     0,     0,     1,       2,     0,    -1,     0,
     .      1,     0,     0,     0,     2,      -2,     0,     1,     0/
 
C
C      Periods: 9.2,  9.3, 27.3, 10.1, 14.6,  5.8, 15.9, 22.5,  5.6,
C               7.3,  9.1, 29.3, 12.8,  4.7,  9.6, 12.7,  8.7, 23.8,
C              13.1
      DATA ((MATRIX(I,J),I=1,9),J=77,95)/
     .      3,     0,     0,     0,     0,       2,     0,     0,     0,
     .      0,     0,     2,     1,     2,       2,     0,    -1,     0,
     .     -1,     0,     0,     0,     2,       1,     0,    -1,     0,
     .      1,     0,     0,    -4,     0,      -1,     0,     0,     0,
     .     -2,     0,     2,     2,     2,       1,     0,    -1,     0,
     .     -1,     0,     2,     4,     2,      -2,     0,     1,     0,
     .      2,     0,     0,    -4,     0,      -1,     0,     0,     0,
     .      1,     1,     2,    -2,     2,       1,     0,    -1,     0,
     .      1,     0,     2,     2,     1,      -1,     0,     1,     0,
     .     -2,     0,     2,     4,     2,      -1,     0,     1,     0,
     .     -1,     0,     4,     0,     2,       1,     0,     0,     0,
     .      1,    -1,     0,    -2,     0,       1,     0,     0,     0,
     .      2,     0,     2,    -2,     1,       1,     0,    -1,     0,
     .      2,     0,     2,     2,     2,      -1,     0,     0,     0,
     .      1,     0,     0,     2,     1,      -1,     0,     0,     0,
     .      0,     0,     4,    -2,     2,       1,     0,     0,     0,
     .      3,     0,     2,    -2,     2,       1,     0,     0,     0,
     .      1,     0,     2,    -2,     0,      -1,     0,     0,     0,
     .      0,     1,     2,     0,     1,       1,     0,     0,     0/
 
C
C     Periods: 35.0, 13.6, 25.4, 14.2, 9.5, 14.2, 34.7, 32.8, 7.1, 4.8,
C              27.3
      DATA ((MATRIX(I,J),I=1,9),J=96,NTERM)/
     .     -1,    -1,     0,     2,     1,       1,     0,     0,     0,
     .      0,     0,    -2,     0,     1,      -1,     0,     0,     0,
     .      0,     0,     2,    -1,     2,      -1,     0,     0,     0,
     .      0,     1,     0,     2,     0,      -1,     0,     0,     0,
     .      1,     0,    -2,    -2,     0,      -1,     0,     0,     0,
     .      0,    -1,     2,     0,     1,      -1,     0,     0,     0,
     .      1,     1,     0,    -2,     1,      -1,     0,     0,     0,
     .      1,     0,    -2,     2,     0,      -1,     0,     0,     0,
     .      2,     0,     0,     2,     0,       1,     0,     0,     0,
     .      0,     0,     2,     4,     2,      -1,     0,     0,     0,
     .      0,     1,     0,     1,     0,       1,     0,     0,     0/
 
 
      IF (FIRST) THEN
 
         FIRST  = .FALSE.
         DPI    = PI()
         DTWOPI = TWOPI()
         RADIAN =  180.0D0 / DPI
         RASEC  = 3600.0D0 * RADIAN
         FACTR  =    1.0D4 * RASEC
         ONEDAY = SPD()
 
 
 
C
C        The following values are direct conversions to degrees from
C        page 114 of the Explanatory Supplement to the Astronomical
C        Almanac.
C
C        L0 through L3 are the coefficients for l---the mean longitude
C        of the Moon minus the mean longitude of the Moon's perigee.
C        Units for the various terms:
C
C           L0      degrees
C           L1      degrees/day
C           L2      degrees/(0.0001 days)**2
C           L3      degrees/(0.0001 days)**3
C
         L0 =                134.0D0*DEG + 57.0D0*MIN + 46.733D0*SEC
         L1 =(1325.0D0*REV + 198.0D0*DEG + 52.0D0*MIN +  2.633D0*SEC)
     .                       / CENT1
         L2 =   31.310D0*SEC / CENT2
         L3 =    0.064D0*SEC / CENT3
 
 
 
C
C        LP0 through LP3 are the coefficients for l'---the mean
C        longitude of the Sun minus the mean longitude of the Sun's
C        perigee. Units for the various terms:
C
C           LP0      degrees
C           LP1      degrees/day
C           LP2      degrees/(0.0001 days)**2
C           LP3      degrees/(0.0001 days)**3
C
         LP0 =               357.0D0*DEG + 31.0D0*MIN + 39.804D0*SEC
         LP1 = (99.0D0*REV + 359.0D0*DEG +  3.0D0*MIN +  1.224D0*SEC)
     .                        / CENT1
         LP2 =   -0.577D0*SEC / CENT2
         LP3 =   -0.012D0*SEC / CENT3
 
 
C
C        F0 through F3 are the coefficients for F---the mean longitude
C        of the Moon minus the mean longitude of the Moon's node. Units
C        for the various terms:
C
C           F0      degrees
C           F1      degrees/day
C           F2      degrees/(0.0001 days)**2
C           F3      degrees/(0.0001 days)**3
C
         F0 =                 93.0D0*DEG + 16.0D0*MIN + 18.877D0*SEC
         F1 = (1342.0D0*REV + 82.0D0*DEG +  1.0D0*MIN +  3.137D0*SEC)
     .                        / CENT1
         F2 =   -13.257D0*SEC / CENT2
         F3 =     0.011D0*SEC / CENT3
 
C
C        D0 through D3 are the coefficients for D---the mean longitude
C        of the Moon minus the mean longitude of the Sun. Units
C        for the various terms:
C
C           D0      degrees
C           D1      degrees/day
C           D2      degrees/(0.0001 days)**2
C           D3      degrees/(0.0001 days)**3
C
         D0 =                297.0D0*DEG + 51.0D0*MIN +  1.307D0*SEC
         D1 =(1236.0D0*REV + 307.0D0*DEG +  6.0D0*MIN + 41.328D0*SEC)
     .                       / CENT1
         D2 =   -6.891D0*SEC / CENT2
         D3 =    0.019D0*SEC / CENT3
 
C
C        MG0 through MG3 are the coefficients for Omega---the longitude
C        of the mean ascending node of the lunar orbit on the ecliptic
C        measured from the mean equinox of date.  NOTE: The constant
C        term MG0 is correct.  The value
C               o
C            135 02' 40".280
C
C        given in the Explanatory Supplement page 114 has a typo.  The
C        correct value is the one used here:
C
C               o
C            125 02' 40".280
C
C           MG0      degrees
C           MG1      degrees/day
C           MG2      degrees/(0.0001 days)**2
C           MG3      degrees/(0.0001 days)**3
C
         MG0 =               125.0D0*DEG + 2.0D0*MIN + 40.280D0*SEC
         MG1 =-( 5.0D0*REV + 134.0D0*DEG + 8.0D0*MIN + 10.539D0*SEC)
     .                        / CENT1
         MG2 =    7.455D0*SEC / CENT2
         MG3 =    0.008D0*SEC / CENT3
 
 
 
      END IF
 
C
C     Compute all of the various time components.  DJ is the delta
C     in the Julian date from the J2000 epoch.
C
      DJ   = ET/ONEDAY
      DD   = DJ/1.0D4
      DDDJ = DD/1.0D4
      DD2  = DD*DD
      T    = DJ/365250.D0
 
C
C     Now compute all of the various angles and their rates
C     at the current epoch
C
      ANGLE(L)  = L0   +  DJ*L1   +  (L2  + DD*L3 )*DD2
      ANGLE(LP) = LP0  +  DJ*LP1  +  (LP2 + DD*LP3)*DD2
      ANGLE(F)  = F0   +  DJ*F1   +  (F2  + DD*F3 )*DD2
      ANGLE(D)  = D0   +  DJ*D1   +  (D2  + DD*D3 )*DD2
      ANGLE(MG) = MG0  +  DJ*MG1  +  (MG2 + DD*MG3)*DD2
 
      ANGRT(L ) = L1   +  DDDJ * (2.0D0*L2  + 3.0D0*DD*L3 )
      ANGRT(LP) = LP1  +  DDDJ * (2.0D0*LP2 + 3.0D0*DD*LP3)
      ANGRT(F ) = F1   +  DDDJ * (2.0D0*F2  + 3.0D0*DD*F3 )
      ANGRT(D ) = D1   +  DDDJ * (2.0D0*D2  + 3.0D0*DD*D3 )
      ANGRT(MG) = MG1  +  DDDJ * (2.0D0*MG2 + 3.0D0*DD*MG3)
 
C
C     Wrap all of the angles and rates to range from 0 to 360, then
C     convert to radians.
C
      DO J = 1,5
         ANGLE(J) = DMOD(ANGLE(J), 360.D0)
         ANGRT(J) = DMOD(ANGRT(J), 360.D0)
 
         ANGLE(J) = ANGLE(J)/RADIAN
         ANGRT(J) = ANGRT(J)/RADIAN
      END DO
C
C     Zero out the components of the nutation array
C
      DO J = 1,4
         DVNUT(J) = ZERO
      END DO
C
C     Now we accumulate the various terms of Delta Psi and Delta
C     epsilon as expressed on page 115 of the Green Book
C     (Explanatory Supplement to the Astronomical Almanac).
C
      DO I = 1,NTERM
 
         ARG   = ZERO
         ARGRT = ZERO
 
         DO J = 1,5
            IF ( MATRIX(J,I) .NE. 0 ) THEN
               ARG   = ARG   + MATRIX(J,I)*ANGLE(J)
               ARGRT = ARGRT + MATRIX(J,I)*ANGRT(J)
               ARG   = MOD(ARG, DTWOPI)
            END IF
         END DO
 
         CL = MATRIX(6,I)
 
         IF ( MATRIX(7,I) .NE. 0 ) THEN
            CL = CL + MATRIX(7,I)*T
         END IF
 
         CE = MATRIX(8,I)
 
         IF ( MATRIX(9,I) .NE. 0 ) THEN
            CE = CE + MATRIX(9,I)*T
         END IF
 
         COSANG   = DCOS(ARG)
         SINANG   = DSIN(ARG)
 
         DVNUT(1) = DVNUT(1) + CL*SINANG/FACTR
         DVNUT(2) = DVNUT(2) + CE*COSANG/FACTR
         DVNUT(3) = DVNUT(3) + CL*COSANG*ARGRT/FACTR
         DVNUT(4) = DVNUT(4) - CE*SINANG*ARGRT/FACTR
 
      END DO
 
C
C     Finally convert DVNUT(3) and DVNUT(4) to radians/second
C
      DVNUT(3) = DVNUT(3)/ONEDAY
      DVNUT(4) = DVNUT(4)/ONEDAY
 
 
      RETURN
      END
