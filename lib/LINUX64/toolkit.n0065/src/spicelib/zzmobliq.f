C$Procedure   ZZMOBLIQ   ( Mean obliquity of date )
 
      SUBROUTINE ZZMOBLIQ ( ET, MOB, DMOB )
 
C$ Abstract
C
C     Return the mean obliquity of the ecliptic, and its time
C     derivative, at a specified epoch.
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
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
C     GEOMETRY
C
C$ Declarations
 
      IMPLICIT NONE
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      MOB
      DOUBLE PRECISION      DMOB
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris time, in seconds past J2000.
C     MOB        O   Mean obliquity of the ecliptic at ET.
C     DMOB       O   Time derivative of the mean obliquity.
C
C$ Detailed_Input
C
C     ET             is the epoch at which the obliquity of the ecliptic
C                    is to be computed.  ET is barycentric dynamical
C                    time, expressed as seconds past J2000.
C
C$ Detailed_Output
C
C     MOB            is the mean obliquity of the ecliptic at epoch ET.
C                    The mean obliquity of the ecliptic is the
C                    inclination of the ecliptic of date to the mean
C                    Earth equator of date.  Output units are radians.
C
C     DMOB           is the time derivative of MOB at ET, expressed in
C                    radians per second.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The expression for mean is obliquity is
C
C                          ''        ''            ''         2
C        MOBLIQ   =   84381 .448 - 46 .8150 * T - 0 .00059 * T
C
C                      ''          3
C                   + 0 .001813 * T
C
C     where T indicates Julian centuries past J2000.  This is from
C     equation 5-153 of reference [2].
C
C$ Examples
C
C     See the routine ENUTAT for an example of usage.
C
C$ Restrictions
C
C     1)  This is a preliminary version of the routine.
C
C$ Literature_References
C
C     [1] "Explanatory Supplement to the Astronomical Almanac"
C          edited by P. Kenneth Seidelmann. University Science
C          Books, 20 Edgehill Road, Mill Valley, CA 94941 (1992)
C
C     [2] "Section 5, Geocentric Space-Fixed Position, Velocity, and
C         Acceleration Vectors of Tracking Station" by T. D. Moyer.
C         Draft of JPL Publication documenting the JPL navigation
C         program "Regres."
C
C
C$ Author_and_Institution
C
C     W.L. Taber         (JPL)
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0 18-JUL-1997 (WLT)
C
C        Adapted Nat'routine to private version making output
C        rate be radians/sec.
C
C-    Beta Version 1.0.0, 29-SEP-1996 (NJB)
C
C-&
 
C$ Index_Entries
C
C     compute mean obliquity of date of the ecliptic
C
C-&
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      RPD
 
C
C     Local parameters
C
C
C     Coefficients for the mean obliquity:
C
      DOUBLE PRECISION      C0
      PARAMETER           ( C0 = 84381.448D0 )
 
      DOUBLE PRECISION      C1
      PARAMETER           ( C1 = -46.8150D0 )
 
      DOUBLE PRECISION      C2
      PARAMETER           ( C2 = -0.00059D0 )
 
      DOUBLE PRECISION      C3
      PARAMETER           ( C3 = 0.001813D0 )
 
C
C     Local variables
C
      DOUBLE PRECISION      T
      DOUBLE PRECISION      RAD
      DOUBLE PRECISION      YEAR
      DOUBLE PRECISION      PERSEC
 
      LOGICAL               FIRST
      SAVE
 
      DATA                  FIRST / .TRUE. /
 
      IF ( FIRST ) THEN
 
         FIRST  = .FALSE.
         YEAR   = JYEAR()
         RAD    = RPD  ()
         PERSEC = 1.0D0 / ( YEAR * 100.0D0 )
 
      END IF
 
C
C     Convert the input epoch to Julian centuries past J2000:
C
      T  =  ( ET / YEAR )  / 100.D0
 
C
C     Compute the obliquity at epoch.  The polynomial yields arcseconds;
C     convert the units to radians.
C
      MOB  = ( RAD / 3.6D3 ) * (  C0 + T*( C1   + T*( C2 + T*C3 ) )  )
 
      DMOB = ( RAD / 3.6D3 ) * (  C1 + T*( 2*C2 + T*3*C3 )  ) * PERSEC
 
      END
