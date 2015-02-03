C$Procedure   ZZEPRC76   ( Earth precession, 1976 IAU model )
 
      SUBROUTINE ZZEPRC76 ( ET, PRECXF )
      IMPLICIT NONE
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Compute the state transformation matrix implementing the IAU 1876
C     precession model.
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
C     ROTATION
C
C$ Keywords
C
C     FRAMES
C     GEOMETRY
C     MATRIX
C     PRIVATE
C     TRANSFORMATION
C
C$ Declarations
 
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      PRECXF ( 6, 6 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Ephemeris time, in seconds past J2000 TDB.
C     PRECXF     O   Precession state transformation matrix at ET.
C
C$ Detailed_Input
C
C     ET             is the epoch at which the precession matrix is
C                    to be computed.  ET is barycentric dynamical time,
C                    expressed as seconds past J2000.
C
C$ Detailed_Output
C
C     PRECXF         is a 6x6 matrix that transforms states from the
C                    J2000 frame to the mean equator and equinox frame
C                    of the earth at the epoch ET. 
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
C     According to reference [2], the precession model used in this
C     routine is that used in the JPL navigation program "Regres."
C
C     The precession matrix is defined using the Euler angles
C
C        zeta ,   z ,  and theta
C            A     A            A
C
C
C     Equation (5-147) of [2] gives the matrix determined by these
C     angles as
C
C        A  =  [ -z   ]   [ theta  ]   [ -zeta  ]
C                  A   3         A  2         A  3
C
C
C     Formulas for the Euler angles are from [2], equation
C     (5-143):
C                                              2                3
C         zeta   =  2306".2181*T  +  0".30188*T   +  0".017998*T
C             A
C
C
C                                              2                3
C         z      =  2306".2181*T  +  1".09468*T   +  0".018203*T
C          A
C
C
C                                              2                3
C         theta  =  2004".3109*T  -  0".42665*T   -  0".041833*T
C              A
C
C$ Examples
C
C     1) Convert a state vector S from J2000 to Earth Mean equator and
C        equinox of date coordinates at epoch ET.  Call the resulting
C        vector SMOD.
C
C           CALL ZZEPRC76 ( ET,     PRECXF        )
C           CALL MXVG     ( PRECXF, S, 6, 6, SMOD )
C
C$ Restrictions
C
C     1) This is a SPICE private routine; the routine is subject to
C        change without notice.  User applications should not call this
C        routine.
C
C     2) Though reference [1] does not specify limitations on the range
C        of valid time inputs for this precession model, the fact that
C        the rotation angles used in the model are defined by
C        polynomials implies that the model is not valid for all time.
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
C     N.J. Bachman       (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.0.0, 18-DEC-2004 (NJB)
C
C-&
 
C$ Index_Entries
C
C     IAU 1976 earth precession transformation
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
      DOUBLE PRECISION      ZETA1
      PARAMETER           ( ZETA1  = 2306.2181D0 )
 
      DOUBLE PRECISION      ZETA2
      PARAMETER           ( ZETA2  = 0.30188D0   )
 
      DOUBLE PRECISION      ZETA3
      PARAMETER           ( ZETA3  = 0.017998D0  )
 
      DOUBLE PRECISION      Z1
      PARAMETER           ( Z1     = 2306.2181D0 )
 
      DOUBLE PRECISION      Z2
      PARAMETER           ( Z2     = 1.09468D0   )
 
      DOUBLE PRECISION      Z3
      PARAMETER           ( Z3     = 0.018203D0  )
 
      DOUBLE PRECISION      THETA1
      PARAMETER           ( THETA1 = 2004.3109D0 )
 
      DOUBLE PRECISION      THETA2
      PARAMETER           ( THETA2 = -0.42665D0 )
 
      DOUBLE PRECISION      THETA3
      PARAMETER           ( THETA3 = -0.041833D0 )
 
C
C     Local variables
C
      DOUBLE PRECISION      CENT
      DOUBLE PRECISION      DTHETA
      DOUBLE PRECISION      DZ
      DOUBLE PRECISION      DZETA
      DOUBLE PRECISION      EULANG ( 6 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      T
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TS
      DOUBLE PRECISION      Z
      DOUBLE PRECISION      ZETA
 
 
C
C     No check-in required; this routine does not participate in
C     SPICELIB error handling.
C
C
C     Compute the precession angles first.  The time argument has
C     units of Julian centuries.  The polynomial expressions yield
C     angles in units of arcseconds prior to scaling.  After scaling,
C     the angles are in units of radians.
C     
      CENT    =  JYEAR() * 100.D0
      T       =  ET    / CENT
      SCALE   =  RPD() / 3600.D0
 
      ZETA    =  T  * (  ZETA1  + T*(   ZETA2  +   T*ZETA3  )  ) * SCALE
      Z       =  T  * (  Z1     + T*(   Z2     +   T*Z3     )  ) * SCALE
      THETA   =  T  * (  THETA1 + T*(   THETA2 +   T*THETA3 )  ) * SCALE

      TS      =  1.D0 / CENT
      DZETA   =  TS * (  ZETA1  + T*( 2*ZETA2  + 3*T*ZETA3  )  ) * SCALE
      DZ      =  TS * (  Z1     + T*( 2*Z2     + 3*T*Z3     )  ) * SCALE
      DTHETA  =  TS * (  THETA1 + T*( 2*THETA2 + 3*T*THETA3 )  ) * SCALE
 
C
C     Now compute the precession matrix.
C
      EULANG(1)  =  -Z
      EULANG(2)  =   THETA
      EULANG(3)  =  -ZETA
      EULANG(4)  =  -DZ
      EULANG(5)  =   DTHETA
      EULANG(6)  =  -DZETA
 
      CALL EUL2XF  ( EULANG, 3, 2, 3, PRECXF )
 
      END
