C$Procedure  SPHSD ( Spherical surface distance )
 
      DOUBLE PRECISION FUNCTION SPHSD (  RADIUS,  LONG1,  LAT1,
     .                                            LONG2,  LAT2  )
 
C$ Abstract
C
C     Return the distance between two points on a sphere, measured
C     along the shortest great circle arc connecting them.
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
 
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      LONG1
      DOUBLE PRECISION      LAT1
      DOUBLE PRECISION      LONG2
      DOUBLE PRECISION      LAT2
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     RADIUS     I   Radius of sphere.
C     LONG1,
C     LAT1       I   Longitude and latitude of first point in radians.
C     LONG2,
C     LAT2       I   Longitude and latitude of second point in radians.
C
C     The function returns the distance between the two input points,
C     measured along the shortest great circle arc connecting them.
C
C$ Detailed_Input
C
C     RADIUS         Radius of the sphere on which the points are
C                    located.
C
C     LONG1,
C     LAT1           Longitude and latitude of the first point.  The
C                    units are radians.
C
C     LONG2,
C     LAT2           Longitude and latitude of the second point. The
C                    units are radians.
C
C$ Detailed_Output
C
C     The function returns the distance between the two input points,
C     measured along the shortest great circle arc connecting them.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If RADIUS is negative, the error SPICE(INPUTOUTOFRANGE)
C         is signalled.  SPHSD is set to zero.  RADIUS may be zero;
C         this case is not treated as an exception.
C
C     2)  Latitudes out of the range [-pi/2, pi/2] are NOT treated
C         as errors, although they are not valid in the latitudinal
C         coordinate system and so may be considered to be exceptional
C         inputs.  All latitude values are used in the same way in the
C         computation, regardless of whether or not they are in range.
C         See the code for the equation used.
C
C     3)  Longitudes out of the range (-pi, pi] are NOT treated
C         as errors, although they are not valid in the latitudinal
C         coordinate system and so may be considered to be exceptional
C         inputs.  All longitude values are used in the same way in the
C         computation, regardless of whether or not they are in range.
C         See the code for the equation used.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     You may need to consider whether a spherical model is adequate
C     for your application; some bodies may be more accurately modelled
C     by an oblate or prolate spheroid, or by a triaxial ellipsoid.
C
C$ Examples
C
C     1)  To find the distance along a sphere of radius 1000 km between
C         the points at
C
C            longitude = 1.570796326794897D0  (pi/2) radians,
C            latitude  = 7.853981633974483D-1 (pi/4) radians
C
C         and
C
C            longitude = 0.0D0 radians,
C            latitude  = 7.853981633974483D-1 (pi/4) radians,
C
C         we could make the function call:
C
C            DIST = SPHSD ( 1.0D3,
C         .                 1.570796326794897D0, 7.853981633974483D-1,
C         .                 0.D0,                7.853981633974483D-1  )
C
C         The value of DIST should be
C
C            1.047197551196598D3,
C
C         which is (very, very close to) 1000 * pi/3.
C
C         The exact numbers used in this example were obtained using
C         VAX Fortran 77 on a VAX 11/780; different compilers and
C         systems may yield different results.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     H.A. Neilan     (JPL)
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 17-MAY-1994 (HAN)
C
C       If the value of the function RETURN is TRUE upon execution of
C       this module, this function is assigned a default value of
C       either 0, 0.0D0, .FALSE., or blank depending on the type of
C       the function.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C       Comment section for permuted index source lines was added
C       following the header.
C
C-    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     spherical surface distance
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      BRCKTD
 
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      SL1SL2
      DOUBLE PRECISION      COSANG
 
 
C
C     Check RETURN but do not check in unless an error is detected.
C
      IF ( RETURN() ) THEN
         SPHSD = 0.0D0
         RETURN
      END IF
 
C
C     Make sure that RADIUS is ok; check in only if it isn't.
C
      IF ( RADIUS .LT. 0 ) THEN
 
         SPHSD = 0.D0
 
         CALL CHKIN  ( 'SPHSD'                  )
         CALL SETMSG ( 'Radius was #.'          )
         CALL ERRDP  ( '#', RADIUS              )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)' )
         CALL CHKOUT ( 'SPHSD'                  )
         RETURN
 
      END IF
 
C
C     The usual equation for the distance between points, measured
C     along a great circle, is:
C
C                  -1
C       DIST  =  COS (   ( COS(LONG1-LONG2) * COS(LAT1) * COS(LAT2) )
C                      + (                    SIN(LAT1) * SIN(LAT2) )  )
C
C              * RADIUS
C
C     To arrive at this equation, we find the cartesian coordinates of
C     the input surface points and take the dot product of the two
C     points.
C
C     To save a trig function reference, however, we implement this
C     calculation slightly differently.
C
 
C
C     COSANG is the cosine of the angle between the two position
C     vectors.  We bracket COSANG 'tween -1 and 1 to make sure
C     round-off error doesn't take it out of the domain of arc
C     cosine...
C
      SL1SL2  =       SIN ( LAT1 ) * SIN ( LAT2 )
 
      COSANG  =       COS ( LONG1 - LONG2 )
     .           * (  COS ( LAT1  - LAT2  )  -  SL1SL2  )
     .           +    SL1SL2
 
      SPHSD   =       RADIUS * ACOS (  BRCKTD ( COSANG, -1.D0, 1.D0 )  )
 
      RETURN
      END
