C$Procedure  EDLIMB   ( Ellipsoid Limb )
 
      SUBROUTINE EDLIMB ( A, B, C, VIEWPT, LIMB )
 
C$ Abstract
C
C     Find the limb of a triaxial ellipsoid, viewed from a specified
C     point.
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
C     ELLIPSES
C
C$ Keywords
C
C     ELLIPSE
C     ELLIPSOID
C     GEOMETRY
C     MATH
C
C$ Declarations
 
      INTEGER               UBEL
      PARAMETER           ( UBEL   =    9 )
 
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      VIEWPT (    3 )
      DOUBLE PRECISION      LIMB   ( UBEL )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     A          I   Length of ellipsoid semi-axis lying on the x-axis.
C     B          I   Length of ellipsoid semi-axis lying on the y-axis.
C     C          I   Length of ellipsoid semi-axis lying on the z-axis.
C     VIEWPT     I   Location of viewing point.
C     LIMB       O   Limb of ellipsoid as seen from viewing point.
C
C$ Detailed_Input
C
C     A,
C     B,
C     C              are the lengths of the semi-axes of a triaxial
C                    ellipsoid.  The ellipsoid is centered at the
C                    origin and oriented so that its axes lie on the
C                    x, y and z axes.  A, B, and C are the lengths of
C                    the semi-axes that point in the x, y, and z
C                    directions respectively.
C
C     VIEWPT         is a point from which the ellipsoid is viewed.
C                    VIEWPT must be outside of the ellipsoid.
C
C$ Detailed_Output
C
C     LIMB           is a SPICELIB ellipse that represents the limb of
C                    the ellipsoid.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the length of any semi-axis of the ellipsoid is
C         non-positive, the error SPICE(INVALIDAXISLENGTH) is signalled.
C         LIMB is not modified.
C
C     2)  If the length of any semi-axis of the ellipsoid is zero after
C         the semi-axis lengths are scaled by the reciprocal of the
C         magnitude of the longest semi-axis and then squared, the error
C         SPICE(DEGENERATECASE) is signalled.  LIMB is not modified.
C
C     3)  If the viewing point VIEWPT is inside the ellipse, the error
C         SPICE(INVALIDPOINT) is signalled.  LIMB is not modified.
C
C     4)  If the geometry defined by the input ellipsoid and viewing
C         point is so extreme that the limb cannot be found, the error
C         SPICE(DEGENERATECASE) is signalled.
C
C     5)  If the shape of the ellipsoid and the viewing geometry are
C         such that the limb is an excessively flat ellipsoid, the
C         limb may be a degenerate ellipse.  You must determine whether
C         this possibility poses a problem for your application.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The limb of a body, as seen from a viewing point, is the boundary
C     of the portion of the body's surface that is visible from that
C     viewing point.  In this definition, we consider a surface point
C     to be `visible' if it can be connected to the viewing point by a
C     line segment that doen't pass through the body.  This is a purely
C     geometrical definition that ignores the matter of which portions
C     of the surface are illuminated, or whether the view is obscured by
C     any additional objects.
C
C     If a body is modelled as a triaxial ellipsoid, the limb is always
C     an ellipse.  The limb is determined by its center, a semi-major
C     axis vector, and a semi-minor axis vector.
C
C     We note that the problem of finding the limb of a triaxial
C     ellipsoid is mathematically identical to that of finding its
C     terminator, if one makes the simplifying assumption that the
C     terminator is the limb of the body as seen from the vertex of the
C     umbra.  So, this routine can be used to solve this simplified
C     version of the problem of finding the terminator.
C
C$ Examples
C
C     1)  We'd like to find the apparent limb of Jupiter, corrected for
C         light time, as seen from a spacecraft's position at time ET.
C
C            C
C            C     Find the viewing point in Jupiter-centered
C            C     coordinates.  To do this, find the apparent position
C            C     of Jupiter as seen from the spacecraft and negate
C            C     this vector.  In this case we'll use light time
C            C     correction to arrive at the apparent limb. JSTAT is
C            C     the Jupiter's state (position and velocity) as seen
C            C     from the spacecraft.  SCPOS is the spacecraft's
C            C     position relative to Jupiter.
C            C
C                  CALL SPKEZ  ( JUPID,  ET, 'J2000', 'LT', SCID,
C                 .              SCSTAT, LT                       )
C
C                  CALL VMINUS ( SCSTAT, SCPOS )
C
C            C
C            C     Get Jupiter's semi-axis lengths...
C            C
C                  CALL BODVCD ( JUPID, 'RADII', 3, N, RAD )
C
C            C
C            C     ...and the transformation from J2000 to Jupiter
C            C     equator and prime meridian coordinates. Note that we
C            C     use the orientation of Jupiter at the time of
C            C     emission of the light that arrived at the
C            C     spacecraft at time ET.
C            C
C                  CALL BODMAT ( JUPID, ET-LT, TIPM )
C
C            C
C            C     Transform the spacecraft's position into Jupiter-
C            C     fixed coordinates.
C            C
C                  CALL MXV ( TIPM, SCPOS, SCPOS )
C
C            C
C            C     Find the apparent limb.  LIMB is a SPICELIB ellipse
C            C     representing the limb.
C            C
C                  CALL EDLIMB ( RAD(1), RAD(2), RAD(3), SCPOS, LIMB )
C
C            C
C            C     LCENTR, SMAJOR, and SMINOR are the limb's center,
C            C     semi-major axis of the limb, and a semi-minor axis
C            C     of the limb.  We obtain these from LIMB using the
C            C     SPICELIB routine EL2CGV ( Ellipse to center and
C            C     generating vectors ).
C            C
C                  CALL EL2CGV ( LIMB, CENTER, SMAJOR, SMINOR )
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.3.0, 23-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCLG call.  Updated header to refer to BODVCD instead
C        of BODVAR.
C
C-    SPICELIB Version 1.2.0, 06-OCT-1993 (NJB)
C
C        Declaration of unused local variable NEAR was removed.
C
C-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.1.0, 04-DEC-1990 (NJB)
C
C        Error message and description changed for non-positive
C        axis length error.
C
C-    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     ellipsoid limb
C
C-&
 
 
 
C$ Revisions
C
C-    SPICELIB Version 1.3.0, 23-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCLG call.  Updated header to refer to BODVCD instead
C        of BODVAR.
C
C-    SPICELIB Version 1.2.0, 06-OCT-1993 (NJB)
C
C        Declaration of unused local variable NEAR was removed.
C
C-    SPICELIB Version 1.1.0, 04-DEC-1990 (NJB)
C
C        Error message and description changed for non-positive
C        axis length error.  The former message and description did
C        not match, and the description was incorrect:  it described
C        `zero-length', rather than `non-positive' axes as invalid.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               UBPL
      PARAMETER           ( UBPL   =    4 )
 
 
C
C     Local variables
C
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      LPLANE ( UBPL )
      DOUBLE PRECISION      NORMAL (    3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SCLA
      DOUBLE PRECISION      SCLA2
      DOUBLE PRECISION      SCLB
      DOUBLE PRECISION      SCLB2
      DOUBLE PRECISION      SCLC
      DOUBLE PRECISION      SCLC2
      DOUBLE PRECISION      TMPEL  ( UBEL )
      DOUBLE PRECISION      V      (    3 )
 
      LOGICAL               FOUND
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'EDLIMB' )
      END IF
 
C
C     The semi-axes must have positive length.
C
      IF (       ( A .LE. 0.D0 )
     .     .OR.  ( B .LE. 0.D0 )
     .     .OR.  ( C .LE. 0.D0 )   )   THEN
 
         CALL SETMSG ( 'Semi-axis lengths:  A = #, B = #, C = #. ' )
         CALL ERRDP  ( '#', A                                      )
         CALL ERRDP  ( '#', B                                      )
         CALL ERRDP  ( '#', C                                      )
         CALL SIGERR ( 'SPICE(INVALIDAXISLENGTH)'                  )
         CALL CHKOUT ( 'EDLIMB'                                    )
         RETURN
 
      END IF
 
C
C     Scale the semi-axes lengths for better numerical behavior.
C     If squaring any one of the scaled lengths causes it to
C     underflow to zero, we cannot continue the computation. Otherwise,
C     scale the viewing point too.
C
      SCALE  =  MAX ( DABS(A), DABS(B), DABS(C) )
 
      SCLA   =  A / SCALE
      SCLB   =  B / SCALE
      SCLC   =  C / SCALE
 
      SCLA2  =  SCLA**2
      SCLB2  =  SCLB**2
      SCLC2  =  SCLC**2
 
      IF (       ( SCLA2  .EQ.  0.D0 )
     .     .OR.  ( SCLB2  .EQ.  0.D0 )
     .     .OR.  ( SCLC2  .EQ.  0.D0 )   )   THEN
 
         CALL SETMSG ( 'Semi-axis too small:  A = #, B = #, C = #. ' )
         CALL ERRDP  ( '#', A                                        )
         CALL ERRDP  ( '#', B                                        )
         CALL ERRDP  ( '#', C                                        )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                       )
         CALL CHKOUT ( 'EDLIMB'                                      )
         RETURN
 
      END IF
 
      CALL VSCL ( 1.0D0 / SCALE, VIEWPT, V )
 
C
C     The viewing point must be outside of the ellipsoid.  LEVEL is the
C     constant of the level surface that V lies on.  The ellipsoid
C     itself is the level surface corresponding to LEVEL = 1.
C
      LEVEL   =     ( V(1)**2 / SCLA2 )
     .           +  ( V(2)**2 / SCLB2 )
     .           +  ( V(3)**2 / SCLC2 )
 
      IF ( LEVEL .LT. 1.D0 ) THEN
 
         CALL SETMSG ( 'Viewing point is inside the ellipsoid.' )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                  )
         CALL CHKOUT ( 'EDLIMB'                                 )
         RETURN
 
      END IF
 
C
C     Find a normal vector for the limb plane.
C
C     To compute this vector, we use the fact that the surface normal at
C     each limb point is orthogonal to the line segment connecting the
C     viewing point and the limb point.   Let the notation
C
C        < a, b >
C
C     indicate the dot product of the vectors a and b.  If we call the
C     viewing point V and the limb point X, then
C
C
C
C                            X(1)         X(2)         X(3)
C        0  =   < V - X,  ( -------- ,   -------- ,   --------  )  >
C                                2           2             2
C                            SCLA        SCLB          SCLC
C
C
C                            X(1)         X(2)         X(3)
C           =   <   V,    ( -------- ,   -------- ,   --------  )  >
C                                2           2             2
C                            SCLA        SCLB          SCLC
C
C
C                            X(1)         X(2)         X(3)
C            - <   X,    ( -------- ,   -------- ,   --------  )  >
C                                2           2             2
C                            SCLA        SCLB          SCLC
C
C                                2           2             2
C                            X(1)        X(2)          X(3)
C           =             --------  +   --------  +  --------
C                               2            2             2
C                           SCLA         SCLB          SCLC
C
C
C           =   1
C
C
C     This last equation is just the equation of the scaled ellipsoid.
C     We can combine the last two equalities and interchange the
C     positions of X and V to obtain
C
C
C                      V(1)         V(2)         V(3)
C        <   X,    ( -------- ,   -------- ,   --------  )  >   =   1
C                          2           2             2
C                      SCLA        SCLB          SCLC
C
C
C     This is the equation of the limb plane.
C
 
C
C     Put together a SPICELIB plane, LPLANE, that represents the limb
C     plane.
C
      NORMAL(1)  =  V(1) / SCLA2
      NORMAL(2)  =  V(2) / SCLB2
      NORMAL(3)  =  V(3) / SCLC2
 
      CALL NVC2PL ( NORMAL, 1.0D0, LPLANE )
 
C
C     Find the limb by intersecting the limb plane with the ellipsoid.
C
      CALL INEDPL ( SCLA,  SCLB,  SCLC,  LPLANE,  LIMB,  FOUND )
 
C
C     FOUND should be true unless we've encountered numerical problems.
C
      IF ( .NOT. FOUND ) THEN
 
         CALL SETMSG ( 'Ellipsoid shape and viewing geometry are too '//
     .                 'extreme; the limb was not found. '             )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                         )
         CALL CHKOUT ( 'EDLIMB'                                        )
         RETURN
 
      END IF
 
C
C     Undo the scaling before returning the limb.
C
      CALL VSCLG ( SCALE, LIMB, UBEL, TMPEL )
      CALL MOVED ( TMPEL, UBEL,       LIMB  )
 
      CALL CHKOUT ( 'EDLIMB' )
      RETURN
      END
