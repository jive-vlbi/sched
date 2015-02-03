C$Procedure ZZEDTERM ( Ellipsoid terminator )
 
      SUBROUTINE ZZEDTERM ( TYPE,   A,      B,     C,    
     .                      SRCRAD, SRCPOS, NPTS,  TRMPTS )
 
C$ Abstract
C
C     SPICE Private routine intended solely for the support of SPICE
C     routines.  Users should not call this routine directly due
C     to the volatile nature of this routine.
C
C     Compute a set of points on the umbral or penumbral terminator of
C     a specified ellipsoid, given a spherical light source.
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
C     BODY
C     GEOMETRY
C     MATH
C
C$ Declarations
 
      IMPLICIT NONE

      CHARACTER*(*)         TYPE
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      SRCRAD
      DOUBLE PRECISION      SRCPOS ( 3 )
      INTEGER               NPTS
      DOUBLE PRECISION      TRMPTS ( 3, NPTS )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     TYPE       I   Terminator type.
C     A          I   Length of ellipsoid semi-axis lying on the x-axis.
C     B          I   Length of ellipsoid semi-axis lying on the y-axis.
C     C          I   Length of ellipsoid semi-axis lying on the z-axis.
C     SRCRAD     I   Radius of light source.
C     SRCPOS     I   Position of center of light source.
C     NPTS       I   Number of points in terminator point set.
C     TRMPTS     O   Terminator point set.
C   
C$ Detailed_Input
C
C     TYPE           is a string indicating the type of terminator to
C                    compute:  umbral or penumbral.  The umbral 
C                    terminator is the boundary of the portion of the
C                    ellipsoid surface in total shadow.  The penumbral
C                    terminator is the boundary of the portion of the
C                    surface that is completely illuminated.  Possible
C                    values of TYPE are
C
C                       'UMBRAL'
C                       'PENUMBRAL'
C
C                    Case and leading or trailing blanks in TYPE are
C                    not significant.
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
C                    Length units associated with A, B, and C must
C                    match those associated with SRCRAD, SRCPOS,
C                    and the output TRMPTS.
C
C     SRCRAD         is the radius of the spherical light source. 
C
C     SRCPOS         is the position of the center of the light source 
C                    relative to the center of the ellipsoid.
C                    
C     NPTS           is the number of terminator points to compute.
C
C     
C$ Detailed_Output
C
C     TRMPTS         is an array of points on the umbral or penumbral
C                    terminator of the ellipsoid, as specified by the
C                    input argument TYPE.  The Ith point is contained
C                    in the array elements
C
C                        TRMPTS(J,I),  J = 1, 2, 3
C                    
C                    The terminator points are expressed in the 
C                    body-fixed reference frame associated with the
C                    ellipsoid.  Units are those associated with
C                    the input axis lengths.
C
C                    Each terminator point is the point of tangency of
C                    a plane that is also tangent to the light source.
C                    These associated points of tangency on the light
C                    source have uniform distribution in longitude when
C                    expressed in a cylindrical coordinate system whose
C                    Z-axis is SRCPOS.  The magnitude of the separation
C                    in longitude between these tangency points on the
C                    light source is
C
C                       2*Pi / NPTS 
C
C                    If the target is spherical, the terminator points
C                    also are uniformly distributed in longitude in the
C                    cylindrical system described above.  If the target
C                    is non-spherical, the longitude distribution of
C                    the points generally is not uniform.
C                    
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the terminator type is not recognized, the error
C         SPICE(NOTSUPPORTED) is signaled.
C
C     2)  If the set size NPTS is not at least 1, the error
C         SPICE(INVALIDSIZE) is signaled.
C
C     3)  If any of the ellipsoid's semi-axis lengths is non-positive,
C         the error SPICE(INVALIDAXISLENGTH) is signaled.
C
C     4)  If the light source has non-positive radius, the error
C         SPICE(INVALIDRADIUS) is signaled.
C
C     5)  If the light source intersects the smallest sphere
C         centered at the origin and containing the ellipsoid, the
C         error SPICE(OBJECTSTOOCLOSE) is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine models the boundaries of shadow regions on an
C     ellipsoid "illuminated" by a spherical light source.  Light rays
C     are assumed to travel along straight lines; refraction is not
C     modeled.
C
C     Points on the ellipsoid at which the entire cap of the light
C     source is visible are considered to be completely illuminated.
C     Points on the ellipsoid at which some portion (or all) of the cap
C     of the light source are blocked are considered to be in partial
C     (or total) shadow.
C
C     In this routine, we use the term "umbral terminator" to denote
C     the curve ususally called the "terminator":  this curve is the
C     boundary of the portion of the surface that lies in total shadow.
C     We use the term "penumbral terminator" to denote the boundary of
C     the completely illuminated portion of the surface.
C
C     In general, the terminator on an ellipsoid is a more complicated
C     curve than the limb (which is always an ellipse).  Aside from
C     various special cases, the terminator does not lie in a plane.
C
C     However, the condition for a point X on the ellipsoid to lie on
C     the terminator is simple:  a plane tangent to the ellipsoid at X
C     must also be tangent to the light source.  If this tangent plane
C     does not intersect the vector from the center of the ellipsoid to
C     the center of the light source, then X lies on the umbral
C     terminator; otherwise X lies on the penumbral terminator.
C
C$ Examples
C
C     See the SPICELIB routine EDTERM.
C
C$ Restrictions
C
C     This is a private SPICELIB routine.  User applications should not
C     call this routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 24-APR-2012 (NJB)
C
C        Deleted computations of unused quantities
C        MAXANG and MINANG.
C     
C-    SPICELIB Version 1.0.0, 03-FEB-2007 (NJB)
C
C-&
 
C$ Index_Entries
C
C     find terminator on ellipsoid
C     find umbral terminator on ellipsoid
C     find penumbral terminator on ellipsoid
C
C-&
 

C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      LOGICAL               RETURN

C
C     Local parameters
C
      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

      INTEGER               MAXITR 
      PARAMETER           ( MAXITR = 10 )

      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 50 )
      
C
C     Local variables
C
      CHARACTER*(TYPLEN)    LOCTYP

      DOUBLE PRECISION      ANGERR
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      D
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      E      ( 3 )
      DOUBLE PRECISION      INANG
      DOUBLE PRECISION      LAMBDA
      DOUBLE PRECISION      MAXRAD
      DOUBLE PRECISION      MINRAD
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      OUTANG
      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      PLCONS
      DOUBLE PRECISION      PRVANG
      DOUBLE PRECISION      PRVDIF
      DOUBLE PRECISION      RMAX
      DOUBLE PRECISION      RMIN
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SRCPT  ( 3 )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TRANS  ( 3, 3 )
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      VTX    ( 3 )
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               I
      INTEGER               NITR

      LOGICAL               UMBRAL


C
C     Standard SPICELIB error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZEDTERM' )

C
C     Check the terminator type.
C
      CALL LJUST ( TYPE,   LOCTYP )
      CALL UCASE ( LOCTYP, LOCTYP )

      IF (  LOCTYP .EQ. 'UMBRAL' ) THEN

         UMBRAL = .TRUE.

      ELSE IF ( LOCTYP .EQ. 'PENUMBRAL' ) THEN

         UMBRAL = .FALSE.

      ELSE

         CALL SETMSG ( 'Terminator type must be UMBRAL or ' 
     .   //            'PENUMBRAL but was actually #.'     )
         CALL ERRCH  ( '#',  TYPE                          )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'               )
         CALL CHKOUT ( 'ZZEDTERM'                          )
         RETURN

      END IF

C
C     Check the terminator set dimension.
C
      IF ( NPTS .LT. 1 ) THEN

         CALL SETMSG ( 'Set must contain at least one point; NPTS '
     .   //            ' = #.'                                        )
         CALL ERRINT ( '#',  NPTS                                     )
         CALL SIGERR ( 'SPICE(INVALIDSIZE)'                           )
         CALL CHKOUT ( 'ZZEDTERM'                                     )
         RETURN

      END IF
 
C
C     The ellipsoid semi-axes must have positive length.
C
      IF (       ( A .LE. 0.D0 )
     .     .OR.  ( B .LE. 0.D0 )
     .     .OR.  ( C .LE. 0.D0 )   )   THEN
 
         CALL SETMSG ( 'Semi-axis lengths:  A = #, B = #, C = #. ' )
         CALL ERRDP  ( '#', A                                      )
         CALL ERRDP  ( '#', B                                      )
         CALL ERRDP  ( '#', C                                      )
         CALL SIGERR ( 'SPICE(INVALIDAXISLENGTH)'                  )
         CALL CHKOUT ( 'ZZEDTERM'                                  )
         RETURN
 
      END IF

C
C     Check the input light source radius.
C
      IF ( SRCRAD .LE. 0.D0 ) THEN
 
         CALL SETMSG ( 'Light source must have positive radius; '
     .   //            'actual radius was #.'                    )
         CALL ERRDP  ( '#', SRCRAD                               )
         CALL SIGERR ( 'SPICE(INVALIDRADIUS)'                    )
         CALL CHKOUT ( 'ZZEDTERM'                                )
         RETURN
 
      END IF

C
C     The light source must not intersect the outer bounding 
C     sphere of the ellipsoid.
C
      D    =  VNORM ( SRCPOS )

      RMAX =  MAX ( A, B, C )
      RMIN =  MIN ( A, B, C )

      IF (  ( SRCRAD + RMAX )  .GE.  D  ) THEN
C
C        The light source is too close.
C
         CALL SETMSG ( 'Light source intersects outer bounding '
     .   //            'sphere of the ellipsoid.  Light source '
     .   //            'radius = #; ellipsoid''s longest axis = '
     .   //            '#; sum = #; distance between centers = #.' )
         CALL ERRDP  ( '#', SRCRAD                                 )
         CALL ERRDP  ( '#', RMAX                                   )
         CALL ERRDP  ( '#', SRCRAD + RMAX                          )
         CALL ERRDP  ( '#', D                                      )
         CALL SIGERR ( 'SPICE(OBJECTSTOOCLOSE)'                    )
         CALL CHKOUT ( 'ZZEDTERM'                                  )
         RETURN

      END IF

C
C     Let the negative of the ellipsoid-light source vector be the
C     Z-axis of a frame we'll use to generate the terminator set.
C
      CALL VMINUS ( SRCPOS, Z )

      CALL FRAME  ( Z,  X,  Y )

C
C     Create the rotation matrix required to convert vectors
C     from the source-centered frame back to the target body-fixed
C     frame.
C
      CALL VEQU ( X, TRANS(1,1) )
      CALL VEQU ( Y, TRANS(1,2) )
      CALL VEQU ( Z, TRANS(1,3) )

C
C     Find the maximum and minimum target radii.
C
      MAXRAD = MAX ( A, B, C )
      MINRAD = MIN ( A, B, C )

      IF ( UMBRAL ) THEN
C
C        Compute the angular offsets from the axis of rays tangent to
C        both the source and the bounding spheres of the target, where
C        the tangency points lie in a half-plane bounded by the line
C        containing the origin and SRCPOS.  (We'll call this line
C        the "axis.")
C
C        OUTANG corresponds to the target's outer bounding sphere;
C        INANG to the inner bounding sphere.
C     
         OUTANG = ASIN ( (SRCRAD - MAXRAD) / D ) 
         INANG  = ASIN ( (SRCRAD - MINRAD) / D )

      ELSE
C
C        Compute the angular offsets from the axis of rays tangent to
C        both the source and the bounding spheres of the target, where
C        the tangency points lie in opposite half-planes bounded by the
C        axis (compare the case above).
C
C        OUTANG corresponds to the target's outer bounding sphere;
C        INANG to the inner bounding sphere.
C     
         OUTANG = ASIN (  ( SRCRAD + MAXRAD )  /  D  ) 
         INANG  = ASIN (  ( SRCRAD + MINRAD )  /  D  ) 

      END IF

C
C     Compute the angular delta we'll use for generating
C     terminator points.
C
      DELTA = TWOPI() / NPTS

C
C     Generate the terminator points.
C
      DO I = 1, NPTS

         THETA = (I-1)*DELTA

C
C        Let SRCPT be the surface point on the source lying in 
C        the X-Y plane of the frame produced by FRAME
C        and corresponding to the angle THETA.
C        
         CALL LATREC ( SRCRAD,  THETA,  0.D0,  SRCPT )

C
C        Now solve for the angle by which SRCPT must be rotated (toward
C        +Z in the umbral case, away from +Z in the penumbral case)
C        so that a plane tangent to the source at SRCPT is also tangent
C        to the target. The rotation is bracketed by OUTANG on the low
C        side and INANG on the high side in the umbral case; the
C        bracketing values are reversed in the penumbral case.
C        
         IF ( UMBRAL ) THEN
            ANGLE  = OUTANG
         ELSE
            ANGLE  = INANG
         END IF

         PRVDIF = TWOPI()
         PRVANG = ANGLE + HALFPI()
         NITR   = 0

         DO WHILE (       ( NITR                        .LE. MAXITR )
     .              .AND. ( TOUCHD( ABS(ANGLE-PRVANG) ) .LT. PRVDIF )  )
         
            NITR    =   NITR  + 1
            PRVDIF  =   TOUCHD ( ABS(ANGLE-PRVANG) )
            PRVANG  =   ANGLE
C
C           Find the closest point on the ellipsoid to the plane
C           corresponding to "ANGLE".
C
C           The tangent point on the source is obtained by rotating
C           SRCPT by ANGLE towards +Z.  The plane's normal vector is
C           parallel to VTX in the source-centered frame.
C
            CALL LATREC ( SRCRAD, THETA, ANGLE, VTX )

            CALL VEQU   ( VTX, DIR )

C
C           VTX and DIR are expressed in the source-centered frame.  We
C           must translate VTX to the target frame and rotate both
C           vectors into that frame.
C
            CALL MXV  ( TRANS,  VTX,   VTEMP )
            CALL VADD ( SRCPOS, VTEMP, VTX   )

            CALL MXV  ( TRANS,  DIR,   VTEMP )
            CALL VEQU ( VTEMP,         DIR   )

C
C           Create the plane defined by VTX and DIR.
C
            CALL NVP2PL ( DIR, VTX, PLANE )
C
C           Find the closest point on the ellipsoid to the plane. At
C           the point we seek, the outward normal on the ellipsoid is
C           parallel to the choice of plane normal that points away
C           from the origin.  We can always obtain this choice from
C           PL2NVC.
C           
            CALL PL2NVC ( PLANE, DIR, PLCONS )

C
C           At the point 
C
C               E = (x, y, z)
C
C           on the ellipsoid's surface, an outward normal
C           is 
C
C               N = ( x/A**2, y/B**2, z/C**2 )  
C
C           which is also
C
C               lambda * ( DIR(1), DIR(2), DIR(3) )
C
C           Equating components in the normal vectors yields
C
C               E = lambda * ( DIR(1)*A**2, DIR(2)*B**2, DIR(3)*C**2 )
C
C           Taking the inner product with the point E itself and
C           applying the ellipsoid equation, we find
C
C               lambda * <DIR, E>  =  < N, E >  =  1
C
C           The first term above is 
C
C               lambda**2 * || ( A*DIR(1), B*DIR(2), C*DIR(3) ) ||**2
C
C           So the positive root lambda is
C
C               1 / || ( A*DIR(1), B*DIR(2), C*DIR(3) ) ||
C
C           Having lambda we can compute E.
C

            CALL VPACK ( A*DIR(1), B*DIR(2), C*DIR(3), V )

            LAMBDA = 1.D0 / VNORM(V)

            CALL VPACK ( A*V(1), B*V(2), C*V(3), E )

            CALL VSCL  ( LAMBDA, E, TRMPTS(1,I) )

C
C           Make a new estimate of the plane rotation required to touch
C           the target.
C
            CALL VSUB ( TRMPTS(1,I), VTX, OFFSET )

C
C           Let ANGERR be an estimate of the magnitude of angular error
C           between the plane and the terminator.
C
            ANGERR = VSEP ( DIR, OFFSET ) - HALFPI()

C
C           Let S indicate the sign of the altitude error:  where
C           S is positive, the plane is above E.
C
            S = SIGN ( 1.D0, VDOT(E,DIR) )

            IF ( UMBRAL ) THEN
C
C              If the plane is above the target, increase the
C              rotation angle; otherwise decrease the angle.
C
               ANGLE = ANGLE  +  S * ANGERR

            ELSE
C
C              This is the penumbral case; decreasing the angle
C              "lowers" the plane toward the target.
C
               ANGLE = ANGLE  -  S * ANGERR

            END IF

         END DO

      END DO

      CALL CHKOUT ( 'ZZEDTERM' )
      RETURN
      END
