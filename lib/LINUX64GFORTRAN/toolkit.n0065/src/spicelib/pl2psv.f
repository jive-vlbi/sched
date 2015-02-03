C$Procedure      PL2PSV ( Plane to point and spanning vectors )
 
      SUBROUTINE PL2PSV ( PLANE, POINT, SPAN1, SPAN2 )
 
C$ Abstract
C
C     Return a point and two orthogonal spanning vectors that generate
C     a specified plane.
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
C     PLANES
C
C$ Keywords
C
C     GEOMETRY
C     MATH
C     PLANE
C
C$ Declarations
 
      INTEGER               UBPL
      PARAMETER           ( UBPL   =   4 )
 
      DOUBLE PRECISION      PLANE ( UBPL )
      DOUBLE PRECISION      POINT (    3 )
      DOUBLE PRECISION      SPAN1 (    3 )
      DOUBLE PRECISION      SPAN2 (    3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     PLANE      I   A SPICELIB plane.
C     POINT,
C     SPAN1,
C     SPAN2      O   A point in the input plane and two vectors
C                    spanning the input plane.
C
C$ Detailed_Input
C
C     PLANE          is a SPICELIB plane that represents the geometric
C                    plane defined by POINT, SPAN1, and SPAN2.
C
C$ Detailed_Output
C
C     POINT,
C     SPAN1,
C     SPAN2          are, respectively, a point and two orthogonal
C                    spanning vectors that generate the geometric plane
C                    represented by PLANE.  The geometric plane is the
C                    set of vectors
C
C                       POINT   +   s * SPAN1   +   t * SPAN2
C
C                    where s and t are real numbers.  POINT is the
C                    closest point in the plane to the origin; this
C                    point is always a multiple of the plane's normal
C                    vector.  SPAN1 and SPAN2 are an orthonormal pair
C                    of vectors.  POINT, SPAN1, and SPAN2 are mutually
C                    orthogonal.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  The input plane MUST have been created by one of the SPICELIB
C         routines
C
C            NVC2PL ( Normal vector and constant to plane )
C            NVP2PL ( Normal vector and point to plane    )
C            PSV2PL ( Point and spanning vectors to plane )
C
C         Otherwise, the results of this routine are unpredictable.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     SPICELIB geometry routines that deal with planes use the `plane'
C     data type to represent input and output planes.  This data type
C     makes the subroutine interfaces simpler and more uniform.
C
C     The SPICELIB routines that produce SPICELIB planes from data that
C     define a plane are:
C
C        NVC2PL ( Normal vector and constant to plane )
C        NVP2PL ( Normal vector and point to plane    )
C        PSV2PL ( Point and spanning vectors to plane )
C
C     The SPICELIB routines that convert SPICELIB planes to data that
C     define a plane are:
C
C        PL2NVC ( Plane to normal vector and constant )
C        PL2NVP ( Plane to normal vector and point    )
C        PL2PSV ( Plane to point and spanning vectors )
C
C$ Examples
C
C     1)  Project a vector V orthogonally onto a plane defined by
C         POINT, SPAN1, and SPAN2.  PROJ is the projection we want; it
C         is the closest vector in the plane to V.
C
C            CALL PSV2PL ( POINT,   SPAN1,    SPAN2,   PLANE )
C            CALL VPRJP  ( V,       PLANE,    PROJ           )
C
C
C     2)  Find the intersection of a plane and the unit sphere.  This
C         is a geometry problem that arises in computing the
C         intersection of a plane and a triaxial ellipsoid.  The
C         SPICELIB routine INEDPL computes this intersection, but this
C         example does illustrate how to use this routine.
C
C
C            C
C            C     The geometric plane of interest will be represented
C            C     by the SPICELIB plane PLANE in this example.
C            C
C            C     The intersection circle will be represented by the
C            C     vectors CENTER, V1, and V2; the circle is the set
C            C     of points
C            C
C            C        CENTER  +  cos(theta) V1  +  sin(theta) V2,
C            C
C            C     where theta is in the interval (-pi, pi].
C            C
C            C     The logical variable FOUND indicates whether the
C            C     intersection is non-empty.
C            C
C
C            C
C            C     The center of the intersection circle will be the
C            C     closest point in the plane to the origin.  This
C            C     point is returned by PL2PSV.  The distance of the
C            C     center from the origin is the norm of CENTER.
C            C
C                  CALL PL2PSV  ( PLANE, CENTER, SPAN1, SPAN2 )
C
C                  DIST = VNORM ( CENTER )
C
C            C
C            C     The radius of the intersection circle will be
C            C
C            C             ____________
C            C         _  /          2
C            C          \/  1 - DIST
C            C
C            C     since the radius of the circle, the distance of the
C            C     plane from the origin, and the radius of the sphere
C            C     (1) are the lengths of the sides of a right triangle.
C            C
C                  RADIUS = SQRT ( 1.0D0 - DIST**2 )
C
C                  CALL VSCL  ( RADIUS, SPAN1, V1 )
C                  CALL VSCL  ( RADIUS, SPAN2, V2 )
C
C                  FOUND = .TRUE.
C
C
C     3)  Apply a linear transformation represented by the matrix M to
C         a plane represented by the normal vector N and the constant C.
C         Find a normal vector and constant for the transformed plane.
C
C            C
C            C     Make a SPICELIB plane from N and C, and then find a
C            C     point in the plane and spanning vectors for the
C            C     plane.  N need not be a unit vector.
C            C
C                  CALL NVC2PL ( N,      C,      PLANE         )
C                  CALL PL2PSV ( PLANE,  POINT,  SPAN1,  SPAN2 )
C
C            C
C            C     Apply the linear transformation to the point and
C            C     spanning vectors.  All we need to do is multiply
C            C     these vectors by M, since for any linear
C            C     transformation T,
C            C
C            C           T ( POINT  +  t1 * SPAN1     +  t2 * SPAN2 )
C            C
C            C        =  T (POINT)  +  t1 * T(SPAN1)  +  t2 * T(SPAN2),
C            C
C            C     which means that T(POINT), T(SPAN1), and T(SPAN2)
C            C     are a point and spanning vectors for the transformed
C            C     plane.
C            C
C                  CALL MXV ( M, POINT, TPOINT )
C                  CALL MXV ( M, SPAN1, TSPAN1 )
C                  CALL MXV ( M, SPAN2, TSPAN2 )
C
C            C
C            C     Make a new SPICELIB plane TPLANE from the
C            C     transformed point and spanning vectors, and find a
C            C     unit normal and constant for this new plane.
C            C
C                  CALL PSV2PL ( TPOINT,  TSPAN1,  TSPAN2,  TPLANE )
C                  CALL PL2NVC ( TPLANE,  TN,      TC              )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1] `Calculus and Analytic Geometry', Thomas and Finney.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     plane to point and spanning vectors
C
C-&
 
 
 
C
C     Local parameters
C
 
C
C     The contents of SPICELIB planes are as follows:
C
C        Elements NMLPOS through NMLPOS + 2 contain a unit normal
C        vector for the plane.
C
C        Element CONPOS contains a constant for the plane;  every point
C        X in the plane satisifies
C
C           < X, PLANE(NMLPOS) >  =  PLANE(CONPOS).
C
C        The plane constant is the distance of the plane from the
C        origin; the normal vector, scaled by the constant, is the
C        closest point in the plane to the origin.
C
C
      INTEGER               NMLPOS
      PARAMETER           ( NMLPOS = 1 )
 
      INTEGER               CONPOS
      PARAMETER           ( CONPOS = 4 )
 
C
C     Local variables
C
      DOUBLE PRECISION      NORMAL ( 3 )
 
 
C
C     Find a unit normal vector for the plane, and find the closest
C     point in the plane to the origin.
C
      CALL PL2NVP ( PLANE, NORMAL, POINT )
 
C
C     Next, find an orthogonal pair of vectors that are also
C     orthogonal to the PLANE's normal vector.  The SPICELIB routine
C     FRAME does this for us.  NORMAL, SPAN1, and SPAN2 form a
C     right-handed orthonormal system upon output from FRAME.
C
      CALL FRAME  ( NORMAL, SPAN1, SPAN2 )
 
      RETURN
      END
