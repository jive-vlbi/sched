C$Procedure      PJELPL ( Project ellipse onto plane )
 
      SUBROUTINE PJELPL ( ELIN, PLANE, ELOUT )
 
C$ Abstract
C
C     Project an ellipse onto a plane, orthogonally.
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
C     PLANES
C
C$ Keywords
C
C     ELLIPSE
C     GEOMETRY
C     MATH
C
C$ Declarations
 
      INTEGER               UBEL
      PARAMETER           ( UBEL    =  9 )
 
      INTEGER               UBPL
      PARAMETER           ( UBPL    =  4 )
 
      DOUBLE PRECISION      ELIN  ( UBEL )
      DOUBLE PRECISION      PLANE ( UBPL )
      DOUBLE PRECISION      ELOUT ( UBEL )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ELIN       I   A SPICELIB ellipse to be projected.
C     PLANE      I   A plane onto which ELIN is to be projected.
C     ELOUT      O   A SPICELIB ellipse resulting from the projection.
C
C$ Detailed_Input
C
C     ELIN,
C     PLANE          are, respectively, a SPICELIB ellipse and a
C                    SPICELIB plane.  The geometric ellipse represented
C                    by ELIN is to be orthogonally projected onto the
C                    geometric plane represented by PLANE.
C
C$ Detailed_Output
C
C     ELOUT          is a SPICELIB ellipse that represents the geometric
C                    ellipse resulting from orthogonally projecting the
C                    ellipse represented by INEL onto the plane
C                    represented by PLANE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input plane is invalid, the error will be diagnosed
C         by routines called by this routine.
C
C     2)  The input ellipse may be degenerate--its semi-axes may be
C         linearly dependent.  Such ellipses are allowed as inputs.
C
C     3)  The ellipse resulting from orthogonally projecting the input
C         ellipse onto a plane may be degenerate, even if the input
C         ellipse is not.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Projecting an ellipse orthogonally onto a plane can be thought of
C     finding the points on the plane that are `under' or `over' the
C     ellipse, with the `up' direction considered to be perpendicular
C     to the plane.  More mathematically, the orthogonal projection is
C     the set of points Y in the plane such that for some point X in
C     the ellipse, the vector Y - X is perpendicular to the plane.
C     The orthogonal projection of an ellipse onto a plane yields
C     another ellipse.
C
C$ Examples
C
C     1)  With  CENTER  = ( 1.D0,  1.D0,  1.D0 ),
C               VECT1   = ( 2.D0,  0.D0,  0.D0 ),
C               VECT2   = ( 0.D0,  1.D0,  1.D0 ),
C               NORMAL  = ( 0.D0,  0.D0,  1.D0 ),
C
C         the code fragment
C
C               CALL NVC2PL ( NORMAL,  0.D0,    PLANE           )
C               CALL CGV2EL ( CENTER,  VECT1,   VECT2,   ELIN   )
C               CALL PJELPL ( ELIN,    PLANE,   ELOUT           )
C               CALL EL2CGV ( ELOUT,   PRJCTR,  PRJMAJ,  PRJMIN )
C
C         returns
C
C               PRJCTR  = ( 1.D0,  1.D0,  0.D0 )
C               PRJMAJ  = ( 2.D0,  0.D0,  0.D0 )
C               PRJMIN  = ( 0.D0,  1.D0,  0.D0 )
C
C
C     2)  With  VECT1   = ( 2.D0,  0.D0,  0.D0 ),
C               VECT2   = ( 1.D0,  1.D0,  1.D0 ),
C               CENTER  = ( 0.D0,  0.D0,  0.D0 ),
C               NORMAL  = ( 0.D0,  0.D0,  1.D0 ),
C
C         the code fragment
C
C               CALL NVC2PL ( NORMAL,  0.D0,    PLANE           )
C               CALL CGV2EL ( CENTER,  VECT1,   VECT2,   ELIN   )
C               CALL PJELPL ( ELIN,    PLANE,   ELOUT           )
C               CALL EL2CGV ( ELOUT,   PRJCTR,  PRJMAJ,  PRJMIN )
C
C         returns
C
C               PRJCTR  = ( 0.D0,  0.D0,  0.D0 )
C
C               PRJMAJ  = ( -2.227032728823213D0,
C                           -5.257311121191336D-1,
C                            0.D0                  )
C
C               PRJMIN  = (  2.008114158862273D-1,
C                           -8.506508083520399D-1,
C                            0.D0                  )
C
C
C
C     3)    An example of actual use:   Suppose we wish to compute the
C           distance from an ellipsoid to a line.   Let the line be
C           defined by a point P and a direction vector DIRECT; the
C           line is the set of points
C
C              P   +   t * DIRECT,
C
C           where t is any real number.  Let the ellipsoid have semi-
C           axis lengths A, B, and C.
C
C           We can reduce the problem to that of finding the distance
C           between the line and an ellipse on the ellipsoid surface by
C           considering the fact that the surface normal at the nearest
C           point to the line will be orthogonal to DIRECT; the set of
C           surface points where this condition holds lies in a plane,
C           and hence is an ellipse on the surface.  The problem can be
C           further simplified by projecting the ellipse orthogonally
C           onto the plane defined by
C
C              < X, DIRECT >  =  0.
C
C           The problem is then a two dimensional one:  find the
C           distance of the projected ellipse from the intersection of
C           the line and this plane (which is necessarily one point).
C           A `paraphrase' of the relevant code is:
C
C
C              C     Step 1.   Find the candidate ellipse CAND.
C              C               NORMAL is a normal vector to the plane
C              C               containing the candidate ellipse.  The
C              C               ellipse must exist, since it's the
C              C               intersection of an ellipsoid centered at
C              C               the origin and a plane containing the
C              C               origin.  For this reason, we don't check
C              C               INEDPL's `found flag' FOUND below.
C              C
C                    NORMAL(1)  =  DIRECT(1) / A**2
C                    NORMAL(2)  =  DIRECT(2) / B**2
C                    NORMAL(3)  =  DIRECT(3) / C**2
C
C                    CALL NVC2PL ( NORMAL, 0.D0, CANDPL )
C
C                    CALL INEDPL ( A, B, C, CANDPL, CAND, FOUND )
C
C              C
C              C     Step 2.   Project the candidate ellipse onto a
C              C               plane orthogonal to the line.  We'll
C              C               call the plane PRJPL and the
C              C               projected ellipse PRJEL.
C              C
C                     CALL NVC2PL ( DIRECT,  0.D0,   PRJPL )
C                     CALL PJELPL ( CAND,    PRJPL,  PRJEL )
C
C              C
C              C     Step 3.   Find the point on the line lying in the
C              C               projection plane, and then find the
C              C               near point PJNEAR on the projected
C              C               ellipse.  Here PRJPT is the point on the
C              C               input line that lies in the projection
C              C               plane.  The distance between PRJPT and
C              C               PJNEAR is DIST.
C
C                    CALL VPRJP  ( LINEPT,  PRJPL,  PRJPT         )
C                    CALL NPEDPT ( PRJEL,   PRJPT,  PJNEAR,  DIST )
C
C              C
C              C     Step 4.  Find the near point PNEAR on the
C              C              ellipsoid by taking the inverse
C              C              orthogonal projection of PJNEAR; this is
C              C              the point on the candidate ellipse that
C              C              projects to PJNEAR.  Note that the output
C              C              DIST was computed in step 3.
C              C
C              C              The inverse projection of PJNEAR is
C              C              guaranteed to exist, so we don't have to
C              C              check FOUND.
C              C
C                    CALL VPRJPI ( PJNEAR, PRJPL, CANDPL, PNEAR, FOUND )
C
C
C           The value of DIST returned is the distance we're looking
C           for.
C
C           The procedure described here is carried out in the routine
C           NPEDLN.
C
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
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 02-NOV-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     project ellipse onto plane
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      CONST
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PRJCTR ( 3 )
      DOUBLE PRECISION      PRJVC1 ( 3 )
      DOUBLE PRECISION      PRJVC2 ( 3 )
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PJELPL' )
      END IF
 
C
C     Find generating vectors of the input ellipse.
C
      CALL EL2CGV ( ELIN, CENTER, SMAJOR, SMINOR )
 
C
C     Find a normal vector for the input plane.
C
      CALL PL2NVC ( PLANE, NORMAL, CONST )
 
C
C     Find the components of the semi-axes that are orthogonal to the
C     input plane's normal vector.  The components are generating
C     vectors for the projected plane.
C
      CALL VPERP ( SMAJOR,  NORMAL,  PRJVC1 )
      CALL VPERP ( SMINOR,  NORMAL,  PRJVC2 )
 
C
C     Find the projection of the ellipse's center onto the input plane.
C     This is the center of the projected ellipse.
C
C     In case the last assertion is non-obvious, note that the
C     projection we're carrying out is the composition of a linear
C     mapping (projection to a plane containing the origin and parallel
C     to PLANE) and a translation mapping (adding the closest point to
C     the origin in PLANE to every point), and both linear mappings and
C     translations carry the center of an ellipse to the center of the
C     ellipse's image.  Let's state this using mathematical symbols.
C     Let L be a linear mapping and let T be a translation mapping,
C     say
C
C        T(x) = x + A.
C
C     Then
C
C           T  (  L ( center + cos(theta)smajor + sin(theta)sminor )  )
C
C        =  A  +  L ( center + cos(theta)smajor + sin(theta)sminor )
C
C        =  A  +  L (center)
C              +  cos(theta) L(smajor)
C              +  sin(theta) L(sminor)
C
C     From the form of this last expression, we see that we have an
C     ellipse centered at
C
C           A  +  L (center)
C
C        =  T  (  L (center)  )
C
C     This last term is the image of the center of the original ellipse,
C     as we wished to demonstrate.
C
C     Now in the case of orthogonal projection onto a plane PL, L can be
C     taken as the orthogonal projection onto a parallel plane PL'
C     containing the origin.  Then L is a linear mapping.  Let M be
C     the multiple of the normal vector of PL such that M is contained
C     in PL (M is the closest point in PL to the origin).  Then the
C     orthogonal projection mapping onto PL, which we will name PRJ,
C     can be defined by
C
C       PRJ (x)  =  L (x)  +  M.
C
C     So PRJ is the composition of a translation and a linear mapping,
C     as claimed.
C
C
      CALL VPRJP  ( CENTER, PLANE,  PRJCTR )
 
C
C     Put together the projected ellipse.
C
      CALL CGV2EL ( PRJCTR, PRJVC1, PRJVC2, ELOUT )
 
      CALL CHKOUT ( 'PJELPL' )
      RETURN
      END
