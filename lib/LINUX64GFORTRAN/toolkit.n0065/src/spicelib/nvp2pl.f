C$Procedure      NVP2PL ( Normal vector and point to plane )
 
      SUBROUTINE NVP2PL ( NORMAL, POINT, PLANE )
 
C$ Abstract
C
C     Make a SPICELIB plane from a normal vector and a point.
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
      PARAMETER           ( UBPL    =   4 )
 
      DOUBLE PRECISION      NORMAL (    3 )
      DOUBLE PRECISION      POINT  (    3 )
      DOUBLE PRECISION      PLANE  ( UBPL )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     NORMAL,
C     POINT      I   A normal vector and a point defining a plane.
C     PLANE      O   An array representing the plane.
C
C$ Detailed_Input
C
C     NORMAL,
C     POINT
C                    are, respectively, a normal vector and point that
C                    define a plane in three-dimensional space.  NORMAL
C                    need not be a unit vector. Let the symbol < a, b >
C                    indicate the inner product of vectors a and b;
C                    then the geometric plane is the set of vectors X
C                    in three-dimensional space that satisfy
C
C                       < X - POINT, NORMAL >  =  0.
C
C$ Detailed_Output
C
C     PLANE          is a SPICELIB plane that represents the geometric
C                    plane defined by POINT and NORMAL.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If the input vector NORMAL is the zero vector, the error
C         SPICE(ZEROVECTOR) is signalled.
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
C     Any of these last three routines may be used to convert this
C     routine's output, PLANE, to another representation of a
C     geometric plane.
C
C$ Examples
C
C     1)  Project a vector V orthogonally onto a plane defined by POINT
C         and NORMAL.  PROJ is the projection we want; it is the
C         closest vector in the plane to V.
C
C            CALL NVP2PL ( NORMAL, POINT,  PLANE )
C            CALL VPRJP  ( V,      PLANE,  PROJ  )
C
C
C     2)  Given a point in a plane and a normal vector, find the
C         distance of the plane from the origin.  We make a
C         `plane' from the point and normal, then convert the
C         plane to a unit normal and constant.  The constant CONST
C         is (according to the specification of PL2NVC) the distance of
C         the plane from the origin.
C
C            CALL NVP2PL ( NORMAL, POINT,  PLANE )
C            CALL PL2NVC ( PLANE,  NORMAL, CONST )
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
C-    SPICELIB Version 1.1.0, 30-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VMINUS call.
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
C     normal vector and point to plane
C
C-&
 
C$ Revisions
C
C-    SPICELIB Version 1.1.0, 30-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VMINUS call.
C
C-& 

 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDOT
 
      LOGICAL               RETURN
      LOGICAL               VZERO
 
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
      DOUBLE PRECISION      TMPVEC ( 3 )

 
C
C     This routine checks in only if an error is discovered.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF
 
C
C     The normal vector must be non-zero.
C
      IF ( VZERO (NORMAL) ) THEN
 
         CALL CHKIN  ( 'NVP2PL'                            )
         CALL SETMSG ( 'Plane''s normal must be non-zero.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                 )
         CALL CHKOUT ( 'NVP2PL'                            )
         RETURN
 
      END IF
 
      CALL VHAT ( NORMAL, PLANE(NMLPOS) )
 
      PLANE(CONPOS)  =  VDOT ( POINT, PLANE(NMLPOS) )
 
C
C     The constant should be the distance of the plane from the
C     origin.  If the constant is negative, negate both it and the
C     normal vector.
C
      IF ( PLANE(CONPOS) .LT. 0.D0 ) THEN
 
         PLANE(CONPOS) =   -PLANE(CONPOS)
         CALL VMINUS     (  PLANE(NMLPOS), TMPVEC        )
         CALL VEQU       (  TMPVEC,        PLANE(NMLPOS) )

      END IF
 
      RETURN
      END
