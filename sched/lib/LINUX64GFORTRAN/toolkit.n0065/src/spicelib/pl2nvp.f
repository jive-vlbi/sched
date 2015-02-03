C$Procedure      PL2NVP ( Plane to normal vector and point )
 
      SUBROUTINE PL2NVP ( PLANE, NORMAL, POINT )
 
C$ Abstract
C
C     Return a unit normal vector and point that define a specified
C     plane.
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
      PARAMETER           ( UBPL   =    4 )
 
      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      NORMAL (    3 )
      DOUBLE PRECISION      POINT  (    3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     PLANE      I   A SPICELIB plane.
C     NORMAL,
C     POINT      O   A unit normal vector and point that define PLANE.
C
C$ Detailed_Input
C
C     PLANE          is a SPICELIB plane.
C
C$ Detailed_Output
C
C     NORMAL,
C     POINT          are, respectively, a unit normal vector and point
C                    that define the geometric plane represented by
C                    PLANE.  Let the symbol < a, b > indicate the inner
C                    product of vectors a and b; then the geometric
C                    plane is the set of vectors X in three-dimensional
C                    space that satisfy
C
C                       < X - POINT, NORMAL >  =  0.
C
C                    POINT is always the closest point in the input
C                    plane to the origin.  POINT is always a
C                    non-negative scalar multiple of NORMAL.
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
C     1)  Given a plane normal and constant, find a point in
C         the plane.  POINT is the point we seek.
C
C            CALL NVC2PL ( NORMAL, CONST,  PLANE )
C            CALL PL2NVP ( PLANE,  NORMAL, POINT )
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
C     plane to normal vector and point
C
C-&
 
 
 
C
C     Local variables
C
      DOUBLE PRECISION      CONST
 
 
 
C
C     Find a unit normal and constant for the plane.  Scaling the
C     unit normal by the constant gives us the closest point in
C     the plane to the origin.
C
      CALL PL2NVC ( PLANE, NORMAL, CONST )
      CALL VSCL   ( CONST, NORMAL, POINT )
 
      RETURN
      END
