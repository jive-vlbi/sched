C$Procedure      VPRJP ( Vector projection onto plane )
 
      SUBROUTINE VPRJP ( VIN, PLANE, VOUT )
 
C$ Abstract
C
C     Project a vector onto a specified plane, orthogonally.
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
C     VECTOR
C
C$ Declarations
 
      INTEGER               UBPL
      PARAMETER           ( UBPL   =   4 )
 
      DOUBLE PRECISION      VIN   (    3 )
      DOUBLE PRECISION      PLANE ( UBPL )
      DOUBLE PRECISION      VOUT  (    3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     VIN        I   Vector to be projected.
C     PLANE      I   A SPICELIB plane onto which VIN is projected.
C     VOUT       O   Vector resulting from projection.
C
C$ Detailed_Input
C
C     VIN            is a 3-vector that is to be orthogonally projected
C                    onto a specified plane.
C
C     PLANE          is a SPICELIB plane that represents the geometric
C                    plane onto which VIN is to be projected.
C
C$ Detailed_Output
C
C     VOUT           is the vector resulting from the orthogonal
C                    projection of VIN onto PLANE.  VOUT is the closest
C                    point in the specified plane to VIN.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  Invalid input planes are diagnosed by the routine PL2NVC,
C         which is called by this routine.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Projecting a vector V orthogonally onto a plane can be thought of
C     as finding the closest vector in the plane to V.  This `closest
C     vector' always exists; it may be coincident with the original
C     vector.
C
C     Two related routines are VPRJPI, which inverts an orthogonal
C     projection of a vector onto a plane, and VPROJ, which projects
C     a vector orthogonally onto another vector.
C
C$ Examples
C
C     1)   Find the closest point in the ring plane of a planet to a
C          spacecraft located at POSITN (in body-fixed coordinates).
C          Suppose the vector NORMAL is normal to the ring plane, and
C          that ORIGIN, which represents the body center, is in the
C          ring plane.  Then we can make a `plane' with the code
C
C             CALL PNV2PL ( ORIGIN, NORMAL, PLANE )
C
C          can find the projection by making the call
C
C             CALL VPRJP ( POSITN, PLANE, PROJ )
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
C     vector projection onto plane
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      DOUBLE PRECISION      VDOT
 
C
C     Local variables
C
      DOUBLE PRECISION      CONST
      DOUBLE PRECISION      NORMAL ( 3 )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'VPRJP' )
      END IF
 
C
C     Obtain a unit vector normal to the input plane, and a constant
C     for the plane.
C
      CALL PL2NVC ( PLANE, NORMAL, CONST )
 
C
C     Let the notation < a, b > indicate the inner product of vectors
C     a and b.
C
C     VIN differs from its projection onto PLANE by some multiple of
C     NORMAL.  That multiple is
C
C
C               < VIN - VOUT, NORMAL >                 *  NORMAL
C
C        =   (  < VIN, NORMAL > - < VOUT, NORMAL >  )  *  NORMAL
C
C        =   (  < VIN, NORMAL > - CONST             )  *  NORMAL
C
C
C     Subtracting this multiple of NORMAL from VIN yields VOUT.
C
 
      CALL VLCOM (  1.0D0,
     .              VIN,
     .              CONST - VDOT ( VIN, NORMAL ),
     .              NORMAL,
     .              VOUT                          )
 
      CALL CHKOUT ( 'VPRJP' )
      RETURN
      END
