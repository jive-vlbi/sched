C$Procedure      VROTV ( Vector rotation about an axis )
 
      SUBROUTINE VROTV ( V, AXIS, THETA, R )
 
C$ Abstract
C
C     Rotate a vector about a specified axis vector by a specified
C     angle and return the rotated vector.
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
C     ROTATION,  VECTOR
C
C$ Declarations
 
      DOUBLE PRECISION   V    ( 3 )
      DOUBLE PRECISION   AXIS ( 3 )
      DOUBLE PRECISION   THETA
      DOUBLE PRECISION   R    ( 3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     V          I   Vector to be rotated.
C     AXIS       I   Axis of the rotation.
C     THETA      I   Angle of rotation (radians).
C     R          O   Result of rotating V about AXIS by THETA.
C
C$ Detailed_Input
C
C     V          is a 3-dimensional vector to be rotated.
C
C     AXIS       is the axis about which the rotation is to be
C                performed.
C
C     THETA      is the angle through which V is to be rotated about
C                AXIS.
C
C$ Detailed_Output
C
C     R          is the result of rotating V about AXIS by THETA.
C                If AXIS is the zero vector, R = V.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1)  If the input axis is the zero vector R will be returned 
C         as V.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     This routine computes the result of rotating (in a right handed
C     sense) the vector V about the axis represented by AXIS through
C     an angle of THETA radians.
C
C     If W is a unit vector parallel to AXIS, then R is given by:
C
C         R = V + ( 1 - cos(THETA) ) Wx(WxV) + sin(THETA) (WxV)
C
C     where "x" above denotes the vector cross product.
C
C$ Examples
C
C      If AXIS = ( 0, 0, 1 ) and THETA = PI/2 then the following results
C      for R will be obtained
C
C              V                           R
C         -------------             ----------------
C         ( 1, 2, 3 )                ( -2, 1, 3 )
C         ( 1, 0, 0 )                (  0, 1, 0 )
C         ( 0, 1, 0 )                ( -1, 0, 0 )
C
C
C      If AXIS = ( 0, 1, 0 ) and THETA = PI/2 then the following results
C      for R will be obtained
C
C              V                           R
C         -------------             ----------------
C         ( 1, 2, 3 )                (  3, 2, -1 )
C         ( 1, 0, 0 )                (  0, 0, -1 )
C         ( 0, 1, 0 )                (  0, 1,  0 )
C
C
C      If AXIS = ( 1, 1, 1 ) and THETA = PI/2 then the following results
C      for R will be obtained
C
C              V                                     R
C         -----------------------------   -----------------------------
C         ( 1.0,     2.0,     3.0     )   ( 2.577.., 0.845.., 2.577.. )
C         ( 2.577.., 0.845.., 2.577.. )   ( 3.0      2.0,     1.0     )
C         ( 3.0      2.0,     1.0     )   ( 1.422.., 3.154.., 1.422.. )
C         ( 1.422.., 3.154.., 1.422.. )   ( 1.0      2.0,     3.0     )
C
C
C$ Restrictions
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 1.0.2, 5-FEB-2003 (NJB)
C
C         Header examples were corrected.  Exceptions section 
C         filled in. Miscellaneous header corrections were made.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     vector rotation about an axis
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 17-FEB-1989 (HAN) (NJB)
C
C         Contents of the Exceptions section was changed
C         to "error free" to reflect the decision that the
C         module will never participate in error handling.
C         Also, the declarations of the unused variable I and the
C         unused function VDOT were removed.
C-&
 
 
 
 
C
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION VNORM
 
C
C     Local Variables
C
      DOUBLE PRECISION C
      DOUBLE PRECISION S
      DOUBLE PRECISION RPLANE (3)
      DOUBLE PRECISION P      (3)
      DOUBLE PRECISION V1     (3)
      DOUBLE PRECISION V2     (3)
      DOUBLE PRECISION X      (3)
 
 
C
C     Just in case the user tries to rotate about the zero vector -
C     check, and if so return the input vector
C
      IF ( VNORM( AXIS ) .EQ. 0.D0 ) THEN
         CALL MOVED (V,3,R)
         RETURN
      END IF
 
C
C     Compute the unit vector that lies in the direction of the
C     AXIS.  Call it X.
C
      CALL VHAT ( AXIS, X )
 
C
C     Compute the projection of V onto AXIS.  Call it P.
C
      CALL VPROJ ( V, X, P )
 
C
C     Compute the component of V orthogonal to the AXIS.  Call it V1.
C
      CALL VSUB  ( V, P,  V1 )
 
C
C     Rotate V1 by 90 degrees about the AXIS and call the result V2.
C
      CALL VCRSS  ( X, V1, V2)
 
C
C     Compute COS(THETA)*V1 + SIN(THETA)*V2. This is V1 rotated about
C     the AXIS in the plane normal to the axis, call the result RPLANE
C
      C = DCOS (THETA)
      S = DSIN (THETA)
 
      CALL VLCOM ( C, V1, S, V2, RPLANE )
 
C
C     Add the rotated component in the normal plane to AXIS to the
C     projection of V onto AXIS (P) to obtain R.
C
      CALL VADD ( RPLANE, P, R )
 
C
      RETURN
      END
