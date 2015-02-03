C$Procedure QXQ (Quaternion times quaternion)

      SUBROUTINE QXQ ( Q1, Q2, QOUT )

C$ Abstract
C
C     Multiply two quaternions.
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
C     MATH
C     POINTING
C     ROTATION
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      Q1   ( 0 : 3 )
      DOUBLE PRECISION      Q2   ( 0 : 3 )
      DOUBLE PRECISION      QOUT ( 0 : 3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     Q1         I   First SPICE quaternion factor.
C     Q2         I   Second SPICE quaternion factor.
C     QOUT       O   Product of Q1 and Q2.
C
C$ Detailed_Input
C
C     Q1             is a 4-vector representing a SPICE-style
C                    quaternion. See the discussion of quaternion
C                    styles in Particulars below.
C
C                    Note that multiple styles of quaternions
C                    are in use.  This routine will not work properly
C                    if the input quaternions do not conform to
C                    the SPICE convention.  See the Particulars
C                    section for details.
C
C     Q2             is a second SPICE-style quaternion.
C
C$ Detailed_Output
C
C     QOUT           is 4-vector representing the quaternion product 
C
C                       Q1 * Q2
C
C                    Representing Q(i) as the sums of scalar (real)
C                    part s(i) and vector (imaginary) part v(i)
C                    respectively,
C
C                       Q1 = s1 + v1
C                       Q2 = s2 + v2
C
C                    QOUT has scalar part s3 defined by
C
C                       s3 = s1 * s2 - <v1, v2>
C
C                    and vector part v3 defined by
C
C                       v3 = s1 * v2  +  s2 * v1  +  v1 x v2
C
C                    where the notation < , > denotes the inner
C                    product operator and x indicates the cross
C                    product operator.
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
C
C     Quaternion Styles
C     -----------------
C
C     There are different "styles" of quaternions used in 
C     science and engineering applications. Quaternion styles
C     are characterized by 
C
C        - The order of quaternion elements
C
C        - The quaternion multiplication formula
C
C        - The convention for associating quaternions
C          with rotation matrices
C
C     Two of the commonly used styles are
C
C        - "SPICE"
C
C           > Invented by Sir William Rowan Hamilton
C           > Frequently used in mathematics and physics textbooks
C
C        - "Engineering"
C
C           > Widely used in aerospace engineering applications
C
C
C     SPICELIB subroutine interfaces ALWAYS use SPICE quaternions.
C     Quaternions of any other style must be converted to SPICE
C     quaternions before they are passed to SPICELIB routines.
C     
C
C     Relationship between SPICE and Engineering Quaternions
C     ------------------------------------------------------
C
C     Let M be a rotation matrix such that for any vector V, 
C
C        M*V
C
C     is the result of rotating V by theta radians in the 
C     counterclockwise direction about unit rotation axis vector A.
C     Then the SPICE quaternions representing M are
C
C        (+/-) (  cos(theta/2), 
C                 sin(theta/2) A(1),  
C                 sin(theta/2) A(2),  
C                 sin(theta/2) A(3)  ) 
C
C     while the engineering quaternions representing M are 
C
C        (+/-) ( -sin(theta/2) A(1),  
C                -sin(theta/2) A(2),  
C                -sin(theta/2) A(3),
C                 cos(theta/2)       )
C
C     For both styles of quaternions, if a quaternion q represents
C     a rotation matrix M, then -q represents M as well.
C
C     Given an engineering quaternion
C
C        QENG   = ( q0,  q1,  q2,  q3 )
C
C     the equivalent SPICE quaternion is
C
C        QSPICE = ( q3, -q0, -q1, -q2 )
C
C
C     Associating SPICE Quaternions with Rotation Matrices
C     ----------------------------------------------------
C
C     Let FROM and TO be two right-handed reference frames, for
C     example, an inertial frame and a spacecraft-fixed frame. Let the
C     symbols
C
C        V    ,   V
C         FROM     TO
C
C     denote, respectively, an arbitrary vector expressed relative to
C     the FROM and TO frames. Let M denote the transformation matrix
C     that transforms vectors from frame FROM to frame TO; then
C
C        V   =  M * V
C         TO         FROM
C
C     where the expression on the right hand side represents left
C     multiplication of the vector by the matrix.
C
C     Then if the unit-length SPICE quaternion q represents M, where
C
C        q = (q0, q1, q2, q3)
C
C     the elements of M are derived from the elements of q as follows:
C
C          +-                                                         -+
C          |           2    2                                          |
C          | 1 - 2*( q2 + q3 )   2*(q1*q2 - q0*q3)   2*(q1*q3 + q0*q2) |
C          |                                                           |
C          |                                                           |
C          |                               2    2                      |
C      M = | 2*(q1*q2 + q0*q3)   1 - 2*( q1 + q3 )   2*(q2*q3 - q0*q1) |
C          |                                                           |
C          |                                                           |
C          |                                                   2    2  |
C          | 2*(q1*q3 - q0*q2)   2*(q2*q3 + q0*q1)   1 - 2*( q1 + q2 ) |
C          |                                                           |
C          +-                                                         -+
C
C     Note that substituting the elements of -q for those of q in the
C     right hand side leaves each element of M unchanged; this shows
C     that if a quaternion q represents a matrix M, then so does the
C     quaternion -q.
C
C     To map the rotation matrix M to a unit quaternion, we start by
C     decomposing the rotation matrix as a sum of symmetric
C     and skew-symmetric parts:
C
C                                        2
C        M = [ I  +  (1-cos(theta)) OMEGA  ] + [ sin(theta) OMEGA ]
C
C                     symmetric                   skew-symmetric
C
C
C     OMEGA is a skew-symmetric matrix of the form
C
C                   +-             -+
C                   |  0   -n3   n2 |
C                   |               |
C         OMEGA  =  |  n3   0   -n1 |
C                   |               |
C                   | -n2   n1   0  |
C                   +-             -+
C
C     The vector N of matrix entries (n1, n2, n3) is the rotation axis
C     of M and theta is M's rotation angle.  Note that N and theta
C     are not unique.
C
C     Let
C
C        C = cos(theta/2)
C        S = sin(theta/2)
C
C     Then the unit quaternions Q corresponding to M are
C
C        Q = +/- ( C, S*n1, S*n2, S*n3 )
C
C     The mappings between quaternions and the corresponding rotations
C     are carried out by the SPICELIB routines
C
C        Q2M {quaternion to matrix}
C        M2Q {matrix to quaternion}
C
C     M2Q always returns a quaternion with scalar part greater than
C     or equal to zero.
C
C
C     SPICE Quaternion Multiplication Formula
C     ---------------------------------------
C
C     Given a SPICE quaternion 
C
C        Q = ( q0, q1, q2, q3 )
C
C     corresponding to rotation axis A and angle theta as above, we can
C     represent Q using "scalar + vector" notation as follows:
C
C        s =   q0           = cos(theta/2)
C
C        v = ( q1, q2, q3 ) = sin(theta/2) * A
C
C        Q = s + v
C
C     Let Q1 and Q2 be SPICE quaternions with respective scalar
C     and vector parts s1, s2 and v1, v2:
C 
C        Q1 = s1 + v1
C        Q2 = s2 + v2
C
C     We represent the dot product of v1 and v2 by
C
C        <v1, v2>
C
C     and the cross product of v1 and v2 by
C
C        v1 x v2
C
C     Then the SPICE quaternion product is
C
C        Q1*Q2 = s1*s2 - <v1,v2>  + s1*v2 + s2*v1 + (v1 x v2)       
C
C     If Q1 and Q2 represent the rotation matrices M1 and M2 
C     respectively, then the quaternion product
C
C        Q1*Q2
C
C     represents the matrix product
C
C        M1*M2
C
C
C$ Examples
C
C     1)  Let QID, QI, QJ, QK be the "basis" quaternions
C
C            QID  =  ( 1, 0, 0, 0 )
C            QI   =  ( 0, 1, 0, 0 )
C            QJ   =  ( 0, 0, 1, 0 )
C            QK   =  ( 0, 0, 0, 1 )
C
C         respectively.  Then the calls
C
C            CALL QXQ ( QI, QJ, IXJ )
C            CALL QXQ ( QJ, QK, JXK )
C            CALL QXQ ( QK, QI, KXI )
C
C         produce the results
C
C            IXJ = QK
C            JXK = QI
C            KXI = QJ
C
C         All of the calls
C
C            CALL QXQ ( QI, QI, QOUT )
C            CALL QXQ ( QJ, QJ, QOUT )
C            CALL QXQ ( QK, QK, QOUT )
C
C         produce the result
C
C            QOUT  =  -QID
C
C         For any quaternion Q, the calls
C
C            CALL QXQ ( QID, Q,   QOUT )
C            CALL QXQ ( Q,   QID, QOUT )
C
C         produce the result
C
C            QOUT  =  Q
C
C
C
C     2)  Composition of rotations:  let CMAT1 and CMAT2 be two
C         C-matrices (which are rotation matrices).  Then the
C         following code fragment computes the product CMAT1 * CMAT2:
C
C
C            C
C            C     Convert the C-matrices to quaternions.
C            C
C                  CALL M2Q ( CMAT1, Q1 )
C                  CALL M2Q ( CMAT2, Q2 )
C
C            C
C            C     Find the product.
C            C
C                  CALL QXQ ( Q1, Q2, QOUT )
C
C            C
C            C     Convert the result to a C-matrix.
C            C
C                  CALL Q2M ( QOUT, CMAT3 )
C
C            C
C            C     Multiply CMAT1 and CMAT2 directly.
C            C
C                  CALL MXM ( CMAT1, CMAT2, CMAT4 )
C
C            C
C            C     Compare the results.  The difference DIFF of
C            C     CMAT3 and CMAT4 should be close to the zero
C            C     matrix.
C            C
C                  CALL VSUBG ( 9, CMAT3, CMAT4, DIFF )
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
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 26-FEB-2008 (NJB)
C
C        Updated header; added information about SPICE 
C        quaternion conventions.
C
C-    SPICELIB Version 1.0.0, 18-AUG-2002 (NJB)
C
C-&
 
C$ Index_Entries
C
C     quaternion times quaternion
C     multiply quaternion by quaternion
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      VDOT

C
C     Local variables
C
      DOUBLE PRECISION      CROSS ( 3 )


C
C     Compute the scalar part of the product.
C
      QOUT(0)  =  Q1(0) * Q2(0)  -  VDOT( Q1(1), Q2(1) )

C
C     And now the vector part.  The SPICELIB routine VLCOM3 computes
C     a linear combination of three 3-vectors.
C
      CALL VCRSS  ( Q1(1),  Q2(1),  CROSS )

      CALL VLCOM3 ( Q1(0),  Q2(1), 
     .              Q2(0),  Q1(1),
     .              1.D0,   CROSS,  QOUT(1) )

      RETURN
      END


      
