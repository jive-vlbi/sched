C$Procedure      M2Q ( Matrix to quaternion )

      SUBROUTINE M2Q ( R, Q )

C$ Abstract
C
C     Find a unit quaternion corresponding to a specified rotation
C     matrix.
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
C     MATRIX
C     ROTATION
C
C$ Declarations
 
      DOUBLE PRECISION      R ( 3,  3 )
      DOUBLE PRECISION      Q ( 0 : 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     R          I   A rotation matrix.
C     Q          O   A unit quaternion representing R.
C
C$ Detailed_Input
C
C     R              is a rotation matrix.
C
C$ Detailed_Output
C
C     Q              is a unit-length SPICE-style quaternion
C                    representing R. See the discussion of quaternion
C                    styles in Particulars below.
C
C                    Q is a 4-dimensional vector. If R rotates vectors
C                    in the counterclockwise sense by an angle of theta
C                    radians about a unit vector A, where 
C
C                       0 < theta < pi
C                         -       -
C
C                    then letting h = theta/2,
C
C                       Q = ( cos(h), sin(h)A ,  sin(h)A ,  sin(h)A ).
C                                            1          2          3
C
C                    The restriction that theta must be in the range
C                    [0, pi] determines the output quaternion Q
C                    uniquely except when theta = pi; in this special
C                    case, both of the quaternions
C
C                       Q = ( 0,  A ,  A ,  A  )
C                                  1    2    3
C                    and
C
C                       Q = ( 0, -A , -A , -A  )
C                                  1    2    3
C
C                   are possible outputs.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)   If R is not a rotation matrix, the error SPICE(NOTAROTATION)
C          is signaled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     A unit quaternion is a 4-dimensional vector for which the sum of
C     the squares of the components is 1. Unit quaternions can be used
C     to represent rotations in the following way: given a rotation
C     angle theta, where
C
C        0 < theta < pi
C          -       -
C
C     and a unit vector A, we can represent the transformation that
C     rotates vectors in the counterclockwise sense by theta radians
C     about A using the quaternion Q, where
C
C        Q = 
C
C        ( cos(theta/2), sin(theta/2)a , sin(theta/2)a , sin(theta/2)a )
C                                     1               2               3
C
C     As mentioned in Detailed Output, our restriction on the range of
C     theta determines Q uniquely, except when theta = pi.
C
C     The SPICELIB routine Q2M is an one-sided inverse of this routine:
C     given any rotation matrix R, the calls
C
C        CALL M2Q ( R, Q )
C        CALL Q2M ( Q, R )
C
C     leave R unchanged, except for round-off error.  However, the
C     calls
C
C        CALL Q2M ( Q, R )
C        CALL M2Q ( R, Q )
C
C     might preserve Q or convert Q to -Q.
C
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
C     1)  A case amenable to checking by hand calculation:
C
C            To convert the rotation matrix
C
C                     +-              -+
C                     |  0     1    0  |
C                     |                |
C               R  =  | -1     0    0  |
C                     |                |
C                     |  0     0    1  |
C                     +-              -+
C
C            also represented as
C
C               [ pi/2 ]
C                       3
C
C            to a quaternion, we can use the code fragment
C
C               CALL ROTATE (  HALFPI(),  3,  R  )
C               CALL M2Q    (  R,             Q  )
C
C            M2Q will return Q as
C
C               ( sqrt(2)/2, 0, 0, -sqrt(2)/2 )
C
C            Why?  Well, R is a reference frame transformation that
C            rotates vectors by -pi/2 radians about the axis vector
C
C               A  = ( 0, 0, 1 )
C
C            Equivalently, R rotates vectors by pi/2 radians in
C            the counterclockwise sense about the axis vector 
C
C               -A = ( 0, 0, -1 )  
C
C            so our definition of Q,
C
C               h = theta/2
C
C               Q = ( cos(h), sin(h)A , sin(h)A , sin(h)A  )
C                                    1         2         3
C
C            implies that in this case,
C
C               Q =  ( cos(pi/4),  0,  0, -sin(pi/4)  )
C
C                 =  ( sqrt(2)/2,  0,  0, -sqrt(2)/2  )
C
C
C     2)  Finding a quaternion that represents a rotation specified by
C         a set of Euler angles:
C
C            Suppose our original rotation R is the product
C
C               [ TAU ]  [ pi/2 - DELTA ]  [ ALPHA ] 
C                      3                 2          3
C
C            The code fragment
C
C               CALL EUL2M  ( TAU,   HALFPI() - DELTA,   ALPHA,
C              .              3,     2,                  3,      R )
C
C               CALL M2Q    ( R, Q )
C
C            yields a quaternion Q that represents R.
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
C     W.L. Taber     (JPL)
C   
C$ Version
C
C-    SPICELIB Version 2.0.1, 27-FEB-2008 (NJB)
C
C        Updated header; added information about SPICE 
C        quaternion conventions. Made various minor edits
C        throughout header.
C
C-    SPICELIB Version 2.0.0, 17-SEP-1999 (WLT)
C
C        The routine was re-implemented to sharpen the numerical
C        stability of the routine and eliminate calls to SIN
C        and COS functions.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 30-AUG-1990 (NJB)
C
C-&
 
C$ Index_Entries
C
C     matrix to quaternion
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               ISROT
 
C
C     Local parameters
C
 
C
C     NTOL and DETOL are used to determine whether R is a rotation
C     matrix.
C
C     NTOL is the tolerance for the norms of the columns of R.
C
C     DTOL is the tolerance for the determinant of a matrix whose
C     columns are the unitized columns of R.
C
C
      DOUBLE PRECISION      NTOL
      PARAMETER           ( NTOL = 0.1D0 )
 
      DOUBLE PRECISION      DTOL
      PARAMETER           ( DTOL = 0.1D0 )
 
C
C     Local Variables
C
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CC4
      DOUBLE PRECISION      FACTOR
      DOUBLE PRECISION      L2
      DOUBLE PRECISION      MTRACE
      DOUBLE PRECISION      POLISH
      DOUBLE PRECISION      S(3)
      DOUBLE PRECISION      S114
      DOUBLE PRECISION      S224
      DOUBLE PRECISION      S334
      DOUBLE PRECISION      TRACE
 
 
C
C     If R is not a rotation matrix, we can't proceed.
C
      IF (  .NOT. ISROT ( R, NTOL, DTOL )  ) THEN
         CALL CHKIN  ( 'M2Q' )
         CALL SETMSG ( 'Input matrix was not a rotation.' )
         CALL SIGERR ( 'SPICE(NOTAROTATION)'              )
         CALL CHKOUT ( 'M2Q'                              )
         RETURN
 
      END IF
C
C
C     If our quaternion is C, S1, S2, S3 (the S's being the imaginary
C     part) and we let
C
C        CSi = C  * Si
C        Sij = Si * Sj
C
C     then the rotation matrix corresponding to our quaternion is:
C
C        R(1,1)      = 1.0D0 - 2*S22 - 2*S33
C        R(2,1)      =         2*S12 + 2*CS3
C        R(3,1)      =         2*S13 - 2*CS2
C
C        R(1,2)      =         2*S12 - 2*CS3
C        R(2,2)      = 1.0D0 - 2*S11 - 2*S33
C        R(3,2)      =         2*S23 + 2*CS1
C
C        R(1,3)      =         2*S13 + 2*CS2
C        R(2,3)      =         2*S23 - 2*CS1
C        R(3,3)      = 1.0D0 - 2*S11 - 2*S22
C
C        From the above we can see that
C
C           TRACE = 3 - 4*(S11 + S22 + S33)
C
C        so that
C
C
C           1.0D0 + TRACE = 4 - 4*(S11 + S22 + S33)
C                         = 4*(CC + S11 + S22 + S33)
C                         - 4*(S11 + S22 + S33)
C                         = 4*CC
C
C        Thus up to sign
C
C          C = 0.5D0 * DSQRT( 1.0D0 + TRACE )
C
C        But we also have
C
C          1.0D0 + TRACE - 2.0D0*R(i,i) = 4.0D0 - 4.0D0(Sii + Sjj + Skk)
C                                       - 2.0D0 + 4.0D0(Sjj + Skk )
C
C                                       = 2.0D0 - 4.0D0*Sii
C
C        So that
C
C           1.0D0 - TRACE + 2.0D0*R(i,i) = 4.0D0*Sii
C
C        and so up to sign
C
C           Si = 0.5D0*DSQRT( 1.0D0 - TRACE + 2.0D0*R(i,i) )
C
C        in addition to this observation, we note that all of the
C        product pairs can easily be computed
C
C         CS1 = (R(3,2) - R(2,3))/4.0D0
C         CS2 = (R(1,3) - R(3,1))/4.0D0
C         CS3 = (R(2,1) - R(1,2))/4.0D0
C         S12 = (R(2,1) + R(1,2))/4.0D0
C         S13 = (R(3,1) + R(1,3))/4.0D0
C         S23 = (R(2,3) + R(3,2))/4.0D0
C
C     But taking sums or differences of numbers that are nearly equal
C     or nearly opposite results in a loss of precision. As a result
C     we should take some care in which terms to select when computing
C     C, S1, S2, S3.  However, by simply starting with one of the
C     large quantities cc, S11, S22, or S33 we can make sure that we
C     use the best of the 6 quantities above when computing the
C     remaining components of the quaternion.
C
 
      TRACE  = R(1,1) + R(2,2) + R(3,3)
      MTRACE = 1.0D0 - TRACE
 
      CC4    = 1.0D0  + TRACE
      S114   = MTRACE + 2.0D0*R(1,1)
      S224   = MTRACE + 2.0D0*R(2,2)
      S334   = MTRACE + 2.0D0*R(3,3)
 
C
C     Note that if you simply add CC4 + S114 + S224 + S334
C     you get four. Thus at least one of the 4 terms is greater than 1.
C
      IF ( 1.0D0 .LE. CC4 ) THEN
         C      =  DSQRT  ( CC4 * 0.25D0 )
         FACTOR =  1.0D0 /( C   * 4.0D0  )
 
         S(1)   = ( R(3,2) - R(2,3) )*FACTOR
         S(2)   = ( R(1,3) - R(3,1) )*FACTOR
         S(3)   = ( R(2,1) - R(1,2) )*FACTOR
 
      ELSE IF ( 1.0D0 .LE. S114 ) THEN
 
         S(1)   = DSQRT  ( S114 * 0.25D0 )
         FACTOR = 1.0D0 /( S(1) * 4.0D0  )
 
         C      = ( R(3,2) - R(2,3) ) * FACTOR
         S(2)   = ( R(1,2) + R(2,1) ) * FACTOR
         S(3)   = ( R(1,3) + R(3,1) ) * FACTOR
 
 
      ELSE IF ( 1.0D0 .LE. S224 ) THEN
 
         S(2)   = DSQRT  ( S224 * 0.25D0 )
         FACTOR = 1.0D0 /( S(2) * 4.0D0  )
 
         C      = ( R(1,3) - R(3,1) ) * FACTOR
         S(1)   = ( R(1,2) + R(2,1) ) * FACTOR
         S(3)   = ( R(2,3) + R(3,2) ) * FACTOR
 
      ELSE
 
         S(3)   = DSQRT  ( S334 * 0.25D0 )
         FACTOR = 1.0D0 /( S(3) * 4.0D0  )
 
         C      = ( R(2,1) - R(1,2) ) * FACTOR
         S(1)   = ( R(1,3) + R(3,1) ) * FACTOR
         S(2)   = ( R(2,3) + R(3,2) ) * FACTOR
 
      END IF
C
C     If the magnitude of this quaternion is not one, we polish it
C     up a bit.
C
      L2 = C*C + S(1)*S(1) + S(2)*S(2) + S(3)*S(3)
 
      IF ( L2 .NE. 1.0D0 ) THEN
         POLISH = 1.0D0/DSQRT(L2)
         C      =    C*POLISH
         S(1)   = S(1)*POLISH
         S(2)   = S(2)*POLISH
         S(3)   = S(3)*POLISH
      END IF
 
      IF ( C .GT. 0.0D0 ) THEN
         Q(0) = C
         Q(1) = S(1)
         Q(2) = S(2)
         Q(3) = S(3)
      ELSE
         Q(0) = -C
         Q(1) = -S(1)
         Q(2) = -S(2)
         Q(3) = -S(3)
      END IF
 
      RETURN
      END
