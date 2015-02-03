C$Procedure      Q2M ( Quaternion to matrix )
 
      SUBROUTINE Q2M ( Q, R )
 
C$ Abstract
C
C     Find the rotation matrix corresponding to a specified unit
C     quaternion.
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
 
      DOUBLE PRECISION      Q ( 0 : 3 )
      DOUBLE PRECISION      R ( 3,  3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     Q          I   A unit quaternion.
C     R          O   A rotation matrix corresponding to Q.
C
C$ Detailed_Input
C
C     Q              is a unit-length SPICE-style quaternion. Q has the
C                    property that
C
C                       || Q ||  =  1
C
C                    See the discussion of quaternion styles in
C                    Particulars below.
C
C$ Detailed_Output
C
C     R              is a 3 by 3 rotation matrix representing the same
C                    rotation as does Q. See the discussion titled
C                    "Associating SPICE Quaternions with Rotation 
C                    Matrices" in Particulars below.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If Q is not a unit quaternion, the output matrix M is
C        the rotation matrix that is the result of converting
C        normalized Q to a rotation matrix.
C
C     2) If Q is the zero quaternion, the output matrix M is
C        the identity matrix.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     If a 4-dimensional vector Q satisfies the equality
C
C        || Q ||   =  1
C
C     or equivalently
C
C            2          2          2          2
C        Q(0)   +   Q(1)   +   Q(2)   +   Q(3)   =  1,
C
C     then we can always find a unit vector A and a scalar r such that
C
C        Q = ( cos(r/2), sin(r/2)A(1), sin(r/2)A(2), sin(r/2)A(3) ).
C
C     We can interpret A and r as the axis and rotation angle of a
C     rotation in 3-space.  If we restrict r to the range [0, pi],
C     then r and A are uniquely determined, except if r = pi.  In this
C     special case, A and -A are both valid rotation axes.
C
C     Every rotation is represented by a unique orthogonal matrix; this
C     routine returns that unique rotation matrix corresponding to Q.
C
C     The SPICELIB routine M2Q is a one-sided inverse of this routine:
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
C            To convert the quaternion
C
C               Q = ( sqrt(2)/2, 0, 0, -sqrt(2)/2 )
C
C            to a rotation matrix, we can use the code fragment
C
C               Q(0) =  DSQRT(2)/2.D0
C               Q(1) =  0.D0
C               Q(2) =  0.D0
C               Q(3) = -DSQRT(2)/2.D0
C
C               CALL Q2M ( Q, R )
C
C            The matrix R will be set equal to
C
C               +-              -+
C               |  0     1    0  |
C               |                |
C               | -1     0    0  |.
C               |                |
C               |  0     0    1  |
C               +-              -+
C
C            Why?  Well, Q represents a rotation by some angle r about
C            some axis vector A, where r and A satisfy
C
C               Q =
C
C               ( cos(r/2), sin(r/2)A(1), sin(r/2)A(2), sin(r/2)A(3) ).
C
C            In this example,
C
C               Q = ( sqrt(2)/2, 0, 0, -sqrt(2)/2 ),
C
C            so
C
C               cos(r/2) = sqrt(2)/2.
C
C            Assuming that r is in the interval [0, pi], we must have
C
C               r = pi/2,
C
C            so
C
C               sin(r/2) = sqrt(2)/2.
C
C            Since the second through fourth components of Q represent
C
C               sin(r/2) * A,
C
C            it follows that
C
C               A = ( 0, 0, -1 ).
C
C            So Q represents a transformation that rotates vectors by
C            pi/2 about the negative z-axis.  This is equivalent to a
C            coordinate system rotation of pi/2 about the positive
C            z-axis; and we recognize R as the matrix
C
C               [ pi/2 ] .
C                       3
C
C
C     2)  Finding a set of Euler angles that represent a rotation
C         specified by a quaternion:
C
C            Suppose our rotation R is represented by the quaternion
C            Q.  To find angles TAU, ALPHA, DELTA such that
C
C
C               R  =  [ TAU ]  [ pi/2 - DELTA ]  [ ALPHA ] ,
C                            3                 2          3
C
C            we can use the code fragment
C
C
C               CALL Q2M    ( Q, R )
C
C               CALL M2EUL  ( R,   3,      2,       3,
C              .                   TAU,    DELTA,   ALPHA  )
C
C               DELTA = HALFPI() - DELTA
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     [1]    NAIF document 179.0, "Rotations and their Habits", by
C            W. L. Taber.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     F.S. Turner    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.2, 26-FEB-2008 (NJB)
C
C        Updated header; added information about SPICE 
C        quaternion conventions.
C
C-    SPICELIB Version 1.1.1, 13-JUN-2002 (FST)
C
C        Updated the Exceptions section to clarify exceptions that
C        are the result of changes made in the previous version of
C        the routine.
C
C-    SPICELIB Version 1.1.0, 04-MAR-1999 (WLT)
C
C        Added code to handle the case in which the input quaternion
C        is not of length 1.
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
C     quaternion to matrix
C
C-&
 
 
 
C
C     Local variables
C
      DOUBLE PRECISION      Q01
      DOUBLE PRECISION      Q02
      DOUBLE PRECISION      Q03
      DOUBLE PRECISION      Q12
      DOUBLE PRECISION      Q13
      DOUBLE PRECISION      Q23
      DOUBLE PRECISION      Q1S
      DOUBLE PRECISION      Q2S
      DOUBLE PRECISION      Q3S
 
      DOUBLE PRECISION      L2
      DOUBLE PRECISION      SHARPN
 
 
C
C     If a matrix R represents a rotation of r radians about the unit
C     vector n, we know that R can be represented as
C
C                                           2
C        I  +  sin(r) N  +  [ 1 - cos(r) ] N ,
C
C     where N is the matrix that satisfies
C
C        Nv = n x v
C
C     for all vectors v, namely
C
C             +-                -+
C             |  0    -n     n   |
C             |         3     2  |
C             |                  |
C        N =  |  n     0    -n   |.
C             |   3           1  |
C             |                  |
C             | -n     n     0   |
C             |   2     1        |
C             +-                -+
C
C
C      Define S as
C
C         sin(r/2) N,
C
C      and let our input quaternion Q be
C
C         ( q ,  q ,  q ,  q ).
C            0    1    2    3
C
C      Using the facts that
C
C                             2
C         1 - cos(r)  =  2 sin (r/2)
C
C      and
C
C         sin(r)      =  2 cos(r/2) sin(r/2),
C
C
C      we can express R as
C
C                                      2
C         I  +  2 cos(r/2) S    +   2 S,
C
C      or
C                                2
C         I  +  2 q  S    +   2 S.
C                  0
C
C      Since S is just
C
C         +-                -+
C         |  0    -q     q   |
C         |         3     2  |
C         |                  |
C         |  q     0    -q   |,
C         |   3           1  |
C         |                  |
C         | -q     q     0   |
C         |   2     1        |
C         +-                -+
C
C      our expression for R comes out to
C
C         +-                                                         -+
C         |          2   2                                            |
C         | 1 - 2 ( q + q  )    2( q q  -  q q )     2 ( q q  + q q ) |
C         |          2   3          1 2     0 3           1 3    0 2  |
C         |                                                           |
C         |                              2   2                        |
C         | 2( q q  +  q q )    1 - 2 ( q + q  )     2 ( q q  - q q ) |.
C         |     1 2     0 3              1   3            2 3    0 1  |
C         |                                                           |
C         |                                                   2   2   |
C         | 2( q q  -  q q )    2 ( q q  + q q )     1 - 2 ( q + q  ) |
C         |     1 3     0 2          2 3    0 1               1   2   |
C         +-                                                         -+
C
C
C      For efficiency, we avoid duplicating calculations where possible.
C
 
 
 
      Q01  =  Q(0) * Q(1)
      Q02  =  Q(0) * Q(2)
      Q03  =  Q(0) * Q(3)
 
      Q12  =  Q(1) * Q(2)
      Q13  =  Q(1) * Q(3)
 
      Q23  =  Q(2) * Q(3)
 
      Q1S  =  Q(1) * Q(1)
      Q2S  =  Q(2) * Q(2)
      Q3S  =  Q(3) * Q(3)
 
C
C     We sharpen the computation by effectively converting Q to
C     a unit quaternion if it isn't one already.
C
      L2   =  Q(0) * Q(0) + Q1S + Q2S + Q3S
 
      IF ( L2 .NE. 1.0D0 .AND. L2 .NE. 0.0D0 ) THEN
 
         SHARPN = 1.0D0 / L2
 
         Q01    = Q01 * SHARPN
         Q02    = Q02 * SHARPN
         Q03    = Q03 * SHARPN
 
         Q12    = Q12 * SHARPN
         Q13    = Q13 * SHARPN
 
         Q23    = Q23 * SHARPN
 
         Q1S    = Q1S * SHARPN
         Q2S    = Q2S * SHARPN
         Q3S    = Q3S * SHARPN
 
      END IF
 
 
 
      R(1,1) =  1.D0  -  2.D0 * ( Q2S + Q3S )
      R(2,1) =           2.D0 * ( Q12 + Q03 )
      R(3,1) =           2.D0 * ( Q13 - Q02 )
 
      R(1,2) =           2.D0 * ( Q12 - Q03 )
      R(2,2) =  1.D0  -  2.D0 * ( Q1S + Q3S )
      R(3,2) =           2.D0 * ( Q23 + Q01 )
 
      R(1,3) =           2.D0 * ( Q13 + Q02 )
      R(2,3) =           2.D0 * ( Q23 - Q01 )
      R(3,3) =  1.D0  -  2.D0 * ( Q1S + Q2S )
 
      RETURN
      END
