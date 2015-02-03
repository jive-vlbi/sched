C$Procedure      RAXISA ( Rotation axis of a matrix )
 
      SUBROUTINE RAXISA ( MATRIX, AXIS, ANGLE )
 
C$ Abstract
C
C     Compute the axis of the rotation given by an input matrix
C     and the angle of the rotation about that axis.
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
C     ANGLE,  MATRIX,  ROTATION
C
C$ Declarations
 
      DOUBLE PRECISION    MATRIX ( 3, 3 )
      DOUBLE PRECISION    AXIS   (    3 )
      DOUBLE PRECISION    ANGLE
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MATRIX     I   3x3 rotation matrix in double precision.
C     AXIS       O   Axis of the rotation.
C     ANGLE      O   Angle through which the rotation is performed.
C
C$ Detailed_Input
C
C     MATRIX     is a 3x3 rotation matrix in double precision.
C
C$ Detailed_Output
C
C     AXIS       is a unit vector pointing along the axis of the
C                rotation.  In other words, AXIS is a unit eigenvector
C                of the input matrix, corresponding to the eigenvalue
C                1.  If the input matrix is the identity matrix, AXIS
C                will be the vector (0, 0, 1). If the input rotation is
C                a rotation by PI radians, both AXIS and -AXIS may be
C                regarded as the axis of the rotation.
C
C     ANGLE      is the angle between V and MATRIX*V for any non-zero
C                vector V orthogonal to AXIS.  Angle is given in
C                radians.  The angle returned will be in the range from
C                0 to PI.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the input matrix is not a rotation matrix (where a fairly
C        loose tolerance is used to check this) a routine in the
C        call tree of this routine will signal an error indicating
C        the problem.
C
C     2) If the input matrix is the identity matrix, this routine
C        returns an angle of 0.0, and an axis of ( 0.0, 0.0, 1.0 ).
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Every rotation matrix has an axis A such any vector, V, parallel
C     to that axis satisfies the equation
C
C        V = MATRIX * V
C
C     This routine returns a unit vector AXIS parallel to the axis of
C     the input rotation matrix.  Moreover for any vector W orthogonal
C     to the axis of the rotation
C
C        AXIS  and  W x MATRIX*W
C
C        (where "x" denotes the cross product operation)
C
C     will be positive scalar multiples of one another (at least to
C     within the ability to make such computations with double
C     precision arithmetic, and under the assumption that the MATRIX
C     does not represent a rotation by zero or Pi radians).
C
C     The angle returned will be the angle between W and MATRIX*W for
C     any vector orthogonal to AXIS.
C
C     If the input matrix is a rotation by 0 or PI radians some choice
C     must be made for the AXIS returned.  In the case of a rotation by
C     0 radians, AXIS is along the positive z-axis. In the case of a
C     rotation by 180 degrees, two choices are
C
C$ Examples
C
C     This routine can be used to numerically approximate the
C     instantaneous angular velocity vector of a rotating object.
C
C     Suppose that R(t) is the rotation matrix whose columns represent
C     the inertial pointing vectors of the bodyfixed axes of an object
C     at time t.
C
C     Then the angular velocity vector points along the vector given
C     by:
C                             T
C         limit  AXIS( R(t+h)R )
C         h-->0
C
C     And the magnitude of the angular velocity at time t is given by:
C
C                             T
C         d ANGLE ( R(t+h)R(t) )
C         ----------------------   at   h = 0
C         dh
C
C     Thus to approximate the angular velocity vector the following
C     code fragment will do
C
C            Load t      into the double precision variable T
C            Load h      into the double precision variable H
C            Load R(t+h) into the 3 by 3 double precision array RTH
C            Load R(t)   into the 3 by 3 double precision array RT
C               .
C               .
C               .
C            compute the infinitesimal rotation R(t+h)R(t)**T
C
C            CALL MXMT   ( RTH,    RT,   INFROT )
C
C            compute the AXIS and ANGLE of the infinitesimal rotation
C
C            CALL RAXISA ( INFROT, AXIS, ANGLE  )
C
C            scale axis to get the angular velocity vector
C
C            CALL VSCL   ( ANGLE/H, AXIS, ANGVEL )
C
C
C$ Restrictions
C
C     1) If the input matrix is not a rotation matrix but is close
C        enough to pass the tests this routine performs on it, no error
C        will be signaled, but the results may have poor accuracy.
C
C     2) The input matrix is taken to be an object that acts on
C        (rotates) vectors---it is not regarded as a coordinate
C        transformation.  To find the axis and angle of a coordinate
C        transformation, input the transpose of that matrix to this
C        routine.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 2.1.2, 02-JAN-2008 (EDW)
C
C        Minor edit to the ANGLE declaration strictly
C        identifying the constant as a double.
C
C        From: 
C
C           ANGLE = 2.0 * DATAN2( VNORM(Q(1)), Q(0) )
C
C        To:
C
C           ANGLE = 2.D0 * DATAN2( VNORM(Q(1)), Q(0) )
C
C-    SPICELIB Version 2.1.1, 05-JAN-2005 (NJB)
C
C        Minor edits and formatting changes were made.
C
C-    SPICELIB Version 2.1.0, 30-MAY-2002 (FST)
C
C        This routine now participates in error handling properly.
C
C-    SPICELIB Version 2.0.0, 19-SEP-1999 (WLT)
C
C        The routine was re-written so as to avoid the numerical
C        instabilities present in the previous implementation for
C        rotations very near zero or 180 degrees.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
C
C        The declaration for the SPICELIB function PI is now
C        preceded by an EXTERNAL statement declaring PI to be an
C        external function. This removes a conflict with any
C        compilers that have a PI intrinsic function.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     axis and angle of a rotation matrix
C
C-&
 
 
C$ Revisions
C
C-    SPICELIB Version 2.1.0, 30-MAY-2002 (FST)
C
C        Calls to CHKIN and CHKOUT in the standard SPICE error
C        handling style were added.  Versions prior to 2.0.0
C        were error free, however the call to M2Q introduced in
C        version 2.0.0 signals an error if the input matrix is
C        not sufficiently close to a rotation.
C
C        Additionally, FAILED is now checked after the call to
C        M2Q.  This prevents garbage from being placed into the
C        output arguments.
C
C-    SPICELIB Version 2.0.0, 21-SEP-1999 (WLT)
C
C        The routine was re-written so as to avoid the numerical
C        instabilities present in the previous implementation for
C        rotations very near zero or 180 degrees.
C
C-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
C
C        The declaration for the SPICELIB function PI is now
C        preceded by an EXTERNAL statement declaring PI to be an
C        external function. This removes a conflict with any
C        compilers that have a PI intrinsic function.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-    Beta Version 1.1.0, 3-JAN-1989 (WLT)
C
C     Even though the routine stipulates that the input matrix
C     should be a rotation matrix, it might not be.  As a result
C     we could have negative numbers showing up where we need
C     to take square roots.  This fix simply bounds these values
C     so that Fortran intrinsics always get reasonable input values.
C
C     Add and example to the header.
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      VNORM
      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO
 
      EXTERNAL              PI
      DOUBLE PRECISION      PI
 
C
C     Local Variables
C
      DOUBLE PRECISION      Q ( 0: 3 )
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'RAXISA' )
      END IF
 
C
C     Construct the quaternion corresponding to the input rotation
C     matrix
C
      CALL M2Q ( MATRIX, Q )
 
C
C     Check FAILED and return if an error has occurred.
C
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'RAXISA' )
         RETURN
      END IF
 
C
C     The quaternion we've just constructed is of the form:
C
C         cos(ANGLE/2) + sin(ANGLE/2) * AXIS
C
C     We take a few precautions to handle the case of an identity
C     rotation.
C
      IF ( VZERO(Q(1)) ) THEN
 
         ANGLE = 0
         AXIS(1) = 0.0D0
         AXIS(2) = 0.0D0
         AXIS(3) = 1.0D0
 
      ELSE IF ( Q(0) .EQ. 0.0D0 ) THEN
 
         ANGLE   = PI()
         AXIS(1) = Q(1)
         AXIS(2) = Q(2)
         AXIS(3) = Q(3)
 
      ELSE
 
         CALL VHAT ( Q(1), AXIS )
         ANGLE = 2.D0 * DATAN2 ( VNORM(Q(1)), Q(0) )
 
      END IF
 
      CALL CHKOUT ( 'RAXISA' )
      RETURN
 
      END
