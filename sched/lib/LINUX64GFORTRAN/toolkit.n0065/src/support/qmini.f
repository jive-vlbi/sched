C$Procedure    QMINI ( Quaternion linear interpolation )

      SUBROUTINE QMINI ( INIT, FINAL, FRAC, QINTRP )
      IMPLICIT NONE

C$ Abstract
C
C     Interpolate between two quaternions using a constant angular
C     rate.
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
C     ROTATIONS
C
C$ Keywords
C
C     MATH
C     QUATERNION
C     ROTATION
C
C$ Declarations
 
      DOUBLE PRECISION      INIT   ( 0 : 3 )
      DOUBLE PRECISION      FINAL  ( 0 : 3 )
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      QINTRP ( 0 : 3 )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     INIT       I   Initial quaternion representing a rotation.
C     FINAL      I   Final quaternion representing a rotation.
C     FRAC       I   Fraction of rotation from INIT to FINAL by which
C                    to interpolate.
C     QINTRP     O   Linearly interpolated quaternion.
C
C$ Detailed_Input
C
C     INIT,
C     FINAL,
C     FRAC           are, respectively, two unit quaternions between
C                    which to interpolate, and an interpolation
C                    fraction.  See the Detailed_Output and Particulars
C                    sections for details.
C
C$ Detailed_Output
C
C     QINTRP         is the quaternion resulting from linear
C                    interpolation between INIT and FINAL by the
C                    fraction FRAC.  By "linear interpolation" we mean
C                    the following:
C
C                       We view INIT and FINAL as quaternions
C                       representing two values of a time-varying
C                       rotation matrix R(t) that rotates at a constant
C                       angular velocity (that is, the row vectors of
C                       R(t) rotate with constant angular velocity).
C                       We can say that
C
C                          INIT  represents R(t0)
C                          FINAL represents R(t1)
C
C                       Equivalently, the SPICELIB routine Q2M maps
C                       INIT and FINAL to rotation matrices
C                       corresponding to R(t0) and R(t1) respectively.
C
C                       "Linear interpolation by the fraction FRAC"
C                       means that QINTRP represents the matrix
C                       R(t) evaluated at time
C
C                          t = t0   +   FRAC * (t1 - t0)
C
C                       and that the sign of QINTRP is such that
C                       QINTRP is closer to both INIT and FINAL
C                       than is -QINTRP.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If either of INIT or FINAL is not a unit quaternion, the error
C         SPICE(NOTAROTATION) is signaled.
C
C     2)  This routine assumes that the quaternion QUOT defined by
C   
C                               *
C            QUOT = FINAL * INIT
C
C         has rotation angle THETA radians, where
C
C            0  <  THETA  <  pi
C               -         
C
C         Above the * superscript denotes quaternion conjugation.
C
C         The caller must test this condition on THETA; it is not
C         tested by this routine. A quick check may be performed by
C         verifying that
C
C            0  <  QUOT(0)
C               
C         Note that this inequality is strict because rotations of
C         pi radians cannot be linearly interpolated so as to
C         produce a unique result.
C
C         This routine cannot distinguish between rotations of THETA
C         radians, where THETA is in the interval [0, pi), and
C         rotations of
C
C            THETA   +   2 * k * pi
C
C         radians, where k is any integer. These "large" rotations will
C         yield invalid results when interpolated.  You must ensure
C         that the inputs you provide to this routine will not be
C         subject to this sort of ambiguity.  If in fact you are
C         interpolating a time-dependent rotation with constant angular
C         velocity AV between times t0 and t1, you must ensure that
C
C            || AV ||  *  |t1 - t0|   <   pi.
C
C         Here we assume that the magnitude of AV is the angular rate
C         of the rotation in units of radians per second.
C
C
C     3)  When FRAC is outside of the interval [0, 1], the process
C         performed is "extrapolation", not interpolation.  Such
C         values of FRAC are permitted.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     In the discussion below, we assume that the conditions specified
C     in item (2) of the Exceptions section have been satisfied.
C
C     As we've said, we view INIT and FINAL as quaternions representing
C     two values of a time-varying rotation matrix R(t) that rotates at
C     a constant angular velocity; we define R(t), t0, and t1 so that
C
C        INIT  represents  R(t0)
C        FINAL represents  R(t1).
C
C     The output quaternion QINTRP represents R(t) evaluated at the
C     time
C
C        t0   +   FRAC * (t1 - t0).
C
C     How do we evaluate R at times between t0 and t1? Since the row
C     vectors of R are presumed to rotate with constant angular
C     velocity, we will first find the rotation axis of the quotient
C     rotation Q that maps the row vectors of R from their initial to
C     final position.  Since the rows of R are the columns of the
C     transpose of R, we can write:
C
C             T               T
C        R(t1)   =   Q * R(t0),
C
C     Since
C
C             T            T                  T
C        R(t1)   =  ( R(t1)  * R(t0) ) * R(t0)
C
C
C     we can find Q, as well as a rotation axis A and an angle THETA
C     in the range [0, pi] such that Q rotates vectors by THETA
C     radians about axis A.
C
C     We'll use the notation
C
C        [ x ]
C             N
C
C     to indicate a coordinate system rotation of x radians about the
C     vector N.  Having found A and THETA, we can write (note that
C     the sign of the rotation angle is negated because we're using
C     a coordinate system rotation)
C
C            T                  (t  - t0)                T
C        R(t)   =  [ - THETA *  ---------  ]    *   R(t0)
C                               (t1 - t0)   A
C
C     Thus R(t) and QINTRP are determined.
C
C     The input argument FRAC plays the role of the quotient
C
C        t  - t0
C        -------
C        t1 - t0
C
C     shown above.
C
C
C$ Examples
C
C     1)  Suppose we want to interpolate between quaternions
C         Q1 and Q2 that give the orientation of a spacecraft structure
C         at times t1 and t2.  We wish to find an approximation of the
C         structure's orientation at the midpoint of the time interval
C         [t1, t2].  We assume that the angular velocity of the
C         structure equals the constant AV between times t1 and t2.  We
C         also assume that
C
C            || AV ||  *  (t2 - t1)   <   pi.
C
C         Then the code fragment
C
C            CALL QMINI ( Q1, Q2, 0.5D0, QINTRP, SCLDAV )
C
C         produces the approximation we desire.
C
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
C-    SPICELIB Version 1.0.1, 28-FEB-2008 (NJB)
C
C        The discussion of exception #2 was expanded.
C
C-    SPICELIB Version 1.0.0, 19-JUL-2005 (NJB)
C
C-&
 
C$ Index_Entries
C
C     linear interpolation between quaternions
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      BRCKTD

C
C     Local variables
C
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      INSTAR ( 0 : 3 )
      DOUBLE PRECISION      INTANG
      DOUBLE PRECISION      Q      ( 0 : 3 )
      DOUBLE PRECISION      QSCALE ( 0 : 3 )
      DOUBLE PRECISION      VMAG

C
C     Use discovery check-in.
C
C
C
C     Find the conjugate INSTAR of the input quaternion INIT.
C
      INSTAR(0) = INIT(0)
      
      CALL VMINUS ( INIT(1),  INSTAR(1) )

C
C     Find the quotient quaternion Q that maps INIT to FINAL.
C
      CALL QXQ ( FINAL, INSTAR, Q )

C
C     Extract the rotation angle from Q. Use arccosine for 
C     speed, sacrificing some accuracy.
C
      ANGLE = 2.D0 * ACOS (  BRCKTD( Q(0), -1.D0, 1.D0 )  )

C
C     Create a quaternion QSCALE from the rotation axis of the quotient 
C     and the scaled rotation angle.
C
      INTANG = FRAC * ANGLE / 2.D0

      QSCALE(0) = COS ( INTANG )

C
C     Get the unit vector parallel to the vector part of Q.  
C     UNORM does exactly what we want here, because if the vector
C     part of Q is zero, the returned "unit" vector will be the
C     zero vector.
C
      CALL UNORM ( Q(1), AXIS, VMAG )

C
C     Form the vector part of QSCALE.
C
      CALL VSCL (  SIN ( INTANG ),  AXIS,  QSCALE(1) )

C
C     Apply QSCALE to INIT to produce the interpolated quaternion we
C     seek.
C      
      CALL QXQ ( QSCALE, INIT, QINTRP )

      RETURN
      END


