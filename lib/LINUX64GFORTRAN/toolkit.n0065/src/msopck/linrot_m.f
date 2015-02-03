C$Procedure    LINROT_M ( Linear interpolation between rotations )
 
      SUBROUTINE LINROT_M ( INIT, FINAL, FRAC, R, SCLDAV )
 
C$ Abstract
C
C     Interpolate between two rotations using a constant angular rate.
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
C     MOSPICE
C     MATRIX
C     ROTATION
C
C$ Declarations
 
      DOUBLE PRECISION      INIT   ( 3, 3 )
      DOUBLE PRECISION      FINAL  ( 3, 3 )
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      R      ( 3, 3 )
      DOUBLE PRECISION      SCLDAV ( 3 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     INIT       I   Initial rotation.
C     FINAL      I   Final rotation.
C     FRAC       I   Fraction of rotation from INIT to FINAL by which
C                    to interpolate.
C     R          O   Linearly interpolated rotation.
C     SCLDAV     O   Scaled angular velocity of rotation.
C
C$ Detailed_Input
C
C     INIT,
C     FINAL,
C     FRAC           are, respectively, two rotation matrices between
C                    which to interpolate, and an interpolation
C                    fraction.  See the $Detailed_Output and
C                    $Particulars sections for details.
C
C$ Detailed_Output
C
C     R              is the matrix resulting from linear interpolation
C                    between INIT and FINAL by the fraction FRAC.  By
C                    `linear interpolation' we mean the following:
C
C                       We view INIT and FINAL as two values of a
C                       time-varying rotation matrix R(t) that rotates
C                       at a constant angular velocity (that is, the
C                       row vectors of R(t) rotate with constant angular
C                       velocity).  We can say that
C
C                          INIT   =  R(t0)
C                          FINAL  =  R(t1).
C
C                       `Linear interpolation by the fraction FRAC'
C                       means that we evalute R(t) at time
C
C                          t0   +   FRAC * (t1 - t0).
C
C
C     SCLDAV         is a scaled version of the constant angular
C                    velocity vector used for interpolation.  When
C                    SCLDAV is divided by the scale factor
C
C                       t1 - t0,
C
C                    the result is the constant angular velocity
C                    assumed for the rows of R(t) in order to perform
C                    linear interpolation.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  If either of INIT or FINAL is not a rotation matrix, the error
C         SPICE(NOTAROTATION) is signalled.
C
C     2)  This routine assumes that the rotation that maps INIT to FINAL
C         has a rotation angle THETA radians, where
C
C            0  <  THETA  <  pi.
C               _
C
C         This routine cannot distinguish between rotations of THETA
C         radians, where THETA is in the interval [0, pi), and
C         rotations of
C
C            THETA   +   2 * k * pi
C
C         radians, where k is any integer.  These `large' rotations will
C         yield invalid results when interpolated.  You must ensure that
C         the inputs you provide to this routine will not be subject to
C         this sort of ambiguity.  If in fact you are interpolating the
C         position of a rotating matrix with constant angular velocity
C         AV between times t0 and t1, you must ensure that
C
C            || AV ||  *  |t1 - t0|   <   pi.
C
C         Here we assume that the magnitude of AV is the angular rate
C         of the rotating matrix in units of radians per second.
C
C
C     3)  When FRAC is outside of the interval [0, 1], the process
C         performed is `extrapolation', not interpolation.  Such
C         values of FRAC are permitted.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     In the discussion below, we assume that the conditions specified
C     in item (2) of $ Exceptions have been satisfied.
C
C     As we've said, we view INIT and FINAL as two values of a
C     time-varying rotation matrix R(t) that rotates at a constant
C     angular velocity; we define R(t), t0, and t1 so that
C
C        INIT   =  R(t0)
C        FINAL  =  R(t1).
C
C     The output matrix R is R(t) evaluated at the time
C
C        t0   +   FRAC * (t1 - t0).
C
C     How do we evaluate R at times between t0 and t1?  Since the
C     column vectors of R are presumed to rotate with constant
C     angular velocity, we will first find the rotation axis of
C     the matrix M that maps the row vectors of R from their initial
C     to final position.  Since the rows of R are the columns of
C     the transpose of R, we can write:
C
C             T               T
C        R(t1)   =   M * R(t0),
C
C     or
C             T              T
C        FINAL   =   M * INIT
C
C     Since
C             T            T                T
C        FINAL   =  ( FINAL  * INIT ) * INIT
C
C     we can find M, as well as a rotation axis A and an angle THETA
C     in the range [0, pi] such that M rotates vectors by THETA
C     radians about axis A.
C
C     We'll use the notation
C
C        [ x ]
C             N
C
C     to indicate a coordinate system rotation of x radians about the
C     vector N.  Having found A and THETA, we can write
C
C            T                 (t  - t0)                 T
C        R(t)   =  [  THETA *  ---------  ]     *   R(t0)
C                              (t1 - t0)    A
C
C     Thus R(t) is determined.
C
C     The input argument FRAC plays the role of the quotient
C
C        t  - t0
C        -------
C        t1 - t0
C
C     shown above.
C
C     SCLDAV is a vector parallel to the rotation axis of M and
C     having length equal to the rotation angle of M (which is in
C     the range [0, pi]).
C
C$ Examples
C
C     1)  Suppose we want to interpolate between two rotation matrices
C         R1 and R2 that give the orientation of a spacecraft structure
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
C            CALL LINROT_M ( R1, R2, 0.5D0, R, SCLDAV )
C
C         produces the approximation we desire.
C
C
C     2)  Suppose R1 is the identity and R2 is
C
C            [ pi/2 ] .
C                    3
C
C         Then the code fragment
C
C            CALL LINROT_M ( R1, R2, FRAC, R, SCLDAV )
C
C         returns SCLDAV as the vector
C
C            ( 0, 0, pi/2 ).
C
C
C     3)  As long as R1 and R2 are not equal, the code fragment
C
C            CALL LINROT_M ( R1,     R2,            FRAC,  R,  SCLDAV )
C            CALL AXISAR   ( SCLDAV, VNORM(SCLDAV),  M                )
C            CALL MXMT     ( R1,     M,             R2                )
C
C         should leave R2 unchanged, except for round-off error.
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
C-    MOSPICE Version 1.0.0, 17-APR-1992 (NJB)
C
C-&
 
C$ Index_Entries
C
C     linear interpolation between rotations
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      LOGICAL               ISROT
 
C
C     Local parameters
C
 
C
C     NTOL and DTOL are tolerances used for determining whether INIT
C     and FINAL are rotation matrices.  NTOL is bound on the deviation
C     of the norms of the columns of a matrix from 1, and DTOL is a
C     bound on the deviation of the determinant of a matrix from 1.
C
      DOUBLE PRECISION      DTOL
      PARAMETER           ( DTOL = 1.D-8 )
 
      DOUBLE PRECISION      NTOL
      PARAMETER           ( NTOL = 1.D-8 )
 
C
C     Local variables
C
      DOUBLE PRECISION      AXIS  ( 3 )
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      DELTA ( 3, 3 )
      DOUBLE PRECISION      M     ( 3, 3 )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LINROT_M' )
      END IF
 
C
C     INIT and FINAL must both be rotation matrices.
C
      IF (  .NOT.  ISROT ( INIT, NTOL, DTOL )  ) THEN
 
         CALL SETMSG ( 'INIT is not a rotation.' )
         CALL SIGERR ( 'SPICE(NOTAROTATION)'     )
         CALL CHKOUT ( 'LINROT_M'                )
         RETURN
 
      ELSE IF (  .NOT.  ISROT ( FINAL, NTOL, DTOL )  ) THEN
 
         CALL SETMSG ( 'FINAL is not a rotation.' )
         CALL SIGERR ( 'SPICE(NOTAROTATION)'      )
         CALL CHKOUT ( 'LINROT_M'                 )
         RETURN
 
      END IF
 
C
C     Little to do, really.  Define M by
C
C             T              T
C        FINAL   =   M * INIT
C
C     Since
C             T            T                T
C        FINAL   =  ( FINAL  * INIT ) * INIT
C
C     and all of the matrices here are non-singular, we have
C
C                          T
C        M       =  ( FINAL  * INIT )
C
C
C     Find an axis and angle for the rotation M, and interpolate the
C     angle.  Form the interpolated rotation DELTA.
C
      CALL MTXM   ( FINAL,  INIT,  M                   )
      CALL RAXISA ( M,      AXIS,  ANGLE               )
      CALL AXISAR (         AXIS,  ANGLE * FRAC, DELTA )
 
C
C     Since
C
C         T                 T
C        R   =  DELTA * INIT
C
C     we can find R directly by computing
C
C                           T
C        R   =  INIT * DELTA
C
 
      CALL MXMT   ( INIT, DELTA, R )
 
C
C     Finding the `constant' angular velocity vector is easy to do.
C
      CALL VSCL   ( ANGLE, AXIS, SCLDAV )
 
 
      CALL CHKOUT ( 'LINROT_M' )
      RETURN
      END
