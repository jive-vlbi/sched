C$Procedure QDQ2AV (Quaternion and quaternion derivative to a.v.)

      SUBROUTINE QDQ2AV ( Q, DQ, AV )

C$ Abstract
C
C     Derive angular velocity from a unit quaternion and its derivative
C     with respect to time.
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

      DOUBLE PRECISION      Q    ( 0 : 3 )
      DOUBLE PRECISION      DQ   ( 0 : 3 )
      DOUBLE PRECISION      AV   ( 3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     Q          I   Unit SPICE quaternion.
C     DQ         I   Derivative of Q with respect to time.
C     AV         O   Angular velocity defined by Q and DQ.
C
C$ Detailed_Input
C
C     Q              is a unit length 4-vector representing a
C                    SPICE-style quaternion. See the discussion of
C                    quaternion styles in Particulars below.
C
C     DQ             is a 4-vector representing the derivative of
C                    Q with respect to time. 
C
C$ Detailed_Output
C
C     AV             is 3-vector representing the angular velocity
C                    defined by Q and DQ, that is, the angular velocity
C                    of the frame defined by the rotation matrix
C                    associated with Q.  This rotation matrix can be
C                    obtained via the SPICELIB routine Q2M; see the
C                    Particulars section for the explicit matrix 
C                    entries.
C
C                    AV is the vector (imaginary) part of the
C                    quaternion product
C
C                             *    
C                       -2 * Q  * DQ  
C
C                    This angular velocity is the same vector that
C                    could be obtained (much less efficiently ) by
C                    mapping Q and DQ to the corresponding C-matrix R
C                    and its derivative DR, then calling the SPICELIB
C                    routine XF2RAV.
C
C                    AV has units of
C
C                       radians / T
C
C                    where
C
C                       1 / T
C
C                    is the unit associated with DQ.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) A unitized version of input quaternion is used in the
C        computation.  No attempt is made to diagnose an invalid
C        input quaternion.
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
C     About this routine
C     ==================
C
C     Given a time-dependent SPICE quaternion representing the
C     attitude of an object, we can obtain the object's angular
C     velocity AV in terms of the quaternion Q and its derivative 
C     with respect to time DQ:
C
C                          *
C        AV  =  Im ( -2 * Q  * DQ )                                  (1)
C     
C     That is, AV is the vector (imaginary) part of the product
C     on the right hand side (RHS) of equation (1).  The scalar part 
C     of the RHS is zero.
C
C     We'll now provide an explanation of formula (1). For any
C     time-dependent rotation, the associated angular velocity at a
C     given time is a function of the rotation and its derivative at
C     that time. This fact enables us to extend a proof for a limited
C     subset of rotations to *all* rotations:  if we find a formula
C     that, for any rotation in our subset, gives us the angular
C     velocity as a function of the rotation and its derivative, then
C     that formula must be true for all rotations.
C
C     We start out by considering the set of rotation matrices
C
C        R(t) = M(t)C                                                (2)
C
C     where C is a constant rotation matrix and M(t) represents a
C     matrix that "rotates" with constant, unit magnitude angular
C     velocity and that is equal to the identity matrix at t = 0.
C
C     For future reference, we'll consider C to represent a coordinate
C     transformation from frame F1 to frame F2.  We'll call F1 the
C     "base frame" of C.  We'll let AVF2 be the angular velocity of
C     M(t) relative to F2 and AVF1 be the same angular velocity
C     relative to F1.
C
C     Referring to the axis-and-angle decomposition of M(t)
C
C                                                2
C        M(t) = I + sin(t)OMEGA + (1-cos(t))OMEGA                    (3)
C
C     (see the Rotation Required Reading for a derivation) we 
C     have
C
C        d(M(t))|
C        -------|     = OMEGA                                        (4)
C          dt   |t=0
C
C     Then the derivative of R(t) at t = 0 is given by
C
C
C        d(R(t))|
C        -------|     = OMEGA  * C                                   (5)
C          dt   |t=0
C
C
C     The rotation axis A associated with OMEGA is defined by        (6)
C
C        A(1) =  - OMEGA(2,3)
C        A(2) =    OMEGA(1,3)
C        A(3) =  - OMEGA(1,2)
C       
C     Since the coordinate system rotation M(t) rotates vectors about A
C     through angle t radians at time t, the angular velocity AVF2 of
C     M(t) is actually given by
C
C        AVF2  =  - A                                                (7)
C
C     This angular velocity is represented relative to the image
C     frame F2 associated with the coordinate transformation C.
C
C     Now, let's proceed to the angular velocity formula for
C     quaternions.
C     
C     To avoid some verbiage, we'll freely use 3-vectors to represent
C     the corresponding pure imaginary quaternions.
C
C     Letting QR(t), QM(t), and QC be quaternions representing the
C     time-dependent matrices R(t), M(t) and C respectively, where
C     QM(t) is selected to be a differentiable function of t in a
C     neighborhood of t = 0, the quaternion representing R(t) is
C
C        QR(t) = QM(t) * QC                                          (8)
C
C     Differentiating with respect to t, then evaluating derivatives
C     at t = 0, we have
C
C        d(QR(t))|         d(QM(t))|
C        --------|     =   --------|     * QC                        (9)
C           dt   |t=0         dt   |t=0
C
C
C     Since QM(t) represents a rotation having axis A and rotation
C     angle t, then (according to the relationship between SPICE
C     quaternions and rotations set out in the Rotation Required
C     Reading), we see QM(t) must be the quaternion (represented as the
C     sum of scalar and vector parts):
C     
C        cos(t/2)  +  sin(t/2) * A                                  (10)
C
C     where A is the rotation axis corresponding to the matrix
C     OMEGA introduced in equation (3).  By inspection
C
C        d(QM(t))|
C        --------|     =   1/2 * A                                  (11)
C           dt   |t=0
C
C     which is a quaternion with scalar part zero.  This allows us to 
C     rewrite the quaternion derivative  
C
C        d(QR(t))|          
C        --------|     =   1/2  *  A  *  QC                         (12)
C           dt   |t=0          
C
C     or for short,
C
C        DQ = 1/2 * A * QC                                          (13)
C
C     Since from (7) we know the angular velocity AVF2 of the frame
C     associated with QM(t) is the negative of the rotation axis
C     defined by (3), we have
C
C        DQ = - 1/2 * AVF2 * QC                                     (14)
C
C     Since 
C
C        AVF2 = C * AVF1                                            (15)
C
C     we can apply the quaternion transformation formula
C     (from the Rotation Required Reading)
C
C                                 *
C        AVF2 =  QC  *  AVF1  * QC                                  (16)
C                     
C     Now we re-write (15) as
C
C                                     *
C        DQ = - 1/2 * ( QC * AVF1 * QC ) * QC                      
C 
C           = - 1/2 *   QC * AVF1                                   (17)
C
C     Then the angular velocity vector AVF1 is given by
C
C                       *
C        AVF1  = -2 * QC  * DQ                                      (18)
C
C     The relation (18) has now been demonstrated for quaternions
C     having constant, unit magnitude angular velocity.  But since
C     all time-dependent quaternions having value QC and derivative
C     DQ at a given time t have the same angular velocity at time t, 
C     that angular velocity must be AVF1.  
C
C$ Examples
C
C     The following test program creates a quaternion and quaternion
C     derivative from a known rotation matrix and angular velocity
C     vector.  The angular velocity is recovered from the quaternion
C     and quaternion derivative by calling QDQ2AV and by an
C     alternate method; the results are displayed for comparison.
C
C              PROGRAM TQDQ2AV
C              IMPLICIT NONE
C        C
C        C     Start with a known rotation and angular velocity.  Find
C        C     the quaternion and quaternion derivative.  The latter is
C        C     computed from
C        C
C        C                       *    
C        C        AV  =   -2  * Q  * DQ  
C        C                   
C        C        DQ  =  -1/2 * Q  * AV
C        C
C        C
C        C     SPICELIB Functions
C        C
C              DOUBLE PRECISION      RPD
C
C        C
C        C     Local variables
C        C
C              DOUBLE PRECISION      ANGLE  ( 3 )
C              DOUBLE PRECISION      AV     ( 3 )
C              DOUBLE PRECISION      AVX    ( 3 )
C              DOUBLE PRECISION      DM     ( 3,  3 )
C              DOUBLE PRECISION      DQ     ( 0 : 3 )
C              DOUBLE PRECISION      EXPAV  ( 3 )
C              DOUBLE PRECISION      M      ( 3,  3 )
C              DOUBLE PRECISION      MOUT   ( 3,  3 )
C              DOUBLE PRECISION      Q      ( 0 : 3 )
C              DOUBLE PRECISION      QAV    ( 0 : 3 )
C              DOUBLE PRECISION      XTRANS ( 6,  6 )
C
C              INTEGER               I
C              INTEGER               J
C
C        C
C        C     Pick some Euler angles and form a rotation matrix.
C        C
C              ANGLE(1) = -20.0 * RPD()
C              ANGLE(2) =  50.0 * RPD()
C              ANGLE(3) = -60.0 * RPD()
C
C              CALL EUL2M ( ANGLE(3), ANGLE(2), ANGLE(1), 3, 1, 3, M )
C
C              CALL M2Q   ( M, Q )
C
C        C
C        C     Choose an angular velocity vector.
C        C
C              EXPAV(1) =  1.0D0
C              EXPAV(2) =  2.0D0
C              EXPAV(3) =  3.0D0
C
C        C
C        C     Form the quaternion derivative.
C        C
C              QAV(0)    =  0.D0
C              CALL VEQU ( EXPAV, QAV(1) )
C 
C              CALL QXQ ( Q, QAV, DQ )
C
C              CALL VSCLG ( -0.5D0, DQ, 4, DQ )
C
C        C
C        C     Recover angular velocity from Q and DQ using QDQ2AV.  
C        C
C              CALL QDQ2AV ( Q, DQ, AV )
C
C        C
C        C     Now we'll obtain the angular velocity from Q and 
C        C     DQ by an alternate method.
C        C
C        C     Convert Q back to a rotation matrix.
C        C
C              CALL Q2M ( Q, M )
C
C        C
C        C     Convert Q and DQ to a rotation derivative matrix.  This
C        C     somewhat messy procedure is based on differentiating the
C        C     formula for deriving a rotation from a quaternion, then
C        C     substituting components of Q and DQ into the derivative
C        C     formula.
C        C
C
C              DM(1,1)  =  -4.D0 * (   Q(2)*DQ(2)  +  Q(3)*DQ(3)  )
C
C              DM(1,2)  =   2.D0 * (   Q(1)*DQ(2)  +  Q(2)*DQ(1)
C             .                      - Q(0)*DQ(3)  -  Q(3)*DQ(0)  ) 
C
C              DM(1,3)  =   2.D0 * (   Q(1)*DQ(3)  +  Q(3)*DQ(1)
C             .                      + Q(0)*DQ(2)  +  Q(2)*DQ(0)  ) 
C
C              DM(2,1)  =   2.D0 * (   Q(1)*DQ(2)  +  Q(2)*DQ(1)
C             .                      + Q(0)*DQ(3)  +  Q(3)*DQ(0)  ) 
C
C              DM(2,2)  =  -4.D0 * (   Q(1)*DQ(1)  +  Q(3)*DQ(3)  )
C
C              DM(2,3)  =   2.D0 * (   Q(2)*DQ(3)  +  Q(3)*DQ(2)
C             .                      - Q(0)*DQ(1)  -  Q(1)*DQ(0)  ) 
C
C              DM(3,1)  =   2.D0 * (   Q(3)*DQ(1)  +  Q(1)*DQ(3)
C             .                      - Q(0)*DQ(2)  -  Q(2)*DQ(0)  ) 
C
C              DM(3,2)  =   2.D0 * (   Q(2)*DQ(3)  +  Q(3)*DQ(2)
C             .                      + Q(0)*DQ(1)  +  Q(1)*DQ(0)  ) 
C
C              DM(3,3)  =  -4.D0 * (   Q(1)*DQ(1)  +  Q(2)*DQ(2)  )
C
C        C
C        C     Form the state transformation matrix corresponding to M
C        C     and DM.
C
C              CALL CLEARD ( 36, XTRANS )
C
C        C
C        C     Upper left block:
C        C
C              DO I = 1, 3
C
C                 DO J = 1, 3 
C                    XTRANS(I,J) = M(I,J)
C                 END DO
C
C              END DO
C
C
C        C
C        C     Lower right block:
C        C
C              DO I = 1, 3
C
C                 DO J = 1, 3 
C                    XTRANS(3+I,3+J) = M(I,J)
C                 END DO
C
C              END DO
C
C        C
C        C     Lower left block:
C        C
C              DO I = 1, 3
C
C                 DO J = 1, 3 
C                    XTRANS(3+I,J) = DM(I,J)
C                 END DO
C
C              END DO
C
C        C
C        C     Now use XF2RAV to produce the expected angular velocity.
C        C
C              CALL XF2RAV ( XTRANS, MOUT, AVX )
C 
C        C
C        C     The results should match to nearly full double 
C        C     precision.
C        C
C              WRITE(*,*) 'Original angular velocity:  ', EXPAV
C              WRITE(*,*) 'QDQ2AV''s angular velocity:  ', AV
C              WRITE(*,*) 'XF2RAV''s angular velocity:  ', AVX
C
C              END
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
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.1, 26-FEB-2008 (NJB)
C
C        Updated header; added information about SPICE 
C        quaternion conventions.
C
C-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCL call.
C
C-    SPICELIB Version 1.0.1, 24-FEB-2004 (NJB)
C
C        Made minor edits to the Particulars header section.
C
C-    SPICELIB Version 1.0.0, 26-AUG-2002 (NJB)
C
C
C-&
 
C$ Index_Entries
C
C     angular velocity from  quaternion and derivative
C-&

C$ Revisions
C
C-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCL call.
C
C-& 

C
C     Local variables
C
      DOUBLE PRECISION      QHAT  ( 0 : 3 )
      DOUBLE PRECISION      QSTAR ( 0 : 3 )
      DOUBLE PRECISION      QTEMP ( 0 : 3 )


C
C     Get a unitized copy of the input quaternion.
C
      CALL VHATG  ( Q,    4,  QHAT     )

C
C     Get the conjugate QSTAR of QHAT.
C
      QSTAR(0) = QHAT(0)
      CALL VMINUS ( QHAT(1),  QSTAR(1) )

C
C     Compute the angular velocity via the relationship
C
C                       *
C           AV  = -2 * Q  * DQ
C             

      CALL QXQ    ( QSTAR,  DQ,        QTEMP )         
      CALL VSCL   ( -2.D0,  QTEMP(1),  AV    )

      RETURN
      END


      
