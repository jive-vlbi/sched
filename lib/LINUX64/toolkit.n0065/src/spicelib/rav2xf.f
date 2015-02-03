C$Procedure      RAV2XF ( Rotation and angular velocity to transform )
 
      SUBROUTINE RAV2XF ( ROT, AV, XFORM )
 
C$ Abstract
C
C     This routine determines from a state transformation matrix
C     the associated rotation matrix and angular velocity of the
C     rotation.
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
C     None.
C
C$ Keywords
C
C     FRAMES
C
C$ Declarations
 
      IMPLICIT NONE
 
      DOUBLE PRECISION      ROT    ( 3, 3 )
      DOUBLE PRECISION      AV     ( 3    )
      DOUBLE PRECISION      XFORM  ( 6, 6 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ROT        I   rotation matrix
C     AV         I   angular velocity vector
C     XFORM      O   state transformation associated with ROT and AV
C
C$ Detailed_Input
C
C     ROT         is a rotation that gives the transformation from
C                 some frame FRAME1 to another frame FRAME2.
C
C     AV          is the angular velocity of the transformation.
C                 In other words, if P is the position of a fixed
C                 point in FRAME2, then from the point of view of
C                 FRAME1,  P rotates (in a right handed sense) about
C                 an axis parallel to AV.  Moreover the rate of rotation
C                 in radians per unit time is given by the length of
C                 AV.
C
C                 More formally, the velocity V of P in FRAME1 is
C                 given by
C                                    t
C                     V  = AV x ( ROT * P )
C
C$ Detailed_Output
C
C     XFORM       is a state transformation matrix associated
C                 with ROT and AV.  If S1 is the state of an object
C                 with respect to FRAME1, then the state S2 of the
C                 object with respect to FRAME2 is given by
C
C                     S2  =  XFORM * S1
C
C                 where "*" denotes Matrix-Vector multiplication.
C
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) No checks are performed on ROT to ensure that it is indeed
C        a rotation matrix.
C
C$ Particulars
C
C     This routine is essentially a macro routine for converting
C     a rotation and angular velocity of the rotation to the
C     equivalent state transformation matrix.
C
C     This routine is an inverse of XF2RAV
C
C$ Examples
C
C     Suppose that you wanted to determine state transformation
C     matrix from a platform frame to J2000.
C
C     CALL CKGPAV ( CKID, TIME, TOL, 'J2000', ROT, AV, CLKOUT, FND )
C
C     Recall that ROT and AV are the rotation and angular velocity
C     of the transformation from J2000 to the platform frame.
C
C     IF ( FND ) THEN
C
C        First get the state transformation from J2000 to the platform
C        frame.
C
C        CALL RAV2XF ( ROT,  AV, J2PLT )
C
C        Invert the state transformation matrix (using INVSTM) to 
C        the desired state transformation matrix.
C
C        CALL INVSTM ( J2PLT, XFORM )
C
C     END IF
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-   SPICELIB Version 1.1.0, 28-JUL-1997 (WLT)
C
C       The example in version 1.0.0 was incorrect.  The example
C       in version 1.1.0 fixes the previous problem.
C
C-   SPICELIB Version 1.0.0, 18-SEP-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C    State transformation to rotation and angular velocity
C
C-&
 
      INTEGER               I
      INTEGER               J
 
      DOUBLE PRECISION      OMEGAT ( 3, 3 )
      DOUBLE PRECISION      DROTDT ( 3, 3 )
 
C
C     A state transformation matrix XFORM has the following form
C
C
C         [      |     ]
C         |  R   |  0  |
C         |      |     |
C         | -----+-----|
C         |  dR  |     |
C         |  --  |  R  |
C         [  dt  |     ]
C
C
C     where R is a rotation and dR/dt is the time derivative of that
C     rotation.  From this we can immediately fill in most of the
C     state transformation matrix.
C
      DO I = 1, 3
         DO J = 1,3
            XFORM(I,  J  ) = ROT(I,J)
            XFORM(I+3,J+3) = ROT(I,J)
            XFORM(I,  J+3) = 0.0D0
         END DO
      END DO
 
C
C     Now for the rest.
C
C     Recall that ROT is a transformation that converts positions
C     in some frame FRAME1 to positions in a second frame FRAME2.
C
C     The angular velocity matrix OMEGA (the cross product matrix
C     corresponding to AV) has the following property.
C
C     If P is the position of an object that is stationary with
C     respect to FRAME2 then the velocity V of that object in FRAME1
C     is given by:
C                          t
C         V  =  OMEGA * ROT  *  P
C
C     But V is also given by
C
C                    t
C               d ROT
C         V =   -----  * P
C                 dt
C
C     So that
C                                  t
C                    t        d ROT
C         OMEGA * ROT    =   -------
C                               dt
C
C     Hence
C
C          d ROT                 t
C          -----   =  ROT * OMEGA
C            dt
C
C
C     From this discussion we can see that we need OMEGA transpose.
C     Here it is.
C
      OMEGAT(1,1) =  0.0D0
      OMEGAT(2,1) = -AV(3)
      OMEGAT(3,1) =  AV(2)
 
      OMEGAT(1,2) =  AV(3)
      OMEGAT(2,2) =  0.0D0
      OMEGAT(3,2) = -AV(1)
 
      OMEGAT(1,3) = -AV(2)
      OMEGAT(2,3) =  AV(1)
      OMEGAT(3,3) =  0.0D0
 
      CALL MXM ( ROT, OMEGAT, DROTDT )
 
      DO I = 1, 3
         DO J = 1,3
            XFORM(I+3,J) = DROTDT(I,J)
         END DO
      END DO
 
      RETURN
      END
