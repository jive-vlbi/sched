C$Procedure      XF2RAV ( Transform to rotation and angular velocity)
 
      SUBROUTINE XF2RAV ( XFORM, ROT, AV )
 
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
 
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      ROT    ( 3, 3 )
      DOUBLE PRECISION      AV     ( 3    )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     XFORM      I   is a state transformation matrix
C     ROT        O   is the rotation associated with XFORM
C     AV         O   is the angular velocity associated with XFORM
C
C$ Detailed_Input
C
C     XFORM       is a state transformation matrix from one frame
C                 FRAME1 to some other frame FRAME2.
C
C$ Detailed_Output
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
C                 The components of AV are given relative to FRAME1.
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
C     1) No checks are performed on XFORM to ensure that it is indeed
C        a state transformation matrix.
C
C$ Particulars
C
C     This routine is essentially a macro routine for converting
C     state transformation matrices into the equivalent representation
C     in terms of a rotation and angular velocity.
C
C     This routine is an inverse of the routine RAV2XF.
C
C$ Examples
C
C     Suppose that you wanted to determine the angular velocity
C     of the earth with respect to J2000 at a particular epoch ET.
C     The following code fragment illustrates a procedure for
C     computing the angular velocity.
C
C        CALL TISBOD ( 'J2000', 399, ET, TSIPM )
C
C
C        Now get the angular velocity by calling XF2RAV
C
C        CALL XF2RAV ( TSPMI, TPMI, AV )
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
C-    SPICELIB Version 1.0.0, 19-SEP-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     State transformation to rotation and angular velocity
C
C-&
 
      INTEGER               I
      INTEGER               J
 
      DOUBLE PRECISION      DRDT  ( 3, 3 )
      DOUBLE PRECISION      OMEGA ( 3, 3 )
 
 
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
C     rotation.  From this we can immediately read the rotation and
C     its derivative.
C
      DO I = 1, 3
         DO J = 1,3
            ROT (I,J) = XFORM(I,  J)
            DRDT(I,J) = XFORM(I+3,J)
         END DO
      END DO
 
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
C                             t
C                       d ROT
C         OMEGA    =   -------  *  ROT
C                         dt
C
C
C
      CALL MTXM ( DRDT, ROT, OMEGA )
 
C
C     Recall that OMEGA has the form
C
C         _                     _
C        |                       |
C        |   0    -AV(3)  AV(2)  |
C        |                       |
C        |  AV(3)    0   -AV(1)  |
C        |                       |
C        | -AV(2)   AV(1)   0    |
C        |_                     _|
C
      AV(1) = OMEGA(3,2)
      AV(2) = OMEGA(1,3)
      AV(3) = OMEGA(2,1)
 
      RETURN
      END
