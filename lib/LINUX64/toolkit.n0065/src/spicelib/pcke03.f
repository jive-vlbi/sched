C$Procedure PCKE03 ( PCK, evaluate data record from type 3 segment )

      SUBROUTINE PCKE03 ( ET, RECORD, ROTMAT )

C$ Abstract
C
C     Evaluate a single PCK data record from a segment of type 03
C     (Variable width Chebyshev Polynomials for RA, DEC, and W) to
C     obtain a state transformation matrix.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTsWARE IS TECHNOLOGY AND SOFTWARE
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
C     PCK
C
C$ Keywords
C
C     PCK
C
C$ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( *    )
      DOUBLE PRECISION      ROTMAT   ( 6, 6 )

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ET         I   Target epoch state transformation.
C     RECORD     I   Data record valid for epoch ET.
C     ROTMAT     O   State transformation matrix at epoch ET.
C
C$ Detailed_Input
C
C     ET          is a target epoch, at which a state transformation
C                 matrix is to be calculated.
C
C     RECORD      is a data record which, when evaluated at epoch ET,
C                 will give RA, DEC, and W and angular velocity
C                 for a body.  The RA, DEC and W are relative to
C                 some inertial frame.  The angular velocity is
C                 expressed relative to the body fixed coordinate frame.
C
C$ Detailed_Output
C
C     ROTMAT      is the state transformation matrix at epoch ET.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The exact format and structure of type 03 PCK segments are
C     described in the PCK Required Reading file.
C
C     A type 03 segment contains six sets of Chebyshev coefficients,
C     one set each for RA, DEC, and W and one set each for the
C     components of the angular velocity of the body.  The coefficients
C     for RA, DEC, and W are relative to some inertial reference
C     frame.  The coefficients for the components of angular velocity
C     are relative to the body fixed frame and must be transformed
C     via the position transformation corresponding to RA, DEC and W.
C
C     PCKE03 calls the routine CHBVAL to evalute each polynomial,
C     to obtain a complete set of values. These values are then
C     used to determine a state transformation matrix that will
C     rotate an inertially referenced state into the bodyfixed
C     coordinate system.
C
C$ Examples
C
C     The PCKEnn routines are almost always used in conjunction with
C     the corresponding PCKRnn routines, which read the records from
C     binary PCK files.
C
C     The data returned by the PCKRnn routine is in its rawest form,
C     taken directly from the segment.  As such, it will be meaningless
C     to a user unless he/she understands the structure of the data type
C     completely.  Given that understanding, however, the PCKRnn
C     routines might be used to examine raw segment data before
C     evaluating it with the PCKEnn routines.
C
C
C     C
C     C     Get a segment applicable to a specified body and epoch.
C     C
C           CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
C
C     C
C     C     Look at parts of the descriptor.
C     C
C           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
C           TYPE   = ICD( 3 )
C
C           IF ( TYPE .EQ. 03 ) THEN
C
C              CALL PCKR03 ( HANDLE, DESCR, ET, RECORD )
C                  .
C                  .  Look at the RECORD data.
C                  .
C              CALL PCKE03 ( ET, RECORD, ROTMAT )
C                  .
C                  .  Apply the rotation and check out the state.
C                  .
C           END IF
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
C     K.R. Gehringer (JPL)
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 3.0.1, 03-JAN-2014 (EDW)
C
C        Minor edits to Procedure; clean trailing whitespace.
C        Removed unneeded Revisions section.
C
C-    SPICELIB Version 3.0.0, 6-OCT-1995 (WLT)
C
C        Brian Carcich at Cornell discovered that the Euler
C        angles were being re-arranged unnecessarily.  As a
C        result the state transformation matrix computed was
C        not the one we expected.  (The re-arrangement was
C        a left-over from  implementation 1.0.0.  This problem
C        has now been corrected.
C
C-    SPICELIB Version 2.0.0, 28-JUL-1995 (WLT)
C
C        Version 1.0.0 was written under the assumption that
C        RA, DEC, W and dRA/dt, dDEC/dt and dW/dt were supplied
C        in the input RECORD.  This version repairs the
C        previous misinterpretation.
C
C-    SPICELIB Version 1.0.0, 14-MAR-1995 (KRG)
C
C-&

C$ Index_Entries
C
C     evaluate type_03 pck segment
C
C-&

C
C     SPICELIB Functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      RPD

      LOGICAL               RETURN
C
C     Local variables
C
      DOUBLE PRECISION      EULANG ( 6    )
      DOUBLE PRECISION      MAV    ( 3    )
      DOUBLE PRECISION      ROT    ( 3, 3 )
      DOUBLE PRECISION      DROTDT ( 3, 3 )

      INTEGER               NCOEFF
      INTEGER               DEGREE
      INTEGER               COFLOC
      INTEGER               I
      INTEGER               J
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCKE03' )
      END IF

C
C     The first number in the record is the number of Chebyshev
C     Polynomial coefficients used to represent each component of the
C     state vector.  Following it are two numbers that will be used
C     later, then the six sets of coefficients.
C
      NCOEFF = INT( RECORD( 1 ) )
C
C     The degree of each polynomial is one less than the number of
C     coefficients.
C
      DEGREE = NCOEFF - 1
C
C     Call CHBVAL once for each quantity to obtain RA, DEC, and W values
C     as well as values for the angular velocity.
C
C     Note that we stick the angular velocity in the components 4 thru 6
C     of the array EULANG even though they are not derivatives of
C     components 1 thru 3.  It's just simpler to do it this way.
C
C     Editorial Comment:
C
C        Unlike every other SPICE routine, the units for the type 03
C        PCK segment are degrees.  This inconsistency exists solely
C        to support the NEAR project and the intransigence of one of the
C        participants of that project.
C
C        It's a bad design and we know it.
C
C        ---W.L. Taber
C
C
      DO I = 1, 6
C
C        The coefficients for each variable are located contiguously,
C        following the first three words in the record.
C
         COFLOC = NCOEFF * ( I - 1 ) + 4
C
C        CHBVAL needs as input the coefficients, the degree of the
C        polynomial, the epoch, and also two variable transformation
C        parameters, which are located, in our case, in the second and
C        third slots of the record.
C
         CALL CHBVAL ( RECORD( COFLOC ), DEGREE, RECORD( 2 ), ET,
     .                 EULANG(I)                                  )
C
C        Convert to radians.
C
         EULANG(I) = RPD() * EULANG(I)

      END DO
C
C     EULANG(1) is RA make it PHI
C     EULANG(2) is DEC make it DELTA
C     EULANG(3) is W
C
      EULANG(1) = HALFPI() + EULANG(1)
      EULANG(2) = HALFPI() - EULANG(2)

C
C     Before we obtain the state transformation matrix, we need to
C     compute the rotation components of the transformation..
C     The rotation we want to perform is:
C
C        [W]  [DELTA]  [PHI]
C           3        1      3
C
C     The array of Euler angles is now:
C
C        EULANG(1) = PHI
C        EULANG(2) = DELTA
C        EULANG(3) = W
C        EULANG(4) = AV_1 (bodyfixed)
C        EULANG(5) = AV_2 (bodyfixed)
C        EULANG(6) = AV_3 (bodyfixed)
C
C
C     Compute the rotation associated with the Euler angles.
C
      CALL EUL2M ( EULANG(3), EULANG(2), EULANG(1), 3, 1, 3, ROT  )

C
C     This rotation transforms positions relative to the inertial
C     frame to positions relative to the bodyfixed frame.
C
C     We next need to get dROT/dt.
C
C     For this discussion let P be the bodyfixed coordinates of
C     a point that is fixed with respect to the bodyfixed frame.
C
C     The velocity of P with respect to the inertial frame is
C     given by
C                 t             t
C        V   = ROT ( AV ) x  ROT ( P )
C
C                  t
C              dROT
C            = ----  ( P )
C               dt
C
C     But
C            t            t            t
C         ROT ( AV ) x ROT ( P ) = ROT  ( AV x P )
C
C     Let OMEGA be the cross product matrix corresponding to AV.
C     Then
C           t                   t
C        ROT  ( AV x P )  =  ROT * OMEGA * P
C
C     where * denotes matrix multiplication.
C
C     From these observations it follows that
C
C                                  t
C           t                  dROT
C        ROT  * OMEGA * P   =  ---- * P
C                                dt
C
C     Consequently, it follows that
C
C        dROT         t
C        ----  = OMEGA  * ROT
C         dt
C
C              = -OMEGA * ROT
C
C     We compute dROT/dt now.  Note that we can get the columns
C     of  -OMEGA*ROT by computing the cross products -AV x COL
C     for each column COL of ROT.
C
      MAV(1) = -EULANG(4)
      MAV(2) = -EULANG(5)
      MAV(3) = -EULANG(6)

      CALL VCRSS ( MAV, ROT(1,1), DROTDT(1,1) )
      CALL VCRSS ( MAV, ROT(1,2), DROTDT(1,2) )
      CALL VCRSS ( MAV, ROT(1,3), DROTDT(1,3) )

C
C     Now we simply fill in the blanks.
C
      DO I = 1, 3
         DO J = 1, 3
            ROTMAT(I,  J  ) = ROT   (I,J)
            ROTMAT(I+3,J  ) = DROTDT(I,J)
            ROTMAT(I,  J+3) = 0.0D0
            ROTMAT(I+3,J+3) = ROT   (I,J)
         END DO
      END DO

      CALL CHKOUT ( 'PCKE03' )
      RETURN
      END
