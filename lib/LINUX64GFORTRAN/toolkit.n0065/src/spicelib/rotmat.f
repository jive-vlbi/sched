C$Procedure      ROTMAT ( Rotate a matrix )
 
      SUBROUTINE ROTMAT ( M1, ANGLE, IAXIS, MOUT )
 
C$ Abstract
C
C     ROTMAT applies a rotation of ANGLE radians about axis IAXIS to a
C     matrix.  This rotation is thought of as rotating the coordinate
C     system.
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
C     MATRIX
C     ROTATION
C
C$ Declarations
 
      DOUBLE PRECISION   M1    ( 3,3 )
      DOUBLE PRECISION   ANGLE
      INTEGER            IAXIS
      DOUBLE PRECISION   MOUT  ( 3,3 )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     M1         I   Matrix to be rotated.
C     ANGLE      I   Angle of rotation (radians).
C     IAXIS      I   Axis of rotation (X=1, Y=2, Z=3).
C     MOUT       O   Resulting rotated matrix [ANGLE]      * M1
C                                                   IAXIS
C
C$ Detailed_Input
C
C     M1       This is a matrix to which a rotation is to be applied.
C              In matrix algebra, the components of the matrix are
C              relevant in one particular coordinate system. Applying
C              ROTMAT changes the components of M1 so that they are
C              relevant to a rotated coordinate system.
C
C     ANGLE    The angle in radians through which the original
C              coordinate system is to be rotated.
C
C     IAXIS    An index for the axis of the original coordinate system
C              about which the rotation by ANGLE is to be performed.
C              IAXIS = 1,2 or 3 designates the x-, y- or z-axis,
C              respectively.
C
C$ Detailed_Output
C
C     MOUT     The matrix resulting from the application of the
C              specified rotation to the input matrix M1.  If
C              [ANGLE]        denotes the rotation matrix by ANGLE
C                     IAXIS
C              radians about IAXIS, (refer to the routine ROTATE) then
C              MOUT is given by the following matrix equation:
C
C                 MOUT = [ANGLE]      * M1
C                               IAXIS
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the axis index is not in the range 1 to 3 it will be
C        treated the same as that integer 1, 2, or 3 that is congruent
C        to it mod 3.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
C
C$ Examples
C
C     Suppose that to rotate a set of inertial axes to body fixed
C     axes, one must first roll the coordinate axes about the x-axis by
C     angle R to get x', y', z'.  From this one must pitch about the y'
C     axis by angle P to get x'', y'', z''.  And finally yaw the x'',
C     y'', z'' about the z'' axis by angle Y to obtain the
C     transformation to bodyfixed coordinates.  If ID is the identity
C     matrix, then the following code fragment generates the
C     transformation from inertial to body fixed.
C
C        CALL ROTMAT ( ID, R,     1,     M1   )
C        CALL ROTMAT ( M1, P,     2,     M2   )
C        CALL ROTMAT ( M2, Y,     3,     TIBF )
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
C     W.M. Owen       (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
C
C-&
 
C$ Index_Entries
C
C     rotate a matrix
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 1.1.0, 3-JAN-1989 (WLT)
C
C     Upgrade the routine to work with negative axis indexes.  Also take
C     care of the funky way the indices (other than the input) were
C     obtained via the MOD function.  It works but isn't as clear
C     (or fast) as just reading the axes from data.
C
C-&
 
 
 
      DOUBLE PRECISION      S
      DOUBLE PRECISION      C
 
      INTEGER               TEMP
      INTEGER               I1
      INTEGER               I2
      INTEGER               I3
      INTEGER               I
 
      DOUBLE PRECISION      PRODM   (3,3)
 
      INTEGER               INDEXS  (5)
      SAVE                  INDEXS
 
      DATA                  INDEXS  / 3, 1, 2, 3, 1 /
C
C  Get the sine and cosine of ANGLE
C
      S = DSIN(ANGLE)
      C = DCOS(ANGLE)
C
C  Get indices for axes. The first index is for the axis of rotation.
C  The next two axes follow in right hand order (XYZ).  First get the
C  non-negative value of IAXIS mod 3 .
C
      TEMP = MOD ( MOD(IAXIS,3) + 3, 3 )
 
      I1   = INDEXS( TEMP + 1 )
      I2   = INDEXS( TEMP + 2 )
      I3   = INDEXS( TEMP + 3 )
C
C  Calculate the output matrix column by column
C
      DO I=1,3
         PRODM(I1,I) =  M1(I1,I)
         PRODM(I2,I) =            C*M1(I2,I) + S*M1(I3,I)
         PRODM(I3,I) =           -S*M1(I2,I) + C*M1(I3,I)
      END DO
C
C  Move the buffered matrix into MOUT.
C
      CALL MOVED (PRODM, 9, MOUT)
C
      RETURN
      END
