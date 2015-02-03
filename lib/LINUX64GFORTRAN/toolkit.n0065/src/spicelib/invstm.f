C$Procedure      INVSTM ( Inverse of state transformation matrix)
 
      SUBROUTINE INVSTM ( MAT, INVMAT )
 
C$ Abstract
C
C     Return the inverse of a state transformation matrix.
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
C     TRANSFORMATION
C
C$ Declarations
 
 
      DOUBLE PRECISION      MAT    ( 6, 6 )
      DOUBLE PRECISION      INVMAT ( 6, 6 )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     MAT        I   A state transformation matrix.
C     INVMAT     O   The inverse of MAT.  
C
C$ Detailed_Input
C
C     MAT        is a state transformation matrix for converting states
C                relative to one frame to states relative to another.
C                The state transformation of a state vector, S, is
C                performed by the matrix-vector product.
C
C                    MAT * S.
C
C                For MAT to be a "true" state transformation matrix
C                it must have the form
C
C                      -            -
C                     |       :      |
C                     |   R   :   0  |
C                     |.......:......|
C                     |       :      |
C                     |  W*R  :   R  |
C                     |       :      |
C                      -            -
C
C                where R is a 3x3 rotation matrix and, 0 is the 3x3 zero
C                matrix and W is a 3x3 skew-symmetric matrix.
C
C                NOTE: no checks are performed on MAT to ensure that it
C                      does indeed have the form described above.
C
C$ Detailed_Output
C
C     INVMAT     is the inverse of MAT under the operation of matrix
C                multiplication.  
C
C                If MAT has the form described above, then INVMAT has
C                the form shown below.
C
C                      -             -
C                     |     t  :      |
C                     |    R   :   0  |
C                     |........:......|
C                     |      t :    t |
C                     | (W*R)  :   R  |
C                     |        :      |
C                      -            -
C
C                (The superscript "t" denotes the matrix transpose
C                operation.)
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) No checks are performed, to insure that the input matrix is
C        indeed a state transformation matrix.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     Given a matrix for transforming states relative frame 1 to
C     states relative frame 2,  the routine produces the inverse
C     matrix.  That is, it returns the matrix for transforming states
C     relative to frame 2 to states relative to frame 1.
C
C     This special routine exists because unlike the inverse of a
C     rotation matrix, the inverse of a state transformation matrix,
C     is NOT simply the transpose of the of the matrix.
C
C$ Examples
C
C     Suppose you had a geometric state, STATE,  of a spacecraft in
C     earth bodyfixed coordinates and wished to express this state
C     relative to earth centered J2000 coordinates.  The following
C     code fragment illustrates how to carry out this computation.
C
C     C
C     C     First get the state transformation from J2000 to earth
C     C     bodyfixed coordinates at the time of interest ET.
C     C
C           EARTH = 399
C           J2000 = 'J2000'
C
C           CALL TISBOD ( J2000, EARTH, ET, MAT )
C
C     C
C     C     Get the inverse of MAT
C     C
C           CALL INVSTM ( MAT,  INVMAT          )
C
C     C
C     C     Transform from bodyfixed state to inertial state.
C     C
C           CALL MXVG ( INVMAT, STATE, 6, 6, ISTATE )
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
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 22-APR-2010 (NJB)
C
C        Header correction: assertions that the output
C        can overwrite the input have been removed.
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 29-OCT-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     inverse of state transformation matrix
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
      INTEGER               NROWS
      PARAMETER           ( NROWS = 6 )
 
      INTEGER               NCOLS
      PARAMETER           ( NCOLS = 6 )
 
      INTEGER               BLOCK
      PARAMETER           ( BLOCK = 3 )
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'INVSTM' )
      END IF
 
C
C     Not much to this.  Just call the more general routine XPOSBL.
C
      CALL XPOSBL ( MAT, NROWS, NCOLS, BLOCK, INVMAT )
 
C
C     That's all folks.
C
      CALL CHKOUT ( 'INVSTM' )
      RETURN
      END
