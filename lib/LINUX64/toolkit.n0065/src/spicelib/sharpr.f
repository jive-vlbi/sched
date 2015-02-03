C$Procedure      SHARPR ( Sharpen a rotation )
 
      SUBROUTINE SHARPR ( ROT )
 
C$ Abstract
C
C     Given a matrix that is "nearly" a rotation, adjust the columns
C     (from left to right in the usual printed presentation of a matrix)
C     so that the columns are numerically unit length and orthogonal.
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
C
C$ Declarations
 
      IMPLICIT NONE
      DOUBLE PRECISION      ROT ( 3, 3 )
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ROT       I/O  The rotation matrix to be sharpened.
C
C$ Detailed_Input
C
C     ROT       a 3x3 matrix that is nearly a rotation matrix.
C
C$ Detailed_Output
C
C     ROT       the input after sharpening the columns.
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
C     1) This routine is not meant to be used on singular or near-
C        singular matrices (in other words, matrices with determinant
C        close to zero).  
C    
C        If the input matrix is singular, the output matrix may not
C        be a rotation matrix.  In any case, the results should be
C        considered unreliable in this case.
C
C        No error handling is done for invalid input matrices.
C
C$ Particulars
C
C     This routine "sharpens" the orthogonality of a potential
C     rotation matrix.  It is intended for use in those situations
C     in which you have a rotation matrix that may be derived
C     from single precision inputs or that may have experienced
C     round off errors in its construction.
C
C$ Examples
C
C     Suppose that you have a rotation matrix that needs to be
C     converted to a quaternion.  The SPICE matrix to quaternion
C     conversion routine M2Q performs error checks on the input
C     matrix and signals an error if it does not meet the checks
C     for a quaternion.  By calling this routine you can ensure that
C     your rotation matrix (provided it's non-singular) will pass
C     the restrictions imposed by M2Q.
C
C         CALL SHARPR ( ROT )
C         CALL M2Q    ( ROT, Q )
C
C$ Restrictions
C
C     See the Exceptions section above.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.1.0, 13-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VHAT call.  Some header updates were made.
C
C-    SPICELIB Version 1.0.0, 16-SEP-1999 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Sharpen the orhogonality of the columns of a rotation
C
C-&

C$ Revisions
C
C-    SPICELIB Version 1.1.0, 13-OCT-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VHAT call. Some header updates were made.
C
C-& 
  
C
C     Unitize the first column of the rotation.
C
      CALL VHATIP ( ROT(1,1) )

C
C     Unitize the third column of the rotation and make it
C     orthogonal to the first two columns.
C
      CALL UCRSS ( ROT(1,1),  ROT(1,2),  ROT(1,3) )

C
C     Unitize the second column of the rotation and make it
C     orthogonal to the first and third columns.
C
      CALL UCRSS ( ROT(1,3),  ROT(1,1),  ROT(1,2) )
  
      RETURN
      END
