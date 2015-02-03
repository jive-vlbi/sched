C$Procedure  MOVEC  ( Move a character array to another )
 
      SUBROUTINE MOVEC ( ARRFRM, NDIM, ARRTO )
 
C$ Abstract
C
C      Copy the elements of one character array into another
C      array.
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
C      ARRAY
C
C$ Declarations
 
      CHARACTER*(*)   ARRFRM ( * )
      INTEGER         NDIM
      CHARACTER*(*)   ARRTO  ( * )
 
C$ Brief_I/O
C
C      VARIABLE  I/O              DESCRIPTION
C      --------  ---  --------------------------------------------------
C      ARRFRM     I     Character array to be moved.
C      NDIM       I     Number of elements to copy, i.e. the dimension
C                       of ARRFRM and ARRTO.
C      ARRTO      O     Destination array.
C
C$ Detailed_Input
C
C      ARRFRM     Array from which to copy items.
C
C      NDIM       Number of items to copy.
C
C$ Detailed_Output
C
C      ARRTO      Array to which items should be copied.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This routine is simply shorthand for the following 3 lines of
C      code.
C
C              DO I = 1, NDIM
C                 ARRTO(I) = ARRFRM(I)
C              END DO
C
C$ Examples
C
C      Often one needs to make a temporary copy of an array so that
C      it can be manipulated without altering the original array.
C      As pointed out in particulars, you could just do this within
C      the code that needs the copy.  However, if you have several
C      arrays to copy, you can cut the number of lines of code that
C      are needed by a third.
C
C      For example:
C
C           DO I = 1, 19
C              TEMPA(I) = A(I)
C           END DO
C
C           DO I = 1, 38
C              TEMPB(I) = B(I)
C           END DO
C
C     Can be rewritten as
C
C           CALL MOVEC ( A, 19, TEMPA )
C           CALL MOVEC ( B, 38, TEMPB )
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      W.M. Owen       (JPL)
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
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
C     move a character array to another character array
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 1.0.1, 4-FEB-1989 (WLT)
C
C      Header fully filled out.
C
C-&
 
C
C     Local variables
C
      INTEGER I
 
 
      DO I = 1, NDIM
        ARRTO(I) = ARRFRM(I)
      END DO
 
 
      RETURN
      END
