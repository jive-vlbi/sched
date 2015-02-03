C$Procedure      IDENT (Return the 3x3 identity matrix)
 
      SUBROUTINE IDENT ( MATRIX )
 
C$ Abstract
C
C    This routine returns the 3x3 identity matrix.
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
      DOUBLE PRECISION      MATRIX ( 3, 3 )
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     MATRIX     O   is the 3x3 identity matrix
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     MATRIX     is the 3x3 Identity matrix.  That is MATRIX is
C                the following
C                  _                       _
C                 |  1.0D0   0.0D0   0.0D0  |
C                 |  0.0D0   1.0D0   0.0D0  |
C                 |  0.0D0   0.0D0   1.0D0  |
C                  -                       -
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
C$ Particulars
C
C     This is a utility routine for obtaining the 3x3 identity matrix
C     so that you may avoid having to write the loop or assignments
C     needed to get the matrix.
C
C$ Examples
C
C     Suppose that you need to construct the matrix sum
C
C        I + OMEGA
C
C     where OMEGA is some matrix you have already computed.
C
C     The code fragment below shows how you could accomplish this
C     with this routine.
C
C        First get the Identity matrix
C
C        DOUBLE PRECISION      I ( 3, 3 )
C
C        CALL IDENT( I  )
C        CALL VSUMG( I, OMEGA, 9, SUM )
C
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
C-    SPICELIB Version 1.0.0, 5-FEB-1996 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Get the 3x3 identity matrix
C
C-&
 
      MATRIX ( 1, 1 ) = 1.0D0
      MATRIX ( 2, 1 ) = 0.0D0
      MATRIX ( 3, 1 ) = 0.0D0
 
      MATRIX ( 1, 2 ) = 0.0D0
      MATRIX ( 2, 2 ) = 1.0D0
      MATRIX ( 3, 2 ) = 0.0D0
 
      MATRIX ( 1, 3 ) = 0.0D0
      MATRIX ( 2, 3 ) = 0.0D0
      MATRIX ( 3, 3 ) = 1.0D0
 
      RETURN
      END
