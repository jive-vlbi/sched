C$Procedure RMDUPD ( Remove duplicates from a double precision array )
 
      SUBROUTINE RMDUPD ( NELT, ARRAY )
 
C$ Abstract
C
C      Remove duplicate elements from a double precision array.
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
 
      INTEGER          NELT
      DOUBLE PRECISION ARRAY   (*)
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      NELT      I/O  Number of elements in the array.
C      ARRAY     I/O  Input/output array.
C
C$ Detailed_Input
C
C      NELT        on input is the number of elements in the input
C                  array.
C
C      ARRAY       on input contains zero or more elements, from which
C                  all duplicate elements are to be removed.
C
C$ Detailed_Output
C
C      NELT        on output is the number of elements in the output
C                  array.
C
C      ARRAY       on output contains the distinct elements of the
C                  input array, sorted in increasing order. (Character
C                  arrays are sorted according to the ASCII collating
C                  sequence).
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      None.
C
C$ Examples
C
C      Let the arrays C and I contain the following elements.
C
C            NC   = 7                NI   =   5
C            C(1) = 'Miranda'        I(1) =  13
C            C(2) = 'Ariel'          I(2) = -13
C            C(3) = 'Umbriel'        I(3) =   0
C            C(4) = 'Titania'        I(4) =   1
C            C(5) = 'Miranda'        I(5) =   0
C            C(6) = 'Oberon'
C            C(7) = 'Umbriel'
C
C      Then following the calls
C
C            CALL RMDUPC ( NC, C )
C            CALL RMDUPI ( NI, I )
C
C      C and I contain the following.
C
C            NC   = 5                NI   =   4
C            C(1) = 'Ariel'          I(1) = -13
C            C(2) = 'Miranda'        I(2) =   0
C            C(3) = 'Oberon'         I(3) =   1
C            C(4) = 'Titania'        I(4) =  13
C            C(5) = 'Umbriel'
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C      None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     remove duplicates from a d.p. array
C
C-&
 
 
 
 
C
C     Local variables
C
      INTEGER          I
      INTEGER          J
 
 
 
C
C     Proceed only if the array actualy contains more than one element.
C
      IF ( NELT .GT. 1 ) THEN
 
C
C        Sort the array in place.
C
         CALL SHELLD ( NELT, ARRAY )
 
C
C        Drop duplicate entries. Compare adjacent entries, and move
C        duplicates forward. (Duplicates are now adjacent, because of
C        sorting.)
C
         J = 1
 
         DO I = 2, NELT
 
            IF ( ARRAY(I) .NE. ARRAY(I-1) ) THEN
               J        = J + 1
               ARRAY(J) = ARRAY(I)
            END IF
 
        END DO
 
        NELT = J
 
      END IF
 
      RETURN
      END
