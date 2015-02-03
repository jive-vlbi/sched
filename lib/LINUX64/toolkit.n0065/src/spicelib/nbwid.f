C$Procedure            NBWID ( Non-blank width of a character array )
 
      INTEGER FUNCTION NBWID ( ARRAY, NELT )
 
C$ Abstract
C
C     Determine the non-blank width of a character array---that is,
C     the largest value of LASTNB for any element in the array.
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
C     ARRAY, CHARACTER
C
C$ Declarations
 
      CHARACTER*(*)         ARRAY   ( * )
      INTEGER               NELT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ARRAY      I   Input array.
C     NELT       I   Number of elements in the array.
C     NBWID      O   Maximum value of LASTNB for the array.
C
C$ Detailed_Input
C
C     ARRAY       is the input array.
C
C     NELT        is the number of elements in the input array.
C
C$ Detailed_Output
C
C     NBWID       is the index of the rightmost non-blank character
C                 in the entire array. This is equivalent to the
C                 maximum value of LASTNB for the array, but somewhat
C                 more efficient to compute. If NELT is not greater
C                 than zero, NBWID is zero.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C     Find the last non-blank character in the first element of the
C     array. Search the rest of the elements, starting at the end of
C     each string and moving back just far enough to determine if the
C     current string is wider than any of the previous ones. (This
C     makes NBWID somewhat more efficient than LASTNB.)
C
C     If any of the strings is found to contain no trailing blanks,
C     NBWID is just the length of the individual elements of the array,
C     and the search is terminated immediately.
C
C$ Examples
C
C     Let ARRAY contain the following strings.
C
C           ARRAY(1) = 'A string of medium length                      '
C           ARRAY(2) = 'A very long string, much longer than the rest  '
C           ARRAY(3) = 'Shorter                                        '
C           ARRAY(4) = 'Short                                          '
C
C     Then the value returned by
C
C           WIDEST = NBWID ( ARRAY, 4 )
C
C     is 45.
C
C     If the word 'rest' in the second element is changed to 'others',
C     the value returned is 47, and the search is terminated after the
C     second element.
C
C$ Restrictions
C
C     None.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C     None.
C
C$ Author_and_Institution
C
C     I.M. Underwood  (JPL)
C
C$ Literature_References
C
C     None.
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
C     non-blank width of a character array
C
C-&
 
 
 
 
C
C     Local variables
C
      INTEGER               STRLEN
      INTEGER               I
      INTEGER               J
 
 
C
C     Nonsense case: no elements.
C
      IF ( NELT .LT. 1 ) THEN
 
         NBWID = 0
 
C
C     Get the length of the individual elements of the string.
C     So far, we have no maximum width, because we haven't examined
C     any elements.
C
      ELSE
 
         STRLEN = LEN ( ARRAY(1) )
         NBWID  = 0
         I      = 0
 
C
C        Continue until the end of the array is reached, or until
C        a string with no trailing blanks is found.
C
         DO WHILE ( I .LT. NELT  .AND.  NBWID .LT. STRLEN )
 
C
C           Search no further than the current value of NBWID.
C
            I = I + 1
            J = STRLEN
 
            DO WHILE ( J .GT. NBWID  .AND.  ARRAY(I)(J:J) .EQ. ' ' )
               J = J - 1
            END DO
 
C
C           NBWID only increases if this string was wider than all
C           previous strings.
C
            NBWID = MAX ( NBWID, J )
 
         END DO
 
      END IF
 
      RETURN
      END
