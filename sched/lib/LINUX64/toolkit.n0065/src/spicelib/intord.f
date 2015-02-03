 
C$Procedure      INTORD ( Convert an integer to ordinal text )
 
      SUBROUTINE INTORD ( N, STRING )
 
C$ Abstract
C
C     Convert an integer to an equivalent written ordinal phrase.
C     For example, convert 121 to 'ONE HUNDRED TWENTY-FIRST'.
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
C     CONVERSION
C     PARSING
C     STRING
C     UNITS
C     UTILITY
C
C$ Declarations
 
      INTEGER               MAXORD
      PARAMETER           ( MAXORD = 148 )
 
      INTEGER               N
      CHARACTER*(*)         STRING
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     N          I   An integer (less than 10**12 in absolute value).
C     STRING     O   An English string representing the ordinal of N.
C
C$ Detailed_Input
C
C     N         is an integer (less than 10**12 in absolute value).
C               Moreover, if N is less than zero, -N must be a
C               a legitimate number on the host machine.
C
C               In the context of this routine N represents the
C               ranking of some item within a group.
C
C
C$ Detailed_Output
C
C     STRING    is the English ordinal equivalent of N.  STRING will
C               contain only upper case letters.
C
C$ Parameters
C
C     MAXORD    is one more than the length of the longest ordinal
C               string that can be produced by a call to this routine:
C               One string of maximum length is:
C
C                  'NEGATIVE '                                  //
C                  'SEVEN HUNDRED SEVENTY-SEVEN BILLION '       //
C                  'SEVEN HUNDRED SEVENTY-SEVEN MILLION '       //
C                  'SEVEN HUNDRED SEVENTY-SEVEN THOUSAND '      //
C                  'SEVEN HUNDRED SEVENTY-SEVENTH'
C
C               It has 147 characters.
C
C               The parameter MAXORD is used to declare a local string
C               of sufficient length to allow the construction of
C               any ordinal string.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the resulting ordinal is longer than the output string,
C        it will be truncated on the right, leaving only the most
C        significant portion of the ordinal.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is used primarily for generating error messages. For example,
C     if the third letter or token in a string is in error, it might
C     be desirable to supply a message like the following:
C
C        'The third token of 31-JAN-198$ is not a valid year.'
C
C$ Examples
C
C      N           STRING
C      ------      -------------------------------------------
C      -6          NEGATIVE SIXTH
C       1          FIRST
C       2          SECOND
C       3          THIRD
C       4          FOURTH
C       20         TWENTIETH
C       21         TWENTY-FIRST
C       99         NINETY-NINTH
C       82131      EIGHTY-TWO THOUSAND ONE HUNDRED THIRTY-FIRST
C
C$ Restrictions
C
C      1) Whatever restrictions apply to INTTXT apply to this routine
C         as well.
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
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 15-AUG-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     convert an integer to ordinal text
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER               LASTNB
 
C
C     Local variables
C
      INTEGER               I
      INTEGER               LAST
      CHARACTER*(MAXORD)    MYSTR
 
C
C     First get the English equivalent of the cardinal N.
C
      MYSTR = ' '
 
      CALL INTTXT ( N, MYSTR )
 
      LAST  = LASTNB(MYSTR)
      I     = LAST
 
C
C     Find the beginning of the last number of MYSTR.
C
      DO WHILE (       ( MYSTR(I:I) .NE. '-' )
     .           .AND. ( MYSTR(I:I) .NE. ' ' )
     .           .AND. ( I          .GT.  1  )  )
 
         I = I - 1
 
      END DO
 
      IF (      ( MYSTR(I:I) .EQ. ' ' )
     .     .OR. ( MYSTR(I:I) .EQ. '-' )  ) THEN
 
         I = I + 1
 
      END IF
 
C
C     Now convert the last cardinal to an ordinal.
C
      IF ( MYSTR(I:LAST) .EQ. 'ONE' ) THEN
 
         MYSTR(I:) = 'FIRST'
 
      ELSE IF ( MYSTR(I:LAST) .EQ. 'TWO' ) THEN
 
         MYSTR(I:) = 'SECOND'
 
      ELSE IF ( MYSTR(I:LAST) .EQ. 'THREE' ) THEN
 
         MYSTR(I:) = 'THIRD'
 
      ELSE IF ( MYSTR(I:LAST) .EQ. 'FIVE' ) THEN
 
         MYSTR(I:) = 'FIFTH'
 
      ELSE IF ( MYSTR(I:LAST) .EQ. 'EIGHT' ) THEN
 
         MYSTR(I:) = 'EIGHTH'
 
      ELSE IF ( MYSTR(I:LAST) .EQ. 'NINE' ) THEN
 
         MYSTR(I:) = 'NINTH'
 
      ELSE IF ( MYSTR (I:LAST) .EQ. 'TWELVE' ) THEN
 
         MYSTR(I:) = 'TWELFTH'
 
      ELSE IF ( MYSTR(LAST:LAST) .EQ. 'Y' ) THEN
 
         MYSTR(LAST:) = 'IETH'
 
      ELSE
 
         CALL SUFFIX ( 'TH', 0, MYSTR )
 
      END IF
 
C
C     Now simply put MYSTR into STRING and return.
C
      STRING = MYSTR
 
      RETURN
      END
 
