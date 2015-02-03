C$Procedure      PREFIX (Prefix a character string)
 
      SUBROUTINE PREFIX ( PREF, SPACES, STRING )
 
C$ Abstract
C
C      Add a prefix to a character string.
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
C      ASSIGNMENT,  CHARACTER,  STRING
C
C$ Declarations
 
      CHARACTER*(*)    PREF
      INTEGER          SPACES
      CHARACTER*(*)    STRING
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      PREF       I   Prefix.
C      SPACES     I   Number of spaces separating prefix and suffix.
C      STRING    I/O  Suffix on input, string on output.
C
C$ Detailed_Input
C
C      PREF        is the prefix to be added to the string. Trailing
C                  blanks are ignored. (A blank prefix is interpreted
C                  as a null prefix.)
C
C      SPACES      is the number of spaces (blanks) in the output
C                  string separating the last non-blank character
C                  of the prefix from the first (blank or non-blank)
C                  character of the suffix. Typically, this will be
C                  zero or one. If not positive, SPACES defaults to
C                  zero.
C
C      STRING      on input is the suffix to which the prefix is to
C                  be added. Leading blanks are significant.
C
C$ Detailed_Output
C
C      STRING      on output is the is the prefixed string. If STRING
C                  is not large enough to contain the output string,
C                  the output string is truncated on the right.
C
C                  STRING may NOT overwrite PREF.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      The suffix is shifted to the right to make room for the prefix
C      and required spaces, which are then added to the front of the
C      string. (The shift operation handles any necessary truncation.)
C
C$ Examples
C
C      The following examples illustrate the use of PREFIX.
C
C            PREF         STRING (input)   SPACES    STRING (output)
C            ----------   --------------   ------    ---------------
C            'abc     '   'def    '             0    'abcdef '
C            'abc     '   'def    '             1    'abc def'
C            'abc     '   ' def   '             0    'abc def'
C            'abc     '   ' def   '             1    'abc  de'
C            ' abc    '   'def    '             0    ' abcdef'
C            ' abc    '   'def    '             1    ' abc de'
C            ' abc    '   ' def   '            -1    ' abc de'
C            '        '   'def    '             0    'def    '
C            '        '   'def    '             1    ' def   '
C            ' abc    '   '       '             0    ' abc   '
C
C$ Restrictions
C
C      PREF and STRING must be distinct.
C
C$ Exceptions
C
C      Error free.
C
C      1) If SPACES is negative it is treated as zero.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     prefix a character_string
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 28-FEB-1989 (WLT)
C
C         Reference to SHIFT replaced by SHIFTL.
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER          LASTNB
 
C
C     Local variables
C
      INTEGER          PLEN
      INTEGER          SLEN
      INTEGER          SHIFT
 
 
 
C
C     L is the location of the last non-blank character in the prefix.
C     PLEN is the length of the prefix. Remember that a blank (null)
C     prefix has zero length.
C
      PLEN = LASTNB ( PREF )
 
C
C     SLEN is the allocated length of the string.
C
      SLEN = LEN ( STRING )
 
C
C     We can't just do a concatenation, because the input and output
C     strings are of indeterminate length. (This would be a violation
C     of the ANSI Fortran 77 standard.) Instead, we will shift the
C     suffix to the right in order to make room for the prefix and
C     the required number of spaces. If part of the string gets
C     truncated, well, that's life.
C
      SHIFT = PLEN + MAX ( SPACES, 0 )
 
      CALL SHIFTR ( STRING, SHIFT, ' ', STRING )
 
C
C     Put the non-blank part of the prefix in the vacated part of
C     the string. The spaces will fill themselves in.
C
      IF ( PLEN .GT. 0 ) THEN
 
         IF ( SHIFT .LT. SLEN ) THEN
            STRING(1:SHIFT) = PREF
 
         ELSE
            STRING = PREF
         END IF
 
      END IF
 
      RETURN
      END
