C$Procedure      SUFFIX (Suffix a character string)
 
      SUBROUTINE SUFFIX ( SUFF, SPACES, STRING )
 
C$ Abstract
C
C      Add a suffix to a character string.
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
 
      CHARACTER*(*)    SUFF
      INTEGER          SPACES
      CHARACTER*(*)    STRING
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      SUFF       I   Suffix.
C      SPACES     I   Number of spaces separating prefix and suffix.
C      STRING    I/O  Prefix on input, string on output.
C
C$ Detailed_Input
C
C      SUFF        is the suffix to be added to the string.
C                  Leading blanks are significant. (A blank
C                  suffix is interpreted as a null suffix.)
C
C      SPACES      is the number of spaces (blanks) in the output
C                  string separating the last non-blank character
C                  of the prefix from the first (blank or non-blank)
C                  character of the suffix. Typically, this will be
C                  zero or one. If not positive, SPACES defaults to
C                  zero.
C
C      STRING      on input is the prefix to which the suffix is
C                  to be added. Leading blanks are significant.
C                  Trailing blanks are ignored.
C
C$ Detailed_Output
C
C      STRING      on output is the suffixed string. If STRING
C                  is not large enough to contain the output string,
C                  the output string is truncated on the right.
C
C                  STRING may NOT overwrite SUFF.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      The suffix is added to the right of the last non-blank character
C      of the prefix. (Any necessary truncation is done automatically.)
C
C$ Examples
C
C      The following examples illustrate the use of SUFFIX.
C
C            SUFF         STRING (input)   SPACES    STRING (output)
C            ----------   --------------   ------    ---------------
C            'abc     '   'def    '             0    'defabc '
C            'abc     '   'def    '             1    'def abc'
C            'abc     '   ' def   '             0    ' defabc'
C            'abc     '   ' def   '             1    ' def ab'
C            ' abc    '   'def    '             0    'def abc'
C            ' abc    '   'def    '             1    'def  ab'
C            ' abc    '   ' def   '            -1    ' def ab'
C            '        '   'def    '             0    'def    '
C            '        '   'def    '             1    'def    '
C            ' abc    '   '       '             0    ' abc   '
C            ' abc    '   '       '             1    '  abc  '
C
C$ Restrictions
C
C      SUFF and STRING must be distinct.
C
C$ Exceptions
C
C      Error free.
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
C     suffix a character_string
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER          LASTNB
 
C
C     Local variables
C
      INTEGER          L
      INTEGER          SLEN
      INTEGER          END
 
 
 
C
C     SLEN is the allocated length of the string. L is the location of
C     the last non-blank character of the prefix.
C
      SLEN = LEN    ( STRING )
      L    = LASTNB ( STRING )
 
C
C     Put the suffix at the end of the string. The spaces will fill
C     themselves in.
C
      END = L + MAX ( SPACES, 0 )
 
      IF ( END .LT. SLEN ) THEN
         STRING(END+1: ) = SUFF
      END IF
 
      RETURN
      END
