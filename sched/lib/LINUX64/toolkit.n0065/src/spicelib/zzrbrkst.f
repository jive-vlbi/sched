C$Procedure ZZRBRKST ( Private --- Reverse Bracketed String Extractor )
 
      SUBROUTINE ZZRBRKST ( STRING,
     .                      LFTEND,
     .                      RGTEND,
     .                      SUBSTR,
     .                      LENGTH,
     .                      BKPRES  )
C$ Abstract
C
C    Extract from a string the last instance of a substring bracketed 
C    by specified left and right strings .
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
C     STRINGS
C     UTILITY
C     SCANNING
C
C$ Declarations
 
      IMPLICIT NONE
 
      CHARACTER*(*)         STRING
      CHARACTER*(*)         LFTEND
      CHARACTER*(*)         RGTEND
      CHARACTER*(*)         SUBSTR
      INTEGER               LENGTH
      LOGICAL               BKPRES
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STRING     I   A string from which to extract SUBSTR.
C     LFTEND     I   A string that brackets SUBSTR on the left.
C     RGTEND     I   A string that brackets SUBSTR on the right.
C     SUBSTR     O   The extracted substring.
C     LENGTH     O   The length of the extracted substring.
C     BKPRES     O   Logical indicating if either bracket is present.
C
C$ Detailed_Input
C
C     STRING     is a string to be searched for a substring bracketed
C                by the strings LFTEND and RGTEND (see below).  
C
C     LFTEND,    are respectively the left and right bracketing strings.
C     RGTEND     Trailing and leading white space is significant. LFTEND
C                may equal RGTEND.  See the Exceptions section for a 
C                discussion of the case in which either of these strings
C                is absent.
C
C$ Detailed_Output
C
C     SUBSTR     is the substring of interest.  It consists of the 
C                substring between the last instances of LFTEND
C                and RGTEND in STRING. Note: The argument passed into
C                the routine should be large enough to hold the entire
C                substring, or else truncation will occur. SUBSTR is
C                padded with trailing blanks.
C
C     LENGTH     is the number of characters placed into SUBSTR. This
C                value permits any significant trailing whitespace to be
C                dealt with appropriately. In the event that no
C                substring is assigned to SUBSTR, LENGTH will be 0.
C
C     BKPRES     is a logical that indicates whether or not at least
C                one of LFTEND or RGTEND is present in STRING.
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
C     1) If LFTEND or RGTEND are not present in STRING, then the routine
C        does not modify the contents of SUBSTR, LENGTH is returned as
C        0, and BKPRES is TRUE only if LFTEND or RGTEND is present.
C
C     2) If LFTEND and RGTEND are adjacent, then SUBSTR is not modified,
C        LENGTH is returned as 0, and BKPRES is TRUE.
C
C$ Particulars
C
C     The purpose of this routine is to extract the last instance of
C     a substring bracketed by two specified strings.  The searching
C     is case sensitive, and all white space is significant.  The
C     characters between LFTEND and RGTEND are placed into SUBSTR,
C     and LENGTH is set to the number of characters copied into SUBSTR.
C     The assignment is not substring assignment, so the resultant
C     SUBSTR will be blank padded.  The logical BKPRES is a flag that
C     indicates whether or not either of the two brackets was found.
C     This is diagnostic information of some limited use in the event
C     that SUBSTR was not assigned a value.
C
C$ Examples
C
C     The following table demonstrates the behavior of this routine:
C     ( If a row in the table has no entry for SUBSTR, then the
C       contents of SUBSTR are not modified by calling the routine
C       with these inputs. )
C
C     STRING               LFTEND   RGTEND   SUBSTR             LENGTH
C     ===================  =======  =======  =================  ======
C     'abc def ghi jkl'    'a'      'l'      'bc def ghi jk'      13
C     'abc def ghi jkl'    'abc'    'ghi'    ' def '              5
C     'abc def ghi jkl'    'abc'    '123'                         0
C     'abc def ghi jkl'    '123'    'def'                         0
C     'abc def ghi jkl'    'jkl'    'zzz'                         0
C     'abc def abc jkl'    'abc'    'abc'    ' def '              5
C     'ab cd ab ef ab '    'ab'     'ab'     ' ef '               4
C     'ab cd ab ef ab '    'ef'     'cd'                          0
C     'abc def-fed abc'    'def'    '-fed'                        0
C     'aaaaaaaaaaaaaaa'    'aa'     'aaaa'                        0
C     'aaaabbbaabababa'    'ba'     'a'      'b'                  1
C     'aaaabbbaababada'    'ba'     'a'      'd'                  1
C     'abcd efgh ijkl '    ' '      'l'      'ijk'                3
C     'abcd efgh ijkl '    '    '   'l'                           0
C     'ab  ef   ijklm '    '  '     'm'      'ijkl'               4
C     'ab   ef  ijklm '    '   '    'm'      'ef  ijkl'           8
C
C$ Restrictions
C
C     1) The size of SUBSTR must be large enough to contain any
C        possible substring bracketed by LFTEND or RGTEND, otherwise
C        truncation will occur at assignment.
C
C$ Author_and_Institution
C
C     F.S. Turner     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 22-MAR-1999 (FST)
C
C
C-&
 
C$ Index_Entries
C
C     reverse bracketed string extraction
C
C-&
 
C
C     SPICELIB Functions
C
      INTEGER               POSR
 
C
C     Local Variables
C
      INTEGER               BSIZE
      INTEGER               LINDEX
      INTEGER               LSIZE
      INTEGER               RINDEX
      INTEGER               RSIZE
 
C
C     Compute the sizes of the bracketing substrings and the text
C     block.
C
      LSIZE = LEN ( LFTEND )
      RSIZE = LEN ( RGTEND )
      BSIZE = LEN ( STRING )
 
C
C     Search from the right for RGTEND.
C
      RINDEX = POSR ( STRING, RGTEND, BSIZE )
 
C
C     Now continue the search from RINDEX to the right, this time
C     looking for LFTEND. If RINDEX comes back as 0, then the right
C     bracketing substring is not present, so search the entire string
C     for LFTEND. Otherwise, search from where the right bracket
C     search left off.
C
      IF ( RINDEX .EQ. 0 ) THEN
 
         LINDEX = POSR ( STRING, LFTEND, BSIZE )
 
      ELSE
 
         LINDEX = POSR ( STRING, LFTEND, RINDEX - LSIZE )
 
      END IF
 
C
C     Interpret the results.  If RINDEX and LINDEX are both non-zero,
C     then return the substring they bracket, otherwise handle the
C     failed case.
C
      IF ( ( RINDEX .NE. 0 ) .AND. ( LINDEX .NE. 0 ) ) THEN
 
C
C        Check to see whether or not the brackets are adjacent, and
C        thus have no characters between them.
C
         IF ( ( LINDEX + LSIZE ) .GT. ( RINDEX - 1 ) ) THEN
 
            BKPRES = .TRUE.
            LENGTH = 0
 
C
C        If they aren't adjacent, then compute the length and prepare
C        SUBSTR.
C
         ELSE
 
            LENGTH = RINDEX - ( LINDEX + LSIZE )
            BKPRES = .TRUE.
            SUBSTR = STRING ( ( LINDEX  + LSIZE ) : ( RINDEX - 1 ) )
 
         END IF
 
      ELSE
 
C
C        Set BKPRES to TRUE only if LINDEX or RINDEX is non-zero,
C        indicating one was found by POSR. Set LENGTH to 0, since we
C        will not be changing SUBSTR.
C
         BKPRES = ( ( LINDEX + RINDEX ) .GT. 0 )
         LENGTH = 0
 
      END IF
 
      RETURN
      END
