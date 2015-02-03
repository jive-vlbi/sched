C$Procedure            NCPOS ( NOT character position )
 
      INTEGER FUNCTION NCPOS ( STR, CHARS, START )
 
C$ Abstract
C
C     Find the first occurrence in a string of a character NOT belonging
C     to a collection of characters, starting at a specified location,
C     searching forwards.
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
C     SCANNING
C
C$ Keywords
C
C     CHARACTER
C     SEARCH
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         STR
      CHARACTER*(*)         CHARS
      INTEGER               START
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     STR        I   Any character string.
C     CHARS      I   A collection of characters.
C     START      I   Position to begin looking for one not in CHARS
C
C     The function returns the index of the first character of STR
C     at or following index START that is not in the collection CHARS.
C
C$ Detailed_Input
C
C     STR        is any character string.
C
C     CHARS      is a character string containing a collection of
C                characters.  Spaces in CHARS are significant.
C
C     START      is the position in STR to begin looking for
C                characters not in CHARS.
C
C$ Detailed_Output
C
C     The function returns the index of the first character of STR (at
C     or following index START) that is not one of the characters in the
C     string CHARS.  If no such character is found, the function returns
C     zero.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error Free.
C
C     1) If START is less than 1, the search begins at the first
C        character of the string.
C
C     2) If START is greater than the length of the string, NCPOS
C        returns zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     NCPOS is case sensitive.
C
C     An entire family of related SPICELIB routines (POS, CPOS, NCPOS,
C     POSR, CPOSR, NCPOSR) is described in the Required Reading.
C
C     Those familiar with the True BASIC language should note that
C     these functions are equivalent to the True BASIC intrinsic
C     functions with the same names.
C
C$ Examples
C
C     Let STRING = 'BOB, JOHN, TED, AND MARTIN    '
C                   123456789012345678901234567890
C
C     Let CHAR   = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
C
C     Normal (Sequential) Searching:
C     ------------------------------
C
C           NCPOS( STRING, CHAR,  1 ) = 4
C           NCPOS( STRING, CHAR,  5 ) = 5
C           NCPOS( STRING, CHAR,  6 ) = 10
C           NCPOS( STRING, CHAR, 11 ) = 11
C           NCPOS( STRING, CHAR, 12 ) = 15
C           NCPOS( STRING, CHAR, 16 ) = 16
C           NCPOS( STRING, CHAR, 17 ) = 20
C           NCPOS( STRING, CHAR, 21 ) = 27
C           NCPOS( STRING, CHAR, 28 ) = 28
C           NCPOS( STRING, CHAR, 29 ) = 29
C           NCPOS( STRING, CHAR, 30 ) = 30
C           NCPOS( STRING, CHAR, 31 ) =  0
C
C     START out of bounds:
C     --------------------
C
C           NCPOS( STRING, CHAR, -12 ) = 4
C           NCPOS( STRING, CHAR,   0 ) = 4
C           NCPOS( STRING, CHAR,  31 ) = 0
C           NCPOS( STRING, CHAR, 123 ) = 0
C
C     Order within CHARS:
C     -------------------
C
C           NCPOS( STRING, 'JOHN', 7 ) = 10
C           NCPOS( STRING, 'OHJN', 7 ) = 10
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
C     W.L. Taber      (JPL)
C     H.A. Neilan     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.3, 31-JAN-2008 (BVS)
C
C        Removed non-standard end-of-declarations marker
C        'C%&END_DECLARATIONS' from comments.
C
C-    SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.1, 26-MAR-1991 (HAN)
C
C        The Required Reading file POSITION was renamed to SCANNING.
C        This header was updated to reflect the change.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     forward search for position of unlisted character
C
C-&
 
 
 
 
C
C     Local variables
C
      LOGICAL               FOUND
      INTEGER               LENSTR
      INTEGER               B
 
 
      LENSTR = LEN ( STR )
 
      B      = MAX(1,START)
      FOUND  = .FALSE.
      NCPOS   = 0
 
      DO WHILE ( .NOT. FOUND )
 
         IF ( B .GT. LENSTR ) THEN
            RETURN
         ELSE IF ( INDEX( CHARS, STR(B:B) ) .EQ. 0 ) THEN
            NCPOS = B
            RETURN
         ELSE
            B = B + 1
         END IF
 
      END DO
 
      RETURN
      END
