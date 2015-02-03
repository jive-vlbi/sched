 
C$Procedure MATCH ( Match string against multiple wildcard templates )
 
      LOGICAL FUNCTION MATCH ( STRING, TEMPL )
      IMPLICIT NONE
 
C$ Abstract
C
C      Determines whether or not a string matches any of a
C      collection of templates containing wildcard characters.
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
C$ Keywords
C
C      SEARCH
C      UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         STRING
      CHARACTER*(*)         TEMPL
 
C$ Brief_I/O
C
C      Variable  I/O  Description
C      --------  ---  --------------------------------------------------
C      STRING     I   String to be matched against templates.
C      TEMPL      I   Collection of templates.
C
C$ Detailed_Input
C
C      STRING     is a character string to be checked for a match
C                 against the specified collection of templates.
C                 Leading and trailing blanks are ignored.
C
C      TEMPL      is a collection of individual templates to be
C                 compared against the specified string. Leading
C                 and trailing blanks are ignored. An empty (blank)
C                 template collection matches only an empty (blank)
C                 string.
C
C$ Detailed_Output
C
C      The function is TRUE whenever the string matches the collection
C      of templates, and is FALSE otherwise.
C
C$ Exceptions
C
C      None.
C
C$ Particulars
C
C      MATCH is exactly equivalent to MATCHM with the special characters
C      defined as follows.
C
C            WCHR    = '%'
C            WSTR    = '*'
C            NOTCHR  = '~'
C            ORCHR   = '|'
C
C$ Examples
C
C      1. Normal Templates
C      -------------------
C
C      Consider the following string
C
C         '  ABCDEFGHIJKLMNOPQRSTUVWXYZ '
C
C      and the following templates.
C
C         Template         Matches STRING?
C         ---------------  ---------------
C         '*A*'            Yes
C         'A%D*'           No
C         'A%C*'           Yes
C         '%A*'            No
C         ' A*   '         Yes
C
C         '%%CD*Z'         Yes
C         '%%CD'           No
C         'A*MN*Y*Z'       Yes
C         'A*MN*Y%Z'       No
C         '*BCD*Z*'        Yes
C         '*bcd*z*'        No
C
C
C      2. Negated Templates
C      --------------------
C
C      Consider the same string, and the following templates.
C
C         Template         Matches STRING?
C         ---------------  ---------------
C         '~%B*D'          Yes
C         '~%B*D*'         No
C         '~ABC'           Yes
C         '~ABC*'          No
C         '~~B*'           Yes
C
C      Note that in the final example, the second '~' is treated not as
C      a second negation but as an ordinary character.
C
C
C      3. Combining Templates
C      ----------------------
C
C      Consider the following strings and templates.
C
C           String          Template             Matches?
C           --------------  -------------------  --------
C           AKRON           *A*|*B*              Yes
C           BELOIT          *B*|*I*              Yes
C           CHAMPAGNE       *B*|*I*              No
C
C
C      4. Combining Negated Templates
C      ------------------------------
C
C      Consider the following strings and templates.
C
C           String          Template             Matches?
C           --------------  -------------------  --------
C           SEQUIOA         ~*A*|~*E*|~*I*       No
C           SAINT PAUL      ~*A*|~*E*|~*I*       Yes
C           HOUSTON         ~*A*|~*E*|~*I*       Yes
C
C
C      5. Negating Combined Templates
C      ------------------------------
C
C      Consider the following strings and templates.
C
C           String          Template             Matches?
C           --------------  -------------------  --------
C           DETROIT         ~|B*|D*              No
C           EUGENE          ~|B*|D*              Yes
C           FAIRBANKS       ~|*A*|*I*|*O*|*U*    No
C           GREENBELT       ~|*A*|*I*|*O*|*U*    Yes
C
C$ Restrictions
C
C      None.
C
C$ Common_Variables
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber     (JPL)
C      I.M. Underwood (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C
C      META/2 Configured Version 2.1.0, 28-DEC-1994 (WLT)
C
C         An initial value of FALSE is assigned to MATCH so
C         that if we are running in RETURN mode the function
C         will have a value.
C
C-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
C
C         This is the configured version of the Command Loop
C         software as of May 9, 1994
C
C
C-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
C
C         This is the configured version of META/2
C         software as of May 3, 1994
C
C
C      Version B 1.0.0, 15-MAY-1988
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL               MATCHM
      LOGICAL               RETURN
 
C
C     Local variables
C
      CHARACTER*1           WSTR
      PARAMETER           ( WSTR =   '*' )
 
      CHARACTER*1           WCHR
      PARAMETER           ( WCHR =   '%' )
 
      CHARACTER*1           NOTCHR
      PARAMETER           ( NOTCHR = '~' )
 
      CHARACTER*1           ORCHR
      PARAMETER           ( ORCHR =  '|')
 
C
C     Give the function an intial value of FALSE
C
      MATCH = .FALSE.
 
C
C     Standard SPICE error handling
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'MATCH' )
      END IF
 
      MATCH = MATCHM ( STRING, TEMPL, WSTR, WCHR, NOTCHR, ORCHR )
 
      CALL   CHKOUT ( 'MATCH' )
      RETURN
      END
