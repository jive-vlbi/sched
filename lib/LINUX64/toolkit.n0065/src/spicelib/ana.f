C$Procedure ANA ( AN or A ? )
 
      CHARACTER*(*) FUNCTION ANA ( WORD, CASE )
 
C$ Abstract
C
C     Return the correct article "a" or "an" used to modify a word
C     and return it capitalized, lower case, or upper case.
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
C     WORD
C
C$ Keywords
C
C     UTILITY
C     WORD
C
C$ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         WORD
      CHARACTER*(*)         CASE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     WORD       I   is a word that should be modified by "a" or "an"
C     CASE       I   'U', 'L', or 'C' to specify capitalization of ANA.
C     ANA        O   'A' or 'AN' appropriately capitalized.
C
C$ Detailed_Input
C
C     WORD       is any english word for which you want to write the
C                correct phrase "a(an) response(answer)".  The case
C                of the letters of word do not matter.
C
C                Leading white space in word is ignored.  The characters
C                " and ' are ignored.  Thus ''' apple '' ' and
C                '"apple"' and ' apple' and 'apple' are all treated as
C                the same word.
C
C     CASE       is a character that describes how the value returned
C                in ANA should be capitalized.  The rules are:
C
C                   'U'  ---  ANA is returned in all caps ( A, AN )
C                   'C'  ---  ANA is returned capitalized ( A, An )
C                   'L'  ---  ANA is returned lower case  ( a, an )
C
C                The case of CASE does not matter.  Any value other
C                than those specified result in ANA being returned
C                in all lower case.
C
C$ Detailed_Output
C
C     ANA        is a character function an will return the correct
C                indefinite article needed to modify the word contained
C                in WORD.  ANA should be declared to be CHARACTER*(2)
C                (or CHARACTER*(N) where N > 1) in the calling
C                program.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error Free
C
C     1) If the uppercase value of CASE is not 'U', 'C' or 'L', it shall
C        be treated as 'L'.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows you to construct grammatically correct phrases
C     when you need to modify a word by an indefinite article.  Using
C     the pronunciations contained in the Webster's Ninth Collegiate
C     Dictionary, the phrase
C
C      ANA(WORD, CASE) // ' ' // WORD
C
C     will be grammatically correct.
C
C$ Examples
C
C     Suppose you wished to construct one of the messages
C
C        'a new file'
C        'an existing file'
C
C     and that the NEW/EXISTING word was in the variable WORD. Then
C     you could write
C
C        MESSAGE = ANA( WORD, 'L' ) // ' ' // WORD // ' file '
C        CALL CMPRSS ( ' ', 1, MESSAGE, MESSAGE )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     Webster's Ninth Collegiate Dictionary.
C
C$ Author_and_Institution
C
C     B.V. Semenov    (JPL)
C     W.L. Taber      (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.1.2, 28-FEB-2008 (BVS) 
C
C        Corrected the contents of the Required_Reading section.
C
C-    SPICELIB Version 1.1.1, 22-SEP-2004 (EDW)
C
C        Added Copyright section.
C
C-    SPICELIB Version 1.1.0, 18-JAN-2001 (WLT)
C
C        Made SCLK and "an" word.
C
C-    SPICELIB Version 1.0.0, 29-NOV-1995 (WLT)
C
C-&
 
C$ Index_Entries
C
C     GET THE CORRECT INDEFINITE ARTICLE
C
C-&
 
      INTEGER               ISRCHC
 
      INTEGER               ANWRDS
      PARAMETER           ( ANWRDS = 22 )
 
      INTEGER               AWRDS
      PARAMETER           ( AWRDS  = 33 )
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      CHARACTER*(8)         ANWORD ( ANWRDS )
      CHARACTER*(8)         AWORD  ( AWRDS  )
 
      CHARACTER*(WDSIZE)    MYWORD
      CHARACTER*(1)         MYCASE
      CHARACTER*(1)         BEGIN
 
 
      INTEGER               I
      INTEGER               CAPS
 
      CHARACTER*(WDSIZE)    START ( 7 )
 
      CHARACTER*(2)         A  ( 3 )
      CHARACTER*(2)         AN ( 3 )
 
      SAVE
 
      DATA                  A  / 'A',  'A',  'a'  /
      DATA                  AN / 'AN', 'An', 'an' /
 
 
      DATA  ANWORD / 'HEIR', 'HONEST', 'HONOR', 'H',  'HOUR',
     .               'HORS ','HOMBRE', 'F',     'L',  'M',
     .               'N',    'R',      'S',     'X',
     .               'UNIN', 'UNIM',   'ONEI',  'ONER',
     .               'SPK',  'EK',     'IK',     'SCLK' /
 
      DATA  AWORD  / 'HORSE',   'ONE',   'ONE-',   'ONCE',
     .               'ONENESS', 'UIG',   'UIN',    'UKA',
     .               'UKE',     'UKO',   'UKI',    'UKU',
     .               'ULOT',    'UNANI', 'UNI',    'UNINU',
     .               'UPA',     'URA',   'URE',    'URO',
     .               'USA',     'USE',   'USU',    'UTE',
     .               'UTI',     'UTO',   'UVA',    'UVE',
     .               'UVU',     'EU',    'EWE',    'UTRI', 'U' /
 
      CALL UCASE ( WORD,              MYWORD )
      CALL REPLCH( MYWORD, '''', ' ', MYWORD )
      CALL REPLCH( MYWORD, '"',  ' ', MYWORD )
      CALL LJUST ( MYWORD,            MYWORD )
      CALL UCASE ( CASE,              MYCASE )
 
 
      ANA = ' '
 
      IF ( MYCASE .EQ. 'U' ) THEN
         CAPS = 1
      ELSE IF ( MYCASE .EQ. 'C' ) THEN
         CAPS = 2
      ELSE
         CAPS = 3
      END IF
C
C     Handle the obvious things first.
C
      BEGIN = MYWORD(1:1)
 
      IF      ( INDEX( 'AI', BEGIN ) .GT. 0 ) THEN
 
         ANA = AN(CAPS)
         RETURN
 
      ELSE IF ( INDEX( 'BCDGJKPQTVWYZ', BEGIN ) .GT. 0 ) THEN
 
         ANA = A(CAPS)
         RETURN
 
      END IF
 
C
C     If we are still here, we need to be a bit more careful
C     in our determination of ANA.
C
C     Get the beginnings of the input word.
C
      DO I = 1, 7
         START(I) = MYWORD(1:I)
      END DO
 
C
C     Now see if the start of the input word belongs to
C     one of the special collections.
C
      DO I = 7, 2, -1
 
         IF ( ISRCHC ( START(I), AWRDS,  AWORD ) .NE. 0 ) THEN
 
            ANA = A (CAPS)
            RETURN
 
         END IF
 
         IF ( ISRCHC ( START(I), ANWRDS, ANWORD ) .NE. 0 ) THEN
 
            ANA = AN(CAPS)
            RETURN
 
         END IF
 
      END DO
C
C     If we got this far we can determine the ANAe by
C     just looking at the beginning of the string.
C
      IF ( INDEX( 'AEIOU', MYWORD(1:1) ) .GT. 0 ) THEN
 
         ANA = AN(CAPS)
 
      ELSE
 
         ANA = A (CAPS)
 
      END IF
 
      RETURN
 
      END
