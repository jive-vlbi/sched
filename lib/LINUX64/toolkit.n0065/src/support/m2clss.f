C$Procedure      M2CLSS (Meta 2 --- meta 2 word classification )
 
      SUBROUTINE M2CLSS ( WORD, NUM, PHRASE  )
 
C$ Abstract
C
C     This routine creates a phrase of the appropiate number
C     that describes the meta2 syntax word WORD.
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
C      None.
C
C$ Keywords
C
C       META2
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         WORD
      INTEGER               NUM
      CHARACTER*(*)         PHRASE
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      WORD       I   A meta-2 keyword.
C      NUM        I   The number of meta-2 keywords
C      PHRASE     O   A description of NUM WORDs.
C
C$ Detailed_Input
C
C     WORD        is a meta-2 keyword such as @int or @number(1:20)
C
C     NUM         is used to indicate if how many of the WORDS we
C                 want to talk about.  For example when describing
C                 @int(1:10) do you want to say
C
C                    integer between 1 and 10
C                 or
C                    integers between 1 and 10
C
C                 If NUM is 1 you get the first phrase.  Otherwise
C                 you get the second one.
C
C$ Detailed_Output
C
C     PHRASE      is a character string that describes WORD and NUM.
C
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      None.
C
C$ Exceptions
C
C     1) Although it has not changed in a long time.  META/2 might
C     have some other word classifiers added.  In that case this
C     routine will have to be updated.  But it will make a reasonable
C     phrase even if the new META/2 keyword isn't recognized yet.
C     Something like
C
C        word(s) of the class WORD
C
C     will be used.
C
C$ Particulars
C
C     This
C
C$ Examples
C
C     Suppose that a message needs to be created that says
C     a word in a string does not match an expected @int(1:10)
C
C     You could use this routine together with the utility function
C     ANA to construct a reasonable message.
C
C     CALL  M2CLSS ( '@int(1:10), 1, PHRASE )
C     ARTCLE = ANA (  PHRASE,    'C'        )
C
C     MESSGE = '# # was expected in the string.'
C
C     CALL REPMC ( MESSGE, '#', ARTCLE, MESSGE )
C     CALL REPMC ( MESSGE, '#', PHRASE, MESSGE )
C
C     The resulting string in MESSGE would be
C
C     'An integer between 1 and 10 was expected.'
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    META/2 Version 2.0.0, 23-MAY-2000 (WLT)
C
C        Updated the routine to support the additional Meta/2 keyword
C        @unit.
C
C-    META/2 Version 1.0.0, 12-AUG-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     META/2 diagnostic message creation tool
C
C-&
C
C     SPICELIB functions
C
      INTEGER               POS
      INTEGER               RTRIM
 
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      CHARACTER*(LNSIZE)    ALPHA (2)
      CHARACTER*(LNSIZE)    BODY  (2)
      CHARACTER*(LNSIZE)    DAY   (2)
      CHARACTER*(LNSIZE)    DP    (2)
      CHARACTER*(LNSIZE)    ENGLSH(2)
      CHARACTER*(LNSIZE)    EPOCH (2)
      CHARACTER*(LNSIZE)    GWORD (2)
      CHARACTER*(LNSIZE)    INT   (2)
      CHARACTER*(LNSIZE)    MONTH (2)
      CHARACTER*(LNSIZE)    NAME  (2)
      CHARACTER*(LNSIZE)    OTHER (2)
      CHARACTER*(LNSIZE)    TIME  (2)
      CHARACTER*(LNSIZE)    UNITS (2)
      CHARACTER*(LNSIZE)    YEAR  (2)
 
 
      INTEGER               B
      INTEGER               C
      INTEGER               E
      INTEGER               NUMBER
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
      CHARACTER*(WDSIZE)    BASE
      LOGICAL               KEY
      LOGICAL               RTEMP
      LOGICAL               FIRST
      SAVE
 
 
      DATA                  FIRST / .TRUE. /
 
 
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
         ALPHA(1)  = 'word beginning with a letter'
         ALPHA(2)  = 'words beginning with a letter'
 
         BODY(1)   = 'body name or id-code'
         BODY(2)   = 'body names or id-codes'
 
         DAY(1)    = 'day of the year'
         DAY(2)    = 'days of the year'
 
         ENGLSH(1) = 'word containing only letters'
         ENGLSH(2) = 'words containing only letters'
 
         EPOCH(1)  = 'epoch'
         EPOCH(2)  = 'epochs'
 
         MONTH(1)  = 'month of the year'
         MONTH(2)  = 'months of the year'
 
         NAME(1)   = 'word of letters and digits starting with a letter'
         NAME(2)   = 'words of letters and digits each starting '
     .   //          'with a letter '
 
         TIME(1)   = 'time of day'
         TIME(2)   = 'times of the day'
 
         YEAR(1)   = 'calendar year (1000 to 3000) '
         YEAR(2)   = 'calendar years (1000 to 3000) '
 
         GWORD(1)  = 'generic word'
         GWORD(2)  = 'generic words'
 
         OTHER(1)  = 'word of class '// WORD
         OTHER(2)  = 'words of class '// WORD
 
         INT  (1)  = 'integer'
         INT  (2)  = 'integers'
 
         DP   (1)  = 'number'
         DP   (2)  = 'numbers'
 
         UNITS(1)  = 'unit specification'
         UNITS(2)  = 'unit specifications'
 
      END IF
 
      IF ( NUM .EQ. 1 ) THEN
         NUMBER = 1
      ELSE
         NUMBER = 2
      END IF
 
      B = 1
      E = RTRIM ( WORD )
 
      CALL M2TRAN ( WORD, B, E, BASE, KEY, RTEMP )
 
      IF ( BASE .EQ. '@int' ) THEN
 
         PHRASE = INT(NUMBER)
 
         IF ( RTEMP ) THEN
 
            C = POS(WORD, ':', B )
 
            IF ( C .EQ. B+1 ) THEN
               CALL SUFFIX ( 'less than or equal to #',  1,   PHRASE )
               CALL REPMC  (  PHRASE, '#',   WORD(B+2:E-1),   PHRASE )
            ELSE IF ( C .EQ. E-1 ) THEN
               CALL SUFFIX ( 'greater than or equal to #', 1, PHRASE )
               CALL REPMC  (  PHRASE, '#',   WORD(B+1:E-2),   PHRASE )
            ELSE
               CALL SUFFIX ( 'between # and # (inclusive)',1, PHRASE )
               CALL REPMC  ( PHRASE, '#',  WORD(B+1:C-1),     PHRASE )
               CALL REPMC  ( PHRASE, '#',  WORD(C+1:E-1),     PHRASE )
            END IF
 
         END IF
 
         RETURN
 
      END IF
 
      IF ( BASE .EQ. '@number' ) THEN
 
         PHRASE = DP(NUMBER)
 
         IF ( RTEMP ) THEN
 
            C = POS(WORD, ':', B+1 )
 
            IF ( C .EQ. B+1 ) THEN
               CALL SUFFIX ( 'less than or equal to #',  1,   PHRASE )
               CALL REPMC  (  PHRASE, '#',   WORD(B+2:E-1),   PHRASE )
            ELSE IF ( C .EQ. E-1 ) THEN
               CALL SUFFIX ( 'greater than or equal to #', 1, PHRASE )
               CALL REPMC  (  PHRASE, '#',   WORD(B+1:E-2),   PHRASE )
            ELSE
               CALL SUFFIX ( 'between # and # (inclusive)',1, PHRASE )
               CALL REPMC  ( PHRASE, '#',  WORD(B+1:C-1),     PHRASE )
               CALL REPMC  ( PHRASE, '#',  WORD(C+1:E-1),     PHRASE )
            END IF
 
         END IF
 
         RETURN
 
      END IF
 
      IF ( BASE .EQ. '@unit' ) THEN
 
         PHRASE = UNITS(NUMBER)
 
         IF ( RTEMP ) THEN
 
            CALL SUFFIX ( 'with dimensions compatible with #',
     .                     1, PHRASE)
            CALL REPMC  ( PHRASE, '#', WORD(B+1:E-1), PHRASE )
 
         END IF
 
         RETURN
 
      END IF
 
      IF      ( BASE .EQ. '@alpha'   ) THEN
 
         PHRASE = ALPHA(NUMBER)
 
      ELSE IF ( BASE .EQ. '@body'    ) THEN
 
         PHRASE = BODY(NUMBER)
 
      ELSE IF ( BASE .EQ. '@day'     ) THEN
 
         PHRASE = DAY(NUMBER)
 
      ELSE IF ( BASE .EQ. '@english' ) THEN
 
         PHRASE = ENGLSH(NUMBER)
 
      ELSE IF ( BASE .EQ. '@epoch'   ) THEN
 
         PHRASE = EPOCH(NUMBER)
 
      ELSE IF ( BASE .EQ. '@month'   ) THEN
 
         PHRASE = MONTH(NUMBER)
 
      ELSE IF ( BASE .EQ. '@name'    ) THEN
 
         PHRASE = NAME(NUMBER)
 
      ELSE IF ( BASE .EQ. '@time'    ) THEN
 
         PHRASE = TIME(NUMBER)
 
      ELSE IF ( BASE .EQ. '@year'    ) THEN
 
         PHRASE = YEAR(NUMBER)
 
      ELSE IF ( BASE .EQ. '@word'    ) THEN
 
         PHRASE = GWORD(NUMBER)
 
      ELSE
 
         PHRASE = OTHER(NUMBER)
 
      END IF
 
      IF ( RTEMP ) THEN
 
         CALL SUFFIX ( 'that matches the pattern ''', 1, PHRASE )
         CALL SUFFIX ( WORD(B+1:E-1),                 0, PHRASE )
         CALL SUFFIX ( '''',                          0, PHRASE )
 
      END IF
 
      RETURN
      END
