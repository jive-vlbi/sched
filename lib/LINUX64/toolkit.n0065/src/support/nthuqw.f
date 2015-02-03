C$Procedure      NTHUQW ( N'th unquoted word)
 
      SUBROUTINE NTHUQW ( STRING, N, EQUOTE, WORD, LOC )
 
C$ Abstract
C
C    This routine finds the N'th non-quoted word in a string.
C    Quoted substrings are ignored and not treated as
C    blanks.
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
C     STRING
C     WORD
C
C$ Declarations
 
      IMPLICIT NONE
 
      CHARACTER*(*)         STRING
      INTEGER               N
      CHARACTER*(1)         EQUOTE
      CHARACTER*(*)         WORD
      INTEGER               LOC
 
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C      STRING     I   Input character string.
C      N          I   Index of the word to be returned.
C      EQUOTE     I   An extra quote character.
C      WORD       O   The N'TH unquoted word in STRING.
C      LOC        O   Location of WORD in STRING.
C
C$ Detailed_Input
C
C      STRING     is the input string to be parsed. It contains
C                 some number of word, where a word is any string
C                 of consecutive non-blank characters not between
C                 balanced quotes.
C
C      N          is the index of the word to be returned. (One for
C                 the first word, two for the second, and so on.)
C
C      EQUOTE     is a special character that users may supply so
C                 that specially marked strings will be skipped
C                 in the selection of words.  If you do not want
C                 any specially marked strings use a blank for EQUOTE
C
C$ Detailed_Output
C
C      WORD       is the N'th word in STRING. If STRING is blank,
C                 or NTH is nonpositive or too large, WORD is blank.
C
C      LOC        is the location of WORD in STRING. (That is, WORD
C                 begins at STRING(LOC:LOC). If STRING is blank, or
C                 NTH is nonpositive or too large, LOC is zero.
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
C     Error free.
C
C     1)
C
C$ Particulars
C
C      NTHUQW, like NTHWD, is useful primarily for parsing input
C      commands consisting of one or more words, where a word is
C      defined to be any sequence of consecutive non-blank characters.
C
C      The chief difference between the two routines is that
C      NTHUQW treats all character starting at and through
C      a balanced quote as blanks.  Both " and ' are treated as
C      quote characters.
C
C$ Examples
C
C
C      Let STRING be ' He said, "Now is the time" and left. '
C                     1234567890123456789012345678901234567
C
C      If N = -1   WORD = ' '          LOC =  0
C              0          ' '                 0
C              1,         'He'                2
C              2,         'said,'             5
C              3,         'and'              29
C              4,         'left.'            33
C              5,         ' '                 0
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C       W.L. Taber      (JPL)
C
C$ Literature_References
C
C       None.
C
C$ Version
C
C-    Inspekt Version 1.0.0, 14-JUL-1995 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Find the n'th unquoted word in a string
C
C-&
C
C     Spice Functions
C
      INTEGER               RTRIM
C
C     Local Variables
C
      INTEGER               B
      INTEGER               DQUOTE
      INTEGER               E
      INTEGER               I
      INTEGER               L
      INTEGER               LAST
      INTEGER               SQUOTE
      INTEGER               WCOUNT
 
      INTEGER               SPCIAL
 
      LOGICAL               ISQ
      LOGICAL               BLANK
 
      LOGICAL               ODDDQ
      LOGICAL               ODDSQ
      LOGICAL               ODDSP
 
      LOGICAL               INWORD
 
C
C     An integer
C
      ISQ  ( L ) =      L .EQ. SQUOTE
     .             .OR. L .EQ. DQUOTE
     .             .OR. L .EQ. SPCIAL
 
      BLANK( L ) =      L .EQ. ICHAR( ' ' )
     .             .OR. ODDDQ
     .             .OR. ODDSQ
     .             .OR. ODDSP
     .             .OR. ISQ(L)
C
C     Take care of the dumb cases first.
C
      IF ( N .LE. 0 ) THEN
         WORD = ' '
         LOC  = 0
         RETURN
      END IF
 
 
      SQUOTE = ICHAR ( ''''        )
      DQUOTE = ICHAR ( '"'         )
 
      SPCIAL = ICHAR ( EQUOTE(1:1) )
 
      IF ( SPCIAL .EQ. ICHAR( ' ' ) ) THEN
         SPCIAL = SQUOTE
      END IF
 
      LAST   =  RTRIM ( STRING )
      WCOUNT =  0
      ODDDQ  = .FALSE.
      ODDSQ  = .FALSE.
      ODDSP  = .FALSE.
      INWORD = .FALSE.
 
      DO I = 1, LAST
C
C        Get the integer value of the I'th character of string.
C
         L = ICHAR( STRING(I:I) )
 
C
C        If this is a quote character, then flip the ODDQ logical
C
         IF ( L .EQ. SPCIAL ) THEN
            ODDSP = .NOT. ODDSP
         END IF
 
         IF ( L .EQ. SQUOTE ) THEN
            ODDSQ = .NOT. ODDSQ
         END IF
 
         IF ( L .EQ. DQUOTE ) THEN
            ODDDQ = .NOT. ODDDQ
         END IF
C
C        If this is a blank ...
C
         IF ( BLANK(L) ) THEN
C
C           if we are in the middle of a word, we are about to
C           end it.  If the word counter WCOUNT has the same
C           value of N then we've found the N'th unquoted word.
C           Set the various outputs and return.
C
            IF ( INWORD .AND. WCOUNT .EQ. N ) THEN
               WORD = STRING(B:E)
               LOC   = B
               RETURN
            END IF
C
C           If we get to here, we just point out that we are
C           not in a word.
C
            INWORD = .FALSE.
 
         ELSE
C
C           If this is not a "blank"  then ODDDQ, ODDSQ and ODDSP are
C           false so we are not inside a quoted string.  We are either
C           already in a word, or we are just starting one.
C
            IF ( INWORD ) THEN
C
C              We are in a word, just bump the end of this one.
C
               E = I
 
            ELSE
C
C              We are beginning a word. Up the word counter,
C              set the end and beginning of the word.
C
               INWORD = .TRUE.
               WCOUNT =  WCOUNT + 1
               B      =  I
               E      =  I
 
 
            END IF
 
         END IF
C
C        Examine the next character.
C
      END DO
 
 
      IF ( INWORD .AND. WCOUNT .EQ. N ) THEN
 
         LOC  = B
         WORD = STRING(B:)
 
      ELSE
 
         LOC  = 0
         WORD = ' '
 
      END IF
 
      RETURN
      END
