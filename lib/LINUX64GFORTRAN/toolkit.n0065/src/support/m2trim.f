C$Procedure      M2TRIM ( META/2 trim the name portion from a word )
 
      SUBROUTINE M2TRIM ( WORD, ROOT )
      IMPLICIT NONE
C$ Abstract
C
C     Extract the "root" of a META/2 template word.  That is trim off
C     the name portion of a template word.
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
C    META/2
C
C$ Keywords
C
C     META1
C
C$ Declarations
 
      CHARACTER*(*)         WORD
      CHARACTER*(*)         ROOT
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     WORD       I   A word from a META/2 template.
C     ROOT       O   The input word trimmed of any name specification.
C
C$ Detailed_Input
C
C     WORD       is a word from a META/2 template.  It may or may not
C                looklike   ROOT // '[name]'
C
C$ Detailed_Output
C
C     ROOT       is the portion of the input word that precedes the
C                name portion of the input WORD.  ROOT may overwrite
C                WORD.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     If ROOT is not sufficiently large to contain all of the output,
C     it will be truncated on the right.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     META/2 template words can have appended to them "variable" names
C     that will be used to store substring boundaries of STRINGS matched
C     against META/2 templates.  For example
C
C           FIND @name[WINDOW]
C           SEPARATION  (2:2){ OF   @int[BODY1] @int[BODY2]
C                            | FROM @int[OBSERVER]          }
C
C     the words
C
C         @name[WINDOW], @int[BODY1], @int[BODY2], @int[OBSERVER]
C
C     all have "varialbe" name substrings.  They are:
C
C         WINDOW, BODY1, BODY2, and OBSERVER respectively.
C
C     The routine removes variable names and associated brackets in WORD
C     if they exist.
C
C$ Examples
C
C     Below is a table descibing sample inputs and outputs.
C
C         WORD                ROOT
C         ---------------     ------------------
C         @int[SPUD]          @int
C         @name[WINDOW]       @name
C         SEARCH[GET]         SEARCH
C         @name               @name
C         @body(2:4)[LIST]    @body(2:4)
C
C$ Restrictions
C
C     None.
C
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
C-    Beta Version 1.0.0, 21-NOV-1991 (WLT)
C
C-&
C
C$ Index_Entry
C
C     Extract the root of a META/2 template word.
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               QRTRIM
 
 
C
C     Local variables
C
      INTEGER               E
      INTEGER               B
      INTEGER               LBRACE
      INTEGER               RBRACE
      INTEGER               BLANK
 
      SAVE
 
 
 
      ROOT   = WORD
      LBRACE = ICHAR ( '['  )
      RBRACE = ICHAR ( ']'  )
      BLANK  = ICHAR ( ' '  )
      E      = LEN   ( WORD )
C
C     This loop is the same as RTRIM only faster.
C
      E = QRTRIM ( WORD )
 
 
C
C     If the length is not at least 4 or the last character is not
C     a right brace, there is no name associated with this word.
C
      IF (      ( ICHAR(WORD(E:E)) .EQ. RBRACE )
     .    .AND. ( E                .GE. 4      ) ) THEN
C
C        Ok. We have a chance at getting a name.  Look for
C        a left brace and if found blank out the end portion of
C        ROOT.
C
         B = 2
 
         DO WHILE (B .LT. E - 1)
 
            IF ( ICHAR(WORD(B:B)) .EQ. LBRACE ) THEN
C
C              We've found the beginning of the name portion
C              of the word.  Record the end of the meta-2
C              word and then reset L so that we exit this loop.
C
               ROOT(B:) = ' '
               B        = E
            END IF
 
            B = B + 1
 
         END DO
 
      END IF
 
      RETURN
      END
