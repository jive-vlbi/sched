C$Procedure      REPLWD ( Replace a word )
 
      SUBROUTINE REPLWD ( INSTR, NTH, NEW, OUTSTR )
 
C$ Abstract
C
C      Replace the Nth word in a string with a new word.
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
C      ASSIGNMENT,  WORD
C
C$ Declarations
 
      CHARACTER*(*)    INSTR
      INTEGER          NTH
      CHARACTER*(*)    NEW
      CHARACTER*(*)    OUTSTR
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      INSTR      I   Input string.
C      NTH        I   Number of the word to be replaced.
C      NEW        I   Replacement word.
C      OUTSTR     O   Output string.
C
C$ Detailed_Input
C
C      INSTR       is the input character string, possibly containing
C                  one or more words, where a word is any string of
C                  consecutive non-blank characters delimited by a
C                  blank or by either end of the string.
C
C      NTH         is the number of the word to be replaced. Words
C                  are numbered from one. If NTH is less than one,
C                  or greater than the number of words in the string,
C                  no replacement is made.
C
C      NEW         is the word which is to replace the specified word
C                  in the input string. Leading and trailing blanks
C                  are ignored. If the replacement word is blank,
C                  the original word is simply removed.
C
C$ Detailed_Output
C
C      OUTSTR      is the output string. This is the input string
C                  with the N'th word replaced by the word NEW.
C                  Any blanks originally surrounding the replaced
C                  word are retained.
C
C                  OUTSTR may overwrite INSTR.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      The effect of this routine is to remove the old word with
C      REMSUB, and insert the replacement word with INSSUB.
C
C$ Exceptions
C
C      Error free.
C
C      1) If NEW is blank, then the Nth word is replaced by a single
C         space.
C
C$ Files
C
C      None.
C
C$ Examples
C
C      Let
C            INSTR  = '  Woodsy is the Anti-Pollution  Owl.'
C
C      and
C            NEW    = '   an   '
C
C      then the following values of NTH yield the following strings.
C
C            NTH      OUTSTR
C            ---      ------------------------------------------
C             -1      '  Woodsy is the Anti-Pollution  Owl.'
C              0      '  Woodsy is the Anti-Pollution  Owl.'
C              1      '  an is the Anti-Pollution  Owl.'
C              3      '  Woodsy is an Anti-Pollution  Owl.'
C              4      '  Woodsy is the an  Owl.'
C              5      '  Woodsy is the Anti-Pollution  an'
C              6      '  Woodsy is the Anti-Pollution  Owl.'
C
C      Note that in the first, second, and last cases, the string
C      was not changed. Note also that in the next to last case,
C      the final period was treated as part of the fifth word in the
C      string.
C
C      If NEW is ' ', and NTH is 3, then
C
C            OUTSTR = '  Woodsy is Anti-Pollution  Owl.'
C
C$ Restrictions
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      H.A. Neilan     (JPL)
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
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     replace a word
C
C-&
 
 
 
C$ Revisions
C
C-     Beta Version 1.3.0, 7-MAR-1989  (WLT)
C
C         To satisfy complaints about me not having enough to do,
C         the case of a blank NEW word has been handled.
C
C-     Beta Version 1.2.0, 28-FEB-1989 (WLT)
C
C         Routine completely rewritten to satify whims of the
C         NAIF group.
C
C-     Beta Version 1.1.1, 17-FEB-1989 (HAN) (NJB)
C
C         Contents of the Exceptions section was changed
C         to "error free" to reflect the decision that the
C         module will never participate in error handling.
C
C         Declaration of the unused variable OUTLEN deleted.
C-&
 
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER               FRSTNB
      INTEGER               LASTNB
 
C
C   Local Variables
C
      CHARACTER*2           SHORT
      INTEGER               BEGIN
      INTEGER               I
      INTEGER               J
      INTEGER               F
      INTEGER               L
      INTEGER               K
      INTEGER               N
      INTEGER               SHIFT
 
 
 
C
C     First just shift the input string into the output string,
C     then do everything in place (for the case when the new
C     word is longer than the old one.  When its shorter we'll
C     need to change this scheme slightly.)
C
      OUTSTR = INSTR
C
C     Where does the word to be replaced begin? If there is none,
C     just return the original string.
C
      CALL NTHWD  ( OUTSTR, NTH, SHORT, BEGIN )
 
      IF ( BEGIN .EQ. 0 ) THEN
 
         RETURN
 
      END IF
C
C     Otherwise, find out where it ends as well.
C
      CALL FNDNWD ( INSTR, BEGIN, I, J )
 
C
C     Now insert only the non-blank part of the replacement string.
C     If the replacement string is blank, don't insert anything.
C
      IF ( NEW .NE. ' ' ) THEN
 
         F = FRSTNB ( NEW )
         L = LASTNB ( NEW )
 
C
C        Except in the lucky case that the word to insert is the
C        same length as the word it's replacing, we will have
C        to shift right or left by some amount.  Compute the
C        appropriate amount to shift right.
C
         SHIFT = ( L - F ) - ( J - I )
 
      ELSE
 
         F     = 1
         L     = 1
         SHIFT = I - J
 
      END IF
 
      IF      ( SHIFT .GT. 0 ) THEN
 
C
C        To shift right in place start at the right most character
C        of the string and copy the character SHIFT spaces to the
C        left.
C
         K = LEN(OUTSTR)
         N = K - SHIFT
 
         DO WHILE ( N .GT. J )
            OUTSTR(K:K) = OUTSTR(N:N)
            K           = K - 1
            N           = N - 1
         END DO
 
C
C        Once the appropriate characters have been shifted out
C        of the way, replace the opened space with the new
C        word.
C
         DO WHILE (       ( F .LE. L           )
     .              .AND. ( I .LE. LEN(OUTSTR) ) )
            OUTSTR(I:I) = NEW(F:F)
            F           = F + 1
            I           = I + 1
         END DO
 
      ELSE
 
C
C        We have a left shift. Fill in the first part of the word
C        we are replacing with the new one.
C
         DO WHILE (       ( F .LE. L           )
     .              .AND. ( I .LE. LEN(OUTSTR) ) )
 
            OUTSTR(I:I) = NEW(F:F)
            F           = F + 1
            I           = I + 1
 
         END DO
 
C
C        Now starting just past the end of the word we are replacing
C        shift the remainder of string left one character at a time.
C
         IF ( SHIFT .LT. 0 ) THEN
 
            J = J + 1
 
            DO WHILE (       ( I .LE. LEN(OUTSTR)  )
     .                 .AND. ( J .LE. LEN(INSTR)   ) )
 
               OUTSTR(I:I) = INSTR(J:J)
               I           = I + 1
               J           = J + 1
            END DO
 
C
C           Finally pad the string with blanks.
C
            IF ( I .LE. LEN(OUTSTR) ) THEN
               OUTSTR(I:) = ' '
            END IF
 
         END IF
 
      END IF
 
      RETURN
      END
