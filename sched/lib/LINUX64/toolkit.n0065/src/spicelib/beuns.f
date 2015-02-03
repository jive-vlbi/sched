C$Procedure            BEUNS  ( Be an unsigned integer? )
 
      LOGICAL FUNCTION BEUNS  ( STRING )
 
C$ Abstract
C
C     Determine whether a string represents an unsigned integer.
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
C     WORDS
C
C$ Keywords
C
C     ALPHANUMERIC
C     NUMBERS
C     SCANNING
C     UTILITY
C
C$ Declarations
 
      CHARACTER*(*)         STRING
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     STRING     I   Character string.
C
C     The function returns TRUE if the string represents an unsigned
C     integer.  Otherwise, it returns FALSE.
C
C$ Detailed_Input
C
C     STRING      is any string.
C
C$ Detailed_Output
C
C     If STRING contains a single word made entirely from the
C     characters '0' through '9', then the function returns TRUE.
C     Otherwise, it returns FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     By definition an unsigned integer is a word made exclusively
C     from the characters '0', '1', '2', '3', '4', '5', '6', '7', '8',
C     and '9'.
C
C$ Examples
C
C
C     Four classes of numbers recognized by the various BE functions.
C
C        UNS      unsigned integer
C        INT      integer                (includes INT)
C        DEC      decimal number         (includes UNS, INT)
C        NUM      number                 (includes UNS, INT, NUM)
C
C     The following table illustrates the differences between
C     the classes. (Any number of leading and trailing blanks
C     are acceptable.)
C
C        String                  Accepted by
C        ------------------      ------------------
C        0                       UNS, INT, DEC, NUM
C        21
C        21994217453648
C
C        +0                      INT, DEC, NUM
C        -13
C        +21946
C
C        1.23                    DEC, NUM
C        12.
C        .17
C        +4.1
C        -.25
C
C        2.3e17                  NUM
C        17.D-13275849
C        -.194265E+0004
C
C     Note that the functions don't take the magnitudes of the numbers
C     into account. They may accept numbers that cannot be represented
C     in Fortran variables. (For example, '2.19E999999999999' probably
C     exceeds the maximum floating point number on any machine, but
C     is perfectly acceptable to BENUM.)
C
C     The following strings are not accepted by any of the functions.
C
C        String             Reason
C        ---------------    ----------------------------------------
C        3/4                No implied operations (rational numbers)
C        37+14              No explicit operations
C        E12                Must have mantissa
C        217,346.91         No commas
C        3.14 159 264       No embedded spaces
C        PI                 No special numbers
C        FIVE               No textual numbers
C        CXIV               No roman numerals
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
C     W.L. Taber     (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 01-DEC-1995 (WLT)
C
C-&
 
C$ Index_Entries
C
C     determine if a string is an unsigned integer
C
C-&
 
C
C     SPICE functions
C
      INTEGER               FRSTNB
 
C
C     Local variables
C
      INTEGER               I
      INTEGER               L
 
      LOGICAL               OK
 
 
 
C
C     Get the length of the string and the position of its
C     first non-blank character.
C
      L     = LEN    ( STRING )
      I     = FRSTNB ( STRING )
 
C
C     If there isn't a non-blank character, this isn't an
C     unsigned integer.
C
      IF ( I .EQ. 0 ) THEN
         BEUNS  = .FALSE.
         RETURN
      END IF
 
C
C     As far as we know right now, everything is ok.  Examine
C     characters until we run out of string or until we
C     hit a non-digit character.
C
      OK = .TRUE.
 
      DO WHILE ( OK .AND. ( I .LE. L ) )
 
         IF ( INDEX('0123456789', STRING(I:I) ) .GT. 0  ) THEN
            I = I + 1
         ELSE
            OK = .FALSE.
         END IF
 
      END DO
 
C
C     If the string still is ok as an unsigned integer, it must be
C     one...
C
      IF ( OK ) THEN
         BEUNS  = .TRUE.
      ELSE
 
C
C     ... otherwise, it's an unsigned integer if the remainder is blank.
C
         BEUNS  = STRING(I:) .EQ. ' '
      END IF
 
      RETURN
      END
