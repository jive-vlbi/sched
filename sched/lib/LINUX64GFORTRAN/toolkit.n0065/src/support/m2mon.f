C$Procedure      M2MON ( Determine whether or not a word is a month )
 
      LOGICAL FUNCTION M2MON ( WORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     This function is true if the input string is a month in the
C     sense of META/2.
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
C     META/2 a language specification language.
C
C$ Keywords
C
C     ALPHANUMERIC
C     ASCII
C     PARSING
C     UTILITY
C     WORD
C
C$ Declarations
 
      CHARACTER*(*)         WORD
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     WORD       I   A character string word
C
C     The function is returned as .TRUE. if word is an META/2 month.
C
C$ Detailed_Input
C
C     WORD      is a character string that is assumed to have no
C               spaces between the first and last non-blank characters.
C
C$ Detailed_Output
C
C     M2MON    returns as .TRUE. if WORD is less than 32 characters
C               in length, starts with an alphabetic character and
C               contains only letters, digits, underscores and hyphens.
C               Otherwise it is returned .FALSE.
C
C$ Error_Handling
C
C     None.
CC
C$ Input_Files
C
C     None.
C
C$ Output_Files
C
C     None.
C
C$ Particulars
C
C     This is a utility routine for the subroutine META2.  It
C     determines whether or not a word is a month in the sense
C     of the language META/2.
C
C$ Examples
C
C     WORD                                  M2MON
C     -------                               ------
C     SPAM                                  .FALSE.
C     JAN                                   .TRUE.
C     FEBR                                  .TRUE.
C     OCTA                                  .FALSE.
C     AUGU                                  .TRUE.
C     JU                                    .FALSE.
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
C     I.M. Underwood (JPL)
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
C     Version B1.0.0, 22-MAR-1988 (WLT) (IMU)
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               BSRCHC
      INTEGER               QRTRIM
      INTEGER               LTRIM
 
C
C     Local variables
C
      INTEGER               I
      INTEGER               LENGTH
      INTEGER               START
      INTEGER               END
 
      CHARACTER*3           SHORT  ( 12 )
      CHARACTER*9           MONTHS ( 12 )
      CHARACTER*9           COPY
 
 
 
      INTEGER               MONTH
 
      SAVE
 
      DATA                  SHORT  / 'APR', 'AUG', 'DEC', 'FEB',
     .                               'JAN', 'JUL', 'JUN', 'MAR',
     .                               'MAY', 'NOV', 'OCT', 'SEP'  /
 
      DATA                  MONTHS / 'APRIL',    'AUGUST',  'DECEMBER',
     .                               'FEBRUARY', 'JANUARY', 'JULY',
     .                               'JUNE',     'MARCH',   'MAY',
     .                               'NOVEMBER', 'OCTOBER', 'SEPTEMBER'/
 
 
 
C
C     Make sure the string has the right length.
C
      START  = LTRIM   ( WORD )
      END    = QRTRIM  ( WORD )
      LENGTH = END - START + 1
 
      IF ( LENGTH .LT. 3 ) THEN
         M2MON = .FALSE.
         RETURN
      END IF
 
      IF ( LENGTH .GT. 9 ) THEN
         M2MON = .FALSE.
         RETURN
      END IF
 
      CALL UCASE ( WORD, COPY )
 
C
C     See if the first three letters match anything we've got so far.
C
      MONTH = BSRCHC ( COPY(START:START+2), 12, SHORT )
 
      IF ( MONTH .EQ. 0 ) THEN
         M2MON = .FALSE.
         RETURN
      END IF
 
C
C     Now make sure that any remaining letters match up exactly.
C
      I     = START + 3
      M2MON = .TRUE.
 
      DO WHILE ( ( I .LE. END ) .AND. M2MON )
 
         M2MON = COPY(I:I) .EQ. MONTHS(MONTH)(I:I)
         I     = I + 1
 
      END DO
 
      RETURN
      END
