C$Procedure      M2YEAR ( Determine whether or not a word is a year )
 
      LOGICAL FUNCTION M2YEAR ( WORD )
      IMPLICIT NONE
 
C$ Abstract
C
C     This function is true if the input string is a year in the
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
C     The function is returned as .TRUE. if word is a META/2 year.
C
C$ Detailed_Input
C
C     WORD      is a character string that is assumed to have no
C               spaces between the first and last non-blank characters.
C
C$ Detailed_Output
C
C     M2YEAR     returns as .TRUE. if WORD is a META/2 year.
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
C     determines whether or not a word is a year in the sense
C     of the language META/2.
C
C$ Examples
C
C     WORD                                  M2YEAR
C     -------                               ------
C     SPAM                                  .FALSE.
C     1                                     .TRUE.
C     0.289E19                              .FALSE.
C     0.2728D12                             .FALSE.
C     -12.1892e-5                           .FALSE.
C     12.E29                                .FALSE.
C     12.E291                               .FALSE.
C     1.2E10                                .TRUE.
C     .E12                                  .FALSE.
C     1.2E.12                               .FALSE.
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
C-     META/2 Configured Version 2.1.0, 29-DEC-1994 (WLT)
C
C         The computation of the length of the input string
C         was incorrect.  It has been fixed.  It used to be
C
C            LENGTH = I3 - I1 + 1
C
C         Now it is
C
C            LENGTH = I4 - I1 + 1
C
C
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
      INTEGER               QRTRIM
      INTEGER               LTRIM
C
C     Local variables
C
      INTEGER               LENGTH
      INTEGER               I
      INTEGER               I1
      INTEGER               I2
      INTEGER               I3
      INTEGER               I4
      INTEGER               VALUE
      INTEGER               VALUES ( 0:255 )
 
      LOGICAL               FIRST
      SAVE
 
      DATA                  FIRST / .TRUE. /
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
C
C        We will construct a value for the string by taking
C        the non-blank portion and computing the value assuming
C        that the first non-blank is a digit with the appropriate
C        power of 10 attached.  Since all non-digit characters
C        will have values of 1000, we will get a value greater
C        than 1000 if any non-digit characters are present.
C
         DO  I =  0,255
            VALUES(I) = 10000
         END DO
 
         VALUES( ICHAR('0') ) = 0
         VALUES( ICHAR('1') ) = 1
         VALUES( ICHAR('2') ) = 2
         VALUES( ICHAR('3') ) = 3
         VALUES( ICHAR('4') ) = 4
         VALUES( ICHAR('5') ) = 5
         VALUES( ICHAR('6') ) = 6
         VALUES( ICHAR('7') ) = 7
         VALUES( ICHAR('8') ) = 8
         VALUES( ICHAR('9') ) = 9
 
      END IF
 
 
C
C     Make sure the string has the right length.
C
      I1     = LTRIM  ( WORD )
      I4     = QRTRIM ( WORD )
      LENGTH = I4 - I1 + 1
 
 
C
C     Rule out the goofy cases that NPARSD will allow.
C
      IF      ( LENGTH .NE. 4 ) THEN
 
         VALUE = 10000
 
      ELSE
 
         I2    =  I1 + 1
         I3    =  I2 + 1
         VALUE =  1000 * VALUES( ICHAR( WORD(I1:I1) ) )
     .         +   100 * VALUES( ICHAR( WORD(I2:I2) ) )
     .         +    10 * VALUES( ICHAR( WORD(I3:I3) ) )
     .         +         VALUES( ICHAR( WORD(I4:I4) ) )
 
 
      END IF
C
C     That's all just make sure that the value is within the
C     bound required of a year.
C
      M2YEAR = (   VALUE .GE. 1000 ) .AND. ( VALUE .LE. 3000 )
 
      RETURN
      END
