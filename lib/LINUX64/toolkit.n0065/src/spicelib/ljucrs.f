C$Procedure      LJUCRS ( Left-justify, Uppercase, Compress )
 
      SUBROUTINE LJUCRS ( N, INPUT, OUTPUT )
 
C$ Abstract
C
C     Left-justify, uppercase, and space-compress a character string.
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
C     ASCII
C     CHARACTER
C     STRING
C
C$ Declarations
 
      INTEGER             N
      CHARACTER*(*)       INPUT
      CHARACTER*(*)       OUTPUT
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     N          I      Maximum consecutive occurrences of space.
C     INPUT      I      Input string.
C     OUTPUT     O      Output string.
C
C$ Detailed_Input
C
C      N           is the maximum number of consecutive occurrences
C                  of space that will be allowed to remain in the
C                  output string.
C
C      INPUT       is the input string.
C
C$ Detailed_Output
C
C      OUTPUT      is the output string. This is the input string that
C                  left-justified and with all occurrences of more than
C                  N consecutive spaces removed.
C
C                  If OUTPUT is not large enough to hold the
C                  compressed string, it is truncated on the right.
C
C                  OUTPUT may overwrite INPUT.
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
C     The string is left-justified and uppercased. Occurrences of more
C     than N consecutive spaces are removed from the input string as it
C     is copied to the output string. If the output string is not large
C     enough to hold the compressed string, it is truncated on the
C     right.
C
C$ Examples
C
C     Let N = 1. Then
C
C         ' Abc  DE F  ',           becomes    'ABC DE F',
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
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 29-JUL-2013 (BVS)
C
C-&
 
C$ Index_Entries
C
C     compress uppercase left-justify a character_string
C
C-&
 
C
C
C     Local Variables
C
      INTEGER               I
      INTEGER               J
      INTEGER               COUNT
 
      INTEGER               INLEN
      INTEGER               OUTLEN

      INTEGER               ICH

C
C     Saved variables
C
      INTEGER               SHIFT
      INTEGER               LOWA
      INTEGER               LOWZ

      LOGICAL               FIRST

      SAVE                  FIRST
      SAVE                  SHIFT
      SAVE                  LOWA
      SAVE                  LOWZ

C
C     Initial Data
C
      DATA                  FIRST / .TRUE. /

C
C     Do some set up stuff the first time through so that we do not
C     need to reinitialize the boundary values used for comparisons
C     and the shift on each call.
C
      IF ( FIRST ) THEN
         FIRST = .FALSE.
         LOWA  = ICHAR ('a')
         LOWZ  = ICHAR ('z')
         SHIFT = ICHAR ('A') - LOWA
      END IF

C
C     Find out how much space there is in the INPUT and OUTPUT strings
C     and initialize the space counter and output place holder.
C
      INLEN  = LEN(INPUT)
      OUTLEN = LEN(OUTPUT)
 
      COUNT  = 0
      J      = 0
 
      DO I = 1, INLEN

C
C        Skip leading spaces.
C
         IF ( J .EQ. 0 .AND. INPUT(I:I) .EQ. ' ' ) THEN

C
C           Another leading space. Skip it.
C

         ELSE
 
C
C           Check this character to see if it is a space or not.
C
            IF ( INPUT(I:I) .EQ. ' ' ) THEN
 
               COUNT = COUNT + 1
 
C
C              Copy spaces until enough consecutive spaces
C              have been accumulated. When enough consecutive spaces
C              have accumulated, we no longer copy them.
C
               IF ( COUNT .LE. N ) THEN
                  J           = J + 1
                  OUTPUT(J:J) = INPUT(I:I)
               END IF
 
            ELSE
 
C
C              We don't have a space here. Set the space counter to
C              zero.
C
               COUNT       = 0

C
C              Copy this character while swapping lowercase with upper
C              case along the way.
C
               J           = J + 1

               ICH = ICHAR( INPUT(I:I) )

               IF ( ( ICH .GE. LOWA ) .AND. ( ICH .LE. LOWZ ) ) THEN
                  OUTPUT(J:J) = CHAR ( ICH + SHIFT )
               ELSE
                  OUTPUT(J:J) = INPUT(I:I)
               END IF
 
            END IF
 
            IF ( J .EQ. OUTLEN ) THEN
               RETURN
            END IF

         END IF
 
      END DO
 
C
C     Pad any left over space in the output string with blanks. Note 
C     that if the input string was blank, J will be zero at this
C     point and the case below will set the whole output string to 
C     blank.
C
      IF ( J .LT. OUTLEN ) THEN
         OUTPUT(J+1:) = ' '
      END IF
 
      RETURN
      END
 
 
