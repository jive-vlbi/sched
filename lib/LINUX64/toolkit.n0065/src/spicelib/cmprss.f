C$Procedure      CMPRSS ( Compress a character string )
 
      SUBROUTINE CMPRSS ( DELIM, N, INPUT, OUTPUT )
 
C$ Abstract
C
C      Compress a character string by removing occurrences of
C      more than N consecutive occurrences of a specified
C      character.
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
C      ASCII,  CHARACTER,  STRING
C
C$ Declarations
 
      CHARACTER*1         DELIM
      INTEGER             N
      CHARACTER*(*)       INPUT
      CHARACTER*(*)       OUTPUT
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      DELIM      I      Delimiter to be compressed.
C      N          I      Maximum consecutive occurrences of DELIM.
C      INPUT      I      Input string.
C      OUTPUT     O      Compressed string.
C
C$ Detailed_Input
C
C      DELIM       is the delimiter to be compressed out of the string.
C                  This may be any ASCII character.
C
C      N           is the maximum number of consecutive occurrences
C                  of DELIM that will be allowed to remain in the
C                  output string.
C
C      INPUT       is the input string.
C
C$ Detailed_Output
C
C      OUTPUT      is the output string. This is the input string
C                  with all occurrences of more than N consecutive
C                  delimiters removed.
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
C$ Particulars
C
C      Occurrences of more than N consecutive delimiters are removed
C      from the input string as it is copied to the output string.
C      If the output string is not large enough to hold the compressed
C      string, it is truncated on the right.
C
C$ Examples
C
C      Let DELIM = '.', and N = 2. Then
C
C         'ABC...DE.F...',           becomes   'ABC..DE.F..'
C         ' ...........'                       ' ..'
C         '.. ..AB....CD'                      '.. ..AB..CD'
C
C      Let DELIM = ' ', and N = 0. Then
C
C         ' DISK:[USER.  SUB  ]'     becomes   'DISK:[USER.SUB]'
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     I.M. Underwood  (JPL)
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
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     compress a character_string
C
C-&
 
 
 
C
C
C     Local Variables
C
      INTEGER          I
      INTEGER          J
      INTEGER          COUNT
 
      INTEGER          INLEN
      INTEGER          OUTLEN
 
 
C
C     Find out how much space there is in the INPUT and OUTPUT strings
C     and initialize the delimiter counter and output place holder.
C
      INLEN  = LEN(INPUT)
      OUTLEN = LEN(OUTPUT)
 
      COUNT  = 0
      J      = 0
 
      DO I = 1, INLEN
 
C
C        Check each character to see if it is a delimiter or not.
C
         IF ( INPUT(I:I) .EQ. DELIM ) THEN
 
            COUNT = COUNT + 1
 
C
C           Copy delimiters until enough consecutive delimiters
C           have been accumulated.  When enough consecutive delimiters
C           have accumulated, we no longer copy them.
C
            IF ( COUNT .LE. N ) THEN
               J           = J + 1
               OUTPUT(J:J) = INPUT(I:I)
            END IF
 
         ELSE
 
C
C           We don't have a delimiter here so we just copy the
C           character and set the delimiter counter to zero.
C
            COUNT       = 0
            J           = J + 1
            OUTPUT(J:J) = INPUT(I:I)
 
         END IF
 
         IF ( J .EQ. OUTLEN ) THEN
            RETURN
         END IF
 
      END DO
 
C
C     Pad any left over space in the output string with blanks.
C
      IF ( J .LT. OUTLEN ) THEN
         OUTPUT(J+1:) = ' '
      END IF
 
      RETURN
      END
 
 
