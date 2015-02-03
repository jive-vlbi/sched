C$Procedure             LASTNB ( Last non-blank character )
 
      INTEGER FUNCTION  LASTNB ( STRING )
 
C$ Abstract
C
C      Return the index of the last non-blank character in
C      a character string.
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
C      ASCII,  CHARACTER,  SEARCH
C
C$ Declarations
 
      CHARACTER*(*)    STRING
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      STRING     I   Input character string.
C      LASTNB     O   Index of the last non-blank character in STRING.
C
C$ Detailed_Input
C
C      STRING      is the input character string.
C
C$ Detailed_Output
C
C      LASTNB      is the index of the last non-blank character
C                  in the input string. If there are no non-blank
C                  characters in the string, LASTNB is zero.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      If the string is blank, return zero. Otherwise, step through
C      the string one character at a time until something other than
C      a blank is found. Return the index of that something within
C      the string.
C
C$ Examples
C
C      The following examples illustrate the use of LASTNB.
C
C            LASTNB ( 'ABCDE'              )   = 5
C            LASTNB ( 'AN EXAMPLE'         )   = 10
C            LASTNB ( 'AN EXAMPLE        ' )   = 10
C            LASTNB ( '                  ' )   = 0
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
C      K.R. Gehringer  (JPL)
C      I.M. Underwood  (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
C
C         Modified the comparison to use integer values and the ICHAR()
C         function. This improves the performance of the subroutine.
C
C-     SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.1,  7-DEC-1990 (IMU)
C
C         Corrected a misprint in the description of LASTNB.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     last non-blank character
C
C-&

C
C     Local parameters
C
      INTEGER               ISPACE
      PARAMETER           ( ISPACE = 32 )

C
C     Local variables
C
      INTEGER          I

C
C     Just like it says in the header.
C
      IF ( STRING .EQ. ' ' ) THEN
         LASTNB = 0
 
      ELSE
         DO I = LEN (STRING), 1, -1
 
            IF ( ICHAR(STRING(I:I)) .NE. ISPACE ) THEN
               LASTNB = I
               RETURN
            END IF
 
         END DO
      END IF
 
      RETURN
      END
