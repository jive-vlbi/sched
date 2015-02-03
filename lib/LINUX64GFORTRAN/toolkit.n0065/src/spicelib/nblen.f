C$Procedure             NBLEN ( Non blank length of a string )
 
      INTEGER FUNCTION  NBLEN ( STRING )
 
C$ Abstract
C
C     Return the non-blank length of a character string. (That is,
C     the index of the last non-blank character when the string is
C     left-justified.)
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
C      ASCII,  CHARACTER,  UTILITY
C
C$ Declarations
 
      CHARACTER*(*)    STRING
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      STRING     I   Input character string.
C      NBLEN      O   Non-blank length of STRING.
C
C$ Detailed_Input
C
C      STRING      is the input character string.
C
C$ Detailed_Output
C
C      NBLEN       is the non-blank length of STRING. This is the same
C                  as the index of the last non-blank character in the
C                  left justified string. If STRING is blank, NBLEN is
C                  zero.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      Locate the first and last non-blank characters in the string.
C      Subtract to get the non-blank length.
C
C$ Examples
C
C      The following examples illustrate the use of NBLEN.
C
C            NBLEN ( 'ABCDE' )             = 5
C            NBLEN ( 'AN EXAMPLE' )        = 10
C            NBLEN ( '   AN EXAMPLE  ' )   = 10
C            NBLEN ( '               ' )   = 0
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
C     non-blank length of a string
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER          FRSTNB
      INTEGER          LASTNB
 
 
 
 
C
C     Blank string is easy.
C
      IF ( STRING .EQ. ' ' ) THEN
         NBLEN = 0
 
      ELSE
         NBLEN = LASTNB ( STRING ) - FRSTNB ( STRING ) + 1
      END IF
 
      RETURN
      END
