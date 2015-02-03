C$Procedure            FRSTPC ( First printable character )
 
      INTEGER FUNCTION FRSTPC ( STRING )
 
C$ Abstract
C
C     Return the index of the first printable character in a character
C     string. ASCII characters 33-126 are printable. (Blanks are not
C     considered printable.)
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
C      ASCII, CHARACTER, SEARCH
C
C$ Declarations
 
      CHARACTER*(*)    STRING
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      STRING     I   Input character string.
C      FRSTPC     O   Index of the first printable character in STRING.
C
C$ Detailed_Input
C
C      STRING      is the input character string.
C
C$ Detailed_Output
C
C      FRSTPC      is the index of the first printable character
C                  in the input string. Characters 33-126 are
C                  considered to be printable characters. Blanks
C                  are not considered printable characters. If
C                  the input string contains no printable characters,
C                  FRSTPC is zero.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      This works exactly like FRSTNB, except that it skips
C      non-printable characters (ASCII control characters) as
C      well as blanks.
C
C$ Examples
C
C      The program
C
C         INTEGER         FRSTNB
C         INTEGER         FRSTPC
C         INTEGER         LASTNB
C         INTEGER         LASTPC
C
C         CHARACTER*10    S
C
C         S( 1: 1) = ' '
C         S( 2: 2) = CHAR (  2 )
C         S( 3: 3) = CHAR (  3 )
C         S( 4: 4) = 'A'
C         S( 5: 5) = 'B'
C         S( 6: 6) = 'C'
C         S( 7: 7) = CHAR (  7 )
C         S( 8: 8) = CHAR (  8 )
C         S( 9: 9) = CHAR (  9 )
C         S(10:10) = ' '
C
C         WRITE (*,*) 'Non-blank from ', FRSTNB(S), ' to ', LASTNB(S)
C         WRITE (*,*) 'Printable from ', FRSTPC(S), ' to ', LASTPC(S)
C
C         END
C
C      produces te following output:
C
C         Non-blank from 2 to 9.
C         Printable from 4 to 6.
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C     Error free.
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
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     first printable character
C
C-&
 
 
 
 
C$ Revisions
C
C-    Beta Version 1.0.1, 27-JAN-1989 (IMU)
C
C        Examples section completed.
C
C-&
 
 
 
C
C     Local variables
C
      INTEGER          I
 
 
 
C
C     Look for the first character in the range [33,126], and return
C     its index.
C
      DO I = 1, LEN ( STRING )
 
         IF (       (ICHAR ( STRING(I:I) ) .GE.  33)
     .        .AND. (ICHAR ( STRING(I:I) ) .LE. 126) ) THEN
 
            FRSTPC = I
            RETURN
         END IF
 
      END DO
 
C
C     Still here? No printable characters. Return zero.
C
      FRSTPC = 0
 
      RETURN
      END
 
