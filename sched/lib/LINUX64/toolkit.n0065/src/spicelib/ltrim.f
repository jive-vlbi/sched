C$Procedure   LTRIM ( Left trim )
 
      INTEGER FUNCTION LTRIM ( STRING )
 
C$ Abstract
C
C     Return the maximum of 1 and the location of the first non-blank
C     character in the string.
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
C     CHARACTER
C     STRING
C
C$ Declarations
 
      CHARACTER*(*)         STRING
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C
C     STRING     I   String to be trimmed.
C
C     The function returns the maximum of 1 and the location of the
C     first non-blank character in STRING.
C
C$ Detailed_Input
C
C     STRING         is a string to be trimmed:  the location of the
C                    first non-blank character is desired.
C
C$ Detailed_Output
C
C     The function returns the maximum of 1 and the location of the
C     first non-blank character in STRING.
C
C     In particular, when STRING is blank, the function returns the
C     value 1.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     When writing a character string to a file, we may wish to omit
C     leading blanks.  We'd like to use FRSTNB as a lower substring
C     bound, but we have to handle the case where FRSTNB returns 0,
C     so we write:
C
C
C        WRITE ( UNIT, '(A)' ),  STRING ( MAX (1, FRSTNB (STRING)) : )
C
C
C     This can be simplified using LTRIM:
C
C
C        WRITE ( UNIT, '(A)' ),  STRING ( LTRIM (STRING) : )
C
C
C     This routine has a counterpart, RTRIM, which finds the maximum of
C     1 and the position of the last non-blank character of a string.
C
C$ Examples
C
C     1)  Write the non-blank portion of each element of a character
C         cell to file SPUD.DAT:
C
C            DO I = 1,  CARDC (CELL)
C
C               CALL WRLINE ( 'SPUD.DAT',
C           .                  CELL(I) ( LTRIM (CELL) : RTRIM (CELL) ) )
C
C            END DO
C
C         When CELL(I) is blank, the string ' ' will be written.
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 02-MAY-1990  (NJB)
C
C-&
 
C$ Index_Entries
C
C     left trim
C
C-&
 
 
 
C
C     SPICELIB functions
C
      INTEGER               FRSTNB
 
C
C     `Just do it'.
C
      LTRIM = MAX ( 1, FRSTNB (STRING) )
 
      END
