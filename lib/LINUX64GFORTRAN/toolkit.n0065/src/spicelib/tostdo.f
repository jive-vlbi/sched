C$Procedure      TOSTDO ( To Standard Output)
 
      SUBROUTINE TOSTDO ( LINE )
 
C$ Abstract
C
C    Write a line of text to standard output.
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
C     UTILITY
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         LINE
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     LINE       I   is a line of text to be written to standard output
C
C$ Detailed_Input
C
C     LINE       is a character string containing text to be written
C                to standard output.
C
C$ Detailed_Output
C
C     None.
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
C     None.
C
C$ Particulars
C
C     This routine is a macro for the subroutine call
C
C        CALL WRITLN ( LINE, STDOUT )
C
C     Where STDOUT is the logical unit connected to standard output.
C
C$ Examples
C
C     Suppose you need to create a message to be printed on the
C     user's terminal.  Here is how to use TOSTDO to handle this
C     task.
C
C        CALL TOSTDO ( 'Hello. '         )
C        CALL TOSTDO ( 'My Name is HAL.' )
C        CALL TOSTDO ( 'I became operational January 12, 1997 on the ' )
C        CALL TOSTDO ( 'campus of the University of Illinois in '      )
C        CALL TOSTDO ( 'Urbana, Illinois.' )
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 18-SEP-1996 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Write a line of text to standard output.
C
C-&
 
      INTEGER               STDOUT
      LOGICAL               FIRST
      SAVE
 
      DATA                  FIRST  / .TRUE. /
 
 
      IF ( FIRST ) THEN
         CALL STDIO ( 'STDOUT', STDOUT )
         FIRST = .FALSE.
      END IF
 
      CALL WRITLN ( LINE, STDOUT )
      RETURN
 
      END
