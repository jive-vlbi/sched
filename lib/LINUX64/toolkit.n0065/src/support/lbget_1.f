 
C$Procedure LBGET ( Line buffer, get )
 
      SUBROUTINE LBGET_1 ( POS, PTRS, BUFFER, LINE, FOUND )
      IMPLICIT NONE
 
C$ Abstract
C
C     Get (return) the line at a particular position within a
C     line buffer.
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
C     CB, LB
C
C$ Keywords
C
C     ASCII
C     CHARACTER
C     STRING
C     TEXT
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               LBCBUF
      PARAMETER           ( LBCBUF =  0 )
 
      INTEGER               POS
      INTEGER               PTRS        ( LBCELL:* )
      CHARACTER*(*)         BUFFER      ( LBCBUF:* )
      CHARACTER*(*)         LINE
      LOGICAL               FOUND
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     POS        I   Position of line.
C     PTRS,
C     BUFFER     I   Line buffer.
C     LINE       O   Line.
C     FOUND      O   True if the line was found.
C
C$ Detailed_Input
C
C     POS         is the position of an existing line within a line
C                 buffer.
C
C     PTRS,
C     BUFFER      are the pointer and character components of a line
C                 buffer.
C
C$ Detailed_Output
C
C     LINE        is a copy of the specified line. If LINE is shorter
C                 than the stored line, it is truncated. If longer, it
C                 is padded with spaces.
C
C     FOUND       is true whenever the specified line exists, and is
C                 false otherwise.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If FOUND is false, LINE is not changed.
C
C$ Particulars
C
C     LBGET is the only way to retrieve lines from a line buffer.
C
C$ Examples
C
C     Let the line buffer (P,B) contain the following lines.
C
C       If you can keep your head when all about you
C       Are losing theirs and blaming it on you;
C       If you can trust yourself when all men doubt you,
C       But make allowance for their doubting too:
C       If you can wait and not be tired by waiting,
C       Or, being lied about, don't deal in lies,
C       Or being hated don't give way to hating,
C       And yet don't look too good, nor talk too wise;
C
C     The code fragment
C
C       N = 1
C       CALL LBGET ( N, P, B, LINE, FOUND )
C
C       DO WHILE ( FOUND )
C          WRITE (*,*) '(', N, ') ', LINE
C
C          N = N + 1
C          CALL LBGET ( N, P, B, LINE, FOUND )
C       END DO
C
C     produces the following output:
C
C       (  1) If you can keep your head when all about you
C       (  2) Are losing theirs and blaming it on you;
C       (  3) If you can trust yourself when all men doubt you,
C       (  4) But make allowance for their doubting too:
C       (  5) If you can wait and not be tired by waiting,
C       (  6) Or, being lied about, don't deal in lies,
C       (  7) Or being hated don't give way to hating,
C       (  8) And yet don't look too good, nor talk too wise;
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
C     Dagny Taggart, (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 19-JAN-1989 (DT)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               MAXLN
      INTEGER               NLINE
      INTEGER               NCOM
      INTEGER               PCARD
      INTEGER               POSPTR
 
C
C     Standard error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LBGET_1' )
      END IF
 
C
C     Recover all the essential control information.
C
      CALL LBDES_1 ( PTRS, MAXLN, NLINE, NCOM, PCARD )
 
C
C     What are the endpoints of the stored line? Once we have
C     them, we can return the line directly.
C
      FOUND = ( POS .GE. 1  .AND.  POS .LE. NLINE )
 
      IF ( FOUND ) THEN
         POSPTR = 2 * POS - 1
 
         CALL CBGET_1 ( PTRS(POSPTR), PTRS(POSPTR+1), BUFFER, LINE )
      END IF
 
      CALL CHKOUT ( 'LBGET_1' )
      RETURN
      END
