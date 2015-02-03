 
C$Procedure LBREM ( Line buffer, remove )
 
      SUBROUTINE LBREM_1 ( POS, PTRS, BUFFER )
      IMPLICIT NONE
 
C$ Abstract
C
C     Remove a line from a line buffer.
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
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     POS        I   Position of line to be removed.
C     PTRS,
C     BUFFER    I,O  Line buffer.
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
C     PTRS,
C     BUFFER      are the pointer and character components of the
C                 same line buffer, after the specified line has been
C                 removed.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If POS is less than one, or if POS is greater than the
C        number of lines currently stored in the buffer, the error
C        'SPICE(LBNOSUCHLINE)' is signalled.
C
C$ Particulars
C
C     Existing lines may be removed from at any position within a line
C     buffer. All subsequent lines are moved forward to take up the
C     slack.
C
C$ Examples
C
C     Let the line buffer (P,B) contain the following lines
C
C       If you can make one heap of all your winnings
C       And risk it on one turn of pitch-and-toss,
C       And lose, and start again at your beginnings,
C       And never breathe a word about your loss:
C       If you can force your heart and nerve and sinew
C       To serve your turn long after they are gone,
C
C     Following the calls
C
C       CALL LBREM ( 3, P, B )
C       CALL LBREP ( 3, P, B )
C
C     it contains the lines
C
C       If you can make one heap of all your winnings
C       And risk it on one turn of pitch-and-toss,
C       If you can force your heart and nerve and sinew
C       To serve your turn long after they are gone,
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
C-    Inspekt Version 3.0.0 9-May-1994 (WLT)
C
C        Added a "TOUCHC" to the input buffer so that compilers
C        won't complain about input arguments not being used.
C
C        And fixed the addition of "TOUCHC" to refere to LBCBUF
C        instead of LBCELL
C
C-    Beta Version 1.0.0, 19-JAN-1989 (DT)
C
C-&
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
C
C     Other Functions
C
      CHARACTER*(1)         TOUCHC
 
C
C     Local variables
C
      INTEGER               MAXLN
      INTEGER               NLINE
      INTEGER               NCOM
      INTEGER               PCARD
 
      INTEGER               POSPTR
      INTEGER               POSCOM
      INTEGER               BEGIN
      INTEGER               END
      INTEGER               PTR            ( 2 )
      INTEGER               OFFSET
      INTEGER               I
 
C
C     Equivalences
C
      EQUIVALENCE         ( BEGIN, PTR(1) )
      EQUIVALENCE         ( END,   PTR(2) )
 
 
C
C     Standard error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LBREM_1' )
      END IF
C
C     We touch the input buffer so that compilers will not complain
C     that BUFFER is an unused argument.  It really is unused, but
C     it's in the calling sequence for the sake of uniformity of
C     the calling sequences for the line buffer routines.
C
      BUFFER(LBCBUF)(1:1) = TOUCHC ( BUFFER(LBCBUF) )
 
 
C
C     Recover the essential control information.
C
      CALL LBDES_1 ( PTRS, MAXLN, NLINE, NCOM, PCARD )
 
C
C     No way to remove a line that's not in the table.
C
      IF ( POS .LT. 1  .OR.  POS .GT. NLINE ) THEN
         CALL SETMSG ( 'Tried to access line # of #.' )
         CALL ERRINT ( '#', POS                       )
         CALL ERRINT ( '#', NLINE                     )
         CALL SIGERR ( 'SPICE(LBNOSUCHLINE)'          )
         CALL CHKOUT ( 'LBREM_1'                        )
         RETURN
      END IF
 
C
C     Save the bounds of the stored line before removing the name
C     and pointers from their respective tables.
C
      POSPTR = 2 * POS - 1
      BEGIN  = PTRS(POSPTR  )
      END    = PTRS(POSPTR+1)
      NLINE  = NLINE - 1
 
      CALL REMLAI ( 2, POSPTR, PTRS(1), PCARD )
 
C
C     Add the interval to the complement. Insert it directly, then
C     do any merges required.
C
      OFFSET = NLINE  * 2
      POSCOM = OFFSET + 1
 
      DO I = OFFSET+2, PCARD, 2
         IF ( BEGIN .GT. PTRS(I) ) THEN
            POSCOM = I + 1
         END IF
      END DO
 
      CALL INSLAI ( PTR, 2, POSCOM, PTRS(1), PCARD )
 
      DO I = PCARD-2, OFFSET+2, -2
         IF ( PTRS(I+1) .EQ. PTRS(I) + 1 ) THEN
            CALL REMLAI ( 2, I, PTRS(1), PCARD )
         END IF
      END DO
 
      NCOM = ( PCARD / 2 ) - NLINE
 
      CALL LBUPD_1 ( NLINE, NCOM, PTRS )
 
      CALL CHKOUT ( 'LBREM_1' )
      RETURN
      END
