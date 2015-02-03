 
C$Procedure LBPACK ( Line buffer, pack )
 
      SUBROUTINE LBPACK_1 ( PTRS, BUFFER )
      IMPLICIT NONE
 
C$ Abstract
C
C     Pack the contents of a line buffer.
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
 
      INTEGER               PTRS        ( LBCELL:* )
      CHARACTER*(*)         BUFFER      ( LBCBUF:* )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     PTRS,
C     BUFFER    I,O  Line buffer.
C
C$ Detailed_Input
C
C     PTRS,
C     BUFFER      are the pointer and character components of a line
C                 buffer.
C
C$ Detailed_Output
C
C     PTRS,
C     BUFFER      are the pointer and character components of the
C                 same line buffer after packing.
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
C     As lines are added to and removed from a line buffer, the
C     buffer becomes fragmented, with free space spread throughout.
C     Occasionally, the LB routines will pull all the current lines
C     toward the front of the buffer, accumulating all the free
C     space in one contiguous chunk.
C
C     LBPACK is provided mainly for internal use by the LB routines,
C     but you may pack a line buffer any time you want. Packing a
C     buffer will typically speed up operations that change the contents
C     of a buffer, but will have no effect on retrieval operations.
C
C$ Examples
C
C     LBPACK is used by LBINS.
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
      INTEGER               SIZECB_1
 
C
C     Local variables
C
      INTEGER               MAXLN
      INTEGER               NLINE
      INTEGER               NCOM
      INTEGER               PCARD
 
      INTEGER               OFFSET
      INTEGER               BEGIN
      INTEGER               END
      INTEGER               INTLEN
      INTEGER               I
      INTEGER               J
 
 
 
C
C     Standard error handling.
C
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'LBPACK_1' )
      END IF
 
C
C     Recover the essential control information.
C
      CALL LBDES_1 ( PTRS, MAXLN, NLINE, NCOM, PCARD )
 
C
C     For each interval in the complement...
C
      OFFSET = NLINE * 2
 
      DO I = OFFSET+1, PCARD, 2
 
C
C        Remove the contents of the interval from the CB, pulling
C        the remaining contents forward.
C
         BEGIN  = PTRS(I  )
         END    = PTRS(I+1)
         INTLEN = END - BEGIN + 1
 
         IF ( BEGIN .LE. END ) THEN
            CALL CBREM_1 ( BEGIN, END, BUFFER )
 
C
C           Adjust the pointers for both the lines and the complement
C           intervals that followed the purged interval.
C
            DO J = 1, PCARD
               IF ( PTRS(J) .GT. END ) THEN
                  PTRS(J) = PTRS(J) - INTLEN
               END IF
            END DO
         END IF
 
      END DO
 
C
C     There is only one interval in the complement now. It begins
C     just after the last line, and runs to the end of the buffer.
C
      CALL MAXAI ( PTRS(1), OFFSET, END, J )
 
      PTRS(OFFSET+1) = END + 1
      PTRS(OFFSET+2) = SIZECB_1 ( BUFFER )
 
      CALL LBUPD_1 ( NLINE, 1, PTRS )
 
      CALL CHKOUT ( 'LBPACK_1' )
      RETURN
      END
