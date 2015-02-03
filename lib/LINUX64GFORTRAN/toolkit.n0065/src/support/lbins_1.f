 
C$Procedure LBINS ( Line buffer, insert )
 
      SUBROUTINE LBINS_1 ( POS, LINE, PTRS, BUFFER )
      IMPLICIT NONE
 
C$ Abstract
C
C     Insert a line into a line buffer.
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
      CHARACTER*(*)         LINE
      INTEGER               PTRS      ( LBCELL:* )
      CHARACTER*(*)         BUFFER      ( LBCBUF:* )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     POS        I   Position of new line.
C     LINE       I   Line to be inserted.
C     PTRS,
C     BUFFER    I,O  Line buffer.
C
C$ Detailed_Input
C
C     POS         is the position (line number) at which the new line is
C                 to be inserted.
C
C     LINE        is the line to be inserted.
C
C     PTRS,
C     BUFFER      are the pointer and character components of a line
C                 buffer.
C
C$ Detailed_Output
C
C     PTRS,
C     BUFFER      are the pointer and character components of the
C                 same line buffer, after the new line has been
C                 inserted.
C
C$ Files
C
C     None.
C
C$ Exceptions
C
C     1) If POS exceeds by exactly one the number of lines currently
C        stored in the buffer, then LINE is appended to the end of
C        the buffer, and no other lines are affected.
C
C     2) If POS is less than one, or if POS exceeds by more than one
C        the number of lines currently stored in the buffer, the error
C        'SPICE(LBNOSUCHLINE)' is signalled.
C
C     3) If the maximum number of lines is currently stored, the
C        error 'SPICE(LBTOOMANYLINES)' is signalled.
C
C     4) If the line buffer contains insufficient free space to store
C        the new line, the error 'SPICE(LBLINETOOLONG)' is signalled.
C
C$ Particulars
C
C     New lines may be inserted at any position within a line buffer.
C     The line currently at the specified position and all subsequent
C     lines are moved back to make room for the new line.
C
C$ Examples
C
C     Let the line buffer (P,B) contain the following lines
C
C       If neither foes nor loving friends can hurt you,
C       If all men count with you, but none too much:
C
C     Following the calls
C
C       CALL LBINS ( 1,
C      .            'If you can talk with crowds and keep your virtue,',
C      .             P, B )
C
C       CALL LBINS ( 2,
C      .            'Or walk with Kings---nor lose the common touch,',
C      .             P, B )
C
C     it contains the lines
C
C       If you can talk with crowds and keep your virtue,
C       Or walk with Kings---nor lose the common touch,
C       If neither foes nor loving friends can hurt you,
C       If all men count with you, but none too much:
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
      INTEGER               LASTNB
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               MAXLN
      INTEGER               NLINE
      INTEGER               NCOM
      INTEGER               PCARD
 
      INTEGER               POSPTR
      INTEGER               F
      INTEGER               L
      INTEGER               LNLEN
      INTEGER               AVAIL
      INTEGER               BEGIN
      INTEGER               END
      INTEGER               PTR         ( 2 )
 
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
         CALL CHKIN ( 'LBINS_1' )
      END IF
 
C
C     Recover all the essential control information.
C
      CALL LBDES_1 ( PTRS, MAXLN, NLINE, NCOM, PCARD )
 
C
C     Where should this line be inserted, if at all?
C
      IF ( NLINE .EQ. MAXLN ) THEN
         CALL SETMSG ( 'Current line limit is #.' )
         CALL ERRINT ( '#', MAXLN                 )
         CALL SIGERR ( 'SPICE(LBTOOMANYLINES)'    )
 
      ELSE IF ( POS .LT. 1  .OR.  POS - NLINE .GT. 1 ) THEN
         CALL SETMSG ( 'Tried to access line # of #.' )
         CALL ERRINT ( '#', POS                       )
         CALL ERRINT ( '#', NLINE                     )
         CALL SIGERR ( 'SPICE(LBNOSUCHLINE)'          )
 
      ELSE
         POSPTR = 2 * POS - 1
 
C
C        Leading blanks are significant; trailing blanks are history.
C        (Store a blank string as a single blank character.)
C
         F      =       1
         L      = MAX ( 1, LASTNB ( LINE ) )
         LNLEN  = L - F + 1
 
C
C        Store each new string at the end of the end of the CB.
C        If the final interval in the complement isn't large enough
C        to hold the new string, pack the CB and try again.
C
         AVAIL = PTRS(PCARD) - PTRS(PCARD-1) + 1
 
         IF ( AVAIL .LT. LNLEN ) THEN
            CALL LBPACK_1 ( PTRS, BUFFER )
 
            CALL LBDES_1 ( PTRS, MAXLN, NLINE, NCOM, PCARD )
            AVAIL  = PTRS(PCARD) - PTRS(PCARD-1) + 1
         END IF
 
C
C        If there still isn't enough room? Well, those are the breaks.
C
         IF ( AVAIL .LT. LNLEN ) THEN
            CALL SIGERR ( 'SPICE(LBLINETOOLONG)' )
 
C
C        If there is room, allocate just enough of the final interval
C        in the complement to contain the new string; store the string;
C        and insert the name and pointers at their proper locations.
C
         ELSE
            BEGIN         = PTRS(PCARD-1)
            END           = BEGIN + LNLEN - 1
            PTRS(PCARD-1) = END   + 1
 
            CALL CBPUT_1  ( BEGIN, END, LINE(F:L), BUFFER )
 
            CALL INSLAI ( PTR, 2, POSPTR, PTRS(1), PCARD )
 
            CALL LBUPD_1  ( NLINE+1, NCOM, PTRS )
         END IF
 
      END IF
 
      CALL CHKOUT ( 'LBINS_1' )
      RETURN
      END
