C$Procedure ZZWNINSD ( Insert an interval into a DP window )
 
      SUBROUTINE ZZWNINSD ( LEFT, RIGHT, CONTEXT, WINDOW )
 
C$ Abstract
C
C      Insert an interval into a double precision window.
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
C      WINDOWS
C
C$ Keywords
C
C      WINDOWS
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE                'errhnd.inc'
 
      INTEGER                 LBCELL
      PARAMETER             ( LBCELL = -5 )
 
      DOUBLE PRECISION        LEFT
      DOUBLE PRECISION        RIGHT
      CHARACTER*(*)           CONTEXT
      DOUBLE PRECISION        WINDOW    ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      LEFT,
C      RIGHT      I   Left, right endpoints of new interval.
C      CONTEXT    I   A call explanation string.
C      WINDOW    I,O  Input, output window.
C
C$ Detailed_Input
C
C      LEFT,
C      RIGHT       are the left and right endpoints of the interval
C                  to be inserted.
C
C      CONTEXT     a context/explaination string to append to the
C                  long error message if an error signals. The caller
C                  need not include a message. A single blank, ' ',
C                  represents no message.
C
C      WINDOW      on input, is a window containing zero or more
C                  intervals.
C
C$ Detailed_Output
C
C      WINDOW      on output, is the original window following the
C                  insertion of the interval from LEFT to RIGHT.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If LEFT is greater than RIGHT, the error SPICE(BADENDPOINTS) is
C        signalled.
C
C     2) If the insertion of the interval causes an excess of elements,
C        the error SPICE(WINDOWEXCESS) is signalled.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      This routine inserts the interval from LEFT to RIGHT into the
C      input window. If the new interval overlaps any of the intervals
C      in the window, the intervals are merged. Thus, the cardinality
C      of the input window can actually decrease as the result of an
C      insertion. However, because inserting an interval that is
C      disjoint from the other intervals in the window can increase the
C      cardinality of the window, the routine signals an error.
C
C      This is the only unary routine to signal an error. No
C      other unary routine can increase the number of intervals in
C      the input window.
C
C      If a non-blank CONTEXT string passes from the caller, any error
C      signal will return the long error message with the CONTEXT 
C      string appended to that message.
C
C$ Examples
C
C      Let WINDOW contain the intervals
C
C            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
C
C      Then the following series of calls
C
C            CALL ZZWNINSD ( 5,  5, CONTEXT, WINDOW)       (1)
C            CALL ZZWNINSD ( 4,  8, CONTEXT, WINDOW)       (2)
C            CALL ZZWNINSD ( 0, 30, CONTEXT, WINDOW)       (3)
C
C      produces the following series of windows
C
C            [ 1,  3 ]  [ 5,  5 ]  [  7, 11 ]  [ 23, 27 ]   (1)
C            [ 1,  3 ]  [ 4, 11 ]  [ 23, 27 ]               (2)
C            [ 0, 30 ]                                      (3)
C
C$ Restrictions
C
C      None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      K.R. Gehringer  (JPL)
C      N.J. Bachman    (JPL)
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 1.0.0, 03-MAR-2009 (EDW)
C
C         This routine is a copy of the SPICELIB WNINSD routine
C         changed only by the addition of the CONTEXT string. 
C
C-&
 
C$ Index_Entries
C
C     insert an interval into a d.p. window, optional context string
C
C-&
 
 
C
C     SPICELIB functions
C
      INTEGER               CARDD
      INTEGER               SIZED
      INTEGER               LASTNB
      LOGICAL               RETURN
 
C
C     Local Variables
C
      INTEGER               SIZE
      INTEGER               CARD
 
      INTEGER               I
      INTEGER               J
  
C
C     Local paramters
C
      CHARACTER*(LMSGLN)    MSG
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZWNINSD' )
      END IF
 
C
C     Get the size and cardinality of the window.
C
      SIZE = SIZED ( WINDOW )
      CARD = CARDD ( WINDOW )
 
C
C     Let's try the easy cases first. No input interval? No change.
C     Signal that an error has occurred and set the error message.
C
      IF ( LEFT .GT. RIGHT ) THEN
      
         MSG = 'Left endpoint greather-than right. Left endpoint '
     .      // 'was #1. Right endpoint was #2.'

         MSG = MSG(1: LASTNB(MSG) ) //' '// CONTEXT(1: LASTNB(CONTEXT) )

         CALL SETMSG ( MSG)
         CALL ERRDP  ( '#1', LEFT  )
         CALL ERRDP  ( '#2', RIGHT )
         CALL SIGERR ( 'SPICE(BADENDPOINTS)' )

         CALL CHKOUT ( 'ZZWNINSD' )
         RETURN
 
      ELSE IF ( CARD .EQ. 0  .OR.  LEFT .GT. WINDOW(CARD) ) THEN

C
C        Empty window? Input interval later than the end of the window?
C        Just insert the interval, if there's room.
C
         IF ( SIZE .GE. CARD+2 ) THEN
 
            CALL SCARDD ( CARD+2, WINDOW )
            WINDOW(CARD+1) = LEFT
            WINDOW(CARD+2) = RIGHT
 
         ELSE

            MSG = 'Window has size, #1, cardinality #2. Cannot '
     .         // 'insert an additional interval into the '
     .         // 'window.'

            MSG = MSG(1: LASTNB(MSG) ) //' '// 
     .            CONTEXT(1: LASTNB(CONTEXT) )

            CALL SETMSG ( MSG)
            CALL ERRINT ( '#1', SIZE  )
            CALL ERRINT ( '#2', CARD )
            CALL SIGERR ( 'SPICE(WINDOWEXCESS)' )
 
         END IF
 
         CALL CHKOUT ( 'ZZWNINSD' )
         RETURN
 
      END IF
 
 
C
C     Now on to the tougher cases.
C
C     Skip intervals which lie completely to the left of the input
C     interval. (The index I will always point to the right endpoint
C     of an interval).
C
      I = 2
 
      DO WHILE ( I .LE. CARD  .AND.  WINDOW(I) .LT. LEFT )
         I = I + 2
      END DO
 
C
C     There are three ways this can go. The new interval can:
C
C        1) lie entirely between the previous interval and the next.
C
C        2) overlap the next interval, but no others.
C
C        3) overlap more than one interval.
C
C     Only the first case can possibly cause an overflow, since the
C     other two cases require existing intervals to be merged.
C
 
C
C     Case (1). If there's room, move succeeding intervals back and
C     insert the new one. If there isn't room, signal an error.
C
      IF ( RIGHT .LT. WINDOW(I-1) ) THEN
 
         IF ( SIZE .GE. CARD+2 ) THEN
 
            DO J = CARD, I-1, -1
               WINDOW(J+2) = WINDOW(J)
            END DO
 
            CALL SCARDD ( CARD+2, WINDOW )
            WINDOW(I-1) = LEFT
            WINDOW(I  ) = RIGHT
 
         ELSE
 
            MSG = 'Window has size, #1, cardinality #2. Cannot '
     .         // 'insert an additional interval into the '
     .         // 'window. The new interval lies entirely '
     .         // 'between the previous interval and the'
     .         // 'next.'

            MSG = MSG(1: LASTNB(MSG) ) //' '// 
     .            CONTEXT(1: LASTNB(CONTEXT) )

            CALL SETMSG ( MSG)
            CALL ERRINT ( '#1', SIZE  )
            CALL ERRINT ( '#2', CARD )
            CALL SIGERR ( 'SPICE(WINDOWEXCESS)' )

            CALL CHKOUT ( 'ZZWNINSD' )
            RETURN
 
         END IF
 
C
C     Cases (2) and (3).
C
      ELSE
 
C
C        The left and right endpoints of the new interval may or
C        may not replace the left and right endpoints of the existing
C        interval.
C
         WINDOW(I-1) = MIN ( LEFT,  WINDOW(I-1) )
         WINDOW(I  ) = MAX ( RIGHT, WINDOW(I  ) )
 
C
C        Skip any intervals contained in the one we modified.
C        (Like I, J always points to the right endpoint of an
C        interval.)
C
         J = I + 2
 
         DO WHILE ( J .LE. CARD  .AND.  WINDOW(J) .LE. WINDOW(I) )
            J = J + 2
         END DO
 
C
C        If the modified interval extends into the next interval,
C        merge the two. (The modified interval grows to the right.)
C
         IF ( J .LE. CARD  .AND.  WINDOW(I) .GE. WINDOW(J-1) ) THEN
            WINDOW(I) = WINDOW(J)
            J         = J + 2
         END IF
 
C
C        Move the rest of the intervals forward to take up the
C        spaces left by the absorbed intervals.
C
         DO WHILE ( J .LE. CARD )
            I           = I + 2
            WINDOW(I-1) = WINDOW(J-1)
            WINDOW(I  ) = WINDOW(J  )
            J           = J + 2
         END DO
 
         CALL SCARDD ( I, WINDOW )
 
      END IF
 
      CALL CHKOUT ( 'ZZWNINSD' )

      RETURN
      END
