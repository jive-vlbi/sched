C$Procedure      WNCOMD ( Complement a DP window )
 
      SUBROUTINE WNCOMD ( LEFT, RIGHT, WINDOW, RESULT )
 
C$ Abstract
C
C      Determine the complement of a double precision window with
C      respect to the interval [LEFT,RIGHT].
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
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      WINDOW   ( LBCELL:* )
      DOUBLE PRECISION      RESULT   ( LBCELL:* )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      LEFT,
C      RIGHT      I   Left, right endpoints of complement interval.
C      WINDOW     I   Input window.
C      RESULT     O   Complement of WINDOW with respect to [LEFT,RIGHT].
C
C$ Detailed_Input
C
C      LEFT,
C      RIGHT       are the left and right endpoints of the complement
C                  interval.
C
C      WINDOW      is the window to be complemented.
C
C$ Detailed_Output
C
C      RESULT      is the output window, containing the complement
C                  of WINDOW with respect to the interval from LEFT
C                  to RIGHT. If the output window is not large enough
C                  to contain the result, as many intervals as will
C                  fit are returned.
C
C                  RESULT must be distinct from WINDOW.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      Mathematically, the complement of a window contains those
C      points that are not contained in the window. That is, the
C      complement of the set of closed intervals
C
C           [ a(1), b(1) ], [ a(2), b(2) ], ..., [ a(n), b(n) ]
C
C      is the set of open intervals
C
C           ( -inf, a(1) ), ( b(1), a(2) ), ..., ( b(n), +inf )
C
C      Because Fortran offers no satisfactory representation of
C      infinity, we must take the complement with respect to a
C      finite interval.
C
C      In addition, Fortran offers no satisfactory floating point
C      representation of open intervals. Therefore, the complement
C      of a floating point window is closure of the set theoretical
C      complement. In short, the floating point complement of the
C      window
C
C           [ a(1), b(1) ], [ a(2), b(2) ], ..., [ a(n), b(n) ]
C
C      with respect to the interval from LEFT to RIGHT is the
C      intersection of the windows
C
C           ( -inf, a(1) ], [ b(1), a(2) ], ..., [ b(n), +inf )
C
C      and
C
C           [ LEFT, RIGHT ]
C
C      Note that floating point intervals of measure zero (singleton
C      intervals) in the original window are replaced by gaps of
C      measure zero, which are filled. Thus, complementing a floating
C      point window twice does not necessarily yield the original
C      window.
C
C$ Examples
C
C      Let WINDOW contain the intervals
C
C            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
C
C      Then the floating point complement of WINDOW with respect
C      to [2,20] contains the intervals
C
C            [ 3, 7 ]  [ 11, 20 ]
C
C      and the complement with respect to [ 0, 100 ] contains
C
C            [ 0, 1 ]  [ 3, 7 ]  [ 11, 23 ]  [ 27, 100 ]
C
C$ Exceptions
C
C      If LEFT is greater than RIGHT, the error SPICE(BADENDPOINTS) is
C      signalled.
C
C$ Files
C
C      None.
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
C      N.J. Bachman    (JPL)
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     complement a d.p. window
C
C-&
 
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 17-FEB-1989 (HAN) (NJB)
C
C         Contents of the Required_Reading section was
C         changed from "None." to "WINDOWS".  Also, the
C         declaration of the unused variable J was removed.
C-&
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER               CARDD
      LOGICAL               RETURN
      LOGICAL               FAILED
 
C
C     Local variables
C
      INTEGER               CARD
      INTEGER               I
 
 
 
C
C     Set up the error processing.
C
      IF ( RETURN () ) RETURN
      CALL CHKIN ( 'WNCOMD' )
 
 
C
C     Get the cardinality of the input window.
C
      CARD = CARDD ( WINDOW )
 
C
C     Empty out the result window before proceeding.
C
      CALL SCARDD ( 0, RESULT )
 
 
C
C     Check to see if the input interval is valid. If it is not, signal
C     an error and return.
C
      IF ( LEFT .GT. RIGHT ) THEN
         CALL SETMSG ( 'WNCOMD: Left endpoint may not exceed right' //
     .                 ' endpoint.' )
         CALL SIGERR ( 'SPICE(BADENDPOINTS)' )
         CALL CHKOUT ( 'WNCOMD' )
         RETURN
      END IF
 
 
 
C
C     There are two trivial cases: the window is empty, or it does not
C     intersect the input interval. In either case, the complement is
C     the entire interval.
C
      IF (       CARD         .EQ. 0
     .     .OR.  WINDOW(1)    .GE. RIGHT
     .     .OR.  WINDOW(CARD) .LE. LEFT  ) THEN
 
         CALL WNINSD ( LEFT, RIGHT, RESULT )
 
         CALL CHKOUT ( 'WNCOMD' )
         RETURN
 
      END IF
 
 
 
C
C     Let WINDOW represent the set of intervals
C
C            [a1,b1], [a2,b2], ..., [aN,bN]
C
C     Then the closure of the complement of WINDOW in the reals is
C
C            (-infinity,a1], [b1,a2], [b2,a3], ..., [bN, infinity)
C
C     Thus the sequence of endpoints of WINDOW is also the sequence
C     of finite endpoints of its complement. Moreover, these endpoints
C     are simply "shifted" from their original positions in WINDOW.
C     This makes finding the complement of WINDOW with respect to
C     a given interval almost trivial.
C
 
C
C     Find the first right not less than the beginning of the input
C     interval.
C
      I = 2
 
      DO WHILE ( I .LE. CARD  .AND.  WINDOW(I) .LT. LEFT )
         I = I + 2
      END DO
 
C
C     If the beginning of the input interval doesn't split an interval
C     in the input window, the complement begins with LEFT.
C
      IF ( I .LE. CARD  .AND.  WINDOW(I-1) .GT. LEFT ) THEN
         CALL WNINSD ( LEFT, WINDOW(I-1), RESULT )
      END IF
 
C
C     Start schlepping endpoints [b(i),a(i+1)] from the input window
C     to the output window. Stop when we find one of our new right
C     endpoints exceeds the end of the input interval.
C
      DO WHILE (        ( .NOT. FAILED () )
     .           .AND.  I           .LT. CARD
     .           .AND.  WINDOW(I+1) .LT. RIGHT )
 
         CALL WNINSD ( WINDOW(I), WINDOW(I+1), RESULT )
         I = I + 2
 
      END DO
 
C
C     If the end of the input interval doesn't split an interval
C     in the input window, the complement ends with RIGHT.
C
      IF ( I .LE. CARD  .AND.  WINDOW(I) .LT. RIGHT ) THEN
         CALL WNINSD ( WINDOW(I), RIGHT, RESULT )
      END IF
 
      CALL CHKOUT ( 'WNCOMD' )
 
      RETURN
      END
