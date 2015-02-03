C$Procedure      WNFETD ( Fetch an interval from a DP window )
 
      SUBROUTINE WNFETD ( WINDOW, N, LEFT, RIGHT )
 
C$ Abstract
C
C     Fetch a particular interval from a double precision window.
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
C     WINDOWS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      DOUBLE PRECISION      WINDOW      ( LBCELL:* )
      INTEGER               N
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      WINDOW     I   Input window.
C      N          I   Index of interval to be fetched.
C      LEFT,
C      RIGHT      O   Left, right endpoints of the Nth interval.
C
C$ Detailed_Input
C
C      WINDOW      is a window containing zero or more intervals.
C
C      N           is the index of a particular interval within the
C                  window. Indices range from 1 to CARD(WINDOW)/2.
C
C$ Detailed_Output
C
C      LEFT,
C      RIGHT       are the left and right endpoints of the Nth interval
C                  in the input window. If the interval is not found,
C                  LEFT and RIGHT are not defined.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If N is less than one, the error SPICE(NOINTERVAL) signals.
C
C     2) If the interval does not exist, i.e. N > CARD(WINDOW)/2, the
C         error SPICE(NOINTERVAL) signals.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      None.
C
C$ Examples
C
C      Let A contain the intervals
C
C            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
C
C      This window has a cardinality of 6, so N may have
C      value 1, 2, or 3 ( N =< CARD(WINDOW)/2 ).
C
C      Then the following calls
C
C            CALL WNFETD ( A,  1, LEFT, RIGHT )       [1]
C            CALL WNFETD ( A,  2, LEFT, RIGHT )       [2]
C            CALL WNFETD ( A,  3, LEFT, RIGHT )       [3]
C
C      yield the following values of LEFT and RIGHT
C
C            LEFT         RIGHT
C            ---------    ---------
C            1            3
C            7            11
C            23           27
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
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 1.0.3, 30-JUL-2007 (EDW)
C
C         Removed erroneous description in the Examples section 
C         indicating "Undefined" as a return state after an error
C         event caused by an invalid value of N.
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
C     fetch an interval from a d.p. window
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER               CARDD
      LOGICAL               RETURN
 
C
C     Local variables
C
      INTEGER               CARD
      INTEGER               END
 
 
 
 
C
C     Set up the error processing.
C
      IF ( RETURN () ) RETURN
      CALL CHKIN ( 'WNFETD' )
 
C
C
C     How many endpoints in the window? Enough? Normally, endpoints
C     of the Nth interval are stored in elements 2N and 2N-1.
C
      CARD = CARDD ( WINDOW )
      END  = 2*N
 
      IF ( N .LT. 1  .OR.  CARD .LT. END ) THEN
         CALL SETMSG ( 'WNFETD: No such interval.' )
         CALL SIGERR ( 'SPICE(NOINTERVAL)' )
      ELSE
         LEFT  = WINDOW(END-1)
         RIGHT = WINDOW(END  )
      END IF
 
      CALL CHKOUT ( 'WNFETD' )
 
      RETURN
      END
