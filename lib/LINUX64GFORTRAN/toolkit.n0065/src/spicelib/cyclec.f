C$Procedure      CYCLEC ( Cycle a character string )
 
      SUBROUTINE CYCLEC ( INSTR, DIR, NCYCLE, OUTSTR )
 
C$ Abstract
C
C      Cycle the contents of a character string to the left or right.
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
C      CHARACTER,  UTILITY
C
C$ Declarations
 
      CHARACTER*(*)      INSTR
      CHARACTER*1        DIR
      INTEGER            NCYCLE
      CHARACTER*(*)      OUTSTR
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      INSTR      I   String to be cycled.
C      DIR        I   Direction to cycle.
C      NCYCLE     I   Number of times to cycle.
C      OUTSTR     O   Cycled string.
C
C$ Detailed_Input
C
C      DIR         is the direction in which the characters in the
C                  string are to be cycled.
C
C                        'L' or 'l'  to cycle left.
C                        'R' or 'r'  to cycle right.
C
C      NCYCLE      is the number of times the characters in the string
C                  are to be cycled.
C
C      INSTR       is the string to be cycled.
C
C$ Detailed_Output
C
C      OUTSTR      the input string after it has been cycled.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C      A string is cycled when its contents are shifted to the left
C      or right by one place. A character pushed off one end of the
C      string is brought around to the other end of the string instead
C      of disappearing.
C
C      Leading and trailing blanks are treated just like any other
C      characters.
C
C      If the output string is not large enough to contain the input
C      string, the cycled string is truncated on the right.
C
C$ Examples
C
C      'abcde'   cycled left twice becomes               'cdeab'
C      'abcde '  cycled left twice becomes               'cde ab'
C      'abcde'   cycled right once becomes               'eabcd'
C      'Apple '  cycled left six times becomes           'Apple '
C      'Apple '  cycled right twenty-four times becomes  'Apple '
C
C$ Restrictions
C
C      The memory used for the output string must be identical to that
C      used for the input string or be disjoint from the input string
C      memory.
C
C      That is:
C
C           CALL CYCLEN ( STRING, DIR, NCYCLE, STRING )
C
C      will produce correct results with output overwriting input.
C
C           CALL CYCLEN ( STRING(4:20), DIR, NCYCLE, STRING(2:18) )
C
C      will produce garbage results.
C
C$ Exceptions
C
C     1) If the direction flag is not one of the acceptable values
C        'r', 'R', 'l', 'L',  the error 'SPICE(INVALIDDIRECTION)' is
C        signalled.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
C
C         Fixed problem with unbalanced CHKIN/CHKOUT calls.
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT)
C
C-&
 
C$ Index_Entries
C
C     cycle a character_string
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 6-FEB-1989 (WLT)
C
C      Error handling for bad direction flag added.
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL          RETURN
      INTEGER          GCD
 
C
C     Local variables
C
      CHARACTER*1      LAST
      CHARACTER*1      TEMP
 
      INTEGER          G
      INTEGER          I
      INTEGER          J
      INTEGER          K
      INTEGER          L
      INTEGER          M
      INTEGER          N
      INTEGER          LIMIT
 
      IF ( RETURN() ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CYCLEC' )
      END IF
C
C     Get the length of the input string.
C
      N     = LEN ( INSTR  )
      LIMIT = LEN ( OUTSTR )
 
C
C     A left cycle is the same as a right cycle by the opposite of
C     NCYCLE.  Moreover a cycle by K is the same as a cycle by
C     K + m*N for any integer m.  Thus we compute the value of the
C     minimum positive right cycle that is equivalent to the inputs.
C
      IF (      (DIR .EQ. 'L')
     .     .OR. (DIR .EQ. 'l') ) THEN
 
         K = MOD ( - NCYCLE, N )
 
      ELSE IF (      ( DIR .EQ. 'R')
     .          .OR. ( DIR .EQ. 'r') ) THEN
 
         K = MOD (   NCYCLE, N )
 
      ELSE
 
         CALL SETMSG ( 'The direction flag should be one of the '     //
     .                 'following: ''r'', ''R'', ''l'', ''L''.  It'   //
     .                 ' was #.'   )
 
         CALL ERRCH  ( '#', DIR )
         CALL SIGERR ( 'SPICE(INVALIDDIRECTION)' )
         CALL CHKOUT ( 'CYCLEC' )
         RETURN
 
      END IF
 
      IF      ( K .LT. 0 ) THEN
         K = K + N
      ELSE IF ( K .EQ. 0 ) THEN
         CALL CHKOUT ( 'CYCLEC' )
         RETURN
      END IF
 
 
C
C     As to the method for performing the cycle in place, we need a
C     few preliminaries.
C
C        1.  Since we are performing a cycle on the input string we
C            can regard the letters of the string as being attached
C            to a circle at N equally spaced points.  Thus a cycle
C            by K has the effect of moving the position of each letter
C            to the K'th point from its current position along the
C            circle.  (The first point from its position is the
C            adjacent point.)
C
C        2.  If we start at some point on the circle and begin moves to
C            other points of the circle by always moving K points
C            at a time, how long will it take until we get back to
C            the starting point?  Answer: N/gcd(K,N)
C
C               Justification of the above answer.
C
C               a.  If we count all of the points that we move past or
C                   onto in such a trip (counting second, third, ...
C                   passes), we will find that we have
C                   moved past or onto i*K points after i steps.
C
C               b.  In order to get back to the starting point we will
C                   have to move past or onto a multiple of N points.
C
C               c.  The first time we will get back to the starting
C                   point is the smallest value of i such that i*K
C                   is a multiple of N.  That value is N/g.c.d.(K,N)
C                   where g.c.d stands for the greatest common divisor
C                   of K and N. Lets call this number M.
C
C                      i.  To see that this is the smallest number we
C                          first show that K*M is in fact a multiple of
C                          N.  The product K*M = K * ( N / gcd(K,N) )
C                                              = N * ( K / gcd(K,N) )
C
C                          Since gcd(K,N) evenly divides K, K/gcd(K,N)
C                          is an integer.  Thus K*M = N*I for some
C                          integer I ( = K / gcd(K,N) ).
C
C                      ii. The least common multiple of K and N is:
C                          K*N / gcd(K,N)  thus the first multiple
C                          of K that is also a multiple of N is the
C                          N/ gcd(K,N) 'th multiple of K.
C
C        3.  The closest stopping point on the circle will be gcd(K,N)
C            points away from our starting point.  To see this recall
C            that we make N/gcd(K,N) moves of size K inorder to get
C            back to the starting point.  The stopping points must
C            be equally spaced around the circle since the set of
C            points must look the same from any one of the points
C            visited --- after all we could get the same set by just
C            starting at one of those visited and making N/gcd(K,N)
C            moves.  But the set of N/gcd(K,N) equally space points
C            out of the original N must be gcd(K,N) points apart.
C
C        4.  To visit every point on the circle we could
C
C            a.  Pick a starting point
C            b.  Take N/gcd(K,N) steps of size K (bringing us back
C                to our starting point.
C            c.  move forward 1 point
C            d.  repeat steps a. b. and c. gcd(K,N) times.
C
C        5.  If in addition to moving around the circle by the
C            prescription of 4. above we:
C               a. pick up the letter at a position when we stop there
C                  (starting being the same as stopping)
C               b. put down the letter we had picked up at a previous
C                  point.
C            then we will cycle every letter by the prescribed value
C            of K.
C
C     In this case the code is much shorter than its explanation.
C
 
      G = GCD ( K, N )
      M = N / G
 
      DO I = 1, G
 
         L    = I
         LAST = INSTR (L:L)
 
         DO J = 1, M
 
            L = L + K
 
C
C           Compute L mod N.
C
            IF ( L .GT. N ) THEN
               L = L - N
            END IF
 
            TEMP        = INSTR (L:L)
 
C
C           Make sure there is someplace to put the letter picked up
C           in the previous pass through the loop.
C
            IF ( L .LE. LIMIT ) THEN
               OUTSTR(L:L) = LAST
            END IF
 
            LAST        = TEMP
 
         END DO
 
      END DO
 
      CALL CHKOUT ( 'CYCLEC' )
      RETURN
      END
 
