C$Procedure   LSTLEC ( Last character element less than or equal to. )
 
      INTEGER FUNCTION  LSTLEC ( STRING, N, ARRAY )
 
C$ Abstract
C
C      Given a character string and an ordered array of character
C      strings, find the index of the largest array element less than
C      or equal to the given string.
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
C      SEARCH,  ARRAY
C
C$ Declarations
 
      CHARACTER*(*)    STRING
      INTEGER          N
      CHARACTER*(*)    ARRAY ( * )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      STRING     I   Value to search against
C      ARRAY      I   Array of possible lower bounds
C      N          I   Number elements in ARRAY
C      LSTLEC     O   the index of the last element of ARRAY <= STRING
C
C$ Detailed_Input
C
C      STRING  Character string for which one desires to find
C              the last ARRAY element less than or equal (lexically)
C              to string.
C
C      ARRAY   Ordered array of character strings. We will find the
C              last element of the sequence that is less than or equal
C              to STRING.
C
C      N       Total number of elements in ARRAY
C
C$ Detailed_Output
C
C      LSTLEC  Index of the last element of the  ordered array
C              {ARRAY(I) : 0 < I < N + 1} that is less than or equal
C              to STRING. (Note that LSTLEC = I for some I in the
C              range 1 to N  unless STRING is less than all of these
C              elements in which case LSTLEC = 0.)
C
C              In the case that N is input with value less than or equal
C              to zero, LSTLEC is returned as zero.
C
C$ Parameters
C
C      None.
C
C$ Particulars
C
C
C      An ordered array of character strings is given.
C      Given a character string STRING, there will be a last one of
C      these strings that is less than or equal to STRING.
C      This routine  finds the index LSTLEC such that ARRAY(LSTLEC) is
C      that string.
C
C      If STRING is not greater than ARRAY(1), LSTLEC will be set to
C      zero.
C
C      This routine uses a binary search algorithm and so requires
C      at most LOG_2(N) steps to find the value of LSTLTI.
C
C      Note:  If you need to find the first element of the array that
C             is greater than STRING, simply add 1 to the result
C             returned by this function and check to see if the result
C             is within the array bounds given by N.
C
C$ Examples
C
C      Suppose that you have a long list of words, sorted alphabetically
C      and entirely in upper case.  Furthermore suppose you wished to
C      find all words that begin the sequence of letters PLA,  then
C      you could execute the following code.
C
C            START = 0
C            I     = 1
C
C            DO I = 1, NWORDS
C
C               IF ( WORD(I)(1:3) .EQ. 'PLA' ) THEN
C
C                  IF ( START .EQ. 0 ) THEN
C                     START = I
C                  END IF
C
C                  END = I
C               END IF
C
C            END DO
C
C      This can of course be improved by stopping the loop once START
C      is non-zero and END remains unchanged after a pass through the
C      loop.  However, this is a linear search  and on average can be
C      expected to take NWORDS/2 comparisons.  The above algorithm
C      fails to take advantage of the structure of the list of words
C      (they are sorted).
C
C      The code below is much simpler to code, simpler to check, and
C      much faster than the code above.
C
C            START = LSTLEC( 'PL ', NWORDS, WORDS ) + 1
C            END   = LSTLEC( 'PLA', NWORDS, WORDS )
C
C            do something in case there are no such words.
C
C            IF ( START .GT. END ) THEN
C               START = 0
C               END   = 0
C            END IF
C
C      This code will never exceed 2 * LOG_2 ( NWORDS ) comparisons.
C      For a large list of words (say 4096) the second method will
C      take 24 comparisons  the first method requires on average
C      2048 comparisons.  About 200 times as much time.  Its clear
C      that if searches such as this must be performed often, that
C      the second approach could make the difference between being
C      able to perform the task in a few minutes as opposed to
C      several hours.
C
C      For more ideas regarding the use of this routine see LSTLEI
C      and LSTLTI.
C
C$ Restrictions
C
C      If the array is not ordered, the program will run
C      to completion but the index found will not mean anything.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     last character element less_than_or_equal_to
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 1.1.0, 9-MAR-1989 (HAN)
C
C        Declaration of the variable I was removed from the code. The
C        variable was declared but not used.
C
C-     Beta Version 1.0.1, 1-Feb-1989 (WLT)
C
C      Example section of header upgraded.
C
C-&
 
 
C
C     Local variables
C
      INTEGER          J
 
      INTEGER          BEGIN
      INTEGER          END
      INTEGER          MIDDLE
      INTEGER          ITEMS
 
 
 
      ITEMS = N
 
      BEGIN = 1
      END   = N
 
 
 
      IF      ( N .LE. 0            ) THEN
 
C
C        There's nobody home---that is there is nothing in the array
C        to compare against.  Zero is the only sensible thing to return.
C
         LSTLEC = 0
 
      ELSE IF ( LLT (STRING, ARRAY(BEGIN)) ) THEN
 
C
C        None of the array elements are less than or equal to STRING
C
         LSTLEC = 0
 
      ELSE IF ( LGE (STRING, ARRAY(END))   ) THEN
 
C
C        STRING is greater than or equal to all elements of the array.
C        Thus the last element of the array is the last item less than
C        or equal to STRING.
C
         LSTLEC = END
 
      ELSE
 
C
C        STRING lies between some pair of elements of the array
C
 
         DO WHILE ( ITEMS .GT. 2 )
 
            J        = ITEMS/2
            MIDDLE   = BEGIN + J
 
            IF ( LLE (ARRAY(MIDDLE), STRING) ) THEN
               BEGIN = MIDDLE
            ELSE
               END   = MIDDLE
            END IF
 
            ITEMS    = 1 + (END - BEGIN)
         END DO
 
         LSTLEC = BEGIN
 
      END IF
 
      RETURN
      END
