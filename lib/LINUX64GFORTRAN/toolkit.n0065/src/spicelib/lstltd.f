C$Procedure   LSTLTD ( Last double precision element less than )
 
       INTEGER FUNCTION  LSTLTD ( X, N, ARRAY )
 
C$ Abstract
C
C      Given a number X and an array of non-decreasing numbers,
C      find the index of the largest array element less than X.
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
 
      DOUBLE PRECISION   X
      INTEGER            N
      DOUBLE PRECISION   ARRAY ( * )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      X          I   Value to search against.
C      ARRAY      I   Array of possible lower bounds.
C      N          I   Number elements in ARRAY.
C      LSTLTD     O   the index of the last element of ARRAY < X.
C
C$ Detailed_Input
C
C      X       Double precision number for which one desires to find
C              the last ARRAY element less than X.
C
C      N       Total number of elements in ARRAY.
C
C      ARRAY   Array of double precision numbers that forms a
C              non-decreasing sequence.  We will find the last element
C              of the sequence that is less than X.
C
C$ Detailed_Output
C
C      LSTLTD  Index of the last element of the non-decreasing sequence:
C              {ARRAY(I) : 0 < I < N + 1} that is less than X.
C              (Note that LSTLTD = I for some I in the range 1 to
C              N  unless X is less than or equal to all of these
C              elements, in which case LSTLTD = 0.)
C
C              In the case that N is input with value less than or equal
C              to zero, LSTLTD is returned as zero.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C
C      An array of double precision numbers is given.  The array
C      ARRAY(I) (0 < I < N+1 ) forms a non-decreasing sequence of
C      numbers.  Given a real number X, there will be a last one of
C      these numbers that is less than X.  This routine
C      finds the index LSTLTD such that ARRAY(LSTLTD) is that number.
C
C      If X is not greater than ARRAY(1), INDEX will be set to zero.
C
C      Note:  If you need to find the first element of the array that
C             is greater than or equal to X, simply add 1 to the
C             result returned by this function and check to see if the
C             result is within the array bounds given by N.
C
C$ Examples
C
C      If ARRAY(I) = -1 + 4*I/3 (real arithmetic implied here)
C
C      N        = 10
C      X        = 7.12
C
C      then
C
C      LSTLTD will be I where
C              (4*I/3) - 1       <      7.12
C      but
C              (4*(I+1)/3) - 1   > or = 7.12 .
C
C      In this case our subsequence is:
C             1/3, 5/3, 9/3, 13/3, 17/3, 21/3, 25/3, .... 37/3
C
C      index:  1    2    3    4     5     6     7    ....  10
C
C      Thus LSTLTD will be returned as 6
C
C      The following table shows the values of LSTLTD that would be
C      returned for various values of X
C
C             X       LSTLTD
C           -----     -------
C            0.12        0
C            1.34        1
C            5.13        4
C            8.00        6
C           15.10       10
C
C$ Restrictions
C
C      If the sequence is not non-decreasing, the program will run
C      to completion but the index found will not mean anything.
C
C$ Exceptions
C
C     None.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      N.J. Bachman    (JPL)
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
C
C-&
 
C$ Index_Entries
C
C     last d.p. element less_than
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 1.1.0, 16-FEB-1989 (NJB)
C
C        Declaration of unused variable I removed.
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
C        to compare against.  Zero is the only sensible thing to return
C
         LSTLTD = 0
 
      ELSE IF ( X .LE. ARRAY(BEGIN) ) THEN
 
C
C        None of the array elements are less than X
C
         LSTLTD = 0
 
      ELSE IF ( ARRAY(END) .LT. X ) THEN
 
C
C        X is greater than all elements of the array.  Thus the last
C        element of the array is the last item less than X.
C
         LSTLTD = END
 
      ELSE
 
C
C        X lies between some pair of elements of the array
C
 
         DO WHILE ( ITEMS .GT. 2 )
 
            J        = ITEMS/2
            MIDDLE   = BEGIN + J
 
            IF ( ARRAY(MIDDLE) .LT. X ) THEN
               BEGIN = MIDDLE
            ELSE
               END   = MIDDLE
            END IF
 
            ITEMS    = 1 + (END - BEGIN)
         END DO
 
         LSTLTD = BEGIN
 
      END IF
 
 
      RETURN
      END
