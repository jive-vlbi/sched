C$Procedure   LSTCLD ( Last closest double precision array element )
 
      INTEGER FUNCTION  LSTCLD ( X, N, ARRAY )
 
C$ Abstract
C
C     Given a number X and an array of non-decreasing numbers, find
C     the index of the array element whose value is closest to X.
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
C     ARRAY
C     SEARCH
C
C$ Declarations
 
      DOUBLE PRECISION   X
      INTEGER            N
      DOUBLE PRECISION   ARRAY ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     X          I   Search value.
C     N          I   Number of elements in ARRAY.
C     ARRAY      I   Array to be searched.
C
C     The function returns the index of the element of ARRAY
C     whose value is closest to X.
C
C$ Detailed_Input
C
C     X       is the value to be compared with the elements of ARRAY.
C
C     N       is the number of elements in ARRAY.
C
C     ARRAY   is an array of double precision numbers such that
C
C                        ARRAY( I ) <= ARRAY( J )
C
C             for all I < J.
C
C$ Detailed_Output
C
C     LSTCLD  is the index of the element of the non-decreasing
C             sequence: {ARRAY(I) : 1 <= I <= N} that is closest
C             to X.  In other words, ARRAY( LSTCLD( X, N, ARRAY ) )
C             is the element of ARRAY whose value is closest to X.
C
C             If X falls precisely on the midpoint of consecutive array
C             elements, the index of the larger of the two values is
C             returned.
C
C             If X is closest to a value which appears more than
C             once in the array (since the array is ordered, these
C             elements would have to be consecutive), the highest index
C             for that value will be returned.
C
C             LSTCLD = I for some I in the range 1 to N, unless N is
C             less than or equal to zero, in which case LSTCLD is zero.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C     LSTCLD uses a binary search algorithm to locate the value closest
C     to X in the non-decreasing sequence of double precision numbers
C     represented by the elements of ARRAY.
C
C$ Examples
C
C     Suppose ARRAY contains the following double precision elements:
C
C     ARRAY: -1    0    1    1.5   1.5    2    3    9    9.5   100
C
C     index:  1    2    3     4     5     6    7    8     9     10
C
C     The following table shows the values of LSTCLD that would be
C     returned for various values of X, and the corresponding closest
C     array element values.
C
C            X      LSTCLD( X,10,ARRAY )   ARRAY( LSTCLD( X,10,ARRAY ))
C          -----    --------------------   ---------------------------
C           0.12             2                          0
C          -0.12             2                          0
C          -2.0              1                         -1
C           2.5              7                          3
C           1.3              5                        1.5
C         100.0             10                        100
C         100.1             10                        100
C
C$ Restrictions
C
C     If the sequence is not non-decreasing, the routine will run
C     to completion but the index found will not mean anything.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the value of N is non-positive, LSTCLD returns the value
C        zero.
C
C$ Files
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     M.J. Spencer    (JPL)
C     R.E. Thurman    (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 07-SEP-1990 (RET)
C
C-&
 
C$ Index_Entries
C
C     last closest d.p. array element
C
C-&
 
 
C$ Revisions
C
C-    Beta Version 1.1.0, 30-AUG-1990 (MJS)
C
C        The following changes were made as a result of the
C        NAIF CK Code and Documentation Review:
C
C        1) The name of this routine was changed from CLOSTD to
C           LSTCLD because it was a more descriptive name.
C        2) All references (comments and code) were changed to reflect
C           the name change.
C
C-    Beta Version 1.0.0, 15-MAY-1990 (RET)
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
 
C
C     Save the size of the array and point to the beginning and ending
C     positions. The pointers delimit the current search interval.
C
      ITEMS = N
      BEGIN = 1
      END   = N
 
      IF      ( N .LE. 0 ) THEN
 
C
C        There is nothing in the array to compare against. Zero is the
C        only sensible thing to return.
C
         LSTCLD = 0
         RETURN
 
      ELSE IF ( X .LE. ARRAY( BEGIN ) ) THEN
 
C
C        All elements of the array are at least as big as X. So the
C        first element is the closest to X.
C
         LSTCLD = 1
 
      ELSE IF ( ARRAY( END ) .LE. X ) THEN
 
C
C        X is at least as big as all elements of the array.  So the last
C        element is the closest to X.
C
         LSTCLD = END
 
      ELSE
 
C
C        X lies between some pair of elements of the array.
C
         DO WHILE ( ITEMS .GT. 2 )
 
            J        = ITEMS/2
            MIDDLE   = BEGIN + J
 
            IF ( ARRAY( MIDDLE ) .LT. X ) THEN
               BEGIN  = MIDDLE
            ELSE
               END    = MIDDLE
            END IF
 
            ITEMS     = 1 + ( END - BEGIN )
 
         END DO
 
C
C        Which of the two is closest?
C
         IF ( X - ARRAY( BEGIN ) .LT. ARRAY( END ) - X ) THEN
            LSTCLD = BEGIN
         ELSE
            LSTCLD = END
         END IF
 
      END IF
 
C
C     March down the array to find the last element equal to the
C     closet value.
C
      DO WHILE (       LSTCLD   .LT. N                   .AND.
     .          ARRAY( LSTCLD ) .EQ. ARRAY( LSTCLD + 1 ))
 
         LSTCLD = LSTCLD + 1
 
      END DO
 
      RETURN
      END
