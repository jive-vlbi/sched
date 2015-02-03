 
 
C$Procedure      ISORDV ( Is it an order vector )
 
      LOGICAL FUNCTION ISORDV ( ARRAY, N )
 
C$ Abstract
C
C     Determine whether an array of N items contains the integers
C     1 through N.
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
C     SEARCH
C     SORT
C     UTILITY
C
C$ Declarations
 
      INTEGER               ARRAY (*)
      INTEGER               N
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ARRAY      I   Array of integers.
C     N          I   Number of integers in ARRAY.
C
C     The function returns TRUE if the array contains the integers
C     1 through N, otherwise it returns FALSE.
C
C$ Detailed_Input
C
C     ARRAY      is an array of integers.  Often this will be an array
C                that is a candidate order vector to be passed to
C                a routine for re-ordering some parallel array.
C
C     N          is the number of elements in ARRAY.
C
C$ Detailed_Output
C
C     The function returns TRUE if the array contains the integers
C     1 through N.  Otherwise it returns FALSE.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If N < 1, the function returns .FALSE.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This function provides a simple means of determining whether
C     or not an array of N integers contains exactly the integers
C     1 through N.
C
C$ Examples
C
C     1) Suppose you wished to reorder an array of strings based upon
C        a ranking array supplied by a user.  If the ranking array
C        contains any duplicates or refers to indices that are out
C        of the range of valid indices for the array of strings,
C        the attempt to reorder the array of strings cannot succeed.
C        Its usually better to detect such a possibility before
C        you begin trying to reorder the array of strings.  This routine
C        will detect the error.
C
C        The block of code below illustrates this idea.
C
C
C           IF ( ISORDV ( ORDVEC, N ) ) THEN
C
C              ...reorder the input array of strings
C
C              CALL REORDC ( ORDVEC, N, STRNGS )
C
C           ELSE
C
C              ...state the problem and let the user decide what
C              to do about it.
C                    .
C                    .
C                    .
C
C           END IF
C
C
C     2) This routine can also be used to determine whether or not an
C        array contains every integer between K and N (where K < N ).
C
C
C           First subtract K-1 from each integer
C
C           DO I = 1, N-K+1
C              ARRAY(I) = ARRAY(I) - K + 1
C           END DO
C
C           See if the modified array is an order vector
C
C           OK = ISORDV ( ARRAY, N-K )
C
C           Return the array to its original state.
C
C           DO I = 1, N-K+1
C              ARRAY(I) = ARRAY(I) + K - 1
C           END DO
C
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
C     N.J. Bachman   (JPL)
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C        Comment section for permuted index source lines was added
C        following the header.
C
C-    SPICELIB Version 1.0.0, 6-MAR-1991 (NJB) (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     test whether an integer array is an order vector
C
C-&
 
 
 
C
C     Local variables
C
      INTEGER               I
      INTEGER               J
 
 
 
 
C
C     Let's take care of the goofy case first.
C
      IF      ( N .LT. 1 ) THEN
         ISORDV = .FALSE.
         RETURN
      ELSE IF ( N .EQ. 1 ) THEN
         ISORDV = ARRAY(1) .EQ. 1
         RETURN
      END IF
 
 
C
C     Make an initial pass through the array to be sure we
C     have legitimate values.
C
      DO I = 1, N
 
         IF (      ( ARRAY(I) .LT. 1 )
     .        .OR. ( ARRAY(I) .GT. N )  ) THEN
            ISORDV = .FALSE.
            RETURN
         END IF
 
      END DO
 
C
C     Ok. All of the values are in range.  We just need to check
C     that this array could actually be used as an order vector.
C
C     For each I between 1 and N,  ARRAY(I) is some integer between 1
C     and N.  The only question remaining is whether the set
C     { ARRAY(I), I=1,N } contains every integer between 1 and N.
C
C     Suppose for a moment we could allocate a logical array called HITS
C
C           LOGICAL               HITS(N)
C
C     Then the following scheme could be used to determine whether or
C     not { ARRAY(I), I=1,N } contains every integer between 1 and N.
C
C        Initialize every entry of HITS to .FALSE.
C
C           DO I = 1, N
C              HITS(I) = .FALSE.
C           END DO
C
C        Then for each I set HITS(ARRAY(I)) to .TRUE.
C
C           DO I = 1, N
C              HITS(ARRAY(I)) = .TRUE.
C           END DO
C
C     What can be said about HITS at this point? If for any entry J,
C     HITS(J) is true then some ARRAY(I) is equal to J.
C
C     If all HITS are .TRUE. then {ARRAY(I), I=1,N} is in fact the
C     set of integers 1 to N.  Otherwise those J such that
C     HITS(J) = .FALSE. are the integers between 1 and N that are
C     missed by ARRAY.
C
C     It turns out we don't need to allocate an array of logicals;
C     we can use just use part of the input array, ARRAY.
C
C     The storage locations ARRAY(1) through ARRAY(N) can be viewed
C     as two parallel arrays:  SIGN_BIT and UNSIGNED
C
C          SIGN
C          BIT  UNSIGNED PORTION
C         +----+-----------------+
C      1  |    |                 |
C         +----+-----------------+
C      2  |    |                 |
C         +----+-----------------+
C      3  |    |                 |
C         +----+-----------------+
C
C                 .
C                 .
C                 .
C
C         +----+-----------------+
C     N-1 |    |                 |
C         +----+-----------------+
C     N   |    |                 |
C         +----+-----------------+
C
C
C     Since we know the value of all of the sign bits (it's '+') we can
C     alter them and then reset them once we are done.
C
C     We will choose for our array of HITS the SIGN_BITS of ARRAY.
C     We regard '+' as FALSE and '-' as TRUE.
C
C        DO I = 1, N
C           SIGN_BIT ( UNSIGNED(I) ) = '-'
C        END DO
C
C     Then check to make sure that all of the sign bits are '-'.
C
      DO I = 1, N
         J        = ABS( ARRAY(I) )
         ARRAY(J) = -    ARRAY(J)
      END DO
 
C
C     Check each item to see if it's been hit.
C
      ISORDV = .TRUE.
 
      DO I = 1, N
         ISORDV   =  ISORDV .AND. ( ARRAY(I) .LT. 0 )
         ARRAY(I) =          ABS  ( ARRAY(I)        )
      END DO
 
 
      RETURN
      END
