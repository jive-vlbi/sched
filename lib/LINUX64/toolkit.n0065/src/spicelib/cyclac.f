C$Procedure      CYCLAC ( Cycle the elements of a character array )
 
      SUBROUTINE CYCLAC ( ARRAY, NELT, DIR, NCYCLE, OUT )
 
C$ Abstract
C
C      Cycle the elements of a character array forward or backward.
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
C      None.
C
C$ Keywords
C
C      ARRAY
C
C$ Declarations
 
      CHARACTER*(*)         ARRAY    ( * )
      INTEGER               NELT
      CHARACTER*1           DIR
      INTEGER               NCYCLE
      CHARACTER*(*)         OUT      ( * )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      ARRAY      I   Input array.
C      NELT       I   Number of elements.
C      DIR        I   Direction to cycle: 'F' or 'B'.
C      NCYCLE     I   Number of times to cycle.
C      OUT        O   Cycled array.
C
C$ Detailed_Input
C
C      ARRAY       is the array to be cycled.
C
C      NELT        is the number of elements in the input array.
C
C      DIR         is the direction in which the elements in the
C                  array are to be cycled.
C
C                        'F' or 'f'  to cycle forward.
C                        'B' or 'b'  to cycle backward.
C
C      NCYCLE      is the number of times the elements in the array
C                  are to be cycled.
C
C$ Detailed_Output
C
C      OUT         is the input array after it has been cycled.
C                  OUT may overwrite ARRAY.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      1) If the value of DIR is not recognized, the error
C         SPICE(INVALIDDIRECTION) is signalled.
C
C      2) If NELT is less than 1, the output array is not modified.
C
C      3) If NCYCLE is negative, the array is cycled NCYCLE times in
C         the opposite direction of DIR.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      An array is cycled when its contents are shifted forward or
C      backward by one place. An element pushed off one end of the
C      array is brought around to the other end of the array instead
C      of disappearing.
C
C$ Examples
C
C      Let the integer array A contain the following elements.
C
C            A(1) = 'apple'
C            A(2) = 'bear'
C            A(3) = 'cake'
C            A(4) = 'dragon'
C
C      Cycling A forward once yields the array
C
C            A(1) = 'dragon'
C            A(2) = 'apple'
C            A(3) = 'bear'
C            A(4) = 'cake'
C
C      Cycling A backward once yields the array
C
C            A(1) = 'bear'
C            A(2) = 'cake'
C            A(3) = 'dragon'
C            A(4) = 'apple'
C
C      Cycling by any multiple of the number of elements in the array
C      yields the same array.
C
C$ Restrictions
C
C      The memory used for the output array must be identical to or
C      disjoint from the memory used for the input array.
C
C      That is:
C
C           CALL CYCLAC ( ARRAY, NELT, DIR, NCYCLE, ARRAY )
C
C      will produce correct results, while
C
C           CALL CYCLAC ( ARRAY, NELT-3, DIR, NCYCLE, ARRAY(4) )
C
C      will produce garbage.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Version
C
C-     SPICELIB Version 1.0.2, 18-MAY-2010 (BVS)
C
C        Removed "C$" marker from text in the header.
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
C     cycle the elements of a character array
C
C-&
 
 
 
 
C$ Revisions
C
C-     Beta Version 2.0.0, 16-JAN-1989 (HAN)
C
C         Error handling was added to detect an invalid value for
C         the cycling direction. If the direction is not recognized
C         the error SPICE(INVALIDDIRECTION) is signalled and the
C         output array is not modified. (The routine used to copy the
C         input array into the output array if the direction was not
C         recognized.)
C
C         The "Exceptions" section was filled out in more detail.
C
C-&
 
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL          RETURN
      INTEGER          GCD
      INTEGER          NBWID
 
C
C     Local variables
C
      INTEGER          OUTLEN
      INTEGER          WIDEST
      INTEGER          LIMIT
 
      CHARACTER*1      LAST
      CHARACTER*1      TEMP
 
      INTEGER          C
      INTEGER          G
      INTEGER          I
      INTEGER          J
      INTEGER          K
      INTEGER          L
      INTEGER          M
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CYCLAC' )
      END IF
 
C
C     Don't even screw around if there are no elements in the array.
C
      IF ( NELT .LT. 1 ) THEN
         CALL CHKOUT ( 'CYCLAC' )
         RETURN
      END IF
 
C
C     A backward cycle is the same as a forward cycle by the opposite
C     of NCYCLE.  Moreover a cycle by K is the same as a cycle by
C     K + m*N for any integer m.  Thus we compute the value of the
C     minimum forward right cycle that is equivalent to the inputs.
C     If the cycling direction is not recognized, signal an error.
C
      IF (          ( DIR .EQ. 'B' )
     .         .OR. ( DIR .EQ. 'b' ) ) THEN
 
         K = MOD ( - NCYCLE, NELT )
 
      ELSE IF (      ( DIR .EQ. 'F' )
     .          .OR. ( DIR .EQ. 'f' ) ) THEN
 
         K = MOD (   NCYCLE, NELT )
 
      ELSE
 
         CALL SETMSG ( 'Cycling direction was *.' )
         CALL ERRCH  ( '*', DIR )
         CALL SIGERR ( 'SPICE(INVALIDDIRECTION)'  )
         CALL CHKOUT ( 'CYCLAC' )
         RETURN
 
      END IF
 
 
 
      IF ( K .LT. 0 ) THEN
 
         K = K + NELT
 
      ELSE IF ( K .EQ. 0 ) THEN
 
         CALL MOVEC  ( ARRAY, NELT, OUT )
         CALL CHKOUT ( 'CYCLAC' )
         RETURN
 
      END IF
 
 
 
 
 
C
C     The algorithm used to cycle arrays is identical to the one used
C     to cycle character strings in CYCLEC. We won't repeat the (rather
C     lengthy) description here.
C
C     The character version of CYCLAx differs from the other
C     versions in that a single character is cycled at a time. That
C     is, the first trip through the outermost loop cycles the first
C     characters of the array elements; the second trip cycles the
C     second characters; and so on. This allows the same algorithm to
C     be used for all the routines. The local storage required is just
C     a couple of characters.
C
 
C
C     Don't swap the ends of strings if they're just blank padded.
C     And don't overwrite the elements of the output array, if they
C     happen to be shorter thAn those in the input array.
C
      OUTLEN = LEN   ( OUT(1)         )
      WIDEST = NBWID ( ARRAY,  NELT   )
      LIMIT  = MIN   ( OUTLEN, WIDEST )
 
C
C     The greatest common divisor need only be computed once.
C
      G = GCD ( K, NELT )
      M = NELT / G
 
C
C     To make this a non-character routine, remove all references to C.
C
      DO C = 1, LIMIT
 
         DO I = 1, G
 
            L    = I
            LAST = ARRAY(L)(C:C)
 
            DO J = 1, M
 
               L = L + K
 
               IF ( L .GT. NELT ) THEN
                  L = L - NELT
               END IF
 
               TEMP        = ARRAY(L)(C:C)
               OUT(L)(C:C) = LAST
               LAST        = TEMP
 
            END DO
 
         END DO
 
      END DO
 
C
C     If needed, pad the output array with blanks.
C
      IF ( OUTLEN .GT. LIMIT ) THEN
 
         DO I = 1, NELT
            OUT(I)(LIMIT+1: ) = ' '
         END DO
 
      END IF
 
 
      CALL CHKOUT ( 'CYCLAC' )
      RETURN
      END
