C$Procedure      REMLAD (Remove elements from a double precision array)
 
      SUBROUTINE REMLAD ( NE, LOC, ARRAY, NA )
 
C$ Abstract
C
C      Remove one or more elements from a double precision array at the
C      indicated location.
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
C      ARRAY,  ASSIGNMENT
C
C$ Declarations
 
      INTEGER          NE
      INTEGER          LOC
      DOUBLE PRECISION ARRAY ( * )
      INTEGER          NA
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      NE         I   Number of elements to be removed.
C      LOC        I   Location of the first removed element.
C      ARRAY     I/O  Input/output array.
C      NA        I/O  Number of elements in the input/output array.
C
C$ Detailed_Input
C
C      NE          is the number of elements to be removed.
C
C      LOC         is the location in the array at which the first
C                  element is to be removed.
C
C      ARRAY       on input, is the original array.
C
C      NA          on input, is the number of elements in ARRAY.
C
C$ Detailed_Output
C
C      ARRAY       on output, is the original array with elements
C                  LOC through LOC+NE-1 removed. Succeeding elements
C                  are moved forward to fill the vacated spaces.
C
C      NA          on output, is the number of elements in ARRAY.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      The elements in positions LOC through LOC+NE-1 are overwritten
C      as the elements beginning at LOC+NE are moved back.
C
C$ Examples
C
C      Let
C
C            NA = 7      ARRAY(1) = 1.0D0
C                        ARRAY(2) = 2.0D0
C                        ARRAY(3) = 3.0D0
C                        ARRAY(4) = 4.0D0
C                        ARRAY(5) = 5.0D0
C                        ARRAY(6) = 6.0D0
C                        ARRAY(7) = 7.0D0
C
C      Then the call
C
C            CALL REMLAD ( 3, 3, ARRAY, NA )
C
C      yields the following result:
C
C            NA = 4      ARRAY(1) = 1.0D0
C                        ARRAY(2) = 2.0D0
C                        ARRAY(3) = 6.0D0
C                        ARRAY(4) = 7.0D0
C
C
C      The following calls would signal errors:
C
C      CALL REMLAD ( 3,  1, ARRAY, -1 )
C      CALL REMLAD ( 3, -1, ARRAY,  7 )
C      CALL REMLAD ( 3,  6, ARRAY,  7 )
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      1) If LOC is not in the interval [1, NA], the error
C         SPICE(INVALIDINDEX) is signalled.
C
C      2) If the number of elements to be removed is greater than the
C         number of elements that can be removed, the error
C         SPICE(NONEXISTELEMENTS) is signalled.
C
C      3) If NE is less than one, the array is not modified.
C
C      4) If NA is less than one, any location is invalid, and the
C         error SPICE(INVALIDINDEX) is signalled.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      H.A. Neilan     (JPL)
C      I.M. Underwood  (JPL)
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
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     remove elements from a d.p. array
C
C-&
 
 
 
C$ Revisions
C
C-     Beta Version 2.0.0, 1-JAN-1989 (HAN)
C
C         Code was added to handle the following exceptinoal
C         inputs.
C
C         If the dimension of the array is less than one, any
C         value of LOC is invalid. The old verison did not check
C         the dimension of the array, and as a result, its output
C         was unpredictable.
C
C         If the location at which the elements are to be removed is
C         not in the interval [1, NA], an error is signalled.
C         Locations not within that interval refer to non-existent
C         array elements. The old routine did not signal an error.
C         It just returned the original array.
C
C         If the number of elements to be removed is greater than the
C         number of elements can be removed, an error is signalled.
C         In the old version, only those elements that could be
C         removed were removed, and no error was signalled.
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      LOGICAL          RETURN
 
 
C
C     Local variables
C
      INTEGER          I
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'REMLAD' )
      END IF
 
 
C
C     If LOC does not point to an actual element, signal an error and
C     check out. If the dimension of the array is less than one, any
C     value of LOC is invalid, and an error is signalled.
C
 
      IF ( ( LOC .LT. 1 ) .OR. ( LOC .GT. NA ) ) THEN
 
         CALL SETMSG ( 'Location was *.'     )
         CALL ERRINT ( '*', LOC              )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)' )
         CALL CHKOUT ( 'REMLAD' )
         RETURN
 
C
C     Don't try to remove non-existent elements.
C
 
      ELSE IF ( NE .GT. NA-LOC+1 ) THEN
 
         CALL SETMSG ( 'Trying to remove non-existent elements.' )
         CALL SIGERR ( 'SPICE(NONEXISTELEMENTS)' )
         CALL CHKOUT ( 'REMLAD' )
         RETURN
 
 
C
C     If there are elements to be removed, remove them. Otherwise,
C     do not modify the array.
C
 
      ELSE IF ( NE .GT. 0 ) THEN
 
C
C        Move the elements forward.
C
         DO I = LOC, NA - NE
            ARRAY(I) = ARRAY(I+NE)
         END DO
 
 
C
C        Update the number of elements in the array.
C
         NA = NA - NE
 
 
      END IF
 
 
 
      CALL CHKOUT ( 'REMLAD' )
      RETURN
      END
