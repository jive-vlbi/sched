C$Procedure      INSLAI (Insert at location in an integer array)
 
      SUBROUTINE INSLAI ( ELTS, NE, LOC, ARRAY, NA )
 
C$ Abstract
C
C      Insert one or more elements into an integer array at
C      the indicated location.
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
 
      INTEGER               ELTS  ( * )
      INTEGER               NE
      INTEGER               LOC
      INTEGER               ARRAY ( * )
      INTEGER               NA
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      ELTS       I   Elements to be inserted.
C      NE         I   Number of elements to be inserted.
C      LOC        I   Location of the first inserted element.
C      ARRAY     I/O  Input/output array.
C      NA        I/O  Number of elements in the input/output array.
C
C$ Detailed_Input
C
C      ELTS        contains one or more elements which are to be
C                  inserted into the input array.
C
C      NE          is the number of elements to be inserted.
C
C      LOC         is the location in the array at which the first
C                  element of ELTS is to be inserted. LOC must be
C                  within the interval [1, NA+1]. To append to
C                  ARRAY, set LOC equal to NA+1.
C
C      ARRAY       on input, is the original array.
C
C      NA          on input, is the number of elements in ARRAY.
C
C$ Detailed_Output
C
C      ARRAY       on output, is the original array with the elements
C                  of ELT inserted into positions LOC through LOC+NE-1.
C                  The original elements in these positions are moved
C                  back to make room for the inserted elements.
C
C      NA          on output, is the number of elements in ARRAY.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      The elements in positions LOC through LOC+NE-1 are moved back
C      by NE spaces to make room for the new elements, which are then
C      inserted into the vacated spaces.
C
C$ Examples
C
C      Let
C
C            ELTS(1) = 5        NA = 4      ARRAY(1) =  1
C            ELTS(2) = 6                    ARRAY(2) =  2
C            ELTS(3) = 7                    ARRAY(3) =  3
C                                           ARRAY(4) =  4
C
C      Then the call
C
C            CALL INSLAI ( ELTS, 3, 3, ARRAY, NA )
C
C      yields the following result:
C
C            NA = 7      ARRAY(1) = 1
C                        ARRAY(2) = 2
C                        ARRAY(3) = 5
C                        ARRAY(4) = 6
C                        ARRAY(5) = 7
C                        ARRAY(6) = 3
C                        ARRAY(7) = 4
C
C
C      The following calls to INSLAI signal errors.
C
C      CALL INSLAI ( ELTS, 3, -1, ARRAY, NA )
C      CALL INSLAI ( ELTS, 3,  6, ARRAY, NA )
C      CALL INSLAI ( ELTS, 3,  2, ARRAY, -1 )
C      CALL INSLAI ( ELTS, 3, -1, ARRAY, -1 )
C
C$ Restrictions
C
C      The array must be large enough to contain both the original
C      and the inserted elements.
C
C$ Exceptions
C
C     1) The dimension of the array is set equal to zero if its
C        input value is less than one.
C
C     2) If LOC is not in the interval [1, NA+1], the error
C        SPICE(INVALIDINDEX) is signalled.
C
C     3) If the number of elements to be inserted is less than one,
C        the array is not modified.
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
C     insert at location in an integer array
C
C-&
 
 
 
 
C$ Revisions
C
C-     Beta Version 2.0.0, 30-DEC-1988 (HAN)
C
C         If the location at which the elements are to be inserted is
C         not in the interval [1, NA+1], an error is signalled.
C         Locations not within that interval refer to non-exixtent
C         array elements. (To append to the array, the location
C         should be equal to NA+1.)
C
C         A negative dimension bug was fixed. The results of the
C         old version were unpredictable if the input array dimension
C         was negative. To avoid this problem the maximum of zero and
C         the input dimension becomes the dimension used by the
C         the routine. In this case, the only valid location at which
C         to insert is 1. If it is not 1, an error is signalled
C         when the location is checked.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      LOGICAL          RETURN
 
 
C
C     Other functions
C
      INTEGER          MAX
 
C
C     Local variables
C
      INTEGER          SIZE
      INTEGER          I
 
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'INSLAI' )
      END IF
 
 
C
C     Check the dimension of the array.
C
      SIZE = MAX ( 0, NA )
 
C
C     Make sure the location at which the elements are to be inserted
C     is not out of range. If it is, signal an error and bail out.
C
 
      IF ( ( LOC .LT. 1 ) .OR. ( LOC .GT. SIZE+1 ) ) THEN
 
         CALL SETMSG ( 'Location was *.'     )
         CALL ERRINT ( '*', LOC              )
         CALL SIGERR ( 'SPICE(INVALIDINDEX)' )
         CALL CHKOUT ( 'INSLAI' )
         RETURN
 
      END IF
 
 
C
C     If the number of elements to be inserted is greater than zero,
C     insert them. If not, do not modify the array.
C
      IF ( NE .GT. 0 ) THEN
 
C
C        Move the trailing elements back to make room for the new ones.
C
         DO I = SIZE, LOC, -1
            ARRAY(I+NE) = ARRAY(I)
         END DO
 
C
C        Now put the new elements in the vacated spaces.
C
         DO I = 1, NE
            ARRAY(LOC+I-1) = ELTS(I)
         END DO
 
C
C        Update the number of elements in the array.
C
         NA = SIZE + NE
 
 
      END IF
 
 
 
      CALL CHKOUT ( 'INSLAI' )
      RETURN
      END
