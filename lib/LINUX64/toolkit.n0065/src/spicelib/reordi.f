C$Procedure      REORDI ( Reorder an integer array )
 
      SUBROUTINE REORDI ( IORDER, NDIM, ARRAY )
 
C$ Abstract
C
C      Re-order the elements of an integer array according to
C      a given order vector.
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
C      ARRAY,  SORT
C
C$ Declarations
 
      INTEGER          IORDER ( * )
      INTEGER          NDIM
      INTEGER          ARRAY  ( * )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      IORDER     I   Order vector to be used to re-order ARRAY.
C      NDIM       I   Dimension of ARRAY.
C      ARRAY     I/O  Array to be re-ordered.
C
C$ Detailed_Input
C
C      IORDER      is the order vector to be used to re-order the input
C                  array. The first element of IORDER is the index of
C                  the first item of the re-ordered array, and so on.
C
C                  Note that the order imposed by REORDI is not the
C                  same order that would be imposed by a sorting
C                  routine. In general, the order vector will have
C                  been created (by one of the ORDER routines) for
C                  a related array, as illustrated in the example below.
C
C      NDIM        is the number of elements in the input array.
C
C      ARRAY       on input, is an array containing some number of
C                  elements in unspecified order.
C
C$ Detailed_Output
C
C      ARRAY       on output, is the same array, with the elements
C                  in re-ordered as specified by IORDER.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      REORDI uses a cyclical algorithm to re-order the elements of
C      the array in place. After re-ordering, element IORDER(1) of
C      the input array is the first element of the output array,
C      element IORDER(2) is the input array is the second element of
C      the output array, and so on.
C
C      The order vector used by REORDI is typically created for
C      a related array by one of the ORDER routines, as shown in
C      the example below.
C
C$ Examples
C
C      In the following example, the ORDER and REORD routines are
C      used to sort four related arrays (containing the names,
C      masses, integer ID codes, and visual magnitudes for a group
C      of satellites). This is representative of the typical use of
C      these routines.
C
C            C
C            C     Sort the object arrays by name.
C            C
C                  CALL ORDERC ( NAMES, N, IORDER )
C
C                  CALL REORDC ( IORDER, N, NAMES )
C                  CALL REORDD ( IORDER, N, MASSES )
C                  CALL REORDI ( IORDER, N, CODES )
C                  CALL REORDR ( IORDER, N, VMAGS )
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      Error free.
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
C     reorder an integer array
C
C-&
 
 
 
 
C
C     Local variables
C
      INTEGER          START
      INTEGER          INDEX
      INTEGER          HOLD
 
      INTEGER          TEMP
 
 
C
C     If the array doesn't have at least two elements, don't bother.
C
      IF ( NDIM .LT. 2 ) THEN
           RETURN
      END IF
 
C
C     START is the position in the order vector that begins the
C     current cycle. When all the switches have been made, START
C     will point to the end of the order vector.
C
      START = 1
 
      DO WHILE ( START .LT. NDIM )
 
C
C        Begin with the element of input vector specified by
C        IORDER(START). Move it to the correct position in the
C        array, after saving the element it replaces to TEMP.
C        HOLD indicates the position of the array element to
C        be moved to its new position. After the element has
C        been moved, HOLD indicates the position of an available
C        space within the array.
C
         INDEX = START
         TEMP  = ARRAY(INDEX)
         HOLD  = IORDER(INDEX)
 
C
C        As each slot in the output array is filled in, the sign
C        of the corresponding element in the order vector is changed
C        from positive to negative. This way, we know which elements
C        have already been ordered when looking for the beginning of
C        the next cycle.
C
C        Keep going until HOLD points to the first array element
C        moved during the current cycle. This ends the cycle.
C
         DO WHILE ( HOLD .NE. START )
            ARRAY(INDEX)  =  ARRAY(HOLD)
            INDEX         =  HOLD
            HOLD          =  IORDER(HOLD)
            IORDER(INDEX) = -IORDER(INDEX)
         END DO
 
C
C        The last element in the cycle is restored from TEMP.
C
         ARRAY(INDEX) =  TEMP
         IORDER(HOLD) = -IORDER(HOLD)
 
C
C        Begin the next cycle at the next element in the order
C        vector with a positive sign. (That is, the next one
C        that hasn't been moved.)
C
         DO WHILE ( IORDER(START) .LT. 0  .AND.  START .LT. NDIM )
            START = START + 1
         END DO
 
      END DO
 
C
C     Restore the original signs of the elements of the order vector,
C     in case the vector is to be used again with another array.
C
      DO INDEX = 1, NDIM
        IORDER(INDEX) = IABS ( IORDER(INDEX) )
      END DO
 
      RETURN
      END
 
