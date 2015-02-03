C$Procedure        BSRCHD ( Binary search for double precision value )
 
      INTEGER FUNCTION BSRCHD ( VALUE, NDIM, ARRAY )
 
C$ Abstract
C
C      Do a binary search for a given value within a DOUBLE PRECISION
C      array, assumed to be in increasing order. Return the index of
C      the matching array entry, or zero if the key value is not found.
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
C      ARRAY,  SEARCH
C
C$ Declarations
 
      DOUBLE PRECISION   VALUE
      INTEGER            NDIM
      DOUBLE PRECISION   ARRAY ( * )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      VALUE      I   Value to find in ARRAY.
C      NDIM       I   Dimension of ARRAY.
C      ARRAY      I   Array to be searched.
C      BSRCHD     O   Index of VALUE in ARRAY. (Zero if not found.)
C
C$ Detailed_Input
C
C      VALUE       is the value to be found in the input array.
C
C      NDIM        is the number of elements in the input array.
C
C      ARRAY       is the array to be searched. The elements in
C                  ARRAY are assumed to sorted in increasing order.
C
C$ Detailed_Output
C
C      BSRCHD      is the index of the input value in the input array.
C                  If ARRAY does not contain VALUE, BSRCHD is zero.
C
C                  If ARRAY contains more than one occurrence of VALUE,
C                  BSRCHD may point to any of the occurrences.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      A binary search is implemented on the input array. If an
C      element of the array is found to match the input value, the
C      index of that element is returned. If no matching element
C      is found, zero is returned.
C
C
C$ Examples
C
C      Let ARRAY contain the following elements:
C
C              -11.D0
C                0.D0
C               22.491D0
C              750.0D0
C
C      Then
C
C            BSRCHD ( -11.D0,    4, ARRAY )    = 1
C            BSRCHD (  22.491D0, 4, ARRAY )    = 3
C            BSRCHD ( 751.D0,    4, ARRAY )    = 0
C
C$ Restrictions
C
C      ARRAY is assumed to be sorted in increasing order. If this
C      condition is not met, the results of BSRCHD are unpredictable.
C
C$ Exceptions
C
C      Error free.
C
C      If NDIM < 1 the value of the function is zero.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      I.M. Underwood  (JPL)
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
C-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     binary search for d.p. value
C
C-&
 
 
 
C$ Revisions
C
C-    Beta Version 1.1.0, 8-JAN-1989 (IMU)
C
C        Now works for all values of NDIM.
C
C-&
 
 
 
C
C     Local variables
C
      INTEGER          LEFT
      INTEGER          RIGHT
      INTEGER          I
 
 
 
C
C     Set the initial bounds for the search area.
C
      LEFT  = 1
      RIGHT = NDIM
 
      DO WHILE ( LEFT .LE. RIGHT )
 
C
C        Check the middle element.
C
         I = (LEFT+RIGHT)/2
 
C
C        If the middle element matches, return its location.
C
         IF ( VALUE .EQ. ARRAY(I) ) THEN
            BSRCHD = I
            RETURN
 
C
C        Otherwise narrow the search area.
C
         ELSE IF ( VALUE .LT. ARRAY(I) ) THEN
            RIGHT = I-1
         ELSE
            LEFT  = I+1
         END IF
 
      END DO
 
C
C     If the search area is empty, return zero.
C
      BSRCHD = 0
 
      RETURN
      END
