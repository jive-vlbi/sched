 
C$Procedure  MINAI  ( Minimum element of array, integer )
 
      SUBROUTINE MINAI ( ARRAY, NDIM, MINVAL, LOC )
 
C$ Abstract
C
C     Locate the minimum element of an integer array.
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
 
      INTEGER               ARRAY ( * )
      INTEGER               NDIM
      INTEGER               MINVAL
      INTEGER               LOC
 
C$ Brief_I/O
C
C      VARIABLE  I/O           DESCRIPTION
C      --------  ---  -------------------------------------------------
C      ARRAY      I   Array.
C      NDIM       I   Number of elements in ARRAY.
C      MINVAL     O   Minimum value in ARRAY.
C      LOC        O   Location of MINVAL in ARRAY.
C
C$ Detailed_Input
C
C      ARRAY       is an arbitrary array.
C
C      NDIM        is the number of elements in ARRAY.
C
C$ Detailed_Output
C
C      MINVAL      is the value in array that is less than or equal
C                  to all other values in the array. If the array
C                  contains more than one element with this value,
C                  the first one is returned.
C
C      LOC         is the location of the minimum element. That is,
C                  MINVAL contains element ARRAY(LOC).
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C     1) If the array is empty (NDIM is less than one), LOC is zero, and
C        MINVAL is not changed.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C      None.
C
C$ Examples
C
C      Let array A contain the following elements.
C
C         A(1) = 16
C         A(2) =  4
C         A(3) = 32
C         A(4) = 64
C         A(5) =  2
C         A(6) =  8
C
C      Then following the call
C
C         CALL MINAI ( A, 6, MINVAL, LOC )
C
C      the values of MINVAL and LOC are 2 and 5 respectively.
C
C$ Restrictions
C
C      None
C
C$ Author_and_Institution
C
C      I.M. Underwood  (JPL)
C
C$ Literature_References
C
C      None
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
C     minimum element of integer array
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 1.0.1, 2-FEB-1989 (IMU)
C
C         Missing header sections completed.
C
C-&
 
 
C
C     Local variables
C
      INTEGER I
 
 
      IF (NDIM .LE. 0) THEN
         LOC = 0
         RETURN
      END IF
 
      MINVAL = ARRAY(1)
      LOC = 1
 
      DO I = 2, NDIM
         IF (ARRAY(I) .LT. MINVAL) THEN
            MINVAL = ARRAY(I)
            LOC = I
         END IF
      END DO
 
      RETURN
      END
