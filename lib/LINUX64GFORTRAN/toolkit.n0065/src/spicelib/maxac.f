 
C$Procedure  MAXAC  ( Maximum element of array, character )
 
      SUBROUTINE MAXAC ( ARRAY, NDIM, MAXVAL, LOC )
 
C$ Abstract
C
C     Locate the maximum element of a character array.
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
 
      CHARACTER*(*)   ARRAY ( * )
      INTEGER         NDIM
      CHARACTER*(*)   MAXVAL
      INTEGER         LOC
 
C$ Brief_I/O
C
C      VARIABLE  I/O           DESCRIPTION
C      --------  ---  -------------------------------------------------
C      ARRAY      I   Array.
C      NDIM       I   Number of elements in ARRAY.
C      MAXVAL     O   Maximum value in ARRAY.
C      LOC        O   Location of MAXVAL in ARRAY.
C
C$ Detailed_Input
C
C      ARRAY       is an arbitrary array.
C
C      NDIM        is the number of elements in ARRAY.
C
C$ Detailed_Output
C
C      MAXVAL      is the value in array that is greater than or equal
C                  to all other values in the array. If the array
C                  contains more than one element with this value,
C                  the first one is returned.
C
C                  Elements in character arrays are compared according
C                  to the ASCII collating sequence.
C
C      LOC         is the location of the maximum element. That is,
C                  MAXVAL contains element ARRAY(LOC).
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
C        MAXVAL is not changed.
C
C     2) If the declared length of MAXVAL is too short to contain the
C        entire element, the element is truncated. (The original value
C        can be accessed via LOC.)
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
C         A(1) = 'Einstein'
C         A(2) = 'Bohr'
C         A(3) = 'Feynman'
C         A(4) = 'Pauli'
C         A(5) = 'Bardeen'
C         A(6) = 'Dirac'
C
C      Then following the call
C
C         CALL MAXAC ( A, 6, MAXVAL, LOC )
C
C      the values of MAXVAL and LOC are 'Pauli' and 4 respectively.
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
C     maximum element of character array
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
 
 
      MAXVAL = ARRAY(1)
      LOC = 1
 
 
      DO I = 2, NDIM
         IF (LGT (ARRAY(I), MAXVAL)) THEN
            MAXVAL = ARRAY(I)
            LOC = I
         END IF
      END DO
 
 
      RETURN
      END
