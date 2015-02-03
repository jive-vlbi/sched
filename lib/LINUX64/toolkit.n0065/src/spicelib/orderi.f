C$Procedure      ORDERI ( Order of an integer array )
 
      SUBROUTINE ORDERI ( ARRAY, NDIM, IORDER )
      IMPLICIT NONE

C$ Abstract
C
C     Determine the order of elements in an integer array.
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
C     ARRAY,  SORT
C
C$ Declarations
 
      INTEGER          ARRAY  ( * )
      INTEGER          NDIM
      INTEGER          IORDER ( * )
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     ARRAY      I    Input array.
C     NDIM       I    Dimension of ARRAY.
C     IORDER     O    Order vector for ARRAY.
C
C$ Detailed_Input
C
C     ARRAY       is the input array.
C
C     NDIM        is the number of elements in the input array.
C
C$ Detailed_Output
C
C     IORDER      is the order vector for the input array.
C                 IORDER(1) is the index of the smallest element
C                 of ARRAY; IORDER(2) is the index of the next
C                 smallest; and so on.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) A negative input dimension causes this routine to
C        leave the output order vector unchanged.
C
C     This routine is error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     ORDERI finds the index of the smallest element of the input
C     array. This becomes the first element of the order vector.
C     The process is repeated for the rest of the elements.
C
C     The order vector returned by ORDERI may be used by any of
C     the REORD routines to sort sets of related arrays, as shown
C     in the example below.
C
C$ Examples
C
C      In the following example, the ORDER and REORD routines are
C      used to sort four related arrays (containing the names,
C      masses, integer ID codes, and visual magnitudes for a group
C      of satellites). This is representative of the typical use of
C      these routines.
C
C         C
C         C     Sort the object arrays by ID code.
C         C
C               CALL ORDERI ( CODES,  N, IORDER )
C
C               CALL REORDC ( IORDER, N, NAMES  )
C               CALL REORDD ( IORDER, N, MASSES )
C               CALL REORDI ( IORDER, N, CODES  )
C               CALL REORDR ( IORDER, N, VMAGS  )
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
C     I.M. Underwood  (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.2, 23-MAR-2010 (NJB)
C
C        Header example was updated to show use of this routine.
C        Exceptions section was updated. Header sections were
C        re-ordered.
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
C     order of an integer array
C
C-&
 
 
 
 
C
C     Local variables
C
      INTEGER          GAP
      INTEGER          I
      INTEGER          J
      INTEGER          JG
 
 
C
C     Begin with the initial ordering.
C
      DO I = 1, NDIM
         IORDER(I) = I
      END DO
 
C
C     Find the smallest element, then the next smallest, and so on.
C     This uses the Shell Sort algorithm, but swaps the elements of
C     the order vector instead of the array itself.
C
      GAP = NDIM / 2
 
      DO WHILE ( GAP .GT. 0 )
 
         DO I = GAP+1, NDIM
 
            J = I - GAP
            DO WHILE ( J .GT. 0 )
               JG = J + GAP
 
               IF ( ARRAY(IORDER(J)) .LE. ARRAY(IORDER(JG)) ) THEN
                  J = 0
               ELSE
                  CALL SWAPI ( IORDER(J), IORDER(JG) )
               END IF
 
               J = J - GAP
            END DO
 
         END DO
 
         GAP = GAP / 2
 
      END DO
 
      RETURN
      END
 
