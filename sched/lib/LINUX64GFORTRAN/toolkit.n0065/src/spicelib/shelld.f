C$Procedure      SHELLD ( Shell sort a double precision array )
 
      SUBROUTINE SHELLD ( NDIM, ARRAY )
 
C$ Abstract
C
C      Sort a double precision array using the Shell Sort algorithm.
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
 
      INTEGER            NDIM
      DOUBLE PRECISION   ARRAY ( * )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      NDIM       I   Dimension of the array.
C      ARRAY     I/O  The array.
C
C$ Detailed_Input
C
C      NDIM        is the number of elements in the array to be sorted.
C
C      ARRAY       on input, is the array to be sorted.
C
C$ Detailed_Output
C
C      ARRAY       on output, contains the same elements, sorted
C                  in increasing order. The actual sorting is done
C                  in place in ARRAY.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      The Shell Sort Algorithm is well known.
C
C$ Examples
C
C      Let ARRAY contain the following elements:
C
C            99.D0
C            33.D0
C            55.D0
C            44.D0
C           -77.D0
C            66.D0
C
C      Then after a call to SHELLD, the array would be ordered as
C      follows:
C
C           -77.D0
C            33.D0
C            44.D0
C            55.D0
C            66.D0
C            99.D0
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
C     shell sort a d.p. array
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
C     This is a straightforward implementation of the Shell Sort
C     algorithm.
C
      GAP = NDIM / 2
 
      DO WHILE ( GAP .GT. 0 )
 
         DO I = GAP+1, NDIM
 
            J = I - GAP
            DO WHILE ( J .GT. 0 )
               JG = J + GAP
 
               IF ( ARRAY(J) .LE. ARRAY(JG) ) THEN
                  J = 0
               ELSE
                  CALL SWAPD ( ARRAY(J), ARRAY(JG) )
               END IF
 
               J = J - GAP
            END DO
 
         END DO
 
         GAP = GAP / 2
 
      END DO
 
      RETURN
      END
