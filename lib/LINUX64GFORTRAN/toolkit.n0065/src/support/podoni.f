C$Procedure      PODONI ( Pod, offset and number, integer )
 
      SUBROUTINE PODONI ( POD, OFFSET, NUMBER )
      IMPLICIT NONE
 
C$ Abstract
C
C     Return the offset of the active group of a pod, and the number
C     of elements in the group.
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
C     PODS
C
C$ Keywords
C
C     ARRAYS
C     CELLS
C     PODS
C
C$ Declarations
 
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      INTEGER               POD    ( LBCELL : * )
      INTEGER               OFFSET
      INTEGER               NUMBER
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     POD        I   Pod.
C     OFFSET     O   Offset of the active group of POD.
C     NUMBER     O   Number of elements in active group.
C
C$ Detailed_Input
C
C     POD       is a pod.
C
C$ Detailed_Output
C
C     OFFSET    is the offset of the first item in the active group
C               of POD. That is, POD(OFFSET + 1) is the first element
C               of the active group.
C
C     NUMBER    is the number of items in the active group of POD.
C               That is, the active group is located in POD(OFFSET+1),
C               POD(OFFSET+2), ..., POD(OFFSET+NUMBER).
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the active group of the pod contains no elements,
C        NUMBER is zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C      PODBE (begin and end) and PODON (offset and number) provide
C      equivalent ways to access the elements of the active group
C      of a pod. Note that there is no way to access any group other
C      than the active group.
C
C$ Examples
C
C      PODBE is typically used to process the elements of the active
C      group of a pod one at a time, e.g.,
C
C         CALL PODBEI ( POD, BEGIN, END )
C
C         DO I = BEGIN, END
C            CALL PROCESS ( ..., POD(I), ... )
C         END DO
C
C      Note that if the elements are to be correlated with the elements
C      of other arrays, PODON may be more convenient:
C
C         CALL PODONI ( POD, OFFSET, N )
C
C         DO I = 1, N
C            CALL PROCESS ( ..., POD(OFFSET+I), ARRAY(I), ... )
C         END DO
C
C      PODON is also more convenient when the group is to be passed
C      to a subprogram as an array:
C
C         CALL SUBPROG ( ..., N, POD(OFFSET+1), ... )
C
C      For example, to sort the elements of the active group of
C      a pod,
C
C         CALL PODONI (      POD, OFFSET,    N )
C         CALL SHELLI ( N,   POD( OFFSET+1 )   )
C
C$ Restrictions
C
C     1) In any pod, only the active group should be accessed,
C        and its location should always be determined by PODBE
C        or PODON. Never assume that the active group begins
C        at POD(1).
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber     (JPL)
C     I.M. Underwood (JPL)
C
C$ Version
C
C-    Beta Version 1.0.0, 15-JUL-1989 (WLT) (IMU)
C
C
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
      INTEGER               CARDI
 
C
C     Local parameters
C
      INTEGER               GRPOFF
      PARAMETER           ( GRPOFF = -2 )
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PODONI' )
      END IF
 
C
C     The offset of the active group can be recovered directly from
C     the control area of the pod. The cardinality of the pod always
C     indicates the end of the active group.
C
      CALL     DCODEI ( POD(GRPOFF),   OFFSET )
      NUMBER = CARDI  ( POD       )  - OFFSET
 
      CALL CHKOUT ( 'PODONI' )
      RETURN
      END
