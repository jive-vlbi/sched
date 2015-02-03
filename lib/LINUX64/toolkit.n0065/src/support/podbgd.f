C$Procedure      PODBGD ( Pod, begin group, double precision )
 
      SUBROUTINE PODBGD ( POD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Begin a new (empty) group within a pod.
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
 
      DOUBLE PRECISION      POD ( LBCELL : * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     POD       I,O  Pod.
C
C$ Detailed_Input
C
C     POD       on input, is an arbitrary pod.
C
C$ Detailed_Output
C
C     POD       on output, is the same pod, in which the active
C               group has been sealed, and a new active group
C               (containing no elements) begun.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If POD does not have sufficient free space to create a new
C        group with room for at least one element, the pod is not
C        changed, and the error SPICE(TOOMANYPEAS) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     There are two ways to create a new group within a pod.
C     PODBG (begin group) seals the current contents of the pod,
C     and creates a new active group containing no elements.
C     PODDG (duplicate group) also seals the current contents
C     of the pod, but places a copy of the previous active
C     group into the new active group.
C
C     In both cases, the active group and all previous groups are
C     unavailable so long as the new group exists.
C
C     The active group of a pod may be removed by any of the
C     following routines: PODEG (end group), PODCG (close group),
C     or PODRG (replace group).
C
C$ Examples
C
C     Let the active group of POD be located in elements 21
C     through 40. Then following the call
C
C        CALL PODBGD ( POD )
C
C     the active group is located in elements 42 through 41.
C     In other words, element 41 has been appropriated by the
C     pod itself, and the active group is empty.
C
C     However, following the call
C
C        CALL PODDG ( POD )
C
C     the active group is located in elements 42 through 61,
C     and contains the same elements as the previous active
C     group.
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
      INTEGER               CARDD
      INTEGER               SIZED
 
C
C     Local parameters
C
      INTEGER               CRDLOC
      PARAMETER           ( CRDLOC =  0 )
 
      INTEGER               GRPOFF
      PARAMETER           ( GRPOFF = -2 )
 
C
C     Local variables
C
      INTEGER               HAVE
      INTEGER               NEED
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PODBGD' )
      END IF
 
C
C     There must be at least two spaces at the end of the pod:
C     one for bookkeeping, and one for the first element of
C     the new group.
C
      HAVE = SIZED ( POD )
      NEED = CARDD ( POD ) + 2
 
      IF ( HAVE .LT. NEED ) THEN
         CALL SIGERR ( 'SPICE(TOOMANYPEAS)' )
         CALL CHKOUT ( 'PODBGD'             )
         RETURN
      END IF
 
C
C     Okay: go ahead and create the group. The offset of the active
C     group is stored in the first empty slot of the pod; when the
C     new group is removed, this will be reinstated as the offset of
C     the active group.
C
      POD(CARDD(POD) + 1) = POD(GRPOFF)
 
C
C     This requires the cardinality of the pod to increase by one.
C
      CALL SCARDD ( CARDD(POD) + 1, POD )
 
C
C     Surprise! The new cardinality is the same as the offset of
C     the new group!
C
      POD(GRPOFF) = POD(CRDLOC)
 
      CALL CHKOUT ( 'PODBGD' )
      RETURN
      END
