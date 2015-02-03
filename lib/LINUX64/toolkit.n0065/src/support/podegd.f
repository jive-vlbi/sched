C$Procedure      PODEGD ( Pod, end group, double precision )
 
      SUBROUTINE PODEGD ( POD )
      IMPLICIT NONE
 
C$ Abstract
C
C     End the active group of a pod, restoring the previous group
C     unchanged.
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
C     ARRAY
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
C     POD       on output, is the same pod after the active group
C               has been removed and the previous group has been
C               restored unchanged.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the active group is the only group in the pod, the
C        cardinality of the POD is set to zero.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The active group of a pod may be removed by any of the
C     following routines: PODEG (end group), PODCG (close group),
C     or PODRG (replace group).
C
C     PODEG effectively returns the pod to its state before the
C     active group was created. The contents of the active group
C     are simply lost.
C
C     PODCG appends the contents of the active group to the previous
C     group to obtain the new active group, reducing the number of
C     groups in the pod by one.
C
C     PODRG also reduces the number of groups, but by replacing the
C     previous group with the active group, as though the previous
C     group had never existed.
C
C$ Examples
C
C     Let NAMES be a character POD containing the following groups:
C
C        Group 1:  NEWTON
C                  GALILEO
C                  KEPLER
C
C        Group 2:  EINSTEIN
C                  BOHR
C                  HEISENBERG
C
C        Group 3:  FEYNMAN
C                  BARDEEN
C
C     Following the call
C
C        CALL PODEGC ( NAMES )
C
C     the active group (Group 2) contains EINSTEIN, BOHR, and
C     HEISENBERG. Following the call
C
C        CALL PODCGC ( NAMES )
C
C     the active group (again, Group 2) contains EINSTEIN, BOHR,
C     HEISENBERG, FEYNMAN, and BARDEEN. Following the call
C
C        CALL PODRGC ( NAMES )
C
C     the active group (also Group 2) contains FEYNMAN and BARDEEN.
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
C-&
 
 
C
C     SPICELIB functions
C
      LOGICAL               RETURN
 
C
C     Local parameters
C
      INTEGER               GRPOFF
      PARAMETER           ( GRPOFF = -2 )
 
C
C     Local variables
C
      INTEGER               NUMBER
      INTEGER               OFFSET
 
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PODEGD' )
      END IF
 
C
C     At any given time, the offset of the active group is stored
C     in location GRPOFF of the control area, so POD(GRPOFF) tells
C     us the location of the element preceding the active group.
C
C     This element is a backward pointer, containing the offset of
C     the previous group; and so on, with turtles all the way down.
C     For example, consider a pod with three groups
C
C         G.  <10>
C         1.  Bob
C         2.  Carol
C         3.  Ted
C         4.  Alice
C         5.  <0>
C         6.  Fred
C         7.  Wilma
C         8.  Barney
C         9.  Bettey
C        10.  <5>
C        11.  Ricky
C        12.  Lucy
C        13.  Fred
C        14.  Ethel
C
C     When the second group was created, the offset of the first
C     group (zero) was appended to the pod; the location of this
C     offset became the offset for the second group. When the
C     third group was created, the offset of the second group (5)
C     was appended; the location of this offset became the offset for
C     the third group. The offset for the third group is located
C     in element GRPOFF.
C
C     To remove a group then, all that is necessary is to look at
C     element GRPOFF to get the offset of the current group; go to
C     that location to get the offset of the previous group; and
C     move that offset into element GRPOFF. The new cardinality,
C     of course, should be one less than the original offset (9).
C
C     If the pod contains only one group, it can't be removed, but
C     it can be emptied by setting the cardinality of the pod to
C     zero.
C
C
      CALL PODOND ( POD, OFFSET, NUMBER )
 
      IF ( OFFSET .EQ. 0 ) THEN
         CALL SCARDD ( 0, POD )
 
      ELSE
         POD(GRPOFF) = POD(OFFSET)
         CALL SCARDD ( OFFSET - 1, POD )
      END IF
 
      CALL CHKOUT ( 'PODEGD' )
      RETURN
      END
