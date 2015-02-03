C$Procedure      PODRED ( Pod, remove elements, double precision )
 
      SUBROUTINE PODRED ( N, LOC, POD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Remove elements beginning at a specified location within the
C     active group of a pod.
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
 
      INTEGER               N
      INTEGER               LOC
      DOUBLE PRECISION      POD     ( LBCELL : * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     N          I   Number of elements to remove.
C     LOC        I   Location of first element to be removed.
C     POD       I,O  Pod.
C
C$ Detailed_Input
C
C     N          is the number of elements to be removed from the
C                active group of POD.
C
C     LOC        is the location (within the active group of the pod)
C                of the first element to be removed.
C
C     POD        on input, is a pod.
C
C$ Detailed_Output
C
C     POD        on output, is the same pod, the active group of
C                which contains the elements preceding and following
C                the removed elements.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If N is not positive, the pod is not changed.
C
C     2) If the location of the last element to be removed (LOC+N-1)
C        is greater than the number of elements in the active group,
C        the pod is not changed, and the error SPICE(NOTENOUGHPEAS)
C        is signalled.
C
C     3) If the location specified for location is not in the range
C        [1,NC], where NC is the number of elements in the active
C        group of the pod, the pod is not changed, and the error
C        SPICE(BADPODLOCATION) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows you to remove elements from the active
C     group of a pod without having to worry about checking for
C     impossible requests beforehand, or updating the cardinality
C     afterwards.
C
C$ Examples
C
C     Elements can be removed from the active group of a pod
C     by hand,
C
C        CALL PODOND ( POD, OFFSET, NUMBER )
C        END = OFFSET + NUMBER
C
C        CALL REMLAD ( N,   OFFSET + LOC, POD(1), END )
C        CALL SCARDD ( END,               POD         )
C
C     However, this is tedious, and it gets worse when you have to
C     check for impossible requests. PODRE accomplishes the same thing,
C
C        CALL PODIED ( N, LOC, POD )
C
C     more simply, and with error-handling built in.
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
 
C
C     Local variables
C
      INTEGER               END
      INTEGER               NUMBER
      INTEGER               OFFSET
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PODRED' )
      END IF
 
 
C
C     Three things can go `wrong':
C
C        1) No items to remove.
C
C        2) Too many items to remove.
C
C        3) No place to remove them from.
C
      CALL PODOND ( POD, OFFSET, NUMBER )
 
      IF ( N .LT. 1 ) THEN
 
         CALL CHKOUT ( 'PODRED')
         RETURN
 
      ELSE IF ( LOC + N - 1 .GT. NUMBER ) THEN
 
         CALL SETMSG ( 'LOC = #; N = #; there are only # elements.' )
         CALL ERRINT ( '#', LOC                                     )
         CALL ERRINT ( '#', N                                       )
         CALL ERRINT ( '#', NUMBER                                  )
         CALL SIGERR ( 'SPICE(NOTENOUGHPEAS)'                       )
         CALL CHKOUT ( 'PODRED'                                     )
         RETURN
 
      ELSE IF ( LOC .LT. 1   .OR.   LOC .GT. NUMBER ) THEN
 
         CALL SETMSG ( 'Location (#) must be in the range [1,#].' )
         CALL ERRINT ( '#',  LOC                                  )
         CALL ERRINT ( '#',  NUMBER                               )
         CALL SIGERR ( 'SPICE(BADPODLOCATION)'                    )
         CALL CHKOUT ( 'PODRED'                                   )
         RETURN
 
      END IF
 
C
C     No problem. This is just like $Examples, above.
C
      END = OFFSET + NUMBER
 
      CALL REMLAD ( N,   OFFSET + LOC, POD(1), END )
      CALL SCARDD ( END,               POD         )
 
      CALL CHKOUT ( 'PODRED' )
      RETURN
      END
