C$Procedure      PODIED ( Pod, insert elements, double precision )
 
      SUBROUTINE PODIED ( ELEMS, N, LOC, POD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Insert elements at a specified location within the active group
C     of a pod.
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
 
      DOUBLE PRECISION      ELEMS   (          * )
      INTEGER               N
      INTEGER               LOC
      DOUBLE PRECISION      POD     ( LBCELL : * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ELEMS      I   New elements.
C     N          I   Number of new elements.
C     LOC        I   Location at which elements are to be inserted.
C     POD       I,O  Pod.
C
C$ Detailed_Input
C
C     ELEMS      contains elements to be inserted into the active
C                group of POD.
C
C     N          is the number of elements in ELEMS.
C
C     LOC        is the location (within the active group of the pod)
C                at which the new elements are to be inserted. The new
C                elements are inserted in front of the element currently
C                at this location.
C
C     POD        on input, is a pod.
C
C$ Detailed_Output
C
C     POD        on output, is the same pod, the active group of
C                which contains the new elements.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C      1) If N is not positive, the pod is not changed.
C
C     2) If there is insufficient room in the pod to insert all
C        of the new elements, the pod is not changed, and the error
C        SPICE(TOOMANYPEAS) is signalled.
C
C     3) If the location specified for location is not in the range
C        [1,NC+1], where NC is the number of elements in the active
C        group of the pod, the pod is not changed, and the error
C        SPICE(BADPODLOCATION) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine allows you to insert elements into the active
C     group of a pod without having to worry about checking for
C     overflow beforehand, or updating the cardinality afterwards.
C
C$ Examples
C
C     Elements can be inserted into the active group of a pod
C     by hand,
C
C        CALL PODOND ( POD, OFFSET, NUMBER )
C        END = OFFSET + NUMBER
C
C        CALL INSLAD ( ELEMS, N, OFFSET + LOC, POD(1), CUREND )
C        CALL SCARDD ( CUREND,                 POD            )
C
C     However, this is tedious, and it gets worse when you have to
C     check for possible overflow. PODIE accomplishes the same thing,
C
C        CALL PODIED ( ELEMS, N, LOC, POD )
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
      INTEGER               SIZED
      INTEGER               CARDD
C
C     Local variables
C
      INTEGER               NUMBER
      INTEGER               OFFSET
      INTEGER               END
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PODIED' )
      END IF
 
C
C     Three things can go `wrong':
C
C        1) No items to insert.
C
C        2) Too many items to insert.
C
C        3) No place to insert them.
C
      CALL PODOND ( POD, OFFSET, NUMBER )
 
      IF ( N .LT. 1 ) THEN
 
         CALL CHKOUT ( 'PODIED' )
         RETURN
 
      ELSE IF ( CARDD(POD) + N .GT. SIZED(POD) ) THEN
 
         CALL SETMSG ( 'Cannot fit # elements into # spaces.' )
         CALL ERRINT ( '#', N                                 )
         CALL ERRINT ( '#', SIZED(POD) - CARDD(POD)           )
         CALL SIGERR ( 'SPICE(TOOMANYPEAS)'                   )
         CALL CHKOUT ( 'PODIED'                               )
         RETURN
 
      ELSE IF ( LOC .LT. 1  .OR.  LOC .GT. NUMBER + 1 ) THEN
 
         CALL SETMSG ( 'Location (#) must be in the range [1,#].' )
         CALL ERRINT ( '#',  LOC                                  )
         CALL ERRINT ( '#',  NUMBER + 1                           )
         CALL SIGERR ( 'SPICE(BADPODLOCATION)'                    )
         CALL CHKOUT ( 'PODIED'                                   )
         RETURN
 
      END IF
 
C
C     In theory, we are home free. The rest looks just like the
C     code in $Examples, above.
C
      END = OFFSET + NUMBER
 
      CALL INSLAD ( ELEMS, N, OFFSET + LOC, POD(1), END )
      CALL SCARDD ( END,                    POD         )
 
      CALL CHKOUT ( 'PODIED' )
      RETURN
      END
