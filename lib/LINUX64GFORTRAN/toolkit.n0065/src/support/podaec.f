C$Procedure      PODAEC ( Pod, append elements, character )
 
      SUBROUTINE PODAEC ( ELEMS, N, POD )
      IMPLICIT NONE
 
C$ Abstract
C
C     Append elements to the active group of a pod.
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
 
      CHARACTER*(*)         ELEMS   (          * )
      INTEGER               N
      CHARACTER*(*)         POD     ( LBCELL : * )
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     ELEMS      I   New elements.
C     N          I   Number of new elements.
C     POD       I,O  Pod.
C
C$ Detailed_Input
C
C     ELEMS      contains elements to be appended to the active group
C                of POD.
C
C     N          is the number of elements in ELEMS.
C
C     POD        on input, is a pod.
C
C$ Detailed_Output
C
C     POD        on output, is the same pod, the active group of
C                which ends with the new elements.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C$
C     1) If N is not positive, the pod is not changed.
C
C     2) If there is insufficient room in the pod to append all
C        ofthe new elements, the pod is not changed, and the error
C        SPICE(TOOMANYPEAS) is signalled.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This is a slightly more general version of APPND, which appends
C     a single item to a cell or to the active group of a pod. PODAE
C     allows you to append several items with a single subroutine call.
C
C$ Examples
C
C     Elements can be appended to a POD by hand,
C
C        END = CARDC ( POD )
C
C        DO I = 1, N
C           POD(END+I) = ELEMS(I)
C        END DO
C
C        CALL SCARDC ( END + N, POD )
C
C     However, this is tedious, and it gets worse when you have to
C     check for possible overflow. PODAE accomplishes the same thing,
C
C        CALL PODAEC ( ELEMS, N, POD )
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
      INTEGER               CARDC
      INTEGER               SIZEC
 
      LOGICAL               RETURN
 
C
C     Local Variables
C
      INTEGER               END
      INTEGER               I
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PODAEC' )
      END IF
 
C
C     We can't append a non-positive number of items.
C
      IF ( N .LT. 1 ) THEN
         CALL CHKOUT ( 'PODAEC' )
         RETURN
      END IF
 
C
C     First see if there is room in the pod to append N elements.
C     If not, bail out.
C
      IF ( SIZEC ( POD ) .LT. CARDC ( POD ) + N ) THEN
 
         CALL SETMSG ( 'Cannot fit # elements into # spaces.' )
         CALL ERRINT ( '#', N                                 )
         CALL ERRINT ( '#', SIZEC(POD) - CARDC(POD)           )
         CALL SIGERR ( 'SPICE(TOOMANYPEAS)'                   )
 
C
C     There is ample room, so we find out where the end of the
C     active group is and simply loop through the individual
C     copies of ELEMS, adjusting the cardinality afterwards.
C     (Just like in $Examples, above.)
C
      ELSE
 
         END = CARDC ( POD )
 
         DO I = 1, N
            POD(END+I) = ELEMS(I)
         END DO
 
         CALL SCARDC ( END + N, POD )
 
      END IF
 
      CALL CHKOUT ( 'PODAEC' )
      RETURN
      END
