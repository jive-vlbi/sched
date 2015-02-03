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
C
C     Add an object to an object list.
C
      SUBROUTINE OBJADD ( OBJECT, OBJLIS, OBJ )
 
      IMPLICIT NONE
      INCLUDE              'object.inc'
 
      INTEGER               OBJECT ( * )
      INTEGER               OBJLIS ( LBCELL : * )
      INTEGER               OBJ    ( * )
 
C
C     SPICELIB Functions
C
      INTEGER               SIZEI
      INTEGER               CARDI
 
C
C     Local Variables
C
      INTEGER               ALLCTD
      INTEGER               COUNT
      INTEGER               I
      INTEGER               J
      INTEGER               MTASIZ
      INTEGER               NEXT
      INTEGER               SIZE
      INTEGER               USED
 
 
      SIZE   = SIZEI  ( OBJLIS )
      ALLCTD = CARDI  ( OBJLIS )
      COUNT  = OBJLIS ( NACTIV )
      MTASIZ = OBJLIS ( RMPOBJ )
      USED   = COUNT*MTASIZ
C
C     Make sure there is room in the object list to hold
C     another object.
C
      IF ( USED .GE. SIZE ) THEN
         CALL CHKIN  ( 'OBJADD')
         CALL SETMSG ( 'The object list already contains # '
     .   //            'objects. It is full. You will need to '
     .   //            'remove an object or increase the '
     .   //            'declared size of the object before '
     .   //            'another object can be added. ' )
         CALL ERRINT ( '#', COUNT )
         CALL SIGERR ( 'SPICE(OBJECTLISTFULL)' )
         CALL CHKOUT ( 'OBJADD' )
         RETURN
      END IF
 
C
C     Ok. We've got room. Construct the idcode for the next
C     object.  And fill in the details in OBJLIS to indicate
C     this ID has been used.
C
 
      NEXT          = OBJLIS( LSTID  ) + 1
      OBJLIS(LSTID) = NEXT
C
C     Do the easy step first. If the objects are already
C     packed together, we add this object to the end
C     of the list.
C
      IF ( ALLCTD .EQ. USED ) THEN
 
         I         = USED + 1
 
         OBJ(1)    = I
         OBJ(2)    = NEXT
 
         OBJLIS(I) = NEXT
 
         DO J = 1, MTASIZ - 1
            I = I + 1
            OBJLIS(I) = OBJECT(J)
         END DO
C
C        Adjust the cardinality of the object list.
C
         CALL SCARDI ( USED + MTASIZ, OBJLIS )
         OBJLIS ( NACTIV )  = OBJLIS ( NACTIV ) + 1
         RETURN
 
 
      ELSE
C
C        There's room available in the object list.  Find
C        a NULL position and use that space for this object.
C
         I = 1
 
         DO WHILE ( I .LT. ALLCTD )
 
            IF ( OBJLIS(I) .EQ. NULL ) THEN
 
               OBJLIS(I) = NEXT
               OBJ(1)    = I
               OBJ(2)    = NEXT
 
               DO J = 1, MTASIZ-1
                  I = I + 1
                  OBJLIS(I) = OBJECT(J)
               END DO
 
               OBJLIS ( NACTIV )  = OBJLIS ( NACTIV ) + 1
 
               RETURN
 
            END IF
 
            I = I + MTASIZ
 
         END DO
 
      END IF
C
C     You are never supposed to be able to reach this
C     point in the code.  If you do, there's a bug somewhere.
C
      CALL CHKIN  ( 'OBJADD' )
      CALL SETMSG ( 'A serious error has occurred.  The object '
     .//            'list is supposed to have room available in '
     .//            'it, but no free areas were located.  The '
     .//            'most likely cause is that the object list '
     .//            'has been inadvertantly corrupted by some '
     .//            'portion of your software.  The other '
     .//            'possibility is that there is a bug in the '
     .//            'SPICE code. ' )
 
      CALL SIGERR ( 'SPICE(BUG)' )
      CALL CHKOUT ( 'OBJADD'     )
      RETURN
 
      END
