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
C     Get the id of the next object in the list.
C
      SUBROUTINE OBJNXT ( OBJ, OBJLIS, OBJN, FOUND )
 
      IMPLICIT NONE
      INCLUDE              'object.inc'
 
      INTEGER               OBJ    ( 2 )
      INTEGER               OBJLIS ( LBCELL : * )
      INTEGER               OBJN   ( 2 )
      LOGICAL               FOUND
 
C
C     Spicelib Function
C
      INTEGER               CARDI
 
C
C     Local Variables
C
      INTEGER               I
      INTEGER               MTASIZ
      INTEGER               SIZE
      INTEGER               PTR
 
      LOGICAL               OK
 
C
C     Perform some sanity checks on the OBJ. Note OBJCHK
C     does all the required checking in and checking out.
C
      CALL OBJCHK ( 'OBJNXT', OBJ, OBJLIS, OK )
 
      SIZE   = CARDI ( OBJLIS )
      MTASIZ = OBJLIS( RMPOBJ )
 
      IF ( .NOT. OK ) THEN
         RETURN
      END IF
 
      PTR  = OBJ ( 1 )
 
      IF ( PTR .EQ. 0 ) THEN
         PTR = 1
      END IF
 
      I = PTR + MTASIZ
 
      DO WHILE ( I .LT. SIZE )
C
C        If this object is a non-null object, then we're done
C        looking.
C
         IF ( OBJLIS(I) .NE. NULL ) THEN
            OBJN(1) = I
            OBJN(2) = OBJLIS(I)
            FOUND   = .TRUE.
            RETURN
         END IF
C
C        Not done yet.  Look at the next object.
C
         I = I + MTASIZ
 
      END DO
 
C
C     If you get to this point, there wasn't a next object.
C     point at the next "available" slot and set the identifier
C     to null.
C
      FOUND     = .FALSE.
      OBJN(1) =  I
      OBJN(2) =  NULL
 
      RETURN
      END
