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
C     Check an object id for sanity.
C
      SUBROUTINE OBJCHK ( NAME, OBJ, OBJLIS, OK )
C
C     This routine checks an object for sanity.  Moreover
C     as needed it refreshes the object in case the pointer
C     component no longer points to the object but the
C     object is still in the object list.
C
      IMPLICIT NONE
 
      INCLUDE              'object.inc'
 
      CHARACTER*(*)         NAME
      INTEGER               OBJ  ( 2 )
      INTEGER               OBJLIS ( LBCELL: * )
      LOGICAL               OK
 
C
C     SPICELIB Functions
C
      INTEGER               CARDI
      INTEGER               SIZEI
 
 
      INTEGER               MTASIZ
      INTEGER               Q
      INTEGER               REMAIN
      INTEGER               I
      INTEGER               SIZE
      INTEGER               ROOM
      INTEGER               OBJSIZ
 
 
C
C     The null object is always present.
C
      IF ( OBJ(1) .EQ. 0 .AND. OBJ(2) .EQ. NULL ) THEN
         OK = .TRUE.
         RETURN
      END IF
C
C     Check for corrupted objects.
C
      IF ( OBJ(1) .LT. 0 ) THEN
         OK = .FALSE.
         CALL CHKIN (  NAME  )
         CALL SETMSG( 'The object id supplied has an invalid '
     .   //           'pointer component. The pointer component '
     .   //           'must always be positive.  It had the '
     .   //           'value #.  This is probably the result of '
     .   //           'supplying an initialized object id. ' )
 
         CALL SIGERR( 'SPICE(BADOBJECTID)' )
         CALL CHKOUT(  NAME )
         RETURN
      END IF
 
      IF ( OBJ(2) .LT. 0 ) THEN
         OK = .FALSE.
         CALL CHKIN (  NAME  )
         CALL SETMSG( 'The object id supplied has an invalid '
     .   //           'identifier component. The identifier '
     .   //           'component must always be positive.  It '
     .   //           'had the value #.  This is probably the '
     .   //           'result of supplying an initialized object '
     .   //           'id. ' )
         CALL SIGERR( 'SPICE(BADOBJECTID)' )
         CALL CHKOUT(  NAME )
         RETURN
      END IF
 
      MTASIZ = OBJLIS ( RMPOBJ )
      OBJSIZ = MTASIZ - 1
      SIZE   = CARDI  ( OBJLIS )
      ROOM   = SIZEI  ( OBJLIS )
 
      CALL RMAINI ( OBJ(1), MTASIZ, Q, REMAIN )
 
      IF ( REMAIN .NE. 1 ) THEN
         OK = .FALSE.
         CALL CHKIN (  NAME  )
         CALL SETMSG( 'The pointer specified has an invalid '
     .   //           'value for the supplied object size.  The '
     .   //           'object size specified was #.  Given this '
     .   //           'object size the object pointer should be '
     .   //           'congruent to 1 MOD #.  The value of the '
     .   //           'object pointer was #. ' )
         CALL ERRINT( '#', OBJSIZ          )
         CALL ERRINT( '#', OBJSIZ+1        )
         CALL ERRINT( '#', OBJ(1)        )
         CALL SIGERR( 'SPICE(BADOBJECTID)' )
         CALL CHKOUT(  NAME )
         RETURN
      END IF
 
 
      IF (       OBJ(1) .GT. ROOM
     .     .AND. OBJ(2) .NE. NULL            ) THEN
         OK = .FALSE.
         CALL CHKIN ( NAME )
         CALL SETMSG( 'The pointer component of the object '
     .   //           'points outside of the object list.  The '
     .   //           'size of the object list is # and the '
     .   //           'value of the object pointer is #.  ')
         CALL ERRINT ( '#', SIZE     )
         CALL ERRINT ( '#', OBJ(1) )
         CALL SIGERR( 'SPICE(BADOBJECTID)' )
         CALL CHKOUT(  NAME )
         RETURN
      END IF
 
      IF ( OBJ(1) .GT. ROOM  .AND. OBJ(2) .EQ. NULL ) THEN
         OK = .TRUE.
         RETURN
      END IF
 
      IF ( OBJLIS(OBJ(1)) .NE. OBJ(2) ) THEN
C
C        It is possible that the object list has been compressed.
C        If so the object id may be out of date.  See if we
C        can find this object elsewhere in the list.
C
         DO I = 1, SIZE, MTASIZ
 
            IF ( OBJLIS(I) .EQ. OBJ(2) ) THEN
C
C              Refressh the object pointer value.
C
               OBJ(1) = I
               OK     = .TRUE.
               RETURN
            END IF
 
         END DO
C
C        If still, here there's a problem.  This cannot be
C        a legitimate object.
C
         OK = .FALSE.
         CALL CHKIN ( NAME )
         CALL SETMSG( 'The pointer component and the identifier '
     .   //           'component of the object are not '
     .   //           'compatible. The pointer points to the '
     .   //           'identifier value #.  The identifier of '
     .   //           'the object given is: #.  You may have a '
     .   //           '"stale" object.' )
         CALL ERRINT( '#', OBJLIS(OBJ(1)) )
         CALL ERRINT( '#', OBJ(2)         )
         CALL SIGERR( 'SPICE(BADOBJECTID)' )
         CALL CHKOUT(  NAME )
         RETURN
      END IF
C
C     If you get to this point, all obvious checks have passed.
C     This object is deemed to be a good one.
C
      OK = .TRUE.
      RETURN
 
      END
