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
C     Round the windows for objects inward.
C
      SUBROUTINE RNDEM ( KERTYP, OBNAM,  OBJLIS, OBJSIZ, INTVAL, FILWIN,
     .                   WINSYM, WINPTR, WINVAL )
 
      IMPLICIT NONE
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )
 
      CHARACTER*(*)         KERTYP
      LOGICAL               OBNAM
      INTEGER               OBJLIS ( LBCELL : * )
      INTEGER               OBJSIZ
      DOUBLE PRECISION      INTVAL
      DOUBLE PRECISION      FILWIN ( LBCELL : * )
      CHARACTER*(*)         WINSYM ( LBCELL : * )
      INTEGER               WINPTR ( LBCELL : * )
      DOUBLE PRECISION      WINVAL ( LBCELL : * )
 
C
C     Spicelib Functions
C
      INTEGER               CARDD
C
C     Local Variables.
C
      INTEGER               I
      INTEGER               N
      INTEGER               OBJ  ( 2 )
      INTEGER               OBJN ( 2 )
      INTEGER               OBJECT ( 3 )
 
      LOGICAL               FOUND
      LOGICAL               FND
      LOGICAL               KEEP
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      CHARACTER*(WDSIZE)    OBJNAM
 
      DOUBLE PRECISION      Q
      DOUBLE PRECISION      R
      DOUBLE PRECISION      OFFSET
 
 
 
 
      CALL OBJNTH ( OBJLIS, 1, OBJ,   FOUND  )
 
      DO WHILE ( FOUND )
 
C
C        Look up the window associated with the current
C        object.  Round the window using the rounding
C        specified.
C
         CALL OBJGET ( OBJ,    OBJLIS,    OBJECT         )
         CALL MAKNAM ( OBJECT, OBJSIZ,    OBNAM, KERTYP, OBJNAM )
         CALL SYGETD ( OBJNAM, WINSYM,    WINPTR, WINVAL,
     .                 N,      FILWIN(1), FND )
         CALL SCARDD ( N,      FILWIN )
C
C        For each interval round it inward to the specified
C        level.
C
         IF ( INTVAL .EQ. 86400.0D0 ) THEN
            OFFSET = 43200.0D0
         ELSE
            OFFSET = 0.0D0
         END IF
 
         DO I = 1, N, 2
 
            FILWIN(I)   = FILWIN(I)   + OFFSET
            FILWIN(I+1) = FILWIN(I+1) + OFFSET
 
            CALL RMAIND ( FILWIN(I), INTVAL, Q, R )
 
            IF ( R .NE. 0.0D0 ) THEN
               FILWIN ( I ) =  MIN( INTVAL*(Q+1), FILWIN(I+1) )
            END IF
 
            CALL RMAIND ( FILWIN(I+1), INTVAL, Q, R )
            FILWIN ( I+1 ) = MAX ( INTVAL*Q, FILWIN(I) )
 
            FILWIN(I)   = FILWIN(I)   - OFFSET
            FILWIN(I+1) = FILWIN(I+1) - OFFSET
         END DO
C
C        Filter out any inteval that is less than the
C        specified rounding level.
C
         CALL WNFILD ( INTVAL - 0.5D0, FILWIN )
C
C        Put the window back into the table.
C
         N = CARDD ( FILWIN )
 
         IF ( N .GT. 0 ) THEN
 
            KEEP = .TRUE.
            CALL SYPUTD (OBJNAM, FILWIN(1), N,
     .                   WINSYM, WINPTR, WINVAL)
         ELSE
            KEEP = .FALSE.
         END IF
C
C        Get the next object.
C
         CALL OBJNXT ( OBJ, OBJLIS, OBJN, FOUND )
 
 
         IF ( .NOT. KEEP ) THEN
C
C           If we rounded away all the coverage, remove
C           this object.
C
            CALL OBJREM ( OBJ, OBJLIS )
 
         END IF
C
C        Move the next object into the current object.
C
         OBJ(1) = OBJN(1)
         OBJ(2) = OBJN(2)
 
 
      END DO
 
C
C     Now Compresss the object list.
C
      CALL OBJCMP ( OBJLIS )
 
      RETURN
      END
