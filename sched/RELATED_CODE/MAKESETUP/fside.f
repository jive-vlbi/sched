      SUBROUTINE FSIDE( NCHAN, ISIDE, IL, IF )
C
C     Expand the *side function in makesetup input.
C
      INCLUDE   'makeset.inc'
C
      INTEGER           I, IF, IL, NCHAN, ISIDE
      INTEGER           LENO, NARG
      CHARACTER         OUTLINE*120, SIDE(32)*1, ARG(20)*20
C ---------------------------------------------------------------------
C     Insist on knowing NCHAN
C
      IF( NCHAN .EQ. 0 ) THEN
         WRITE(*,*) 'FSIDE:  Must have NCHAN for side(.'
         STOP
      END IF
C
C     Read the parameters for the function.
C
      NARG = 20
      CALL ARGGET( IL, IF, ISIDE+5, NARG, ARG )
      IF( NARG .NE. 4 ) THEN
         WRITE(*,*) 'FSIDE:  Wrong number of arguments (', NARG,
     1         ') for SIDE function.', IL, IF
         STOP
      END IF
      DO I = 1, 4
         SIDE(I) = ARG(I)(1:1)
      END DO
C
C     Fill out all the sideband values.
C
      DO I = 5, 32
         SIDE(I) = SIDE(MOD(I-1,4)+1)
      END DO
C
C     Check that side( was not at the start of a line.
C
      IF( ISIDE .LE. 1 ) THEN
         WRITE(*,*) 'FUNCT: A side( at the start of a line?'
         WRITE(*,*) FILE(IF)
         STOP
      END IF
C
C     Construct the output line.
C    
      OUTLINE = FILETEXT(IL,IF)(1:ISIDE-1)
      WRITE( OUTLINE(ISIDE:ISIDE+1), '( A )' ) SIDE(1)
      LENO = ISIDE
      IF( NCHAN .LE. 16 ) THEN
         WRITE( OUTLINE(ISIDE:120), '( 16( 1X, A, :, '','' ) )' ) 
     1             (SIDE(I),I=1,NCHAN)
         FILETEXT(IL,IF) = OUTLINE
      ELSE
         WRITE( OUTLINE(ISIDE:120), '( 16( 1X, A, :, '','' ), '','' )' ) 
     1             (SIDE(I),I=1,16)
         FILETEXT(IL,IF) = OUTLINE
         CALL SHIFT( FILETEXT(1,IF), MLF, IL+1,  FILEL(IF), 1 )
         IL = IL + 1
         OUTLINE = ' '
         WRITE( OUTLINE(ISIDE:120), '( 16( 1X, A, :, '','' ) )' ) 
     1             (SIDE(I),I=17,NCHAN)
         FILETEXT(IL,IF) = OUTLINE
      END IF
C
      RETURN
      END







