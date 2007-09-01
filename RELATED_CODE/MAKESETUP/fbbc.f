      SUBROUTINE FBBC( NCHAN, IBBC, IL, IF )
C
C     Expand the *bbc function in makesetup input.
C
      INCLUDE   'makeset.inc'
C
      INTEGER           NCHAN
      INTEGER           I, IF, IL, IBBC
      INTEGER           IA, BBC(32), LENO, ICHAN, NARG
      CHARACTER         OUTLINE*120, ARG(20)*20
C ---------------------------------------------------------------------
      write(*,*) 'fbbc: I was editing fbbc for reasons not remembered'
      write(*,*) '      investigate (ibbc new?)'
      stop
C
C     Insist on knowing NCHAN
C
      IF( NCHAN .EQ. 0 ) THEN
         WRITE(*,*) 'FBBC:  Must have NCHAN for bbc(.'
         STOP
      END IF
C
C     Read the parameters for the function.
C
      NARG = 20
      CALL ARGGET( IL, IF, IBBC+4, NARG, ARG )
      IF( NARG .NE. 5 ) THEN
         WRITE(*,*) 'FBBC:  Wrong number of arguments (', NARG,
     1         ') for BBC function.', IL, IF
         STOP
      END IF
      READ( ARG(1), * ) IA
      DO I = 1, 4
         READ( ARG(I+1), * ) BBC(I)
      END DO
C
C     Calculate the BBC values.
C
      DO I = 5, 32
         BBC(I) = BBC(MOD(I-1,4)+1) + IA * INT( (I - 1) / 4 )
      END DO
C
C     Check that bbc( was not at the start of a line.
C
      IF( IBBC .LE. 1 ) THEN
         WRITE(*,*) 'FUNCT: A bbc( at the start of a line?'
         WRITE(*,*) FILE(IF)
         STOP
      END IF
C
C     Construct the output line.
C    
      OUTLINE = FILETEXT(IL,IF)(1:IBBC-1)
      WRITE( OUTLINE(IBBC:IBBC+1), '( I2 )' ) BBC(1)
      LENO = IBBC + 1
      IF( NCHAN .GE. 2 ) THEN
         DO ICHAN = 2, NCHAN
            IF( LENO .LT. 75 ) THEN
               WRITE( OUTLINE(LENO+1:LENO+3), '( A, I2 )' )
     1                ',', BBC(ICHAN)
               LENO = LENO + 3
            ELSE
               OUTLINE(LENO+1:LENO+1) = ','
               FILETEXT(IL,IF) = OUTLINE
               CALL SHIFT( FILETEXT(1,IF), MLF, IL+1,  FILEL(IF), 1 )
               IL = IL + 1
               LENO = IBBC - 1
               OUTLINE = ' '
               WRITE( OUTLINE(LENO+1:LENO+2), '( I2 )' ) BBC(ICHAN)
               LENO = LENO + 2
            END IF
         END DO
         FILETEXT(IL,IF) = OUTLINE
      END IF
C
      RETURN
      END
