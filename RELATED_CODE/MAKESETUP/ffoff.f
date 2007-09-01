      SUBROUTINE FFOFF( NCHAN, BW, IFOFF, IL, IF )
C
C     Expand the *foff function in makesetup input.
C
      INCLUDE   'makeset.inc'
C
      INTEGER           NCHAN
      INTEGER           I, IF, IL, IFOFF, IM
      INTEGER           LENO, ICHAN, NREP, INCR, NARG
      LOGICAL           DOSX
      REAL              FOFF(32), BW, FOFFMID
      CHARACTER         OUTLINE*120, ARG(20)*20
C ---------------------------------------------------------------------
C
C     Insist on knowing NCHAN
C
      IF( NCHAN .EQ. 0 ) THEN
         WRITE(*,*) 'FFOFF:  Must have NCHAN for *foff.'
         STOP
      END IF
C
C     Insist on knowing BW
C
      IF( BW .EQ. 0.0 ) THEN
         WRITE(*,*) 'FFOFF:  Must have BBFILTER for *foff.'
         STOP
      END IF
C
C     Read the parameters for the function.
C
      NARG = 20
      CALL ARGGET( IL, IF, IFOFF+5, NARG, ARG )
      IF( NARG .NE. 3 .AND. NARG .NE. 4 ) THEN
         WRITE(*,*) 'FFOFF:  Wrong number of arguments (', NARG,
     1         ') for FOFF function.', IL, IF
         STOP
      END IF
      READ( ARG(1), * ) NREP
      READ( ARG(2), * ) INCR
      READ( ARG(3), * ) FOFF(1)
      IF( NARG .EQ. 4 ) THEN
         DOSX = .TRUE.
         READ( ARG(4), * ) FOFFMID
      ELSE
         DOSX = .FALSE.
      END IF
C
C     Calculate the frequency offset values.
C
      IF( DOSX ) THEN
         DO I = 2, NCHAN / 2
            FOFF(I) = FOFF(1) + BW * INCR * ((I-1)/NREP)
         END DO
         IM = 1 + NCHAN / 2
         FOFF(IM) = FOFFMID
         DO I = IM + 1, NCHAN
            FOFF(I) = FOFF(IM) + BW * INCR * ((I-IM)/NREP)
         END DO
      ELSE
         DO I = 2, NCHAN
            FOFF(I) = FOFF(1) + BW * INCR * ((I-1)/NREP)
         END DO
      END IF
C
C     Check that foff( was not at the start of a line.
C
      IF( IFOFF .LE. 1 ) THEN
         WRITE(*,*) 'FUNCT: A foff( at the start of a line?'
         WRITE(*,*) FILE(IF)
         STOP
      END IF
C
C     Construct the output line.
C    
      OUTLINE = FILETEXT(IL,IF)(1:IFOFF-1)
      WRITE( OUTLINE(IFOFF:IFOFF+6), '(F7.2)' ) FOFF(1)
      LENO = IFOFF + 6
      IF( NCHAN .GE. 2 ) THEN
         DO ICHAN = 2, NCHAN
C
C           If the line will be too long, break it.
C           Then add to the output line
C
            IF( LENO .GT. 70 ) THEN
               OUTLINE(LENO+1:LENO+1) = ','
               FILETEXT(IL,IF) = OUTLINE
               CALL SHIFT( FILETEXT(1,IF), MLF, IL+1,  FILEL(IF), 1 )
               IL = IL + 1
               LENO = IFOFF - 1
               OUTLINE = ' '
               WRITE( OUTLINE(LENO+1:LENO+8), '( F7.2 )' )
     1                FOFF(ICHAN)
               LENO = LENO + 7
            ELSE
C
C              For shorter lines, add to the output.
C
               WRITE( OUTLINE(LENO+1:LENO+8), '( A, F7.2 )' )
     1                ',', FOFF(ICHAN)
               LENO = LENO + 8
            END IF
         END DO
         FILETEXT(IL,IF) = OUTLINE
      END IF
C
  999 CONTINUE
      RETURN
      END

