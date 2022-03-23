      SUBROUTINE FFREQ( NCHAN, IFREQ, IL, IF )
C
C     Routine for MAKESETUP to implement the FREQ function to 
C     expand frequencies.  In most cases, it will simply give
C     one value which is the single argument.  But if there are
C     two arguments, it will give nchan/2 of the first argument
C     and then nchan/2 of the second argument.  This is for s/x
C     observations.
C
      INCLUDE     'makeset.inc'
C
      INTEGER     NCHAN, IFREQ, IL, IF, ICHAN
      INTEGER     NARG, LENO
      REAL        FREQ1, FREQ2, FREQW
      CHARACTER   ARG(20)*20, OUTLINE*120
C --------------------------------------------------------------------
C
C     Insist on knowing NCHAN
C
      IF( NCHAN .EQ. 0 ) THEN
         WRITE(*,*) 'FFREQ:  Must have NCHAN for freq(.'
         STOP
      END IF
C
C     Check that bbc( was not at the start of a line.
C
      IF( IFREQ .LE. 1 ) THEN
         WRITE(*,*) 'FUNCT: A freq( at the start of a line?'
         WRITE(*,*) FILE(IF)
         STOP
      END IF
C
C     Read the parameters for the function.
C
      NARG = 20
      CALL ARGGET( IL, IF, IFREQ+5, NARG, ARG )
      IF( NARG .NE. 1 .AND. NARG .NE. 2 ) THEN
         WRITE(*,*) 'FFREQ:  Wrong number of arguments (', NARG,
     1         ') for FREQ function.', IL, IF
         STOP
      END IF
      READ( ARG(1), * ) FREQ1
      IF( NARG .EQ. 2 ) READ( ARG(2), * ) FREQ2
C
C     First deal with the simple case of one output number.
C
      IF( NARG .EQ. 1 .OR. NCHAN .EQ. 1 ) THEN
         OUTLINE = FILETEXT(IL,IF)(1:IFREQ-1)
         WRITE( OUTLINE(IFREQ:120), '( F9.2 )' ) FREQ1
         FILETEXT(IL,IF) = OUTLINE
      ELSE
C
C        Now the rather more complicated case of two reference
C        frequencies with half the channels in each.
C
         OUTLINE = FILETEXT(IL,IF)(1:IFREQ-1)
         WRITE( OUTLINE(IFREQ:IFREQ+8), '( F9.2 )' ) FREQ1
         LENO = IFREQ + 8
         DO ICHAN = 2, NCHAN
C
C           Set the frequency to use for this channel.
C
            IF( ICHAN .LE. NCHAN/2 ) THEN
               FREQW = FREQ1
            ELSE
               FREQW = FREQ2
            END IF
C
C           Can just add it to this line.
C
            IF( LENO .LT. 70 ) THEN
               WRITE( OUTLINE(LENO+1:LENO+10), '( A, F9.2 )' )
     1                ',', FREQW
               LENO = LENO + 10
            ELSE
C
C              Need new line.
C
               OUTLINE(LENO+1:LENO+1) = ','
               FILETEXT(IL,IF) = OUTLINE
               CALL SHIFT( FILETEXT(1,IF), MLF, IL+1,  FILEL(IF), 1 )
               IL = IL + 1
               LENO = IFREQ - 1
               OUTLINE = ' '
               WRITE( OUTLINE(LENO+1:LENO+9), '( F9.2 )' ) FREQW
               LENO = LENO + 9
            END IF
         END DO
         FILETEXT(IL,IF) = OUTLINE
C
      END IF
C
      RETURN
      END
