      SUBROUTINE FUNCT
C
C     Implement functions in MAKESETUP.
C
      INCLUDE     'makeset.inc'
C
      INTEGER           IF, IL, IEQ, INCHAN, NCHAN, N1, N2
      INTEGER           IBBC, ISIDE, INBW, IFOFF, IFREQ
      CHARACTER         LINE*120
      REAL              BW
C      DOUBLE PRECISION  GETNUM
C ---------------------------------------------------------------------
C     Loop over files.
C
C
      DO IF = 1, NF
         NCHAN = 0
         BW = 0
C
C        Look through the file for the variables that might be needed.
C
         DO IL = 1, FILEL(IF)
C
C           Watch for the NCHAN specification.
C
            LINE = FILETEXT(IL,IF)
            CALL UPCASE( LINE )
            INCHAN = INDEX( LINE, 'NCHAN' )
            IF( INCHAN .GT. 0 ) THEN
C
C              Remove the next equal sign if there is one.
C
               IEQ = INDEX( LINE(INCHAN+5:120), '=' ) + INCHAN + 4
               IF( IEQ .NE. 0 ) LINE(IEQ:IEQ) = ' '
C
C              Get the next word and turn it into a number.
C
               CALL NXTWRD( LINE, INCHAN+5, N1, N2 )
               READ( LINE(N1:N2), * ) NCHAN
C               NCHAN = GETNUM( LINE, N1, N2 )
C
            END IF
C
C           Do the same for the BBFILTER specification.
C
            INBW = INDEX( LINE, 'BBFILTER' )
            IF( INBW .GT. 0 ) THEN
               IEQ = INDEX( LINE(INBW+8:120), '=' ) + INBW + 7
               IF( IEQ .NE. 0 ) LINE(IEQ:IEQ) = ' '
               CALL NXTWRD( LINE, INBW+8, N1, N2 )
               READ( LINE(N1:N2), * ) BW
C               BW = GETNUM( LINE, N1, N2 )
            END IF
C
         END DO
C
C        Loop over lines expanding the functions.
C        Use GOTO because FILEL(IF) could change if some functions 
C        expand to more than one line.
C
         IL = 0
  100    CONTINUE
            IL = IL + 1
C
C           Look for the bbc function.
C
            IBBC = INDEX( FILETEXT(IL,IF), 'bbc(' )
            IF( IBBC .NE. 0 ) THEN

            END IF
C
C           Look for the side function.
C
            ISIDE = INDEX( FILETEXT(IL,IF), 'side(' )
            IF( ISIDE .NE. 0 ) THEN
               CALL FSIDE( NCHAN, ISIDE, IL, IF )
            END IF
C
C           Look for the foff function.
C
            IFOFF = INDEX( FILETEXT(IL,IF), 'foff(' )
            IF( IFOFF .NE. 0 ) THEN
               CALL FFOFF( NCHAN, BW, IFOFF, IL, IF )
            END IF
C
C           Look for the freq function.
C
            IFREQ = INDEX( FILETEXT(IL,IF), 'freq(' )
            IF( IFREQ .NE. 0 ) THEN
               CALL FFREQ( NCHAN, IFREQ, IL, IF )
            END IF
C
C           Go back for the next line of the file.
C
            IF( IL .LT. FILEL(IF) ) GO TO 100
C
      END DO
C
      RETURN
      END


