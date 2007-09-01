      SUBROUTINE GETDAT
C
C     Read in the file, breaking it into modes and lines.
C
      INCLUDE  'makeset.inc'
C
      INTEGER     LEN1, NWORDS, WLEN(30), ICH, NL
      CHARACTER   INLINE*120, WORD(30)*30
C ---------------------------------------------------------------------
C     Initialize some things:
C
      NF = 0
      NM = 0
      NUMLINE = 0
C
C     Set up for testing on nonstandard file.
C
C      WRITE(*,'(A)') ' Input master file:'
C      READ(*,'(A)')  INFILE
      INFILE = 'Master_setups'
      OPEN(UNIT=10,FILE=INFILE,FORM='FORMATTED',STATUS='OLD')
      WRITE(*,*) 'Opened: ', INFILE(1:LEN1(INFILE))
C
C     Loop over the input lines, copying the modes and files to where
C     they belong.
C
  100 CONTINUE
         NWORDS = MW
         CALL GETLINE( WORD, WLEN, NWORDS, INLINE, 10, 6 )
         IF( NWORDS .EQ. -1 ) GO TO 500
         NUMLINE = NUMLINE + 1
C
C        Get a mode definition.
C
         IF( WORD(1) .EQ. '#mode' ) THEN
            NM = NM + 1
            IF( NM .GT. MM ) THEN
               WRITE(*,*) ' Too many modes.  Fix MAKESETUP.'
               STOP
            END IF
            MODE(NM) = WORD(2)
C
C           Read the mode lines.
C
            NL = 0
  200       CONTINUE
               NL = NL + 1
               IF( NL .GT. MLM ) THEN
                  WRITE(*,*) ' Too many lines in mode '//MODE(NM)
                  STOP
               END IF
               MODETEXT(NL,NM) = ' '
               NWORDS = MW
               CALL GETLINE( WORD, WLEN, NWORDS, MODETEXT(NL,NM),10,6)
               IF( NWORDS .EQ. -1 ) THEN
                  WRITE(*,*) 'ERROR: Input ended in middle of mode '//
     1                'definition - stop.'
                  STOP
               ELSE IF( NWORDS .EQ. 0 ) THEN
                  MODETEXT(NL,NM) = ' ' 
                  GO TO 200
               END IF
               NUMLINE = NUMLINE + 1
C
C              End of mode if #end is last word of line.
C
               IF( WORD(NWORDS) .NE. '#end' ) THEN
                   GO TO 200
               ELSE
                   IF( NWORDS .EQ. 1 ) THEN
                      MODEL(NM) = NL - 1
                   ELSE
                      ICH = INDEX( MODETEXT(NL,NM), '#end' ) - 1
                      MODETEXT(NL,NM) = MODETEXT(NL,NM)(1:ICH) // ' '
                      MODEL(NM) = NL
                   END IF
               END IF
         ELSE IF( WORD(1) .EQ. '#file' ) THEN
C
C           Get a file definition.
C
            NF = NF + 1
            IF( NF .GT. MF ) THEN
               WRITE(*,*) ' Too many files.  Fix MAKESETUP.'
               STOP
            END IF
            FILE(NF) = WORD(2)
            NL = 0
C
C           Pick anything on the #file line and make a line in
C           FILETEXT with it.  Don't worry about any details except
C           a #end.
C
            IF( NWORDS .GE. 3 ) THEN
               ICH = INDEX( INLINE, WORD(2)(1:WLEN(2)) ) + WLEN(2)
               NL = NL + 1
               FILETEXT(NL,NF) = INLINE(ICH:LEN1(INLINE))
C
C              If it ended with #end, finish up the #file.
C
               IF( WORD(NWORDS) .EQ. '#end' ) THEN
                  FILEL(NF) = NL
                  ICH = LEN1( FILETEXT(NL,NF) )
                  FILETEXT(NL,NF)(ICH-3:ICH) = ' '
                  GO TO 100
               END IF
            END IF
C
C           Read the file lines.
C
  300       CONTINUE
               NL = NL + 1
               IF( NL .GT. MLF ) THEN
                  WRITE(*,*) ' Too many lines in file '//FILE(NF)
                  STOP
               END IF
               NWORDS = MW
               CALL GETLINE( WORD, WLEN, NWORDS, FILETEXT(NL,NF),10,6)
               IF( NWORDS .EQ. -1 ) THEN
                  WRITE(*,*) 'ERROR: Input ended in middle of file '//
     1                'definition - stop.'
                  STOP
               ELSE IF( NWORDS .EQ. 0 ) THEN
                  FILETEXT(NL,NM) = ' ' 
                  GO TO 300
               END IF
               NUMLINE = NUMLINE + 1
C
C              Look for the last line.  If not there, go back for more.
C
               IF( WORD(NWORDS) .NE. '#end' ) THEN
                   GO TO 300
               ELSE
C
C                  If it is the last line, wrap up.
C
                   IF( NWORDS .EQ. 1 ) THEN
                      FILEL(NF) = NL - 1
                   ELSE
                      ICH = INDEX( FILETEXT(NL,NF), '#end' ) - 1
                      FILETEXT(NL,NF) = FILETEXT(NL,NF)(1:ICH)//' '
                      FILEL(NF) = NL
                   END IF
               END IF
         END IF
C
C        Either done with a definition or not a useful line.
C
         GO TO 100
C
C     Jump here when out of input data.
C
  500 CONTINUE
C
      WRITE( *, '( A, I4, A, I4, A, I5, A )' )
     1      'GETDAT:  Read ', NM, ' #modes and ', NF, ' #files in ',
     2      NUMLINE, ' total lines.'
C
      RETURN
      END

