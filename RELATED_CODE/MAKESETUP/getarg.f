      SUBROUTINE ARGGET( IL, IF, I1, NWORDS, WORD )
C
C     Routine for reading function arguments for MAKESETUP. 
C
C     Adapted from GETLINE, 5 June 1997  RCW.
C
C     There are an unknown number of arguments separated by blanks.
C     they end with a ).
C
C     Input:
C        NWORDS  I              Size of the WORD and WLEN arrays.
C     Output:
C        WORD    C*(*)  Array   Array of arguments
C        NWORDS  I              Number of words found.
C
      INCLUDE        'makeset.inc'
C
      INTEGER        IL, IF, I1, LENT
      CHARACTER      WORD(*)*(*)
      INTEGER        NWORDS, MWORDS, CHAR1, CHARN, MCH, LEN1
      INTEGER        START, I
C ----------------------------------------------------------------------
C
      MWORDS = NWORDS
      LENT = LEN1( FILETEXT(IL,IF) )
C
      DO I = 1, MWORDS
         WORD(I) = ' '
      END DO
C
C     See how far down INLINE to look for words.
C
      MCH = I1 - 1 + INDEX( FILETEXT(IL,IF)(I1:LENT), ')' ) - 1
      IF( MCH .LT. I1 ) THEN
         WRITE(*,*) 'ARGGET:  Function with no arguments!'
         STOP
      END IF
C
C     If there is anything left in the input line, push it down
C     to the next line.
C
      IF( MCH .LT. LENT - 1 ) THEN
         CALL SHIFT( FILETEXT(1,IF), MLF, IL+1, FILEL(IF), 1 )
         FILETEXT(IL+1,IF) = FILETEXT(IL,IF)(MCH+2:LENT)
         FILETEXT(IL,IF)(MCH+2:LENT) = ' '
      END IF
C
C     Break arguments into words.
C
      START = 1
      NWORDS = 0
  130 CONTINUE
         CALL NXTWRD( FILETEXT(IL,IF)(I1:MCH), START, CHAR1, CHARN )
         IF( CHAR1 .EQ. 0 ) GO TO 140
         NWORDS = NWORDS + 1
C
         IF( NWORDS .GT. MWORDS ) THEN
            WRITE(*,*) ' ARGGET: Too many words found in line.  '
            STOP
         ELSE
            WORD(NWORDS) = FILETEXT(IL,IF)(CHAR1+I1-1:CHARN+I1-1)
            START = CHARN + 1
            IF( START .GT. MCH ) GO TO 140
         END IF
         GO TO 130
C
  140 CONTINUE
C
      RETURN
      END



