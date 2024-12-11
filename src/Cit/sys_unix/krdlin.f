C*KRDLIN -- read one line of text from input file (KEYIN) [UNIX]
C+
      SUBROUTINE KRDLIN(UNIT, REC, MAXREC, OUTC, DIALOG, REFLEC, IER)
      INTEGER UNIT, MAXREC, OUTC, IER
      LOGICAL DIALOG, REFLEC
      CHARACTER*(*) REC
C
C KEYIN: read one line of text from input file and perform DCL symbol
C substitution.
C
C Also turn any line feed characters into blanks to allow reading DOS
C format files.
C
C Protect against crazies who put more than 255 characters in a line.
C  Jan 2014 RCW.
C-----------------------------------------------------------------------
      INTEGER LEN1, NSUB, I
      CHARACTER*255 T
      CHARACTER*260 TL
      CHARACTER     CR*1, BLANK*1
C
      CR = CHAR( 13 )
      BLANK = ' '
      IF (UNIT.EQ.0) GOTO 340
   10 IF (DIALOG) WRITE (OUTC, '(''* '',$)')
      READ (UNIT, '(A)', END=340) TL
      MAXREC = LEN1(TL)
      IF( MAXREC .GT. 255 ) THEN
         WRITE( OUTC, '( A, /, A )' ) 
     1    'KRDLIN: ERROR - input line longer than 255 characters',
     2    TL
         STOP
      ELSE
         T = TL
      END IF
      IF (MAXREC.LT.1) GOTO 10
      IF (REFLEC) WRITE (OUTC, '(1X,A)') T(1:MAXREC)
      DO 20 I = 1, MAXREC
         IF( T(I:I) .EQ. CR ) T(I:I) = BLANK
   20 CONTINUE
      CALL SYMSUB(T(1:MAXREC), REC, MAXREC, NSUB)
      REC(MAXREC+1:) = ' '
      IF (NSUB.GT.0) WRITE (OUTC, '(1X,A)') REC(1:MAXREC)
      IER = 0
      RETURN
  340 IER = 1
      RETURN
      END
