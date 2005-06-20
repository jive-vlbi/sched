C*KEYOUT -- SAVE/SHOW routine for KEYIN
C+
      SUBROUTINE KEYOUT(KEYS,VALUES,N,P)
      INTEGER N, P
      DOUBLE PRECISION KEYS(N), VALUES(N)
C
C SAVE/SHOW routine for KEYIN
C - list parameters and values to unit P
C-----------------------------------------------------------------------
      DOUBLE PRECISION BLANK8
      LOGICAL KEYCHK
      INTEGER I, J, K, LEN1, IVAL, LB
      CHARACTER*1 CONTIN
      CHARACTER*10 WORD
      CHARACTER*80 TEXT, DUMMYC
      DATA LB/0/
C
      CALL KPACK( '        ', BLANK8 )
C
      I = 1
      CONTIN = ' '
  100 IF (I.GT.N) RETURN
      WRITE (WORD,'(A8)') KEYS(I)
      IF (CONTIN.EQ.',') WORD = ' '
      J = 1
  110 IF (I+J.LE.N .AND. KEYS(I+J).EQ.BLANK8) THEN
          J = J+1
          GOTO 110
      END IF
      WRITE (TEXT,'(10A8)') (VALUES(K),K=I,I+MIN(J-1,9))
      CONTIN = ' '
      IF (I.LT.N .AND. KEYS(I).EQ.KEYS(I+J)) CONTIN = ','
      IF (WORD.NE.' ') CALL KEYPUT(WORD//' =',P,LB)
      IF (J.GT.1 .OR. KEYCHK(VALUES(I),8)) THEN
          IF (TEXT.NE.' ') THEN
              DUMMYC = TEXT
              TEXT = '"'//DUMMYC(1:LEN1(DUMMYC))//'"'
          ELSE
              TEXT = '""'
          END IF
      ELSE IF (DABS(VALUES(I)).LT.1D7) THEN
          IF (DMOD(VALUES(I),1D0).EQ.0D0) THEN
              IVAL = IDINT(VALUES(I))
              WRITE (TEXT,620) IVAL
          ELSE
              WRITE(TEXT,630) VALUES(I)
          END IF
      ELSE
          WRITE (TEXT,630) VALUES(I)
      END IF
  615 IF (TEXT(1:1).EQ.' ') THEN
          TEXT = TEXT(2:)
          GOTO 615
      ENDIF
      IF (TEXT.NE.' ') THEN
          DUMMYC = TEXT
          IF (CONTIN.EQ.',') TEXT = DUMMYC(1:LEN1(DUMMYC))//','
      ELSE
          IF (CONTIN.EQ.',') TEXT = ','
      ENDIF
      CALL KEYPUT(' '//TEXT(1:LEN1(TEXT)),P,LB)
      IF (CONTIN.NE.',') CALL KEYPUT('***END',P,LB)
      I = I+J
      GOTO 100
C
  620 FORMAT(I9)
  630 FORMAT(1PG25.16)
C
      END
