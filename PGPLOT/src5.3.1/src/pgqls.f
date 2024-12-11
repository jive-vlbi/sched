C*PGQLS -- inquire line style
C%void cpgqls(int *ls);
C+
      SUBROUTINE PGQLS (LS)
      INTEGER  LS
C
C Query the current Line Style attribute (set by routine PGSLS).
C
C Argument:
C  LS     (output) : the current line-style attribute (in range 1-10).
C--
C  5-Nov-1985 - new routine [TJP].
C 28-Dec-2001 - updated header comments for 10 styles [TJP].
C-----------------------------------------------------------------------
      LOGICAL PGNOTO
C
      IF (PGNOTO('PGQLS')) THEN
         LS = 1
      ELSE
         CALL GRQLS(LS)
      END IF
      END
