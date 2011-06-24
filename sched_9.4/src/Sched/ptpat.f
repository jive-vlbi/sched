      SUBROUTINE PTPAT( IUVBA, DOAZ, DOEL, PTINCR, PTOFF, PTDUR, QL )
C
C     Routine for SCHED called by PTVLBA that writes the 
C     actual pointing pattern.  It is broken out of PTVLBA to 
C     simplify adding forcus rotation patterns.
C
      REAL        DOAZ, DOEL, PTINCR, PTOFF
      INTEGER     PTDUR, IUVBA, QL
C --------------------------------------------------------------------
C     Start with a setup scan.
C
      WRITE( IUVBA, '( A, I3, A, I4, A )' )
     1    'qual=', 30+QL,'  dur=', 2 * PTDUR, 's     !NEXT!'
C
      WRITE( IUVBA,'( A )' ) '  !BEGIN LOOP!'
C
C     Then do the 10 point pattern.
C
      WRITE( IUVBA, '( A, I3, A, F7.2, A, I4, A, /, 
     1    5( A, I3, A, F7.2, A, /),
     2    A, I3, A, F7.2, A, F7.2, A, /, 2( A, I3, A, F7.2, A, /),
     3    A, I3, A, F7.2, A, F7.2)' )'qual=', 12+QL, ' azcolim=', 
     4   DOAZ-PTOFF, ' dur=', PTDUR, 's  !NEXT!',
     5   'qual=', 11+QL, ' azcolim=', DOAZ-PTINCR,   ' !NEXT!',
     6   'qual=', 10+QL, ' azcolim=', DOAZ,          ' !NEXT!',
     7   'qual=', 13+QL, ' azcolim=', DOAZ+PTINCR,   ' !NEXT!',
     8   'qual=', 14+QL, ' azcolim=', DOAZ+PTOFF, ' !NEXT!',
     9   'qual=', 22+QL, ' elcolim=', DOEL-2*PTINCR, ' !NEXT!',
     A   'qual=', 21+QL, ' elcolim=', DOEL-PTINCR,
     B      ' azcolim=',DOAZ,               ' !NEXT!',
     C   'qual=', 20+QL, ' elcolim=', DOEL,          ' !NEXT!',
     D   'qual=', 23+QL, ' elcolim=', DOEL+PTINCR,   ' !NEXT!',
     E   'qual=', 24+QL, ' elcolim=', DOEL+2*PTINCR, 
     F      ' azcolim=', DOAZ-PTOFF
      WRITE( IUVBA, '( A )' )  ' !LOOP BACK!'
      WRITE( IUVBA, '( A )' )               ' !NEXT!'
C
      RETURN
      END
