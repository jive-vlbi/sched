      SUBROUTINE TAVLBA( ISCN, PTADD, TSTOP )
C
C     Writes antenna temperature measuring sequence.
C     Based closely on PTVLBA
C
      INCLUDE     'sched.inc'
      INCLUDE     'schset.inc'
C
      INTEGER     PTADD, I, ISCN
      INTEGER     RLEVEL(MCHAN), QL
      REAL        DOAZ, DOEL
      CHARACTER   TSTOP*9
C --------------------------------------------------------------------
C     Get the center pointing position.  This is a sum from both
C     the main schedule and the setup file.
C
      DOAZ = SAZCOL(ISCN) + AZCOLIM(LS)
      DOEL = SELCOL(ISCN) + ELCOLIM(LS)
C
C     Do setup scan on half power point to get reasonable levels.
C
      DO I = 1, NCHAN(LS)
         RLEVEL(I) = -1 
      END DO
      CALL VLBAINT( 'level', 5, NCHAN(LS), RLEVEL, LLEVEL, MLEVEL,
     1           .FALSE., IUVBA )
      WRITE( IUVBA, '( ''azcolim='',F7.2, '' elcolim='',F7.2, 
     2      '' dur='',I4,''s'', /, 
     3      ''stop='', A, ''   !NEXT! '' )' )
     3      DOAZ - PTINCR(LS), 
     4      DOEL, PTSLEW(LS) + PTADD, TSTOP
C
      DO I = 1, NCHAN(LS)
         RLEVEL(I) = 256
      END DO
      QL = QUAL(ISCN)
      CALL VLBAINT( 'level', 5, NCHAN(LS), RLEVEL, LLEVEL, MLEVEL,
     1           .FALSE., IUVBA )
      WRITE( IUVBA, '( A, I3, A, I4, A )' )
     1   'qual=', 40+QL, '  dur=', PTDUR, 's  !NEXT!' 
C
      WRITE( IUVBA,'( A, I3, A,  F7.2, '' !BEGIN LOOP! !NEXT!'',/, 
     1    2( A, I3, A,  F7.2, '' !NEXT!'',/),
     3    A, I3, A,  F7.2, '' !LOOP BACK!  !NEXT!'')')
     4   'qual=', 12+QL, ' azcolim=', DOAZ-PTOFF(LS),
     6   'qual=', 10+QL, ' azcolim=', DOAZ,
     8   'qual=', 24+QL, ' azcolim=', DOAZ+PTOFF(LS),
     6   'qual=', 10+QL, ' azcolim=', DOAZ
C
      RETURN
      END
