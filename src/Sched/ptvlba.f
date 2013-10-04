      SUBROUTINE PTVLBA( ISCN, PTADD, TSTOP, KS )
C
C     Writes pointing sequence.  
C
      INCLUDE     'sched.inc'
      INCLUDE     'schset.inc'
C
      INTEGER     I, PTADD, ISCN, IDUR, KS
      INTEGER     RLEVEL(MCHAN)
      REAL        DOAZ, DOEL
      CHARACTER   TSTOP*9
C --------------------------------------------------------------------
C     Get the center pointing position.  This is a sum from both
C     the main schedule and the setup file.
C
      DOAZ = SAZCOL(ISCN) + AZCOLIM(KS)
      DOEL = SELCOL(ISCN) + ELCOLIM(KS)
C
C     Do setup scan on half power point to get reasonable levels.
C     First go to half power point.  Note that subroutine VLBA
C     only does setup scans if tape is being written.
C
      DO I = 1, NCHAN(KS)
         RLEVEL(I) = -1 
      END DO
      CALL VLBAINT( 'level', 5, NCHAN(KS), RLEVEL, LLEVEL, MLEVEL,
     1           .FALSE., IUVBA )
      WRITE( IUVBA, '( ''azcolim='',F7.2, '' elcolim='',F7.2, 
     2      '' dur='',I4,''s'', /, 
     3      ''stop='', A, ''   !NEXT! '' )' )
     3      DOAZ - PTINCR(KS), 
     4      DOEL, PTSLEW(ISCN) + PTADD, TSTOP
C
C     Then sit there while fixing the BBC levels.  Take long
C     enough for this to get a pcx test.  Assume that 2 times
C     PTDUR will be long enough.
C
      DO I = 1, NCHAN(KS)
         RLEVEL(I) = 256
      END DO
      CALL VLBAINT( 'level', 5, NCHAN(KS), RLEVEL, LLEVEL, MLEVEL,
     1           .FALSE., IUVBA )
C
C     Check the scan duration - see if it is an even number of 
C     loops.
C
      IDUR = NINT( DUR(ISCN) * 86400.D0 ) - 2 * PTDUR - PTSLEW(ISCN)
      IF( MOD( IDUR, 10 * PTDUR ) .NE. 0 ) THEN
         CALL WLOG( 1, 'PTVLBA: Scan length is not an even number ' //
     1          'of pointing loops (PTSLEW+2*PTDUR+N*10*PTDUR).' )
      END IF
C
      CALL PTPAT( IUVBA, DOAZ, DOEL, PTINCR(KS), PTOFF(KS), PTDUR, 
     1            QUAL(ISCN) )
C
      RETURN
      END


