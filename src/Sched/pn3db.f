      SUBROUTINE PN3DB( ISCN, PTADD, TSTOP, KS )
C
C     Writes a sequency of half power tracking scans for the PN3DB
C     type of test.  This is used to check the encoders.  This 
C     routine is based on PTVLBA.
C
      INCLUDE     'sched.inc'
      INCLUDE     'schset.inc'
C
      INTEGER     I, PTADD, ISCN, QL, KS
      INTEGER     RLEVEL(MCHAN), TRKDUR
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
C     Then sit there while fixing the BBC levels.
C
      DO I = 1, NCHAN(KS)
         RLEVEL(I) = 256
      END DO
      CALL VLBAINT( 'level', 5, NCHAN(KS), RLEVEL, LLEVEL, MLEVEL,
     1           .FALSE., IUVBA )
C
C     Make the level fixing scan a fixed 15 seconds.
C
      QL = QUAL(ISCN)
      WRITE( IUVBA, '( A, I3, A, I4, A )' )
     1    'qual=', 30+QL,'  dur=', 15, 's     !NEXT!'
C
C     Get the time to track at the Azimuth half power point.
C     The elevation scan should be the same length, but it will
C     end with the stop time.
C
      TRKDUR = ( DUR(ISCN) * 86400D0 - PTSLEW(ISCN) - 15.D0
     1         - 4.D0 * PTDUR ) / 2.D0
C
      IF( TRKDUR .LE. 0 ) THEN
         CALL ERRLOG( 'PN3DB:  Scan duration shorter than setups.' )
      END IF    
C
C     Then do the pattern
C
      WRITE( IUVBA, '( 5( A, I3, A, F7.2, A, I4, A, / ),' //
     1   'A, I3, A, F7.2, A )' )
     2   'qual=', 12+QL, ' azcolim=', DOAZ-PTOFF(KS), 
     3          ' dur=', PTDUR, 's  !NEXT!',
     4   'qual=', 10+QL, ' azcolim=', DOAZ,
     5          ' dur=', PTDUR, 's  !NEXT!',
     6   'qual=', 11+QL, ' azcolim=', DOAZ-PTINCR(KS),
     7          ' dur=', TRKDUR, 's  !NEXT!',
     8   'qual=', 22+QL, ' azcolim=', DOAZ-PTOFF(KS), 
     9          ' dur=', PTDUR, 's  !NEXT!',
     1   'qual=', 20+QL, ' azcolim=', DOAZ,           
     2          ' dur=', PTDUR, 's  !NEXT!',
     3   'qual=', 23+QL, ' elcolim=', DOEL+PTINCR(KS),
     4          ' dur=0   !NEXT!'
C
      RETURN
      END


