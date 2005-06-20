      SUBROUTINE TPREV( ISCN, ISTA, LSTOPJ, TPCLEN, TPMODMIN, 
     1                  TOOLONG, DOTAPE, DOREW, DOFAST,
     2                  IPASS, TPCDIR, TPCFOOT, SCNFOOT, SLOP, TPTOL )
C
C     Deal with requests or situations that require a tape to change
C     direction when automatic tape allocation is not in use.  
C
C     For REWIND and FASTF, try to go to end of tape.  
C
C     For REVERSE, change direction without moving.  If scan won't 
C     fit on this pass, space to end of tape and start next pass. 
C
C     Note that if a rewind is issued while going forward or a fastf
C     is issued while going backward, a whole pass must be skipped
C     and the next recorded data will be in the same direction as
C     the last scan.  The current pass configuration requires that
C     this be on the same head or on the last one plus 2 since
C     the last one plus one should be recording in the opposite
C     direction.
C
C     Modified 13 Oct 94.  Do not space to end of tape at reversals
C     unless that is necessary to fit the scan.  15 Nov. 1994 - Also
C     space to end if more than SLOP feet to go.
C
C     Split off from TPSCH on 98feb12 by RCW.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER             ISCN, ISTA, IPASS, TPCDIR, TPMODMIN(*)
      REAL                TPCFOOT, REWSP, SLOP, TPTOL, TPCLEN, SCNFOOT
      LOGICAL             DOTAPE, DOREW, DOFAST
      LOGICAL             TOOLONG
      DOUBLE PRECISION    LSTOPJ
C
      REAL                SETFOOT
C
      PARAMETER           (REWSP = 330.0) 
C  --------------------------------------------------------------------
      IPASS = IPASS + 1
C
C     Requested tape reversal.
C
      IF( REVERSE(ISCN,ISTA) ) THEN
         TPCDIR = -TPCDIR
C
C        Warn if the scan won't fit.
C
         IF( ( TPCDIR .EQ. 1 .AND. 
     1               TPCFOOT + SCNFOOT - TPTOL .GT. TPCLEN )  .OR.
     2      (TPCDIR .EQ. -1 .AND. TPCFOOT .LT. SCNFOOT - TPTOL ) ) THEN
            CALL SCANID( 1, ISCN, ISTA )
            CALL ERRLOG( 'Too little tape left for scan after '//
     1                 'forced REVERSE.' )
         END IF
C
C     Commanded rewind or too little tape remains for scan 
C     and need to space to front to get enough room for scan.
C
      ELSE IF( REWIND(ISCN,ISTA) .OR. 
     1     ( TOOLONG .AND. TPCDIR .EQ. -1 .AND. 
     2         ( TPCFOOT + SCNFOOT - TPTOL .GT. TPCLEN .OR. 
     3            TPCFOOT .GT. SLOP ) ) ) THEN 
         DOREW = .TRUE.
C
C        Add another pass if TPCDIR will not change.
C
         IF( TPCDIR .EQ. 1 ) IPASS = IPASS + 1
         TPCDIR = 1
         IF( NOREC(ISCN) ) THEN
            SETFOOT = ( STOPJ(ISCN) - LSTOPJ )*86400*REWSP/12.0 
         ELSE
            SETFOOT = ( STARTJ(ISCN) - LSTOPJ )*86400*REWSP/12.0 
         END IF
         TPCFOOT = TPCFOOT - SETFOOT
         IF( TPCFOOT .LT. 0.0 ) TPCFOOT = 0.0
C
C        Warn if setup scan was too short to complete requested 
C        rewind.
C
         IF( REWIND(ISCN,ISTA) .AND. TPCFOOT .NE. 0.0 ) THEN
            CALL SCANID( 0, ISCN, ISTA )
            CALL WLOG( 0, 'Insufficient time to complete requested '//
     1                   'REWIND' )
         END IF
C
C        Unable to arrange enough tape for this scan.
C
         IF( TPCLEN - TPCFOOT .LT. SCNFOOT - TPTOL ) THEN
            CALL SCANID( 1, ISCN, ISTA )
            CALL ERRLOG( 'Insufficient room on tape for above scan' )
         END IF
C
C     Commanded fast forward or too little tape remains for scan
C     and need to go to end to get enough for scan.
C
      ELSE IF( FASTF(ISCN,ISTA) .OR. (TOOLONG .AND. TPCDIR .EQ. 1 .AND.
     1    ( SCNFOOT - TPTOL .GT. TPCFOOT .OR. 
     2      TPCLEN - TPCFOOT .GT. SLOP ) ) ) THEN
         DOFAST = .TRUE.
C
C        Add another pass if TPCDIR will not change.
C
         IF( TPCDIR .EQ. -1 ) IPASS = IPASS + 1
         TPCDIR = -1
         IF( NOREC(ISCN) ) THEN
            SETFOOT = ( STOPJ(ISCN) - LSTOPJ )*86400*REWSP/12.0 
         ELSE
            SETFOOT = ( STARTJ(ISCN) - LSTOPJ )*86400*REWSP/12.0 
         END IF
         TPCFOOT = TPCFOOT + SETFOOT
         IF( TPCFOOT .GT. TPCLEN ) TPCFOOT = TPCLEN
C
C        Warn if setup scan was too short to complete requested 
C        fast forward. 
C
         IF( FASTF(ISCN,ISTA) .AND. TPCFOOT .NE. TPCLEN ) THEN
            CALL SCANID( 0, ISCN, ISTA )
            CALL WLOG( 0, 'Insufficient time to complete FASTFOR '//
     1             'requested for above scan' )
         END IF
C
C        Unable to arrange enough tape for this scan.
C
         IF( TPCFOOT .LT. SCNFOOT - TPTOL ) THEN
            CALL SCANID( 1, ISCN, ISTA )
            CALL ERRLOG( 'Insufficient room on tape for above scan' )
         END IF
C
C     Too little tape remains for scan and a simple reversal can
C     be done.  The cases where there isn't enough tape are dealt
C     with above.
C
      ELSE IF( TOOLONG ) THEN
         TPCDIR = -TPCDIR
C
      END IF
C
C     Deal with required tape change.
C
      IF( IPASS .GT. NHDPOS(ISTA) * TPMODMIN(ISTA) ) THEN
         DOTAPE = .TRUE.
         IPASS = 1
         TPCDIR  = 1
         TPCFOOT = 0.0
      END IF
C
      RETURN
      END

