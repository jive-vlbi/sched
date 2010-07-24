      SUBROUTINE RECONFIG( ISCN, LSCN, ISTA, NEWCONF, NEWPCAL )
C
C     Compare setup group KS with group JS to determine if they
C     will require a formatter reconfigure on VLBA systems.
C
C     Warning, this routine must be called after the pcal sets
C     have been determined.  That means after CHKSCN which in turn
C     means after schedule optimization etc.  So it can only
C     be used for checking, not scan time setting.
C
C     This is a stripped down and modified version of SAMESET.
C     Note that this time, frequencies must be considered, hence
C     the scan numbers.
C
      INTEGER   ISCN, LSCN, ISTA
      INTEGER   KS, JS, KP, JP, ICH, ITP
      INTEGER   PSETI
      LOGICAL   LOCDBG, NEWCONF, NEWPCAL, SAMECONF
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      PARAMETER  (LOCDBG=.FALSE.)
C ---------------------------------------------------------------------
C     Don't do this if this is the first scan.
C     This also protects later parts of the program from index 0.
C     Also don't do if no setups are being used (eg project planning)
C     Note SAMECONF tests for same configuration, then at end, we
C     invert it to get NEWCONF.
C
      IF( LSCN .EQ. 0 .OR. NOSET ) THEN 
         SAMECONF = .TRUE.
         NEWPCAL = .FALSE.
         GO TO 999
      END IF
C
C     Get the setups that must be considered.
C
      KS = NSETUP(ISCN,ISTA)
      JS = NSETUP(LSCN,ISTA)
C
C     Write the debugging "I am here" line.
C
      IF( SDEBUG .OR. LOCDBG ) THEN
         WRITE( MSGTXT, '( A, 4I4, A, I4 )' )
     1      'RECONFIG: Starting on scans, station, setups ', 
     2      ISCN, LSCN, ISTA, KS, ' and ', JS
            CALL WLOG( 0, MSGTXT )
            MSGTXT = ' '
      END IF
C
C     First test the obvious items that will cause a reconfigure.
C
      SAMECONF = NCHAN(KS)    .EQ. NCHAN(JS)    .AND.
     1          SAMPRATE(KS) .EQ. SAMPRATE(JS) .AND.
     2          FANOUT(KS)   .EQ. FANOUT(JS)   .AND.
     3          FORMAT(KS)   .EQ. FORMAT(JS)   .AND.
     4          BARREL(KS)   .EQ. BARREL(JS) 
C
C     Test some channel dependent values.
C
      DO ICH = 1, NCHAN(KS)
         SAMECONF = SAMECONF .AND. 
     1     SIDEBD(ICH,KS)  .EQ. SIDEBD(ICH,JS)  .AND.
     2     BITS(ICH,KS) .EQ. BITS(ICH,JS)       .AND.
     3     BBC(ICH,KS)  .EQ. BBC(ICH,JS)
      END DO
C
C     Worry about the pcal setup.  The pcal set concept makes this 
C     easy.
C
      KP = PSETI(ISCN,ISTA)
      JP = PSETI(LSCN,ISTA)
      NEWPCAL = KP .NE. JP
C
C     Jump here on first or non-recording scan.
C
  999 CONTINUE
C
      NEWCONF = .NOT. SAMECONF
      IF( LOCDBG ) THEN
         WRITE( MSGTXT, '( A, L1 )' )
     1     'RECONFIG END: ', NEWCONF, NEWPCAL
         CALL WLOG( 0, MSGTXT )
         MSGTXT = ' '
      END IF
C
      RETURN
      END
