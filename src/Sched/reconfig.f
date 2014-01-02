      SUBROUTINE RECONFIG( ISCN, LSCN, ISTA, NEWCONF, NEWPCAL )
C
C     Compare setup group KS used in scan ISCN with group JS used
C     in scan LSCN to determine if they will require a formatter 
C     reconfigure on the legacy VLBA systems.  The results also
C     apply to MarkIV systems.
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
C
C     It is not clear that this routine is up to date.  The legacy
C     VLBA systems have been retired for data recording (Dec. 2013).
C     Change is happening to the other systems too.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER   ISCN, LSCN, ISTA
      INTEGER   KS, JS, ICH, IPC
      LOGICAL   LOCDBG, NEWCONF, NEWPCAL, SAMECONF
C
C     For checking a pcal detector setup change.
C
      INTEGER       KF, LF
      CHARACTER     KPCX1(MAXPC)*3, KPCX2(MAXPC)*3
      CHARACTER     LPCX1(MAXPC)*3, LPCX2(MAXPC)*3
      INTEGER       KPCFR1(MAXPC), KPCFR2(MAXPC)
      INTEGER       LPCFR1(MAXPC), LPCFR2(MAXPC)
C
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
C     Note that the frequency, Doppler, and bandwidth commands only
C     affect the formatter through the pulse cal detection.  The
C     sample rate does not change when BW is changed by scan.
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
C     Worry about the pcal setup.  This was easier with the pcal 
C     groups, but those are gone.  Now compare the settings to 
C     be used with the last ones.  First get the frequency group 
C     for both inputs, then calculate the detector setup and compare.
C
      KF = FSETI(ISCN,ISTA)
      LF = FSETI(LSCN,ISTA)
      CALL PCALFQ( FSPCAL(KF), KF, KPCX1, KPCX2, KPCFR1, KPCFR2 )
      CALL PCALFQ( FSPCAL(LF), LF, LPCX1, LPCX2, LPCFR1, LPCFR2 )
C
      NEWPCAL = FSPCAL(KF) .NE. FSPCAL(LF) 
      DO IPC = 1, MAXPC
         IF( KPCX1(IPC) .NE. LPCX1(IPC) .OR.
     1       KPCX2(IPC) .NE. LPCX2(IPC) .OR.
     2       KPCFR1(IPC) .NE. LPCFR1(IPC) .OR.
     3       KPCFR2(IPC) .NE. LPCFR2(IPC) ) NEWPCAL = .TRUE.
      END DO
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
