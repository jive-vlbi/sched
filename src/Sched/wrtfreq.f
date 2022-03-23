      SUBROUTINE WRTFREQ( ISCN, ISTA, FIRSTS, WRTSET )
C
C     Routine for SCHED, called by VLBASU, that writes the frequency
C     and bandwidth specifications, plus the pulse cal commands,
C     to the VLBA crd files that control the VLBA legacy system.
C
C     If the RDBE is in use and bandwidths greater than 16 MHz have
C     been requested, or if CRDDOP, CRDFREQ, or CRDBW has been 
C     specified, this routine will write channels somewhat different 
C     than the main schedule channels.  The main channels may have 
C     bandwidths too great for the legacy BBCs.  Also the main channels
C     may need to conform to RDBE (especially PFB) tuning restrictions
C     which make pointing on masers difficult.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
      INCLUDE  'schfreq.inc'
C
      INTEGER           ISCN, ISTA, KP, LKP, KF, LKF
      INTEGER           LEN1, NPC, ich
      INTEGER           CRDN, CRSETC(MAXCHN)
      DOUBLE PRECISION  LOSUM(MCHAN)
      DOUBLE PRECISION  BBCBW(MCHAN), BBCFREQ(MCHAN)
      DOUBLE PRECISION  CRDF(MCHAN), CRDB(MCHAN), CRDLOSUM(MCHAN)
      LOGICAL           FIRSTS, WRTSET
      CHARACTER         CRDS(MCHAN)*1
      CHARACTER         PCX1(MAXPC)*3, PCX2(MAXPC)*3
      INTEGER           PCFR1(MAXPC), PCFR2(MAXPC)
C
C     Keep track of last use.  
C
      INTEGER           MBBCBW, MBBCFREQ
      DOUBLE PRECISION  LBBCBW(MCHAN), LBBCFREQ(MCHAN)
      INTEGER           LPCALFR1(MAXPC), LPCALFR2(MAXPC)
      INTEGER           MPCALFR1, MPCALFR2, MPCALX1, MPCALX2
      CHARACTER         LPCALX1(MAXPC)*3, LPCALX2(MAXPC)*3
      CHARACTER         LPCAL*4, UPPCAL*4
C
C     Common for communication with FSVLBA (for frequency switching).
C
      COMMON  /CVLBA/   LBBCFREQ, MBBCFREQ
C
      SAVE    /CVLBA/, LKF, LKP, MBBCBW, LBBCBW, LPCAL
      SAVE    LPCALX1, LPCALX2, MPCALX1, MPCALX2
      SAVE    LPCALFR1, LPCALFR2, MPCALFR1, MPCALFR2
C----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'WRTFREQ starting ' )
C
C     Force writing bbsynth and bbfilter on first file specified.
C     Then will write whenever there are changes.
C
      IF( FIRSTS ) THEN
         MBBCFREQ = 0
         MBBCBW = 0
         LKF = 0
         LKP = 0
         LPCAL = ' '
      END IF
C
C     Get the frequency set.  We are no longer using pcal sets.
C
      KF = FSETI(ISCN,ISTA)
C
C     Get the frequency etc for this station/scan.  Save some work
C     if there hasn't been a change.  Write the results for any
C     changes.
C
C     Note that the use of CRDBW or CRDFREQ (directly or through 
C     CRDDOP) triggers a new frequency set, so testing for a new
C     set should pick up changes needed for those reasons in addition
C     to true setup file changes.
C
      IF( WRTSET .OR. KF .NE. LKF ) THEN
         LKF = KF
C
C        Get the frequencies.  For this routine, the values delivered
C        by FSFREQ in the CRD variables can be used in all cases.  Even
C        when CRDFREQ or CRDDOP are not used, FSFREQ transfers the correct
C        values to the CRD numbers.
C
         CALL FSFREQ( KF, LOSUM, BBCFREQ, BBCBW,
     1         CRDN, CRDF, CRDB, CRDS, CRDLOSUM, CRSETC )
C
C        Write the frequency and bandwidth.
C        FSFREQ takes care of restricting the data to the proper
C        setup channels when the number of crd channels does not
C        match the number of RDBE channels.
C
         CALL VLBAREAL( 'bbsynth', 7, CRDN, CRDF, LBBCFREQ,
     1         MBBCFREQ, '(F6.2)', 6, FIRSTS, IUVBA )
         CALL VLBABWS( 'bbfilter', 8, CRDN, CRDB, LBBCBW,
     1         MBBCBW, FIRSTS, IUVBA )
C
C
C        Get the pulse cal detector settings.
C
C        Note that, with Vex, it seems that only a numerical index 
C        of which tone in the band to detect is specified.  Treat
C        that separately from the legacy VLBA system.  PCALFQ
C        deals with the details of calculating the detection data.
C
         CALL PCALFQ( FSPCAL(KF), KF, PCX1, PCX2, PCFR1, PCFR2 )
C
C        First, specify the setting for the generators.
C        The old files had an all upper case version of pcal, do duplicate
C        that.  I'm not sure that is needed.
C        
         UPPCAL = FSPCAL(KF)
         CALL UPCASE( UPPCAL )
         IF( FSPCAL(KF) .NE. LPCAL) 
     1      WRITE( IUVBA, '( A, A )' ) 'pcal=', 
     2             UPPCAL(1:LEN1(UPPCAL))
         LPCAL = FSPCAL(KF)
C
C        Now write the pcalx commands based on the returns from PCALFQ.
C        These should be the normal defaults when MARK5A recordings are
C        being made.
C
         NPC = MAXPC
         CALL VLBACHAR( 'pcalxbit1', 9, NPC, 
     1          PCX1, LPCALX1, MPCALX1, FIRSTS, IUVBA )
         CALL VLBACHAR( 'pcalxbit2', 9, NPC, 
     1          PCX2, LPCALX2, MPCALX2, FIRSTS, IUVBA )
         CALL VLBAINT( 'pcalxfreq1', 10, NPC, 
     1          PCFR1, LPCALFR1, MPCALFR1, FIRSTS, IUVBA )
         CALL VLBAINT( 'pcalxfreq2', 10, NPC, 
     1          PCFR2, LPCALFR2, MPCALFR2, FIRSTS, IUVBA )
C
      END IF
      LKP = KP         
C
C
      IF( DEBUG ) CALL WLOG( 1, 'WRTFREQ Ending ' )
      RETURN
      END


