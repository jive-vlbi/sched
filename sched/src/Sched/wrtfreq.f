      SUBROUTINE WRTFREQ( ISCN, ISTA, FIRSTS, WRTSET )
C
C     Routine for SCHED, called by VLBASU, that writes the frequency
C     and bandwidth specifications, plus the pulse cal commands,
C     to VLBA files.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
      INCLUDE  'schfreq.inc'
C
      INTEGER           ISCN, ISTA, NCH, KP, LKP, KF, LKF, PSETI
      INTEGER           LEN1
      DOUBLE PRECISION  LOSUM(MCHAN)
      REAL              BBCBW(MCHAN), BBCFREQ(MCHAN)
      LOGICAL           FIRSTS, WRTSET
C
C     Keep track of last use.  
C
      INTEGER           MBBCBW, MBBCFREQ
      REAL              LBBCBW(MCHAN), LBBCFREQ(MCHAN)
      INTEGER           LPCALFR1(MAXPC), LPCALFR2(MAXPC)
      INTEGER           MPCALFR1, MPCALFR2, MPCALX1, MPCALX2
      CHARACTER         LPCALX1(MAXPC)*3, LPCALX2(MAXPC)*3
      CHARACTER         LPCAL*4
C
C     Common for communication with FSVLBA (for frequency switching).
C
      COMMON  /CVLBA/   LBBCFREQ, MBBCFREQ
C
      SAVE    /CVLBA/, LKF, LKP, MBBCBW, LBBCBW, LPCAL
      SAVE    LPCALX1, LPCALX2, MPCALX1, MPCALX2
      SAVE    LPCALFR1, LPCALFR2, MPCALFR1, MPCALFR2
C----------------------------------------------------------------------
      NCH = NCHAN(LS)
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
C     Get the frequency etc for this station/scan.  Save some work
C     if there hasn't been a change.  Write the results for any
C     changes.
C
      KF = FSETI(ISCN,ISTA)
      IF( WRTSET .OR. KF .NE. LKF ) THEN
C
         CALL FSFREQ( KF, LOSUM, BBCFREQ, BBCBW )
C
         CALL VLBAREAL( 'bbsynth', 7, NCH, BBCFREQ, LBBCFREQ,
     1         MBBCFREQ, '(F6.2)', 6, FIRSTS, IUVBA )
         CALL VLBABWS( 'bbfilter', 8, NCH, BBCBW, LBBCBW,
     1         MBBCBW, FIRSTS, IUVBA )
C
      END IF
      LKF = KF
C
C     Deal with pulse cal.
C
      KP = PSETI( ISCN, ISTA )      
      IF( WRTSET .OR. KP .NE. LKP ) THEN
C
         IF( PSPCAL(KP) .NE. LPCAL) 
     1      WRITE( IUVBA, '( A, A )' ) 'pcal=', 
     2             PSPCAL(KP)(1:LEN1(PSPCAL(KP)))
         LPCAL = PSPCAL(KP)
C
         CALL VLBACHAR( 'pcalxbit1', 9, MAXPC, 
     1             PSX1(1,KP), LPCALX1, MPCALX1, FIRSTS, IUVBA )
         CALL VLBACHAR( 'pcalxbit2', 9, MAXPC, 
     1             PSX2(1,KP), LPCALX2, MPCALX2, FIRSTS, IUVBA )
         CALL VLBAINT( 'pcalxfreq1', 10, MAXPC, 
     1             PSFR1(1,KP), LPCALFR1, MPCALFR1, FIRSTS, IUVBA )
         CALL VLBAINT( 'pcalxfreq2', 10, MAXPC, 
     1             PSFR2(1,KP), LPCALFR2, MPCALFR2, FIRSTS, IUVBA )
C
      END IF
      LKP = KP         
C
C
      RETURN
      END


