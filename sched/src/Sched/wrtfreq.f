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
      INTEGER           LEN1, ICH, NPC, KSTA
      DOUBLE PRECISION  LOSUM(MCHAN)
      REAL              BBCBW(MCHAN), BBCFREQ(MCHAN)
      LOGICAL           FIRSTS, WRTSET
C
C     Keep track of last use.  
C
      INTEGER           MBBCBW, MBBCFREQ
      REAL              LBBCBW(MCHAN), LBBCFREQ(MCHAN)
      REAL              TBBCBW(MCHAN), TBBCFREQ(MCHAN)
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
C
C     Get pointer to station catalog entry for schedule station ISTA
C
      KSTA = STANUM(ISTA)
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
C        If using the RDBE, be sure not to ask the crd file for too
C        many channels.  Also check against invalid values of 
C        frequency and bandwidth.  Transfer the arrays to local
C        versions.  If the BBC frequency is over 1000.0 MHz, drop it
C        by 25 MHz.  This will not change the pcal tone frequencies.
C
         IF( DAR(KSTA) .NE. 'RDBE' ) THEN
            NCH = NCHAN(LS)
            DO ICH = 1, NCH
               TBBCBW(ICH) = BBCBW(ICH)
               TBBCFREQ(ICH) = BBCFREQ(ICH)
            END DO
         ELSE
            NCH = MIN( NCHAN(LS), 8 )
            DO ICH = 1, NCH
               TBBCBW(ICH) = BBCBW(ICH)
               IF( TBBCBW(ICH) .GT. 16.0 ) TBBCBW(ICH) = 16.0
               TBBCFREQ(ICH) = BBCFREQ(ICH)
               IF( TBBCFREQ(ICH) .GT. 1000.0 ) 
     1             TBBCFREQ(ICH) = TBBCFREQ(ICH) - 25.0
            END DO
         END IF
C
C        Write the frequency and bandwidth.
C
         CALL VLBAREAL( 'bbsynth', 7, NCH, TBBCFREQ, LBBCFREQ,
     1         MBBCFREQ, '(F6.2)', 6, FIRSTS, IUVBA )
         CALL VLBABWS( 'bbfilter', 8, NCH, TBBCBW, LBBCBW,
     1         MBBCBW, FIRSTS, IUVBA )
C
      END IF
      LKF = KF
C
C     Deal with pulse cal through function PSETI.
C
      KP = PSETI( ISCN, ISTA )      
      IF( WRTSET .OR. KP .NE. LKP ) THEN
C
         IF( PSPCAL(KP) .NE. LPCAL) 
     1      WRITE( IUVBA, '( A, A )' ) 'pcal=', 
     2             PSPCAL(KP)(1:LEN1(PSPCAL(KP)))
         LPCAL = PSPCAL(KP)
C
C        When using the RDBE, only write the first 8 tone
C        detection commands - 4 per xbit1 and 4 per xbit2.  
C        The rest might be for too high channel numbers.
C        All of this is pretty casually done because this should
C        not affect data as it is not being recorded through
C        this path.
C
         NPC = MAXPC
         IF( DAR(KSTA) .EQ. 'RDBE' ) NPC = 4
         CALL VLBACHAR( 'pcalxbit1', 9, NPC, 
     1             PSX1(1,KP), LPCALX1, MPCALX1, FIRSTS, IUVBA )
         CALL VLBACHAR( 'pcalxbit2', 9, NPC, 
     1             PSX2(1,KP), LPCALX2, MPCALX2, FIRSTS, IUVBA )
         CALL VLBAINT( 'pcalxfreq1', 10, NPC, 
     1             PSFR1(1,KP), LPCALFR1, MPCALFR1, FIRSTS, IUVBA )
         CALL VLBAINT( 'pcalxfreq2', 10, NPC, 
     1             PSFR2(1,KP), LPCALFR2, MPCALFR2, FIRSTS, IUVBA )
C
      END IF
      LKP = KP         
C
C
      RETURN
      END


