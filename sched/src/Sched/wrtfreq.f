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
      INTEGER           ISCN, ISTA, NCH1, NCH2, NCH, KP, LKP, KF, LKF
      INTEGER           PSETI, LEN1, ICH, KCH, NPC, KSTA, ISIDE(MCHAN)
      INTEGER           RFSIDE(MCHAN), TPSFR(MCHAN)
      DOUBLE PRECISION  LOSUM(MCHAN), TLOSUM(MCHAN), TONEINT
      DOUBLE PRECISION  FTONE
      DOUBLE PRECISION  BBCBW(MCHAN), BBCFREQ(MCHAN), FSHIFT
      LOGICAL           FIRSTS, WRTSET
      CHARACTER         TPSX1(MAXPC)*3, TPSX2(MAXPC)*3
      INTEGER           IPD, TPSFR1(MAXPC), TPSFR2(MAXPC)
      INTEGER           KCHA, KCHB
C
C     Keep track of last use.  
C
      INTEGER           MBBCBW, MBBCFREQ
      DOUBLE PRECISION  LBBCBW(MCHAN), LBBCFREQ(MCHAN)
      DOUBLE PRECISION  TBBCBW(MCHAN), TBBCFREQ(MCHAN) 
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
C     Get the frequency and pcal sets.
C
      KF = FSETI(ISCN,ISTA)
      KP = PSETI( ISCN, ISTA )      
C
C     Get the frequency etc for this station/scan.  Save some work
C     if there hasn't been a change.  Write the results for any
C     changes.
C
      IF( WRTSET .OR. KF .NE. LKF ) THEN
C
         CALL FSFREQ( KF, LOSUM, BBCFREQ, BBCBW )
C
C        If using the RDBE, be sure not to ask the crd file for too
C        many channels.   If NCHAN is greater than 8, use the center
C        8 channels.  Start on an odd channel in case of dual pol.
C        Also check against invalid values of 
C        frequency and bandwidth.  Transfer the arrays to local
C        versions.  If the BBC frequency is over 1000.0 MHz, drop it
C        by 25 MHz.  This will not change the pcal tone frequencies.
C        If needed, center the Mark5A channel on the RDBE channel.
C        Collect some information like the LOSUM for use in setting
C        the pcal frequencies.  Round frequencies to 10 kHz.
C
         IF( DAR(KSTA) .NE. 'RDBE' ) THEN
            NCH = NCHAN(LS)
            DO ICH = 1, NCH
               TBBCBW(ICH) = BBCBW(ICH)
               TBBCFREQ(ICH) = BBCFREQ(ICH)
            END DO
         ELSE
            IF( NCHAN(LS) .GT. 8 ) THEN
               NCH1 = 1 + 2 * INT( ( NCHAN(LS) - 8 ) / 4 )
               NCH2 = NCH1 + 7
               NCH = 8
            ELSE
               NCH1 = 1
               NCH2 = NCHAN(LS)
               NCH = NCH2
            END IF
            DO ICH = NCH1, NCH2
               KCH = ICH - NCH1 + 1
               ISIDE(KCH) = 1
               IF( SIDEBD(ICH,LS) .EQ. 'L' ) ISIDE(KCH) = -1
               RFSIDE(KCH) = 1
               IF( NETSIDE(ICH,LS) .EQ. 'L' ) RFSIDE(KCH) = -1
               TBBCBW(KCH) = BBCBW(ICH)
               TBBCFREQ(KCH) = BBCFREQ(ICH)
               TBBCFREQ(KCH) = NINT( TBBCFREQ(KCH) * 100.D0 ) / 100.D0
               FSHIFT = 0.0D0
               IF( TBBCBW(KCH) .GT. 16.0D0 ) THEN
C
C                 Center the band on the wider RDBE band, but 
C                 make the shift an even number of 5 MHz
C                 to try to preserve the pcal calcualations
C                 in the common case of a 5 MHz tone.
C
                  TBBCBW(KCH) = 16.0D0
                  FSHIFT = ISIDE(KCH) * 
     1                  0.5D0 * ( BBCBW(KCH) - TBBCBW(KCH) )
               END IF
               IF( TBBCFREQ(KCH) .GT. 1000.0D0 ) THEN
                  FSHIFT = -25.0D0 + FSHIFT
               END IF
               TBBCFREQ(KCH) = TBBCFREQ(KCH) + FSHIFT
               TLOSUM(KCH) = LOSUM(ICH) + FSHIFT
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
C     Deal with pulse cal through function PSETI.  KP needed
C     for the RDBE frequency shifting so it was set above.
C
      IF( WRTSET .OR. KP .NE. LKP ) THEN
C
         IF( PSPCAL(KP) .NE. LPCAL) 
     1      WRITE( IUVBA, '( A, A )' ) 'pcal=', 
     2             PSPCAL(KP)(1:LEN1(PSPCAL(KP)))
         LPCAL = PSPCAL(KP)
         NPC = MAXPC
         IF( DAR(KSTA) .NE. 'RDBE' .OR. ( PSPCAL(KP) .NE. '1MHZ'
     1        .AND. PSPCAL(KP) .NE. '5MHZ' ) ) THEN
            CALL VLBACHAR( 'pcalxbit1', 9, NPC, 
     1             PSX1(1,KP), LPCALX1, MPCALX1, FIRSTS, IUVBA )
            CALL VLBACHAR( 'pcalxbit2', 9, NPC, 
     1             PSX2(1,KP), LPCALX2, MPCALX2, FIRSTS, IUVBA )
            CALL VLBAINT( 'pcalxfreq1', 10, NPC, 
     1             PSFR1(1,KP), LPCALFR1, MPCALFR1, FIRSTS, IUVBA )
            CALL VLBAINT( 'pcalxfreq2', 10, NPC, 
     1             PSFR2(1,KP), LPCALFR2, MPCALFR2, FIRSTS, IUVBA )
         ELSE
C
C           When using the RDBE, we need something else - sigh, and
C           we need to keep the details in this routine in local
C           variables.  Utilize number such as NCH1, NCH2, TBBCBW,
C           and TBBCFREQ to help.  Also take advantage of the fact
C           that there will be at most 8 channels (1 digit).  This
C           is a simplified verision of what is done for non-RDBE
C           projects.
C
C
C           Get the first tone frequency for each band.  These are for
C           the channels handled in the frequency section above.
C           Don't let tones be too close to the LO.
C           Note FTONE will be slightly below the LO.  So for the
C           upper sideband case, need to go up.
C 
            TONEINT = 1.0D0
            IF( PSPCAL(KP) .EQ. '5MHZ' ) TONEINT = 5.0D0
            DO KCH = 1, NCH
               FTONE =  TONEINT * INT( TLOSUM(KCH) / TONEINT )
               IF( RFSIDE(KCH) .GT. 0 ) THEN
                  FTONE =  FTONE + TONEINT
                  IF( ABS( FTONE - TLOSUM(KCH) ) .LT. TONEINT / 1.D1 )
     1                FTONE =  FTONE + TONEINT
                  TPSFR(KCH) = 1000.D0 * ( FTONE - TLOSUM(KCH) )
               ELSE
                  IF( ABS( FTONE - TLOSUM(KCH) ) .LT. TONEINT / 1.D1 )
     1                FTONE =  FTONE - TONEINT
                  TPSFR(KCH) = 1000.D0 * ( TLOSUM(KCH) - FTONE )
               END IF
            END DO
C
            NPC = NCH/2
            DO IPD = 1, NPC
               KCHA = 2 * IPD - 1
               KCHB = KCHA + 1
               WRITE( TPSX1(IPD), '( A, I1 )' ) 'S', KCHA
               WRITE( TPSX2(IPD), '( A, I1 )' ) 'S', KCHB
               TPSFR1(IPD) = TPSFR(KCHA)
               TPSFR2(IPD) = TPSFR(KCHB)
            END DO
            CALL VLBACHAR( 'pcalxbit1', 9, NPC, 
     1             TPSX1, LPCALX1, MPCALX1, FIRSTS, IUVBA )
            CALL VLBACHAR( 'pcalxbit2', 9, NPC, 
     1             TPSX2, LPCALX2, MPCALX2, FIRSTS, IUVBA )
            CALL VLBAINT( 'pcalxfreq1', 10, NPC, 
     1             TPSFR1, LPCALFR1, MPCALFR1, FIRSTS, IUVBA )
            CALL VLBAINT( 'pcalxfreq2', 10, NPC, 
     1             TPSFR2, LPCALFR2, MPCALFR2, FIRSTS, IUVBA )
C
         END IF
      END IF
      LKP = KP         
C
C
      RETURN
      END


