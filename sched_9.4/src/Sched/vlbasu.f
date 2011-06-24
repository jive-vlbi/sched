      SUBROUTINE VLBASU(ISCN, ISTA, FIRSTS, FRS, WRTSET )
C
C     Subroutine for SCHED that writes setup parameters for VLBA 
C     systems.  The setup files are read in GETSET.  For each 
C     parameter, only write it out if it has changed or if this is 
C     the first scan for the station.  The VLBAINT etc. routines 
C     take care of detecting changes.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER       I, NNCHAN, LEN1, LENS, ISTA, ISCN, ICH, KSTA
      LOGICAL       FIRSTS, FRS, LPNTVLBA, LTANVLBA, LDOPN3DB, WRTSET
C
C     Parameters to hold previous values to avoid duplication.
C     Each one needs a counter for the number of channels.
C
      INTEGER       MBBC, MPERIOD, MBITS, MIFDIST
      INTEGER       MSYNTH, MSIDEBD, MNOISE, MFE, MIFCHAN, MSAMPR 
C
      INTEGER       LBBC(MCHAN), LNCHAN
      INTEGER       RPERIOD(MCHAN), LPERIOD(MCHAN), LBITS(MCHAN)
      INTEGER       RLEVEL(MCHAN)
      REAL          LAZCOLIM, LELCOLIM, LROTAT, LFOCUS
      REAL          DOAZ, DOEL
      DOUBLE PRECISION  DSYNTH(3), LSYNTH(3), DSAMPR, LSAMPR
      LOGICAL       LDUALX, LUSEDIF(4), WARNCRD
      CHARACTER     LSIDEBD(MCHAN)*1, LNOISE(4)*6, LFE(4)*5
      CHARACTER     LIFCHAN(MCHAN)*1, LLOGGING*8, LSTRING(4)*80
      CHARACTER     LFORMAT*8, LIFDIST(4)*3
      CHARACTER     LLCP50CM*6, LRCP50CM*6, LNOISEF*4
C
C     Save all the numbers that we need to keep between calls.
C
      SAVE          LPNTVLBA, LTANVLBA, LDOPN3DB
      SAVE          MBBC, MPERIOD, MBITS, MIFDIST
      SAVE          MSYNTH, MSIDEBD, MNOISE, MFE, MIFCHAN, MSAMPR 
      SAVE          LBBC, LNCHAN, LPERIOD, LBITS
      SAVE          LAZCOLIM, LELCOLIM, LROTAT, LFOCUS
      SAVE          LSAMPR, LSYNTH
      SAVE          LDUALX, LUSEDIF, LSIDEBD, LNOISE, LFE
      SAVE          LIFCHAN, LLOGGING, LSTRING, LFORMAT, LIFDIST
      SAVE          LLCP50CM, LRCP50CM, LNOISEF, WARNCRD
C
C     External L.O. stuff.
C
      DOUBLE PRECISION  EXTLO(4), LEXTLO(4)
      LOGICAL       USEDIF(4)
      CHARACTER     IFLABEL(4)*1, SIDEX(4)*1, LSIDEX(4)*1
      SAVE          LEXTLO, LSIDEX
C
      DATA          IFLABEL / 'A', 'B', 'C', 'D' /
      DATA          WARNCRD / .TRUE. /
C
C ----------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. 3 ) CALL WLOG( 0, 'VLBASU: Starting.' )
C
      NNCHAN = NCHAN(LS)
C
C     Need pointer to entries in the station catalog for this 
C     schedule station ISTA.
C
      KSTA = STANUM(ISTA)
C
C     When using the RDBE, we need to be sure the crd file doesn't 
C     contain items that will cause the on-line system to die.  First
C     protect against too many channels.  The RDBE will have a separate
C     "BBC" for each channel, so limit the total to 8.  Note that the
C     NBBC parameter for the station will apply to the RDBE so don't 
C     use that.  Later I need to protect against invalid BBC frequencies,
C     bandwidths, formats, and pcal detection information (yuk).
C
      IF( DAR(KSTA) .EQ. 'RDBE' ) THEN
         NNCHAN = MIN( NNCHAN, 8 )
      END IF
C
C     Protect against requesting too many BBC's.
C
      DO I = 1, NNCHAN
         IF( BBC(I,LS) .GT. NBBC(KSTA) ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I3, A, 2I4 )' )
     1         'VLBASU: ' // STATION(KSTA) // ' only has',
     2         NBBC(KSTA), ' BBCs.  Setup requested more.',
     3         BBC(I,LS), BBC(NNCHAN,LS)
            CALL ERRLOG( MSGTXT )
         END IF
      END DO
C
C     If there has been no change of setup or station, skip much of 
C     this routine.
C
      IF( WRTSET ) THEN
C
C        Much of setup is not desired for a station with just a
C        VLBA DAR (VLA, Green Bank, Bonn ...).  This is triggered
C        by either CONTROL='VLA' or by CONTROL(5:5)='V'
C
         IF( .NOT. VLBADAR(KSTA) ) THEN
C
C           Front end spec.
C
            CALL VLBACHAR( 'fe', 2, 4, FE(1,LS), LFE, MFE,
     1                     FIRSTS, IUVBA )
C
C           Set transfer switch if dual X band is to be used.
C
            IF( FIRSTS .OR. (DUALX(LS) .NEQV. LDUALX) ) THEN
               IF( DUALX(LS) ) THEN
                  WRITE( IUVBA, '( ''fexfer=(2,split)'' )' )
               ELSE
                  WRITE( IUVBA, '( ''fexfer=(2,norm)'' )' )
               END IF
               LDUALX = DUALX(LS)
            END IF
C
C           50 cm filter.  Don't write if the default of NARROW
C           is used.
C
            IF( FIRSTS ) THEN
               LLCP50CM = 'NARROW'
               LRCP50CM = 'NARROW'
            END IF
            IF( LCP50CM(LS) .NE. LLCP50CM ) THEN
               WRITE( IUVBA, '( A, A )' ) 'lcp50cm=', LCP50CM(LS)
            END IF
            IF( RCP50CM(LS) .NE. LRCP50CM ) THEN
               WRITE( IUVBA, '( A, A )' ) 'rcp50cm=', RCP50CM(LS)
            END IF
            LLCP50CM = LCP50CM(LS)
            LRCP50CM = RCP50CM(LS)
C
C           Various parameters in standard formats not wanted for VLA.
C
            CALL VLBACHAR( 'noise', 5, 4, NOISE(1,LS), LNOISE, 
     1             MNOISE, FIRSTS, IUVBA )
C
C           Prep synth to use in double precision.  The rounding is
C           to prevent issues comparing with the previous result.
C
            DO I = 1, 3
               DSYNTH(I) = SYNTH(I,LS)
               DSYNTH(I) = DNINT( DSYNTH(I) * 1000.0D0 ) / 1000.D0
            END DO
            CALL VLBAREAL( 'synth', 5, 3, DSYNTH, LSYNTH, 
     1             MSYNTH, '(F4.1)', 4, FIRSTS, IUVBA )
C
C           For Pie Town link experiments, tell PT to switch to the
C           other noise cal switching frequency.
C
            IF( STANAME(ISTA) .EQ. 'VLBA_PT' .AND. 
     1          ( FIRSTS .OR. NOISEFRQ(LS) .NE. LNOISEF ) ) THEN
               WRITE( IUVBA, '( 2A )' ) 'noisefreq=', 
     1           NOISEFRQ(LS)(1:LEN1(NOISEFRQ(LS)))
               LNOISEF = NOISEFRQ(LS)
            END IF
C
C           End of non-VLA items.
C
         ELSE
C
C           However, for the VLA, we should check that an error
C           has not been made with the IF assignments.
C
C           The distinctions between the three VLA "stations" are being
C           removed as mixed modes are now allowed.  Comment out for 
C           now, but don't remove in case problems are encountered.
C
C            IF( (STANAME(ISTA) .EQ. 'VLA' .OR.
C     1           STANAME(ISTA) .EQ. 'VLA27') .AND.
C     2           VLAMODE(ISCN) .EQ. 'VS' ) THEN
C               CALL WLOG( 1, 'VLBASU: VS mode specified for station '//
C     1            'VLA or VLA27.' )
C               CALL WLOG( 1, 'VLBASU: That is for single dish '//
C     1            '(VLA1), not phased array.' )
C               CALL WLOG( 1, 'VLBASU: The patch panel will probably '//
C     1            'be set wrong.' )
C            ELSE IF( STANAME(ISTA) .EQ. 'VLA1' .AND.
C     1           ( VLAMODE(ISCN) .EQ. 'VA' .OR.
C     2             VLAMODE(ISCN) .EQ. 'VB' .OR.
C     3             VLAMODE(ISCN) .EQ. 'VR' .OR.
C     4             VLAMODE(ISCN) .EQ. 'VL' .OR.
C     5             VLAMODE(ISCN) .EQ. 'VX' ) ) THEN
C               CALL WLOG( 1, 'VLBASU: '//VLAMODE(ISCN)//
C     1            ' mode specified for station VLA1.' )
C               CALL WLOG( 1, 'VLBASU: The patch panel will probably '//
C     1            'be set wrong.' )
C               CALL WLOG( 1, 'VLBASU: Use VS for single dish mode.' )
C            END IF
C
         END IF
C
C        Write the external LO and sideband specifications for all 
C        non-VLBA stations, even if they want fe etc (eg Bonn). 
C        Trigger on the station name - I don't really like doing this
C        but I don't see a clean alternative.
C        These are by channel.  They are supposed to be in GHz.
C
         IF( STANAME(ISTA)(1:4) .NE. 'VLBA' ) THEN
C
C           First find the data for each  used IF channel.
C
            DO I = 1, 4
               USEDIF(I) = .FALSE.
               IF( FIRSTS ) THEN
                  LEXTLO(I) = UNSET
                  LSIDEX(I) = ' '
                  LUSEDIF(I) = .FALSE.
               END IF
               DO ICH = 1, NNCHAN
                  IF( IFCHAN(ICH,LS) .EQ. IFLABEL(I) ) THEN
                     EXTLO(I) = FIRSTLO(ICH,LS) / 1000.D0
                     SIDEX(I) = SIDE1(ICH,LS)
                     USEDIF(I) = .TRUE.
                  END IF
               END DO
            END DO
C
C           Now write out the information for any channels that
C           have changed or are seen for the first time.
C
            DO I = 1, 4
               IF( USEDIF(I) .AND. ( EXTLO(I) .NE. LEXTLO(I) .OR.
     1                SIDEX(I) .NE. LSIDEX(I) .OR. 
     2                .NOT. LUSEDIF(I) )  ) THEN
                  WRITE( IUVBA, '( A, I1, A, F14.10, A )' )
     1                'extlo = (', I, ',', EXTLO(I), ')' 
                  WRITE( IUVBA, '( A, I1, A, A1, A )' )
     1                'extlosideband = (', I, ',', SIDEX(I), ')' 
                  LEXTLO(I) = EXTLO(I)
                  LSIDEX(I) = SIDEX(I)
               END IF
               LUSEDIF(I) = USEDIF(I)
            END DO
C
         END IF
C
C        Logging type:
C
         IF( FIRSTS .OR. LOGGING(LS) .NE. LLOGGING ) THEN
            WRITE( IUVBA, '( A, A )' ) 'logging=', LOGGING(LS)
            LLOGGING = LOGGING(LS)
         END IF
C
C        Number of channels.
C
         IF( FIRSTS .OR. NNCHAN .NE. LNCHAN ) THEN 
            WRITE( IUVBA, '( A, I2 )' ) 'nchan=', NNCHAN
            LNCHAN = NNCHAN
         END IF
C
C        Various parameters in standard formats. 
C        For the format, make it "NONE" if using the RDBE.
C
         IF( FIRSTS .OR. FORMAT(LS) .NE. LFORMAT ) THEN
            IF( DAR(KSTA) .NE. 'RDBE' ) THEN
               WRITE( IUVBA, '( 2A )' )
     1             'format=', FORMAT(LS)(1:LEN1(FORMAT(LS)))
            ELSE
               IF( DOMKA ) THEN
C
C                 Write disk.  Need a format.  Be simple.
C
                  IF( SAMPRATE(LS) .GE. 32.0 ) THEN
                      WRITE( IUVBA, '( A )' ) 'format=VLBA1:4'
                  ELSE IF( SAMPRATE(LS) .EQ. 16.0 ) THEN
                      WRITE( IUVBA, '( A )' ) 'format=VLBA1:2'
                  ELSE IF( SAMPRATE(LS) .EQ. 16.0 ) THEN
                      WRITE( IUVBA, '( A )' ) 'format=VLBA1:1'
                  END IF
               ELSE
                  IF( WARNCRD ) THEN
                     CALL WRTMSG( 0, 'VLBASU', 'CRD_RDBE_Warning' )
                     WARNCRD = .FALSE.
                  END IF
                  WRITE( IUVBA, '( A )' ) 'format=NONE'
               END IF
            END IF
            LFORMAT = FORMAT(LS)
         END IF
C
C        Removed barrel roll spec for tape.
C
C        Signal routing and properties information.
C
         CALL VLBACHAR( 'ifdistr', 7, 4, IFDIST(1,LS), LIFDIST, 
     1          MIFDIST, FIRSTS, IUVBA )
         CALL VLBAINT( 'baseband', 8, NNCHAN, BBC(1,LS), LBBC,
     1          MBBC, FIRSTS, IUVBA )
         CALL VLBACHAR( 'ifchan', 6, NNCHAN, IFCHAN(1,LS), 
     1          LIFCHAN, MIFCHAN, FIRSTS, IUVBA )
         CALL VLBACHAR( 'sideband', 8, NNCHAN, SIDEBD(1,LS), 
     1          LSIDEBD, MSIDEBD, FIRSTS, IUVBA )
         CALL VLBAINT( 'bits', 4, NNCHAN, BITS(1,LS), LBITS,
     1          MBITS, FIRSTS, IUVBA )
C
C        Some parameters for which NCHAN versions should be printed,
C        but for which SCHED only allows one input.
C
         IF( FIRSTS .OR. PERIOD(LS) .NE. LPERIOD(1) ) THEN
            DO I = 1, NNCHAN
               RPERIOD(I) = PERIOD(LS)
            END DO
            CALL VLBAINT( 'period', 6, NNCHAN, RPERIOD, LPERIOD,
     1           MPERIOD, FIRSTS, IUVBA )
         END IF
C
C        Write any direct transfer strings.
C
         IF( FIRSTS ) THEN
            DO I = 1, 4
               LSTRING(I) = ' '
            END DO
         END IF
         DO I = 1, 4
            IF( STRING(I,LS) .NE. LSTRING(I) ) THEN
               LENS = LEN1( STRING(I,LS) )
               IF( LENS .NE. 0 ) THEN
                  WRITE( IUVBA, '(A)' ) STRING(I,LS)(1:LENS)
               END IF
               LSTRING(I) = STRING(I,LS)
            END IF
         END DO
C
      END IF                   ! New setup.
C
C     Reset the levels.  This can happen even without a new setup
C     if there have been pointing or Ta scans.
C
      IF( FIRSTS .OR. LEVEL(LS) .NE. LLEVEL(1) ) THEN
         DO I = 1, NNCHAN
            RLEVEL(I) = LEVEL(LS)
         END DO
         CALL VLBAINT( 'level', 5, NNCHAN, RLEVEL, LLEVEL,
     1           MLEVEL, FIRSTS, IUVBA )
      END IF
C
C     Write focus and rotation commands if needed.
C
      IF( FIRSTS ) THEN
         LFOCUS = 0.0
         LROTAT = 0.0
      END IF
      IF( ROTPAT .EQ. 0 .AND. 
     1    ( FOCUS(ISCN) .NE. LFOCUS .OR. ROTATION(ISCN) .NE. LROTAT ) )
     2     THEN
         WRITE( IUVBA, '( A, F7.2, A, F7.2 )' ) 
     1      'focus = ', FOCUS(ISCN), '  rotation = ', ROTATION(ISCN)
         LFOCUS = FOCUS(ISCN)
         LROTAT = ROTATION(ISCN)
      END IF
C
C     Colimation offsets - do on first scan, when there has been a
C     change, and when going from pointing or Ta scans to non-pointing
C     scans.  Note that colimation offsets can come from either the
C     setup file or the schedule.
C
      DOAZ = SAZCOL(ISCN) + AZCOLIM(LS)
      DOEL = SELCOL(ISCN) + ELCOLIM(LS)
C
      IF( FIRSTS .OR. DOAZ .NE. LAZCOLIM .OR. DOEL .NE. LELCOLIM .OR.
     1    ( LPNTVLBA .AND. .NOT. PNTVLBA(ISCN) ) .OR.
     2    ( LTANVLBA .AND. .NOT. TANVLBA(ISCN) ) .OR. 
     3    ( LDOPN3DB .AND. .NOT. DOPN3DB(ISCN) ) ) THEN
C
         WRITE( IUVBA, '( A, F7.2, A, F7.2 )' ) 'azcolim=', 
     1              DOAZ, '  elcolim=', DOEL
C
      END IF
      LAZCOLIM = DOAZ
      LELCOLIM = DOEL
      LPNTVLBA = PNTVLBA(ISCN)
      LTANVLBA = TANVLBA(ISCN)
      LDOPN3DB = DOPN3DB(ISCN)
C
C     Deal with synthesizer settings (freq and bw) and pulse cal.  
C     This can change scan by scan because of DOPCAL and main 
C     routine FREQ and BW.
C
      CALL WRTFREQ( ISCN, ISTA, FIRSTS, WRTSET )
C
C     Tape track and sample rate specifications.  
C     Tracks can also change without setup change, mainly on direction
C     changes when not all heads are in use.
C     Initialize the "L" variables indepently on the first scan in
C     case the output calls are skipped because the first scan is 
C     pointing with format NONE.  Without this, the station can wind
C     up with no track assignments.
C
      IF( FIRSTS ) THEN
         LSAMPR = 0.0
      END IF
C
C     Don't write formatter type commands for non-recording scans.
C     This is to try to prevent formatter reconfigures when things
C     like reference pointing are done.
C     When I first tried this, omitting the samplerate, cksched 
C     complained about incompatible samplerate and format.  So I
C     moved the NOREC check to cover only the track specification.
C     Also protect against too high samplerate when using the RDBE.
C
      IF( VLBITP .AND. FORMAT(LS) .NE. 'NONE' ) THEN
         IF( DAR(KSTA) .NE. 'RDBE' ) THEN
            DSAMPR = SAMPRATE(LS)
         ELSE
            DSAMPR = MIN( SAMPRATE(LS), 32.0 )
         END IF
         CALL VLBABWS( 'samplerate', 10, 1, DSAMPR, LSAMPR, 
     1       MSAMPR, FIRSTS, IUVBA )
C
C        Removed track assignments that were used for tape.
C
      END IF
C
C     Set frequency switching request.
C
      FRS = FRSWITCH(LS)
C
      RETURN
      END          

