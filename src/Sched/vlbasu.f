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
      INTEGER       I, LEN1, LENS, ISTA, ISCN, ICH, KSTA
      INTEGER       CRDN, CRSETC(MAXCHN), KS, ICHS, ISYN
      LOGICAL       FIRSTS, FRS, LPNTVLBA, LTANVLBA, LDOPN3DB
      LOGICAL       WRTSET, SWARNS
      LOGICAL       SCRD, XCRD, SALL, XALL
C
C     Parameters to hold previous values to avoid duplication.
C     Each one needs a counter for the number of channels.
C
      INTEGER       MBBC, MPERIOD, MBITS, MIFDIST
      INTEGER       MSYNTH, MSIDEBD, MNOISE, MFE, MIFCHAN, MSAMPR 
C
      INTEGER       USEBBC(MCHAN), LBBC(MCHAN), LNCHAN
      INTEGER       RPERIOD(MCHAN), LPERIOD(MCHAN), LBITS(MCHAN)
      INTEGER       RLEVEL(MCHAN), TEMPINT(MAXCHN)
      REAL          LAZCOLIM, LELCOLIM, LROTAT, LFOCUS
      REAL          DOAZ, DOEL
      DOUBLE PRECISION  DSYNTH(3), LSYNTH(3), DSAMPR, LSAMPR, RSYNTH
      LOGICAL       LDUALX, LUSEDIF(4), WARNCRD, TOOMANY
      CHARACTER     LSIDEBD(MCHAN)*1, LNOISE(4)*6, LFE(4)*5
      CHARACTER     LIFCHAN(MCHAN)*1, LLOGGING*8, LSTRING(4)*80
      CHARACTER     LFORMAT*8, PFORMAT*8, LIFDIST(4)*3
      CHARACTER     LLCP50CM*6, LRCP50CM*6, LNOISEF*4
      CHARACTER     TEMPCHAR(MAXCHN)*2
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
      SAVE          LEXTLO, LSIDEX, SWARNS
C
      DATA          IFLABEL / 'A', 'B', 'C', 'D' /
      DATA          WARNCRD / .TRUE. /
C
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'VLBASU: Starting.' )
C
C     Get the number of channels and which setup channels to use.  When
C     using the RDBE, this might not be the same as the setup file.
C
      KS = NSETUP(ISCN,ISTA)
      CALL GETCRDN( ISCN, ISTA, CRDN, CRSETC )
C
C     Do an important legacy system check.  Done here rather than in chk
C     routines because it is legacy VLBA system specific, as is this routine,
C     and will go away when the VME's are replaced.  The legacy system 
C     controls the subreflector.  The new system controls the ellipsoid.  Both
C     sense the difference between S/X projects and pure S or X projects by
C     the presence of both bands among the baseband channels.  When there are
C     fewer crd channels than RDBE channels, it is possible to end up with
C     crd channels in only one band.  For example, S band can only take a small
C     number of channels from the RDBE_PFB, so simple defaulting can end up
C     only assigning X band channels.  Then the subreflector gets pointed 
C     at the X band which is blocked by the ellipsoid.  Don't ask how we lost
C     two days of wide band observing this way!
C
C     Use a simple method to detect the bad situation.  Note this might claim
C     some C band setups have X band, but that should not matter as they
C     will not have S band and will test as a single band setup.
C
      SCRD = .FALSE.
      XCRD = .FALSE.
      SALL = .FALSE.
      XALL = .FALSE.
      DO ICH = 1, CRDN
         IF( FREQREF(CRSETC(ICH),KS) .GT. 2000.D0 .AND.
     1       FREQREF(CRSETC(ICH),KS) .LT. 2900.D0 ) SCRD = .TRUE.
         IF( FREQREF(CRSETC(ICH),KS) .GT. 7700.D0 .AND.
     1       FREQREF(CRSETC(ICH),KS) .LT. 9500.D0 ) XCRD = .TRUE.
      END DO
      DO ICH = 1, NCHAN(KS)
         IF( FREQREF(ICH,KS) .GT. 2000.D0 .AND.
     1       FREQREF(ICH,KS) .LT. 2900.D0 ) SALL = .TRUE.
         IF( FREQREF(ICH,KS) .GT. 7700.D0 .AND.
     1       FREQREF(ICH,KS) .LT. 9500.D0 ) XALL = .TRUE.
      END DO
C
C     Detect an S/X observation.  If have one, make sure the crd 
C     channels cover both bands.  Note that the crd channels cannot
C     cover a band not in the main setup, so don't worry about S/X
C     in the CRD channels and only one band in the main setup - can't
C     happen.
C
      IF( XALL .AND. SALL ) THEN
         IF( .NOT. XCRD .OR. .NOT. SCRD ) THEN
            CALL WLOG( 1, 
     1         'VLBASU:  The main setup file has both S and X band.' )
            CALL WLOG( 1, 
     1         '         The crd files (VLBA legacy system) have '//
     2         'only one band. ' )
            CALL WLOG( 1, 
     1         '         The subreflector will not be positioned '//
     2         'properly.' )
            IF( .NOT. SCRD ) CALL WLOG( 1, 
     1         '         There will be no fringes!' )
            CALL WLOG( 1,
     1         '         Setup file: ' // SETNAME(KS) )
     2         
            CALL ERRLOG( '  Use SCHED defaults or use CRDSETCH '//
     1         'and CRDNCH to set both S and X band channels '//
     2         'for the crd files.' )
         END IF
      END IF
C
      IF( FIRSTS ) SWARNS = .TRUE.
C
C     Need pointer to entries in the station catalog for this 
C     schedule station ISTA.
C
      KSTA = STANUM(ISTA)
C
C     Protect against requesting too many BBC's.
C     GETCRDN also does some of this, but be sure.
C
      TOOMANY = .FALSE.
      IF( DAR(KSTA)(1:4) .EQ. 'RDBE' ) THEN
         IF( CRDN .GT. NBBC(KSTA) ) TOOMANY = .TRUE.
      ELSE
         DO I = 1, CRDN
            IF( BBC(CRSETC(I),KS) .GT. NBBC(KSTA) ) TOOMANY = .TRUE.
         END DO
      END IF
      IF( TOOMANY ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I3, A, I4, A, 2I4 )' )
     1         'VLBASU: ' // STATION(KSTA) // ' only has',
     2         NBBC(KSTA), ' BBCs.  Setup ', KS, ' requested more.',
     3         BBC(CRSETC(1),KS), BBC(CRSETC(CRDN),KS)
         CALL ERRLOG( MSGTXT )
      END IF
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

C     worry about validity of VLBADAR in the modern world.

         IF( .NOT. VLBADAR(KSTA) ) THEN
C
C           Front end spec.
C
            IF( FIRSTS ) THEN
               DO I = 1, 4
                  LFE(I) = 'xxxxx'
               END DO
            END IF
            CALL VLBACHAR( 'fe', 2, 4, FE(1,KS), LFE, MFE,
     1                     FIRSTS, IUVBA )
C
C           Set transfer switch if dual X band is to be used.
C
            IF( FIRSTS .OR. (DUALX(KS) .NEQV. LDUALX) ) THEN
               IF( DUALX(KS) ) THEN
                  WRITE( IUVBA, '( ''fexfer=(2,split)'' )' )
               ELSE
                  WRITE( IUVBA, '( ''fexfer=(2,norm)'' )' )
               END IF
               LDUALX = DUALX(KS)
            END IF
C
C           50 cm filter.  Don't write if the default of NARROW
C           is used.
C
            IF( FIRSTS ) THEN
               LLCP50CM = 'NARROW'
               LRCP50CM = 'NARROW'
            END IF
            IF( LCP50CM(KS) .NE. LLCP50CM ) THEN
               WRITE( IUVBA, '( A, A )' ) 'lcp50cm=', LCP50CM(KS)
            END IF
            IF( RCP50CM(KS) .NE. LRCP50CM ) THEN
               WRITE( IUVBA, '( A, A )' ) 'rcp50cm=', RCP50CM(KS)
            END IF
            LLCP50CM = LCP50CM(KS)
            LRCP50CM = RCP50CM(KS)
C
C           Various parameters in standard formats not wanted for VLA.
C
            CALL VLBACHAR( 'noise', 5, 4, NOISE(1,KS), LNOISE, 
     1             MNOISE, FIRSTS, IUVBA )
C
C           Round SYNTH to try to avoid digital precision issues
C           resulting in repeat prints.  It is not clear that this
C           is still needed after the change of SYNTH to double
C           precision, but it shouldn't hurt. Use 1 kHz rounding.  
C           The new synthesizers will be more finely tunable but will 
C           still be multiples of 10 kHz. Recall that the SYNTH values are GHz.
C
            DO I = 1, 3
               DSYNTH(I) = DNINT( SYNTH(I,KS) * 1.0D6 ) / 1.D6
            END DO
C
C           If using the new synthesizers (should be installed in 
C           2013/2014), lie to the legacy system to avoid generating
C           complaints about invalid frequencies.  The new synthesizers
C           are controlled by the Executor, so they will not be affected
C           by what is given here.  They, and the correlator are controlled
C           by the VEX file.  Lying here will result in incorrect frequencies
C           being given in monitor data and operator displays, but we
C           do not believe that is a problem.  Put a warning in the
C           crd file if the value has been changed.  Recall synthesizer 3
C           will still be the old style, so don't change it.
C
C
            IF( MODETEST(KS) ) THEN
               DO I = 1, 2
                  RSYNTH = 0.5D0 * DNINT( DSYNTH(I) * 2.D0 )
                  IF( DSYNTH(I) .GT. RSYNTH ) THEN
                    RSYNTH = RSYNTH + 0.1D0
                  ELSE
                    RSYNTH = RSYNTH - 0.1D0
                  END IF

                  IF( DSYNTH(I) .NE. RSYNTH .AND. SWARNS ) THEN
                     WRITE( IUVBA, '( A )' ) ' '
                     MSGTXT = '!*  synth 1 & 2 values shown are '//
     1               'not actual values used in new synthesizers.   *!'
                     WRITE( IUVBA, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
                     MSGTXT = '!*  This suppresses warnings for '//
     1               'settings that the old synthesizers cannot use.*!'
                     WRITE( IUVBA, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
                     WRITE( IUVBA, '( A )' ) ' '
                     SWARNS = .FALSE.
                  END IF
                  DSYNTH(I) = RSYNTH
               END DO
            END IF
C           
C
C           For the new (2012) C band receiver on the VLBA, the sideband
C           after the FIRSTLO mixes is not unique.  The same FIRSTLO can
C           be used for different frequencies.  The desired sideband is
C           encoded in SIDE1, so try to use that.  The variable VFESYN
C           was set in CHKVLBA to give the synthesizer being used for
C           each channel.  Here set the sign of the synthesizer setting
C           according to the first channel that uses that synthesizer.
C           Do not change anything if the synthesizer is not used (it
C           may be getting set to a benign value).  Look over all 
C           channels, not just the psuedo subset that will be written
C           for the old hardware when using the RDBE.
C
            DO ICHS = 1, NCHAN(KS)
C
C              For negative IF sideband, make DSYNTH negative.
C              Loop over all setup file parameters for this one.
C
               IF( FIRSTLO(ICHS,KS) .GT. FREQREF(ICHS,KS) .AND. 
     1            STANAME(ISTA)(1:4) .EQ. 'VLBA' ) THEN
                  ISYN = VFESYN(ICHS,KS)
                  IF( ISYN .GE. 1 .AND. ISYN .LE. 3 ) THEN
                     DSYNTH(ISYN) = -1.D0 * ABS( DSYNTH(ISYN) )
                  ELSE IF( FREQREF(ICHS,KS) .GT. 800.D0 .AND.
     1                     SETSTA(1,KS)(1:4) .EQ. 'VLBA' ) THEN
                     MSGTXT = ' ' 
                     WRITE( MSGTXT, '( A, A, I3, I4, F10.2 )' ) 
     1                  'VLBASU: Program error. ',
     2                  'VFESYN not set ', ICHS, KS, FREQREF(ICHS,KS)
                     CALL WLOG( 1, MSGTXT )
                  END IF
               END IF
            END DO
            CALL VLBAREAL( 'synth', 5, 3, DSYNTH, LSYNTH, 
     1             MSYNTH, '(F4.1)', 4, FIRSTS, IUVBA )
C
C           For Pie Town link experiments, tell PT to switch to the
C           other noise cal switching frequency.
C
           IF( STANAME(ISTA) .EQ. 'VLBA_PT' .AND. 
     1          ( FIRSTS .OR. NOISEFRQ(KS) .NE. LNOISEF ) ) THEN
               WRITE( IUVBA, '( 2A )' ) 'noisefreq=', 
     1           NOISEFRQ(KS)(1:LEN1(NOISEFRQ(KS)))
               LNOISEF = NOISEFRQ(KS)
            END IF
C
C           End of non-VLA items.
C
         ELSE
C
C           Removed some VLAMODE checking here.
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
C           For this, loop over all setup file channels rather than
C           just the ones to be specified in the crd file.
C
            DO I = 1, 4
               USEDIF(I) = .FALSE.
               IF( FIRSTS ) THEN
                  LEXTLO(I) = UNSET
                  LSIDEX(I) = ' '
                  LUSEDIF(I) = .FALSE.
               END IF
               DO ICH = 1, NCHAN(KS)
                  IF( IFCHAN(ICH,KS) .EQ. IFLABEL(I) ) THEN
                     EXTLO(I) = FIRSTLO(ICH,KS) / 1000.D0
                     SIDEX(I) = SIDE1(ICH,KS)
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
         IF( FIRSTS .OR. LOGGING(KS) .NE. LLOGGING ) THEN
            WRITE( IUVBA, '( A, A )' ) 'logging=', LOGGING(KS)
            LLOGGING = LOGGING(KS)
         END IF
C
C        Number of channels.
C
         IF( FIRSTS .OR. CRDN .NE. LNCHAN ) THEN 
            WRITE( IUVBA, '( A, I2 )' ) 'nchan=', CRDN
            LNCHAN = CRDN
         END IF
C
C        Various parameters in standard formats. 
C
C        For the format, don't just make it "NONE" if using 
C        the RDBE.  That shuts off all formatter configuration
C        which means the pulse cal doesn't get set up.
C        So specify a valid format for the samplerate.  Note
C        that CRDBW may cause a high oversampling, but that
C        should be ok.  We can't just use the setup file format
C        (formats are VDIF and MARK5B) because that will not 
C        encode the fan-out that the legacy system wants to 
C        know about.  So base it on the samprate.
C
         IF( DAR(KSTA)(1:4) .EQ. 'RDBE' ) THEN
            IF( FORMAT(KS) .EQ. 'NONE' ) THEN
               PFORMAT = 'NONE'
            ELSE IF( SAMPRATE(KS) .GT. 16.0 ) THEN
               PFORMAT = 'VLBA1:4'
            ELSE IF( SAMPRATE(KS) .EQ. 16.0 ) THEN
               PFORMAT = 'VLBA1:2'
            ELSE IF( SAMPRATE(KS) .LT. 16.0 ) THEN
               PFORMAT = 'VLBA1:1'
            ELSE
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I5, F10.2, A )' )
     1            'VLBASU:  Something wrong with SAMPRATE ',
     2            KS, SAMPRATE(KS), '  Programming error?'
               CALL ERRLOG( MSGTXT )
            END IF
C
C           Write a message about the crd files - just once.
C
            IF( WARNCRD ) THEN
               CALL WRTMSG( 0, 'VLBASU', 'CRD_RDBE_Warning' )
               WARNCRD = .FALSE.
            END IF
C
         ELSE
C
C           For non-RDBE projects, use the setup file format.
C
            PFORMAT = FORMAT(KS)
C
         END IF
C
C        Now write the format if it has changed.
C
         IF( FIRSTS .OR. PFORMAT .NE. LFORMAT ) THEN
C
C           For non-RDBE projects, use the setup file format.
C
            WRITE( IUVBA, '( 2A )' )
     1             'format=', PFORMAT(1:LEN1(PFORMAT))
         END IF
         LFORMAT = PFORMAT
C
C        Removed barrel roll spec for tape.
C
C        When the VLBA legacy system is being used for recording, 
C        or for non-VLBA systems (which shouldn't get into this routine
C        anyway), the baseband converter assignment should be
C        as per the setup file.  But when the VLBA the RDBE system
C        is used, different assignments are needed.  Most obviously,
C        when the RDBE_PFB is used, there are too many channels.
C        For these cases, the number of channels is restricted to
C        no more than 8 (4 after early 2014), the number of BBCs, and 
C        we can just assign the BBCs sequentially (BBC number = channel 
C        number).
C
C        Note CONTROL(KSTA) .EQ. 'VLBA' .OR. VLBADAR(KSTA) can be
C        assumed because of the call to VLBA in CRDWRT.  VLBA calls
C        this routine.  For non-RDBE cases, CRSETC(ICH) will almost certainly
C        be ICH and the second loop will actually be over 1 to NCHAN(KS).
C
         IF( DAR(KSTA)(1:4) .EQ. 'RDBE' ) THEN
            DO ICH = 1, CRDN
               USEBBC(ICH) = ICH
            END DO
         ELSE 
            DO ICH = 1, CRDN
               USEBBC(ICH) = BBC(CRSETC(ICH),KS)
            END DO
         END IF
C
C
C        Signal routing and properties information.
C        Now we use the restricted set of channels.
C        But IFDIST is per IF, not baseband channel.
C

         CALL VLBACHAR( 'ifdistr', 7, 4, IFDIST(1,KS), LIFDIST, 
     1          MIFDIST, FIRSTS, IUVBA )
         CALL VLBAINT( 'baseband', 8, CRDN, USEBBC, LBBC,
     1          MBBC, FIRSTS, IUVBA )
         DO ICH = 1, CRDN
            TEMPCHAR(ICH) = IFCHAN(CRSETC(ICH),KS)
         END DO
         CALL VLBACHAR( 'ifchan', 6, CRDN, TEMPCHAR, 
     1          LIFCHAN, MIFCHAN, FIRSTS, IUVBA )
         DO ICH = 1, CRDN
            TEMPCHAR(ICH) = SIDEBD(CRSETC(ICH),KS)
         END DO
         CALL VLBACHAR( 'sideband', 8, CRDN, TEMPCHAR, 
     1          LSIDEBD, MSIDEBD, FIRSTS, IUVBA )
         DO ICH = 1, CRDN
            TEMPINT(ICH) = BITS(CRSETC(ICH),KS)
         END DO
         CALL VLBAINT( 'bits', 4, CRDN, TEMPINT, LBITS,
     1          MBITS, FIRSTS, IUVBA )
C
C        Some parameters for which NCHAN versions should be printed,
C        but for which SCHED only allows one input.
C
         IF( FIRSTS .OR. PERIOD(KS) .NE. LPERIOD(1) ) THEN
            DO I = 1, CRDN
               RPERIOD(I) = PERIOD(KS)
            END DO
            CALL VLBAINT( 'period', 6, CRDN, RPERIOD, LPERIOD,
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
            IF( STRING(I,KS) .NE. LSTRING(I) ) THEN
               LENS = LEN1( STRING(I,KS) )
               IF( LENS .NE. 0 ) THEN
                  WRITE( IUVBA, '(A)' ) STRING(I,KS)(1:LENS)
               END IF
               LSTRING(I) = STRING(I,KS)
            END IF
         END DO
C
      END IF                   ! New setup.
C
C     Reset the levels.  This can happen even without a new setup
C     if there have been pointing or Ta scans.
C
      IF( FIRSTS .OR. LEVEL(KS) .NE. LLEVEL(1) ) THEN
         DO I = 1, CRDN
            RLEVEL(I) = LEVEL(KS)
         END DO
         CALL VLBAINT( 'level', 5, CRDN, RLEVEL, LLEVEL,
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
      DOAZ = SAZCOL(ISCN) + AZCOLIM(KS)
      DOEL = SELCOL(ISCN) + ELCOLIM(KS)
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
C     routine FREQ and BW.  It can also be set by the CRD parameters
C     when using the RDBE.
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
C     Note tht the samprate will be likely be rather far from 
C     appropriate if crdbw is selected far different from bbcbw.
C     I don't think that matters, but keep an eye on it.
C
      IF( VLBITP .AND. FORMAT(KS) .NE. 'NONE' ) THEN
C
         IF( DAR(KSTA)(1:4) .NE. 'RDBE' ) THEN
            DSAMPR = SAMPRATE(KS)
         ELSE
            DSAMPR = MIN( SAMPRATE(KS), 32.0 )
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
      FRS = FRSWITCH(KS)
C
      IF( DEBUG ) CALL WLOG( 0, 'VLBASU: Ending' )
      RETURN
      END          
