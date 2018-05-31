      SUBROUTINE CHKVLBA( KS, ERRS )
Cf2py intent(in, out) ERRS
C
C     Routine for SCHED, called by CHKSET, that checks a number of
C     items specific to the VLBA.
C
C     Allow 4 IFs from 6cm and allow wider LO range as it is 4-8 GHz.
C     Nov 2011  RCW.
C
C     Set VFESYN while going through bands.  That is for the VLBA and
C     gives which of the front end synthesizers is being used.  This 
C     will be used later to set a sign on the synthesizer setting
C     to indicate the IF sideband.  It only worries about the 
C     second mix as the mix in the high frequency front ends
C     is (so far) always upper sideband.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
C     Test is a tolerance limit for FIRSTLO - SYNTH tests in MHz.
C
      INTEGER     I, KS, ICH, IIF, KIIF, JCH
      LOGICAL     ERRS, OK, BADLO, LOIFWARN, IFOK
      LOGICAL     WARNED(MAXSET), GOTWARN
      DOUBLE PRECISION  R8FREQ(3), TEST, TFR1, TFR2
      DOUBLE PRECISION  FRLOW, FRHIGH
      PARAMETER   (TEST=1.D-3)
      SAVE        IFNAMES, WARNED
      CHARACTER   IFNAMES(4)*1
      DATA        IFNAMES   / 'A', 'B', 'C', 'D' /
      DATA        WARNED / MAXSET * .FALSE. /
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKVLBA: Starting VLBA checks.' )
C
C     Check the FE specifications.
C     For understandability, step through the allowed combinations.
C
      OK = .FALSE.
C
      IF( ( FE(2,KS) .EQ. '90cm' .OR. FE(2,KS) .EQ. '50cm' .OR. 
     1      FE(2,KS) .EQ. 'omit' ) .AND.
     2    ( FE(4,KS) .EQ. '90cm' .OR. FE(4,KS) .EQ. '50cm' .OR. 
     3      FE(4,KS) .EQ. 'omit' ) .AND.
     4    ( FE(1,KS) .EQ. 'omit' .AND. FE(3,KS) .EQ. 'omit' ) ) 
     5   OK = .TRUE.
C
      IF( ( FE(1,KS) .EQ. '20cm' .OR. FE(1,KS) .EQ. 'omit' ) .AND.
     1    ( FE(3,KS) .EQ. '20cm' .OR. FE(3,KS) .EQ. 'omit' ) .AND.
     2    ( FE(2,KS) .EQ. '90cm' .OR. FE(2,KS) .EQ. 'omit' ) .AND. 
     3    ( FE(4,KS) .EQ. '90cm' .OR. FE(4,KS) .EQ. 'omit' ) ) 
     4   OK = .TRUE.
C
      IF( ( FE(1,KS) .EQ. '13cm' .OR. FE(1,KS) .EQ. 'omit' ) .AND.
     1    ( FE(3,KS) .EQ. '13cm' .OR. FE(3,KS) .EQ. 'omit' ) .AND.
     2    ( FE(2,KS) .EQ. '4cm' .OR. FE(2,KS) .EQ. 'omit' ) .AND.
     3    ( FE(4,KS) .EQ. '4cm' .OR. FE(4,KS) .EQ. 'omit' ) ) 
     4   OK = .TRUE.
C
      IF( ( FE(1,KS) .EQ. '6cm'  .OR. FE(1,KS) .EQ. 'omit' ) .AND.
     1    ( FE(3,KS) .EQ. '6cm'  .OR. FE(3,KS) .EQ. 'omit' ) .AND. 
     2    ( FE(2,KS) .EQ. '6cm'  .OR. FE(2,KS) .EQ. 'omit' ) .AND. 
     3    ( FE(4,KS) .EQ. '6cm'  .OR. FE(4,KS) .EQ. 'omit' ) )
     4   OK = .TRUE.
C
      IF( ( FE(2,KS) .EQ. '3cm' .OR. FE(2,KS) .EQ. 'omit' ) .AND.
     1    ( FE(4,KS) .EQ. '3cm' .OR. FE(4,KS) .EQ. 'omit' ) .AND.
     2    ( FE(1,KS) .EQ. 'omit' .AND. FE(3,KS) .EQ. 'omit' ) ) 
     3   OK = .TRUE.
C
      IF( ( FE(2,KS) .EQ. '2cm' .OR. FE(2,KS) .EQ. 'omit' ) .AND.
     1    ( FE(4,KS) .EQ. '2cm' .OR. FE(4,KS) .EQ. 'omit' ) .AND.
     2    ( FE(1,KS) .EQ. 'omit' .AND. FE(3,KS) .EQ. 'omit' ) ) 
     3   OK = .TRUE.
C
      IF( ( FE(2,KS) .EQ. '1cm' .OR. FE(2,KS) .EQ. '1.3cm' .OR. 
     1      FE(2,KS) .EQ. 'omit' ) .AND.
     2    ( FE(4,KS) .EQ. '1cm' .OR. FE(4,KS) .EQ. '1.3cm' .OR. 
     3      FE(4,KS) .EQ. 'omit' ) .AND.
     2    ( FE(1,KS) .EQ. 'omit' .AND. FE(3,KS) .EQ. 'omit' ) ) 
     3   OK = .TRUE.
C
      IF( ( FE(1,KS) .EQ. '7mm'  .OR. FE(1,KS) .EQ. 'omit' ) .AND.
     1    ( FE(3,KS) .EQ. '7mm'  .OR. FE(3,KS) .EQ. 'omit' ) .AND.
     2    ( FE(2,KS) .EQ. 'omit' .OR. FE(4,KS) .EQ. 'omit' ) ) 
     3   OK = .TRUE.
C
      IF( ( FE(2,KS) .EQ. '3mm' .OR. FE(2,KS) .EQ. 'omit' ) .AND.
     1    ( FE(4,KS) .EQ. '3mm' .OR. FE(4,KS) .EQ. 'omit' ) .AND.
     2    ( FE(1,KS) .EQ. 'omit' .AND. FE(3,KS) .EQ. 'omit' ) ) 
     3   OK = .TRUE.
C
      IF( .NOT. OK ) THEN
         CALL WLOG( 1, 'CHKVLBA: Invalid combination of FEs: ' )
         CALL WLOG( 1, '         1:'//FE(1,KS)//' 2:'//FE(2,KS)//
     1                ' 3:'//FE(3,KS)//' 4:'//FE(4,KS) )     
         ERRS = .TRUE.
      END IF
C
C     Make sure some front end was specified.
C
      IF( FE(1,KS) .EQ. 'omit' .AND. FE(2,KS) .EQ. 'omit' .AND.
     1    FE(3,KS) .EQ. 'omit' .AND. FE(4,KS) .EQ. 'omit' ) THEN
         CALL WLOG( 1, 'CHKVLBA: Some FEs must be specified for'//
     1           ' the VLBA.' )
         ERRS = .TRUE.
      END IF
C
C     Check for valid IF names.  I think user supplied names get
C     checked earlier, but an error can happen with freq.dat if some
C     channels, but not all, fit in the best freq.dat entry.  I had
C     this happen with and S/X wideband setup where I had not set
C     the sidebands properly, but had set the LOs.
C
      DO ICH = 1, NCHAN(KS)
         IFOK = .FALSE.
         DO IIF = 1, 4
            IF( IFCHAN(ICH,KS) .EQ. IFNAMES(IIF) ) THEN
               IFOK = .TRUE.
            END IF
         END DO
         IF( .NOT. IFOK ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I3, 3A )' )
     1         'CHKVLBA: Bad IF channel name, channel: ', ICH, 
     2         ', name: ''', IFCHAN(ICH,KS), ''''
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, '         Hint - check what came from ' //
     1         'the frequency file.' )
            ERRS = .TRUE.
         END IF
      END DO
C
C     Check the channels to be sure a front end was specified.
C     Allow for alternate inputs - eg mm VLBI.
C
      DO ICH = 1, NCHAN(KS)
         DO IIF = 1, 4
            IF( IFCHAN(ICH,KS) .EQ. IFNAMES(IIF) .AND. 
     1          FE(IIF,KS) .EQ. 'omit' .AND. (
     2          IFDIST(IIF,KS) .NE. 'A  ' .AND. 
     3          IFDIST(IIF,KS) .NE. 'A20' .AND.
     4          IFDIST(IIF,KS) .NE. '20A' ) ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I2, A, A, A, I2, A )' )
     1            'CHKVLBA: IF for channel ', ICH, ' is ', IFNAMES(IIF),
     2            ' but FE(', IIF, ') was not specified. '
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
            IF( IFCHAN(ICH,KS) .EQ. IFNAMES(IIF) .AND. (
     1          IFDIST(IIF,KS) .EQ. 'A  ' .OR.
     2          IFDIST(IIF,KS) .EQ. 'A20' .OR.
     3          IFDIST(IIF,KS) .EQ. '20A' ) ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( 2A )' )
     1           'CHKVLBA: WARNING - alternate IF specified for IF ', 
     2           IFNAMES(IIF)
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1, '          I hope you know what you '//
     1                      'are doing!')
            END IF
         END DO
      END DO
C
C     Now check the synthesizer settings.  R8FREQ is a double
C     precision version of SYNTH in MHz.  It is used later too.
C     March 2013 SYNTH converted to double so this is simpler.  It is
C     now just the MHz version of Synth.
C
      DO I = 1, 3
C         R8FREQ(I) = DNINT( SYNTH(I,KS) * 1.D6 ) / 1.D3
         R8FREQ(I) = SYNTH(I,KS) * 1.D3
C
C        We need two versions here, one for the old synthesizers and
C        one for the new.  For the transition, only allow the fine
C        tuning if MODETEST is set.  Also, the new synthesizer hardware
C        will only be used for synthesizers 1 and 2.  An old style
C        synthesizer with its coarse tuning restrictions will be 
C        used for synthesizer 3.  The old synthesizers are thought to
C        put out a cleaner signal and, hence, are better for multiplying
C        up for receiver first LOs.
C
         IF( MODETEST(KS) .AND. ( I .EQ. 1 .OR. I .EQ. 2 ) ) THEN
            IF( BADLO( 'SYNTH X 1000.', R8FREQ(I), 0.01D0, 0, 0.D0,
     1          0.D0, 2000.0D0, 16000.D0, MSGTXT ) ) ERRS = .TRUE.
         ELSE
            IF( BADLO( 'SYNTH X 1000.', R8FREQ(I), 500.D0, 1, 100.D0, 
     1          0.D0, 2000.0D0, 16000.D0, MSGTXT ) ) ERRS = .TRUE.
         END IF
      END DO
C
C     Check the synthesizer settings against FIRSTLO.  Note that
C     this effectively checks that the right synthesizer was set.
C     Note that if alternate inputs are being used, believe the FIRSTLO.
C     It would be likely that the VLBA receiver systems are not being
C     used (eg mm VLBI)
C
C     While going through the receivers, set the bookkeeping of which
C     synthesizer is used for each IF.  This is only the synthesizer 
C     1 or 2, not synthesizer 3 which is used for the mix in the front 
C     end for the 1cm, 7mm, and 3mm receivers. The exception is when 
C     synthesizer 3 is used for the extra bandwidth mode in S/X.  zero 
C     means none which will apply for 50/90cm.
C
      DO ICH = 1, NCHAN(KS)
         OK = .TRUE.
         KIIF = 0
         VFESYN(ICH,KS) = 0
         DO IIF = 1, 4
            IF( IFCHAN(ICH,KS) .EQ. IFNAMES(IIF) .AND. (
     1          IFDIST(IIF,KS) .NE. 'A  ' .AND. 
     2          IFDIST(IIF,KS) .NE. 'A20' .AND.
     3          IFDIST(IIF,KS) .NE. '20A' ) ) THEN
C
C              90 and 50 cm use fixed LO's .  Actually, these bands
C              are equivalent - the frequency is selected with the
C              BBC frequency.  90cm has 500 MHz added.  50cm passes
C              straight through to the IF.
C
               IF( FE(IIF,KS) .EQ. '90cm' .OR. FE(IIF,KS) .EQ. '50cm' )
     1             THEN
                  IF( FIRSTLO(ICH,KS) .NE. -500.0D0 .AND.
     1                FIRSTLO(ICH,KS) .NE. 0.0D0 ) OK = .FALSE.
                  VFESYN(ICH,KS) = 0
C
C              4cm can use either of two synthesizers.
C
               ELSE IF( FE(IIF,KS) .EQ. '4cm' .AND. DUALX(KS) ) THEN
                  IF( IFCHAN(ICH,KS) .EQ. 'B' ) THEN
                     IF( ABS( FIRSTLO(ICH,KS) - R8FREQ(1) ) .GT. TEST )
     1                  OK = .FALSE.
                     VFESYN(ICH,KS) = 1
                  ELSE IF( IFCHAN(ICH,KS) .EQ. 'D' ) THEN
                     IF( ABS( FIRSTLO(ICH,KS) - R8FREQ(3) ) .GT. TEST )
     1                     OK = .FALSE.
                     VFESYN(ICH,KS) = 3
                  ELSE
                     CALL WLOG( 1, 'CHKVLBA: Bad IFCHAN in DUALX mode.')
                     ERRS = .TRUE.
                  END IF
C
C              Bands that use synthesizer 1.
C
               ELSE IF( FE(IIF,KS) .EQ. '3cm' .OR.
     1             ( FE(IIF,KS) .EQ. '4cm' .AND. .NOT. DUALX(KS) ) .OR.
     2               FE(IIF,KS) .EQ. '2cm' ) THEN
                  IF( ABS( FIRSTLO(ICH,KS) - R8FREQ(1) ) .GT. TEST )
     1               OK = .FALSE.
                  VFESYN(ICH,KS) = 1
C
C              Bands that use synthesizer 2.
C
               ELSE IF( FE(IIF,KS) .EQ. '20cm' .OR.
     1                  FE(IIF,KS) .EQ. '13cm' ) THEN
                  IF( ABS( FIRSTLO(ICH,KS) - R8FREQ(2) ) .GT. TEST) 
     1                OK = .FALSE.
                  VFESYN(ICH,KS) = 2
C
C              Deal with 6 cm which can use either.
C
               ELSE IF( FE(IIF,KS) .EQ. '6cm' .AND. 
     1                ( IFCHAN(ICH,KS) .EQ. 'A' .OR. 
     2                  IFCHAN(ICH,KS) .EQ. 'C' ) ) THEN
                  IF( ABS( FIRSTLO(ICH,KS) - R8FREQ(2) ).GT.TEST ) THEN
                     OK = .FALSE.
                  END IF
                  VFESYN(ICH,KS) = 2
               ELSE IF( FE(IIF,KS) .EQ. '6cm' .AND. 
     1                ( IFCHAN(ICH,KS) .EQ. 'B' .OR. 
     2                  IFCHAN(ICH,KS) .EQ. 'D' ) ) THEN
                  IF( ABS( FIRSTLO(ICH,KS) - R8FREQ(1) ).GT.TEST ) THEN
                     OK = .FALSE.
                  END IF
                  VFESYN(ICH,KS) = 1
C
C              1cm, 7mm, and 3mm use 2 synthesizers.
C
               ELSE IF ( FE(IIF,KS) .EQ. '1cm' .OR.
     1                   FE(IIF,KS) .EQ. '1.3cm' ) THEN
                  IF( ABS( FIRSTLO(ICH,KS) - ( R8FREQ(1) + R8FREQ(3) ) )
     1               .GT. TEST ) OK = .FALSE.
                  VFESYN(ICH,KS) = 1
C
               ELSE IF ( FE(IIF,KS) .EQ. '7mm' ) THEN
                  IF( ABS( FIRSTLO(ICH,KS) - 
     1                ( R8FREQ(2) + 3.0D0 * R8FREQ(3) ) ) .GT. TEST ) 
     2               OK = .FALSE.
                  VFESYN(ICH,KS) = 2
C
               ELSE IF ( FE(IIF,KS) .EQ. '3mm' ) THEN
                  IF( ABS( FIRSTLO(ICH,KS) - 
     1                ( R8FREQ(1) + 6.0D0 * R8FREQ(3) ) ) .GT. TEST ) 
     2               OK = .FALSE.
                  VFESYN(ICH,KS) = 1
C
C              Shouldn't get here.
C
               ELSE
                  CALL WLOG( 1, 'CHKVLBA: Cannot check synthesizers '//
     1                  'because of bad FE' )
                  ERRS = .TRUE.
               END IF              
               KIIF = IIF
            END IF
         END DO
C
C        Write message for this channel.
C
         IF( .NOT. OK ) THEN
            MSGTXT = ' '
            IF( IIF .NE. 0 ) THEN
               WRITE( MSGTXT, '( A, I2, A, A )' )
     1           'CHKVLBA: FIRSTLO for channel ', ICH, 
     2           ' incorrect for SYNTH setting for ', FE(KIIF,KS)
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1, '         Check settings and which '//
     1                   'synthesizer is being set.' )
            ELSE
               CALL WLOG( 1, 'CHKVLBA: Bad IFCHAN spec - cannot '//
     1               'check FIRSTLO' )
            END IF
            ERRS = .TRUE.
         END IF
C
      END DO      
C
C     Check the 50 cm filter.
C
      IF( LCP50CM(KS) .NE. 'NARROW' .AND. LCP50CM(KS) .NE. 'BROAD' )
     1   THEN
         CALL WLOG( 1, 'CHKVLBA: Invalid LCP50CM = '//LCP50CM(KS) )
         ERRS = .TRUE.
      END IF
      IF( RCP50CM(KS) .NE. 'NARROW' .AND. RCP50CM(KS) .NE. 'BROAD' )
     1   THEN
         CALL WLOG( 1, 'CHKVLBA: Invalid RCP50CM = '//RCP50CM(KS) )
         ERRS = .TRUE.
      END IF
C
C     Check the noise specifications.
C
      IF( NOISEFRQ(KS) .NE. 'VLA' .AND. NOISEFRQ(KS) .NE. 'VLBA' ) THEN
         CALL WLOG( 1, 'CHKVLBA: Invalid NOISEFRQ = '//NOISEFRQ(KS) )
         ERRS = .TRUE.
      END IF
      DO IIF = 1, 4
         IF( NOISE(IIF,KS) .NE. 'off' .AND.
     1       NOISE(IIF,KS) .NE. 'low-s' .AND.
     2       NOISE(IIF,KS) .NE. 'low-c' .AND.
     3       NOISE(IIF,KS) .NE. 'high-s' .AND.
     4       NOISE(IIF,KS) .NE. 'high-c' .AND.
     5       NOISE(IIF,KS) .NE. 'low-s' ) THEN
            CALL WLOG( 1, 'CHKVLBA: Invalid NOISE = '//NOISE(IIF,KS) )
            ERRS = .TRUE.
         END IF
      END DO
C
C     Try to prevent LOs from some channels interfering with other 
C     channels.  This is mainly an issue for the wide C band but 
C     do a more generic check.  Don't attempt to be clever to avoid
C     looking at all channels.
C
C     The limits used for the test are rather arbitrary and drawn out
C     of a hat, but should be reasonable.
C     Don't check P band this way.
C     Jan. 10, 2014  Saw plots of the filter shapes today.  The IF
C     filters in the IF converters are very soft on the low side and
C     somewhat sharper on the high side.  Plus the RDBE band is shifted
C     higher than the previous center.  Previously the low and high
C     limits for this warning were 400 and 1200.  Keep the 1200,
C     but drop the lower limit to 300.
C
      LOIFWARN = .TRUE.
      DO ICH = 1, NCHAN(KS)
         IF( FIRSTLO(ICH,KS) .GT. 1000.D0 ) THEN
            IF( SIDE1(ICH,KS) .EQ. 'U' ) THEN
               TFR1 = FIRSTLO(ICH,KS) + 300.0D0
               TFR2 = FIRSTLO(ICH,KS) + 1200.0D0
            ELSE 
               TFR1 = FIRSTLO(ICH,KS) - 1200.0D0
               TFR2 = FIRSTLO(ICH,KS) - 300.0D0
            END IF
            DO JCH = 1, NCHAN(KS)
               IF( FIRSTLO(JCH,KS) .GT. TFR1 .AND. 
     1             FIRSTLO(JCH,KS) .LT. TFR2 .AND.
     2             LOIFWARN .AND. FIRSTLO(JCH,KS) .GT. 1000.D0 ) THEN
                  CALL WLOG( 1, ' ' )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I3, A, A, A )' )
     1               'CHKVLBA WARNING:  In setup ', KS, ' the first ',
     2               ' LO for one channel is in or close to the IF ',
     3               ' for another.'
                  CALL WLOG( 1, MSGTXT )
                  CALL WLOG( 1, 
     1              '               This can cause bad interference' )
                  CALL WLOG( 1,
     1              '               See sched.runlog for details.' )
                  CALL PRTSET( KS, ILOG ) 
                  LOIFWARN = .FALSE.
               END IF
            END DO
         END IF
      END DO
C
C     Do some special S band checks.
C
      IF( ( FE(1,KS) .EQ. '13cm' .OR. FE(1,KS) .EQ. '13cm' ) .AND.
     1    .NOT. WARNED(ISETNUM(KS)) ) THEN
         GOTWARN = .FALSE.
         DO ICH = 1, NCHAN(KS)
            IF( IFCHAN(ICH,KS) .EQ. 'A' .OR. 
     1          IFCHAN(ICH,KS) .EQ. 'C' ) THEN
C
C              Get the high and low RF frequencies.
C
               IF( NETSIDE(ICH,KS) .EQ. 'U' ) THEN
                  FRLOW = FREQREF(ICH,KS)
                  FRHIGH = FREQREF(ICH,KS) + BBFILT(ICH,KS)
               ELSE
                  FRLOW = FREQREF(ICH,KS) - BBFILT(ICH,KS)
                  FRHIGH = FREQREF(ICH,KS)
               END IF
C
C              Warn if out of RFI filter.
C
               IF( FRLOW .LT. 2200.D0 ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I4, A, A, A, I3, A, F7.0, ' //
     1                    ' A, F7.0, A )' )
     2                'CHKVLBA:  Setup ', KS, ' at ', SETSTA(1,KS),
     3                '.  Channel ', ICH, ', ', FRLOW, '-', FRHIGH,
     4                ' MHz partially or fully '
                  CALL WLOG( 1, MSGTXT )
C
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, A )' )
     1                '          below the bottom of the 2200-2400 ',
     2                 'MHz RFI filter at all sites but PT, MK, FD'
                  CALL WLOG( 1, MSGTXT )
                  GOTWARN = .TRUE.
               END IF

               IF( FRHIGH .GT. 2400.D0 ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I4, A, A, A, I3, A, F7.0, ' //
     1                    ' A, F7.0, A )' )
     2                'CHKVLBA:  Setup ', KS, ' at ', SETSTA(1,KS),
     3                '.  Channel ', ICH, ', ', FRLOW, '-', FRHIGH,
     4                ' MHz partially or fully '
                  CALL WLOG( 1, MSGTXT )
C
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, A )' )
     1                '          above the top of the 2200-2400 ',
     2                 'MHz RFI filter at all sites but PT, MK, FD'
                  CALL WLOG( 1, MSGTXT )
                  GOTWARN = .TRUE.
               END IF
C
C              Warn of satellite radio
C
               IF( FRLOW .LT. 2345.D0 .AND. FRHIGH .GT. 2320.D0 ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I4, A, A, A, I3, A,  F7.0, ' //
     1                    ' A, F7.0, A )' )
     2                'CHKVLBA:  Setup ', KS, ' at ', SETSTA(1,KS),
     3                '.  Channel ', ICH, ', ', FRLOW, '-', FRHIGH,
     4                ' MHz will be affected'
                  CALL WLOG( 1, MSGTXT )
C
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, A )' )
     1                '          by strong RFI from Sirius and ',
     2                 'XM satellite radio between 2320 and 2345 MHz.'
                  CALL WLOG( 1, MSGTXT )
                  GOTWARN = .TRUE.
               END IF
            END IF
         END DO
         IF( GOTWARN ) THEN
            WARNED(ISETNUM(KS)) = .TRUE.
            CALL WLOG( 1, 'CHKVLBA:  S band warnings not repeated '//
     1          'for other stations using the same setup file.' )
         END IF
      END IF
C
C     That's all for now.  More could be added some day.
C
      RETURN
      END

