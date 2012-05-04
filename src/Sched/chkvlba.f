      SUBROUTINE CHKVLBA( KS, ERRS )
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
      INTEGER     I, KS, ICH, IIF, KIIF
      LOGICAL     ERRS, OK, BADLO
      DOUBLE PRECISION  R8FREQ(3), TEST
      PARAMETER   (TEST=1.D-3)
      SAVE        IFNAMES
      CHARACTER   IFNAMES(4)*1
      DATA        IFNAMES   / 'A', 'B', 'C', 'D' /
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
C
      DO I = 1, 3
         R8FREQ(I) = DNINT( SYNTH(I,KS) * 1.D6 ) / 1.D3
         IF( BADLO( 'SYNTH X 1000.', R8FREQ(I), 500.D0, 1, 100.D0, 0.D0, 
     1               2000.0D0, 16000.D0, MSGTXT ) ) ERRS = .TRUE.
      END DO
C
C     Check the synthesizer settings against FIRSTLO.  Note that
C     this effectively checks that the right synthesizer was set.
C     Note that if alternate inputs are being used, believe the FIRSTLO.
C     It would be likely that the VLBA receiver systems are not being
C     used (eg mm VLBI)
C
C     While going through the receivers, set the synthesizer used for
C     each IF.  This is only the synthesizer 1 or 2, not synthesizer
C     3 which is used for the mix in the front end for the 1cm, 7mm, 
C     and 3mm receivers. The exception is when synthesizer 3 is used
C     for the extra bandwidth mode in S/X.  zero means none which will 
C     apply for 50/90cm.
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
C     That's all for now.  More could be added some day.
C
      RETURN
      END

