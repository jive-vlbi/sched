      SUBROUTINE CHKRDBE( KS, ERRS )
C
C     Routine for SCHED called by CHKSET that checks items related
C     to the use of the RDBE digital backend.
C
C     The frequency and bandwidth checks are broken out to a 
C     subroutine (CHKRDFQ) so that it can also be called by FSFREQ
C     to check in-schedule changes of frequency and bandwidth.
C
C     The RDBE PFB personality can only do lower sideband.  But
C     DiFX can invert sidebands.  So, if an upper sideband is
C     found, add the bandwidth to the BBC frequency and invert
C     the sideband.  Oct 11, 2011.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER           KS, ICH
      LOGICAL           ERRS, DDWARN, SBWARN
      CHARACTER         USEDIFC(2)*2
      DATA              DDWARN, SBWARN / .TRUE., .TRUE. /
      SAVE              DDWARN, SBWARN
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKRDBE: Starting' )
C
C     ---------
C     Need to do something about the solar attenuators similar to 
C     the IFDIST entry for the VLBA DAR.
C     ---------
C
      IF( VLBITP .AND. FORMAT(KS) .NE. 'NONE' ) THEN 
C
C        Make sure there are only 2 IFs requested for now.
C
         USEDIFC(1) = ' '
         USEDIFC(2) = ' '
         DO ICH = 1, NCHAN(KS)
            IF( IFCHAN(ICH,KS) .EQ. USEDIFC(1) .OR. 
     1          IFCHAN(ICH,KS) .EQ. USEDIFC(2) ) THEN
C              Do nothing
            ELSE IF( USEDIFC(1) .EQ. ' ' ) THEN 
               USEDIFC(1) = IFCHAN(ICH,KS)
            ELSE IF( USEDIFC(2) .EQ. ' ' ) THEN
               USEDIFC(2) = IFCHAN(ICH,KS)
            ELSE
C
C              Here we have a problem - a third IF.
C
               CALL WLOG( 1, 'CHKRDBE: More than 2 IF''s requested '//
     1             'for RDBE.  Cannot do that yet.' )
               ERRS = .TRUE.
            END IF
         END DO
C
C        Check the RDBE_PFB personality.
C        Most checks are meant to be used both before and after 
C        the filter selection is available.
C        There is one check that is meant to be temporary that
C        forces the current frequencies.
C
         IF( DBE(KS) .EQ. 'RDBE_PFB' ) THEN
C
C           NCHAN must be 16 (in initial version).
C
            IF( NCHAN(KS) .NE. 16 ) THEN
               MSGTXT = ' '              
               WRITE( MSGTXT, '( A, A, I4 )' )
     1           'CHKRDBE: For DBE=RDBE_PFB, NCHAN must be 16.', 
     2           ' Setup specified: ', NCHAN(KS)
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
C
C           Sample rate must be 64 Msamp/sec.
C
            IF( SAMPRATE(KS) .NE. 64.0 ) THEN
               MSGTXT = ' '              
               WRITE( MSGTXT, '( A, F8.3, A )' )
     1           'CHKRDBE: Invalid SAMPRATE specified: ', SAMPRATE(KS),
     2           ' for DBE=VLBA_PFB. Must be 32.0 Msamp/s.'
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
C
C           Bits per sample must be 2.
C
            DO ICH = 1, NCHAN(KS)
               IF( BITS(ICH,KS) .NE. 2 ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, A, I3 )' )
     1               'CHKRDBE: BITS must be 2 for DBE=RDBE_PFB. ',
     2               '  Value specified is: ', BITS(ICH,KS)
                  CALL WLOG( 1, MSGTXT )
                  ERRS = .TRUE.
               END IF
            END DO
C
C           All baseband sidebands must be lower.
C           Here is were we get a bit tricky and try to use
C           inverted sidebands and let the correlator 
C           fix it.
C
            DO ICH = 1, NCHAN(KS)
               IF( SIDEBD(ICH,KS) .NE. 'L' ) THEN
                  IF( SBWARN ) THEN
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( A, A, A )' )
     1                  'CHKRDBE: SIDEBAND must be LSB for ',
     2                  '  DBE=RDBE_PFB. Value specified is: ', 
     3                  SIDEBD(ICH,KS)
                     CALL WLOG( 1, MSGTXT )
                  END IF
C
C                 Now let's fix it, if it was 'U' - ie not some
C                 other bad spec.
C                 Add other correlators to the list eventually.
C
                  IF( SIDEBD(ICH,KS) .EQ. 'U' .AND.
     1                ( CORREL .EQ. 'SOCORRO' .OR.
     2                  CORREL .EQ. 'VLBA' .OR.
     3                  CORREL .EQ. 'VLBADIFX' ) ) THEN
C
C                    Tell the user we will invert sidebands.
C
                     IF( SBWARN ) THEN
                        MSGTXT = ' '
                        WRITE( MSGTXT, '( A,A )' )
     1                     '         DiFX can invert the sideband',
     2                     ' so we''ll do that.'
                        CALL WLOG( 1, MSGTXT )
                     END IF
C
C                    Now do the inversion.
C                    For FREQREF, we need the net sideband.
C                    CORINV is the amount that got added to FREQREF 
C                    to get the LO setting. For comparison with 
C                    channels that will get correlated against
C                    this one, subtract CORINV.  Note in all cases
C                    we will be increasing the BBSYN frequency.
C                    FREQREF can go either way.
C
                     IF( NETSIDE(ICH,KS) .EQ. 'U' ) THEN
                        NETSIDE(ICH,KS) = 'L'
                        CORINV(ICH,KS) = DBLE( SAMPRATE(KS) ) / 2.D0
                     ELSE
                        NETSIDE(ICH,KS) = 'U'
                        CORINV(ICH,KS) = 
     1                       -1.D0 * DBLE( SAMPRATE(KS) ) / 2.D0
                     END IF
                     SIDEBD(ICH,KS) = 'L'
                     BBSYN(ICH,KS) = BBSYN(ICH,KS) + 
     1                              ABS( CORINV(ICH,KS) )
                     FREQREF(ICH,KS) = FREQREF(ICH,KS) + CORINV(ICH,KS)
                  ELSE
                     IF( SIDEBD(ICH,KS) .NE. 'U' ) ERRS = .TRUE.
                  END IF
                  SBWARN = .FALSE.
               END IF
            END DO
C
C           Bandwidths and frequencies will be checked in CHKRDFQ.
C
C           Still to be checked - IF, POL.
C
         END IF
C
C        ===  Now check the DDC personality. ===
C
         IF( DBE(KS) .EQ. 'RDBE_DDC' ) THEN
C
            IF( DDWARN ) THEN
               CALL WLOG( 1, 
     1           '** WARNING:  DBE=RDBE_DDC is experimental. ** ' )
            END IF
            DDWARN = .FALSE.
C
C           Set the code for testing the DDC.
C
C
C           NCHAN must be 8 or less.
C
            IF( NCHAN(KS) .GT. 8 ) THEN
               MSGTXT = ' '              
               WRITE( MSGTXT, '( A, A, I4 )' )
     1           'CHKRDBE: For DBE=RDBE_DDC, NCHAN must <= 8.', 
     2           ' Setup specified.', NCHAN(KS)
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
C
C           Sample rate can have many values.  (Not yet confirmed)
C
            IF( SAMPRATE(KS) .NE. 128.0 .AND. 
     1          SAMPRATE(KS) .NE. 64.0 .AND.
     2          SAMPRATE(KS) .NE. 32.0 .AND.
     3          SAMPRATE(KS) .NE. 16.0 .AND.
     4          SAMPRATE(KS) .NE. 8.0 .AND.
     5          SAMPRATE(KS) .NE. 4.0 .AND.
     6          SAMPRATE(KS) .NE. 2.0 .AND.
     7          SAMPRATE(KS) .NE. 1.0 .AND.
     8          SAMPRATE(KS) .NE. 0.5 .AND.
     9          SAMPRATE(KS) .NE. 0.25 ) THEN
               MSGTXT = ' '              
               WRITE( MSGTXT, '( A, F8.3, A )' )
     1           'CHKRDBE: Invalid SAMPRATE specified: ', SAMPRATE(KS),
     2           ' for DBE=VLBA_DDC. Must be 0.25 to 128 Msamp/s.'
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
C
C           Bits per sample must be 2.
C
            DO ICH = 1, NCHAN(KS)
               IF( BITS(ICH,KS) .NE. 2 ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, A, I3 )' )
     1               'CHKRDBE: BITS must be 2 for DBE=RDBE_DDC. ',
     2               '  Value specified is: ', BITS(ICH,KS)
                  CALL WLOG( 1, MSGTXT )
                  ERRS = .TRUE.
               END IF
            END DO
C
C           Bandwidths and frequencies checked in CHKRDFQ
C
         END IF
C
C        Check the frequencies and bandwidths.  This is pulled out
C        so that it can be used again later on frequencies set in-line
C        or using Doppler.
C
         CALL CHKRDFQ( KS, BBFILT(1,KS), BBSYN(1,KS), ERRS )
C
      END IF
C
      RETURN
      END
