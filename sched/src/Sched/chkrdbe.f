      SUBROUTINE CHKRDBE( KS, ERRS )
C
C     Routine for SCHED called by CHKSET that checks items related
C     to the use of the RDBE digital backend.
C
C     The frequency and bandwidth checks are broken out to a 
C     subroutine (CHKRDFQ) so that it can also be called by FSFREQ
C     to check in-schedule changes of frequency and bandwidth.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER           KS, ICH
      LOGICAL           ERRS, DDWARN
      DATA              DDWARN / .TRUE. /
      SAVE              DDWARN
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
C        First check the RDBE_PFB personality.
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
C           All sidebands must be lower.
C
            DO ICH = 1, NCHAN(KS)
               IF( SIDEBD(ICH,KS) .NE. 'L' ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, A, A )' )
     1               'CHKRDBE: SIDEBAND must be LSB for DBE=RDBE_PFB. ',
     2               '  Value specified is: ', SIDEBD(ICH,KS)
                  CALL WLOG( 1, MSGTXT )
                  ERRS = .TRUE.
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
     1           '***** WARNING:  DBE=RDBE_DDC is not yet available. ' )
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
