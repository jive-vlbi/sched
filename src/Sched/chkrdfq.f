      SUBROUTINE CHKRDFQ( KS, BBCBW, BBCFREQ, ERRS )
C
C     Routine for SCHED called by CHKRDBE and FSFREQ that checks
C     the frequencies and bandwidths requested for the RDBE.
C     CHKRDBE checks the input setup file.  FSFREQ checks just
C     before writing the scan info to be sure in-line changes
C     don't cause trouble.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER           KS, ICH, ISIDEBD
      DOUBLE PRECISION  BBOFF, BB1, BB2, CR1, CR2
      DOUBLE PRECISION  BBCBW(*), BBCFREQ(*)
      LOGICAL           ERRS, DEQUAL
C -----------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKRDFQ: Starting' )
C
C     Only do anything if this is a recording scan.
C     (Actually the checks might be needed for non-recording scans too.
C
      IF( VLBITP .AND. FORMAT(KS) .NE. 'NONE' ) THEN 
C
C        First check the RDBE_PFB personality.
C
         IF( DBE(KS) .EQ. 'RDBE_PFB' ) THEN
C
C           All bandwidths must be 32 MHz.
C
            DO ICH = 1, NCHAN(KS)
               IF( BBCBW(ICH) .NE. 32.0D0 ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, A, F8.3 )' )
     1               'CHKRDFQ: Bandwidth must be 32.0 MHz for ',
     2               'DBE=RDBE_PFB. Value specified is: ', 
     3               SIDEBD(ICH,KS)
                  CALL WLOG( 1, MSGTXT )
                  ERRS = .TRUE.
               END IF
            END DO
C
C           Baseband frequencies must be 1024.0-16.0-N*32.0 for
C           N = 0 to 14.
C
            DO ICH = 1, NCHAN(KS)
               BBOFF = 1024.0D0 - 16.0D0 - BBCFREQ(ICH)
               IF( MOD( BBOFF, 32.0D0 ) .NE. 0.0D0 .OR.
     1             NINT( BBOFF / 32.0D0 ) .LT. 0 .OR.
     2             NINT( BBOFF / 32.0D0 ) .GT. 14 ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, F8.2, A )' )
     1               'CHKRDFQ: Invalid BBSYN for DBE=RDBE_PFB: ', 
     2               BBCFREQ(ICH),
     3               '.  Must be 1024-16-N*32 for N=0-14.'
                  CALL WLOG( 1, MSGTXT )
                  ERRS = .TRUE.
               END IF
            END DO
C
C           Temporarily, BBSYN must be 1024-16-INT((ICH-1)/2)*2*32.
C           Complain, but don't trigger an error because soon we'll
C           have the switched version.  Actually it is so close I'm
C           going to comment this out.
C
C            DO ICH = 1, NCHAN(KS)
C               BBOFF = 1024.0D0 - 16.0D0 - INT( (ICH-1)/2) * 64.0D0
C               IF( BBCFREQ(ICH) .NE. BBOFF ) THEN
C                  MSGTXT = ' '
C                  WRITE( MSGTXT, '( A, A, F8.2 )' )
C     1               'CHKRDFQ: Wrong BBSYN for initial implementation ',
C     2               'of the RDBE (DBE=RDBE_PFB): ', 
C     3               BBCFREQ(ICH)
C                  CALL WLOG( 1, MSGTXT )
C                  MSGTXT = ' '
C                  WRITE( MSGTXT, '( A, I5, A, F8.3, A )' )
C     1               '   For channel ', ICH, ' the frequency must be',
C     2               1024.0-16.0-INT((ICH-1)/2)*2.0*32.0,
C     3               '  (ie 1024-16-INT((ICH-1)/2)*2*32).'
C                  CALL WLOG( 1, MSGTXT )
C
C               END IF
C            END DO
         END IF
C
C        ===  Now check the DDC personality. ===
C
         IF( DBE(KS) .EQ. 'RDBE_DDC' ) THEN
C
C           All bandwidths must be between 125 kHz and 64 MHz.
C
            DO ICH = 1, NCHAN(KS)
               IF( .NOT. ( DEQUAL( BBCBW(ICH), 64.0D0 ) .OR. 
     1             DEQUAL( BBCBW(ICH), 32.0D0 ) .OR. 
     2             DEQUAL( BBCBW(ICH), 16.0D0 ) .OR. 
     3             DEQUAL( BBCBW(ICH), 8.0D0 ) .OR. 
     4             DEQUAL( BBCBW(ICH), 4.0D0 ) .OR. 
     5             DEQUAL( BBCBW(ICH), 2.0D0 ) .OR. 
     6             DEQUAL( BBCBW(ICH), 1.0D0 ) .OR. 
     7             DEQUAL( BBCBW(ICH), 0.5D0 ) .OR. 
     8             DEQUAL( BBCBW(ICH), 0.25D0 ) .OR. 
     9             DEQUAL( BBCBW(ICH), 0.125D0 ) ) ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, A, F8.3 )' )
     1               'CHKRDFQ: Bandwidth must be 0.125 to 64 MHz for ',
     2               'DBE=RDBE_DDC. Value specified is: ', 
     3               SIDEBD(ICH,KS)
                  CALL WLOG( 1, MSGTXT )
                  ERRS = .TRUE.
               END IF
            END DO
C
C           Baseband frequencies must be between 512 and 1024 MHz.
C           They can be set to within 1 Hz, but the BBSYN variable
C           is only REAL*4 which is inadequate to store that level
C           of precision.  Therefore, until the infrastructure is
C           improved, force the use of even 10 kHz.
C           Finally prevent basebands crossing the crossover points.
C
            DO ICH = 1, NCHAN(KS)
               IF( BBCFREQ(ICH) .LT. 512.0D0 .OR.
     1             BBCFREQ(ICH) .GT. 1024.0D0 ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, F8.2, A )' )
     1               'CHKRDFQ: Invalid BBSYN for DBE=RDBE_DDC: ', 
     2               BBCFREQ(ICH),
     3               '.  Must be between 512 and 1024 MHz.'
                  CALL WLOG( 1, MSGTXT )
                  ERRS = .TRUE.
               END IF
               IF( DMOD( BBCFREQ(ICH) + 1.D-7, 0.01D0 ) .GT. 1.D-6 ) 
     1             THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( 3A )' ) 'CHKRDFQ:  ',
     1              ' BBSYN for the RDBE_DDC must be an even 10kHz.'
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( 2A, F15.6, A, I3, A, I3 )' ) 
     1               '          ',
     2               ' Value of: ', BBCFREQ(ICH), 
     3               ' found in channel ', ICH, ' of setup ', ks
                  CALL WLOG( 1, MSGTXT )
               END IF
C
C              Test the crossover points for the DDC.
C              Use the trick that, if one filter edge is above the
C              crossover point and the other below, the product of
C              the differences between the filter edges and the 
C              crossover frequency will be negative.  Otherwise it
C              will be positive (both differences positive or both
C              negative).
C
               ISIDEBD = 1
               IF( SIDEBD(ICH,KS) .EQ. 'L' ) ISIDEBD = -1
               BB1 = BBCFREQ(ICH)
               BB2 = BBCFREQ(ICH) + ISIDEBD * BBCBW(ICH)
               CR1 = 640.0D0
               CR2 = 896.0D0
               IF( ( BB1 - CR1 ) * ( BB2 - CR1 ) .LT. 0.0D0 .OR.
     1             ( BB1 - CR2 ) * ( BB2 - CR2 ) .LT. 0.0D0 ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I4, A, I4, A, F10.2, A )' )
     1               'CHKRDFQ: Baseband ', ICH, ' in setup ', KS, 
     2               ' at ', BBCFREQ(ICH), ' MHz spans a crossover.'
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A )' )
     1               '         This will produce corrupted data.'
                  CALL WLOG( 1, MSGTXT )
               END IF
            END DO
C
         END IF
      END IF
C
      RETURN
      END
