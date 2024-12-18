      SUBROUTINE CHKDBFQ( KS, BBCBW, BBCFREQ, ERRS, VERBOSE )
Cf2py intent(in) KS, BBCBW, BBCFREQ, VERBOSE
Cf2py intent(in, out) ERRS
C
C     Routine for SCHED called by CHKDBBC and FSFREQ that checks
C     the frequencies and bandwidths requested for the DBBC.
C     CHKDBBC checks the input setup file.  FSFREQ checks just
C     before writing the scan info to be sure in-line changes
C     don't cause trouble.
C
C     Note that this routine can get called multiple times for 
C     each setup file, especially if different frequency sets
C     are being checked.  Also, since it is called from FSFREQ,
C     it can get called from print routines etc. The VERBOSE parameter
C     lets you control the output volume at call time.
C
C     Original based on CHKDBFQ, but modified by CR to the best of
C     current knowledge...
C     Removed unused variables from declarations.  Aug. 30, 2013. RCW
C     2017-06-22 added DBBC filter 4 (1024-1536 MHz). CR
C     2018-06-28 added DBBC filters 3 (1536-2048 MHz) and 6 (0-1024 MHz). CR
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER           I
      INTEGER           KS, ICH, JCH, ISIDEBD ! , nwarn
      INTEGER           N40WARN(MAXSET), N528WARN(MAXSET)
      INTEGER           NFWARN, NIFERR
      INTEGER           LEN1, ISETF
      INTEGER           NFILT, FILT_USED, IFILT
      DOUBLE PRECISION  BBOFF, BB1, BB2, SLOP, IF_LOW, IF_HI
      DOUBLE PRECISION  BBCBW(*), BBCFREQ(*)
      PARAMETER         (NFILT=6)
      DOUBLE PRECISION  FILT_LOW(NFILT), FILT_HI(NFILT)
      LOGICAL           ERRS, DEQUAL, OBWARN, SHOWID, FILTERR, ALREADY
      LOGICAL           VERBOSE, FILTWARN
      CHARACTER         IFERRS(MCHAN)*2
C
      DATA     OBWARN   / .TRUE. /
      DATA     N40WARN  / MAXSET*1 /
      DATA     N528WARN / MAXSET*1 /
      DATA     NFWARN   / 0 /
      SAVE     N40WARN, N528WARN, NFWARN, OBWARN
C
C     FILT_LOW and FILT_HI are low and high boundaries for each of the
C     DBBC filters. Order is important as only filters 1&2 are available
C     on *all* DBBC versions. See code for further explanation.
C
      DATA     FILT_LOW / 10.0D0, 512.0D0, 1024.0D0, 10.0D0,
     1                   1536.0D0, 1150.0D0 /
      DATA     FILT_HI / 512.0D0, 1024.0D0, 1536.0D0, 1024.0D0,
     1                   2048.0D0, 1750.0D0 /
C -----------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKDBFQ: Starting' )
C
      SHOWID = .FALSE.
C
C     Get the setup file number
C
      ISETF = ISETNUM(KS)

C
C     Note the checks are needed for all setups, not just ones used for
C     recording.
C
C     First check the DBBC_PFB personality.
C
      IF( DBE(KS) .EQ. 'DBBC_PFB' ) THEN
C
C        All bandwidths must be 32 MHz.
C   ***********************  True.
C
         DO ICH = 1, NCHAN(KS)
            IF( BBCBW(ICH) .NE. 32.0D0 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A, F8.3 )' )
     1            'CHKDBFQ: Bandwidth must be 32.0 MHz for ',
     2            'DBE=DBBC_PFB. Value specified is: ', 
     3            BBCBW(ICH)
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
         END DO
C
C        Baseband frequencies must be 1024.0-16.0-N*32.0 for
C        N = 0 to 14.  Also allow 15 so that there can be
C        16 channels in one polarization, but warn that it 
C        will be bad.
C
C        Jan 2012.  Allow the "-1" channel from 1040-1008 MHz 
C        for when people want to span the full 512 MHz IF in
C        one polarization.  The on-line system wants this one,
C        not the one at the low end of the IF (#15).
C
         DO ICH = 1, NCHAN(KS)
            BBOFF = 1024.0D0 + 16.0D0 - BBCFREQ(ICH)
            IF( MOD( BBOFF, 32.0D0 ) .NE. 0.0D0 .OR.
     1          NINT( BBOFF / 32.0D0 ) .LT. 0 .OR.
     2          NINT( BBOFF / 32.0D0 ) .GT. 16 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F8.2, A )' )
     1            'CHKDBFQ: Invalid BBSYN for DBE=DBBC_PFB: ', 
     2            BBCFREQ(ICH),
     3            '.  Must be 1024+16-N*32 for N=0-15.'
               ERRS = .TRUE.
               CALL WLOG( 1, MSGTXT )
            END IF
            IF( BBCFREQ(ICH) .EQ. 1040.0D0 .AND. 
     1            N40WARN(ISETF) .LE. 1 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F8.2, A )' )
     1            'CHKDBFQ: Baseband frequency ', BBCFREQ(ICH),
     2            '.  Will produce poor data with the PFB.'
               CALL WLOG( 1, MSGTXT )
               N40WARN(ISETF) = N40WARN(ISETF) + 1
               SHOWID = .TRUE.
            END IF
            IF( BBCFREQ(ICH) .EQ. 528.0D0 .AND. 
     1            N528WARN(ISETF) .LE. 1 ) THEN
C      **********************   Is this one true for the DBBC?
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F8.2, A, A )' )
     1            'CHKDBFQ: Baseband frequency ', BBCFREQ(ICH),
     2            '.  Will be changed to 1040 by the VLBA on-line',
     3            ' system.'
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' ' 
               WRITE( MSGTXT, '( A, A )' )
     1            'CHKDBFQ: Correlation of that channel will be ',
     2            ' corrupted.'
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' ' 
               WRITE( MSGTXT, '( A, A, A )' )
     1            'CHKDBFQ:  This is normal for 16 chan, 1 pol, ',
     3            'with sideband inversion at some station.'
               CALL WLOG( 1, MSGTXT )
               N528WARN(ISETF) = N528WARN(ISETF) + 1
               SHOWID = .TRUE.
            END IF
         END DO
      END IF
C
C     ===  Now check the DDC personality. ===
C
      IF( DBE(KS) .EQ. 'DBBC_DDC' ) THEN
C
C        All bandwidths must be between 1 and 16 MHz. 32 MHz is
C        available with the e-series firmware.
C
         DO ICH = 1, NCHAN(KS)
            IF( .NOT. ( DEQUAL( BBCBW(ICH), 32.0D0 ) .OR. 
     2          DEQUAL( BBCBW(ICH), 16.0D0 ) .OR. 
     3          DEQUAL( BBCBW(ICH), 8.0D0 ) .OR. 
     4          DEQUAL( BBCBW(ICH), 4.0D0 ) .OR. 
     5          DEQUAL( BBCBW(ICH), 2.0D0 ) .OR. 
     6          DEQUAL( BBCBW(ICH), 1.0D0 ) ) ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A, F8.3 )' )
     1            'CHKDBFQ: Bandwidth must be 1 to 32 MHz for ',
     2            'DBE=DBBC_DDC. Value specified is: ', 
     3            SIDEBD(ICH,KS)
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
C
C           Also Sampling must be Nyquist rate.
C
            IF( .NOT. DEQUAL(1.0D0*SAMPRATE(KS), 2.0D0*BBCBW(ICH))) THEN
               MSGTXT = ' '              
               WRITE( MSGTXT, '( A, A, F8.3, A, F8.3, A )' )
     1           'CHKDBFQ: Invalid SAMPRATE and BBFILTER specified: ', 
     2           'SAMPRATE= ', SAMPRATE(KS), ', BBFILTER= ', BBCBW(ICH),
     3           ' Only Nyquist sampling is permitted on DBBCs.'
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
         END DO
C
C        CR: the available filters depend on the version of the DBBC.
C        For DBBC Versions 1 and 2 the following appears to be correct:
C        Baseband frequencies for a given filter must be between 10-512
C        MHz (filter 2) or 512-1024 MHz (filter 1) or between 1024-1536
C        MHz (filter 4 - not in all firmware versions). They appear to
C        be fully flexible within each range.
C
C        The DBBC version 3 also permits:
C        10-1024 MHz (filter 6), 1536-2048 MHz (filter 3), 1150-1750 MHz
C        (filter 5)
C 
C        
C
         NIFERR = 0
         FILTWARN = .FALSE.
         DO ICH = 1, NCHAN(KS)
            FILTERR = .TRUE.
C
C           Check that it is in a valid IF, and only one filter set is used
C           for each IF
C
            ISIDEBD = 1
            IF( SIDEBD(ICH,KS) .EQ. 'L' ) ISIDEBD = -1
            BB1 = BBCFREQ(ICH)
            BB2 = BBCFREQ(ICH) + ISIDEBD * BBCBW(ICH)
            IF( BB1 .LT. 10.0D0 .OR.
     1          BB1 .GT. 2048.0D0 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F8.2, A )' )
     1            'CHKDBFQ: Invalid BBSYN for DBE=DBBC_DDC: ', 
     2            BBCFREQ(ICH),
     3            '.  Must be between 10 and 2048 MHz.'
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            ELSE
C              Check that all other BBCs using this IFCHAN fit in one of
C              the available filters. Record the filter that is being
C              used.
               IF_LOW = BB1
               IF_HI = BB1
               DO JCH = ICH+1, NCHAN(KS)
                  IF( IFCHAN(JCH,KS) .EQ. IFCHAN(ICH,KS) ) THEN
                     IF( BBCFREQ(JCH) < IF_LOW ) THEN
                        IF_LOW = BBCFREQ(JCH)
                     END IF
                     IF( BBCFREQ(JCH) > IF_HI ) THEN
                        IF_HI = BBCFREQ(JCH)
                     END IF
                  END IF
               END DO
               FILT_USED = 0
               DO IFILT = 1, NFILT
C                 Find the first matching filter
                  IF( FILT_USED < 1 .AND. 
     1                  IF_LOW > FILT_LOW(IFILT) .AND. 
     2                  IF_HI < FILT_HI(IFILT) ) THEN
                     FILT_USED = IFILT
                  END IF
               END DO
               IF( FILT_USED > 0 ) THEN
                  FILTERR = .FALSE.
               END IF
            END IF


            IF( FILTERR ) THEN
               ALREADY = .FALSE.
               DO I = 1, NIFERR
                  IF( IFCHAN(ICH,KS) .EQ. IFERRS(I) ) ALREADY = .TRUE.
               END DO
               NIFERR = NIFERR + 1
               IFERRS(NIFERR) = IFCHAN(ICH,KS)
               IF( VERBOSE .AND. .NOT. ALREADY ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, A, A, A, A )' )
     1               'CHKDBFQ: Illegal BBCSYN placement for ',
     2               'DBE=DBBC_DDC. All BBSYN of an IF ',
     3               'must fit one of the DBBC filters below. ', 
     4               'Check frequencies for IF ', IFCHAN(ICH,KS)
                  CALL WLOG( 1, MSGTXT )
                  CALL WRTMSG( 1, 'CHKDBFQ', 'dbbcfilters' )
                  ERRS = .TRUE.
               END IF
            ELSE
C              Filters 3,5,6 only available on DBBC Version 3.
               IF(VERBOSE .AND. .NOT. FILTWARN .AND. FILT_USED > 3) THEN
                  FILTWARN = .TRUE.
                  MSGTXT = ' '
                  WRITE( MSGTXT, '(A, A, A, A, A)' )
     1               'CHKDBFQ: IF ', IFCHAN(ICH,KS), 
     2               ' uses one of DBBC filters ',
     3               '3, 5 or 6. These are only available on ',
     4               'DBBC Version 3. Check you have it!'
               CALL WLOG( 1, MSGTXT )
               CALL WRTMSG( 1, 'CHKDBFQ', 'dbbcfilters' )
               SHOWID = .TRUE.
               END IF
            END IF
C
C           Check the other end of the sideband.  Just warn of the
C           issue in this case.  Only warn once if it is less than
C           1% of the band - it's probably because of trying to get
C           decent pcal frequencies.
C
            IF( FILT_USED > 0 ) THEN
               SLOP = MAX( FILT_LOW(FILT_USED) - BB2, 
     1                  BB2 - FILT_HI(FILT_USED) )
            ELSE
               SLOP = 0.0D0
            END IF
            IF( VERBOSE ) THEN
               IF( ( SLOP .GT. 0.D0 .AND. OBWARN ) .OR. 
     1               SLOP .GT. 0.01 * BBCBW(ICH) ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I3, A, F8.2, A )' )
     1              'CHKDBFQ: WARNING!  Channel ', ICH, 
     2              ' with bandwidth ', BBCBW(ICH), ' MHz extends '
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, F8.3, A, F8.1, A, F8.1, A )' )
     1               '          ', SLOP,
     2               ' MHz outside the IF band of ',
     3               FILT_LOW(FILT_USED), '-', FILT_HI(FILT_USED), 
     4               ' MHz.'
                  CALL WLOG( 1, MSGTXT )
                  CALL WLOG( 1, '         That part of the band '//
     1               'will be corrupted.' )
                  IF( SLOP .LE. 0.01 * BBCBW(ICH) ) THEN
                     OBWARN = .FALSE.
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( 2A )' )
     1                 '         This is small and may be ',
     2                 'intentional for good pulse cal.'
                     CALL WLOG( 1, MSGTXT )
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( A )' )
     1                 '         This warning will not be repeated.'
                     CALL WLOG( 1, MSGTXT )
                  END IF
                  SHOWID = .TRUE.
               END IF
            END IF
C
C           The DBBC has both a binary and a decimal tuning mode. The
C           binary mode can set the frequency with resolution
C           1024MHz/2^31=0.476 Hz
C           The decimal mode has 10KHz resolution and is normally used.
C           We will enforce the decimal mode here.
C           Allowing some tolerance as Craig did for the RDBE.
C
            IF( DMOD( BBCFREQ(ICH) + 1.D-7, 0.010D0 ) .GT. 2.D-7
     1          .AND. NFWARN .LE. 16 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( 3A )' ) 'CHKDBFQ:  ',
     1           ' BBSYN for the DBBC_DDC must be an even multiple',
     2           ' of 10.0 kHz.'
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' '
               WRITE( MSGTXT, '( 2A, F15.6, A, I3, A, A )' ) 
     1            '          ',
     2            ' Value of: ', BBCFREQ(ICH), 
     3            ' MHz found in channel ', ICH, ' of setup ', 
     4            SETNAME(KS)
               CALL WLOG( 1, MSGTXT )
               IF( NFWARN .GE. 16 ) THEN
                  MSGTXT = 'CHKRDFQ:   Further warnings suppressed '
     1               //'- too many.'
                  CALL WLOG( 1, MSGTXT )                     
               END IF                  
               NFWARN = NFWARN + 1
               SHOWID = .TRUE.
            END IF


         END DO

      END IF
C
C     Identify the setup if there was a problem.
C      
      IF( ERRS .OR. SHOWID ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, A, A, A )' )
     1     'CHKDBFQ: The above problem is for setup ', 
     2     SETNAME(KS)(1:LEN1(SETNAME(KS))),
     3     ' and at least station ', SETSTA(1,KS)
         CALL WLOG( 1, MSGTXT )
      END IF
C
      RETURN
      END
