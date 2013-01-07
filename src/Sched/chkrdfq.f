      SUBROUTINE CHKRDFQ( KS, BBCBW, BBCFREQ, ERRS )
C
C     Routine for SCHED called by CHKRDBE and FSFREQ that checks
C     the frequencies and bandwidths requested for the RDBE.
C     CHKRDBE checks the input setup file.  FSFREQ checks just
C     before writing the scan info to be sure in-line changes
C     don't cause trouble.  Note that, for non-RDBE setups, this
C     routine will fall through without doing anything thanks
C     to the second level of IF statements.
C
C     Note that this routine can get called multiple times for 
C     each setup file, especially if different frequency sets
C     are being checked.  But, since it is called from FSFREQ,
C     it can get called from print routines etc.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER           KS, ICH, ISIDEBD, nwarn
      INTEGER           N40WARN(MAXSET), N528WARN(MAXSET)
      INTEGER           NFWARN, N2WARN
      INTEGER           LSETF
      INTEGER           IPF, I, NPBW, LEN1, ISETF
      DOUBLE PRECISION  BBOFF, BB1, BB2, CR1, CR2, SLOP
      DOUBLE PRECISION  BBCBW(*), BBCFREQ(*), PLO(3), PHI(3)
      LOGICAL           ERRS, DEQUAL, PBWARN, OBWARN, SHOWID
C
      DATA     nwarn    / 0 /
      DATA     NPBW     / 0 /
      DATA     OBWARN   / .TRUE. /
      DATA     N40WARN  / MAXSET*1 /
      DATA     N528WARN / MAXSET*1 /
      DATA     NFWARN, N2WARN / 0, 0 /
      DATA     LSETF    / 0 /
      DATA     PLO      / 512.D0, 640.D0, 896.D0 /
      DATA     PHI      / 640.D0, 896.D0, 1024.D0 /
      DATA     CR1, CR2 / 640.D0, 896.D0 /
      SAVE     nwarn
      SAVE     N40WARN, N528WARN, NFWARN, N2WARN, NPBW, OBWARN
      SAVE     LSETF
C
C      PLO and PHI are the ranges for the DDC initial polyphase filter.
C -----------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKRDFQ: Starting' )
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
C     First check the RDBE_PFB personality.
C
      IF( DBE(KS) .EQ. 'RDBE_PFB' ) THEN
C
C        All bandwidths must be 32 MHz.
C
         DO ICH = 1, NCHAN(KS)
            IF( BBCBW(ICH) .NE. 32.0D0 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A, F8.3 )' )
     1            'CHKRDFQ: Bandwidth must be 32.0 MHz for ',
     2            'DBE=RDBE_PFB. Value specified is: ', 
     3            SIDEBD(ICH,KS)
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
C        Jan 2012.  Allow the "0" channel from 1040-1008 MHz 
C        for when people want to span the full 512 MHz IF in
C        one polarization.  The on-line system wants this one,
C        not the one at the low end of the IF.
C
         DO ICH = 1, NCHAN(KS)
            BBOFF = 1024.0D0 + 16.0D0 - BBCFREQ(ICH)
            IF( MOD( BBOFF, 32.0D0 ) .NE. 0.0D0 .OR.
     1          NINT( BBOFF / 32.0D0 ) .LT. 0 .OR.
     2          NINT( BBOFF / 32.0D0 ) .GT. 16 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F8.2, A )' )
     1            'CHKRDFQ: Invalid BBSYN for DBE=RDBE_PFB: ', 
     2            BBCFREQ(ICH),
     3            '.  Must be 1024+16-N*32 for N=0-15.'
               ERRS = .TRUE.
               CALL WLOG( 1, MSGTXT )
            END IF
            IF( BBCFREQ(ICH) .EQ. 1040.0 .AND. 
     1            N40WARN(ISETF) .LE. 1 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F8.2, A )' )
     1            'CHKRDFQ: Baseband frequency ', BBCFREQ(ICH),
     2            '.  Will produce poor data with the PFB.'
               CALL WLOG( 1, MSGTXT )
               N40WARN(ISETF) = N40WARN(ISETF) + 1
               SHOWID = .TRUE.
            END IF
            IF( BBCFREQ(ICH) .EQ. 528.0 .AND. 
     1            N528WARN(ISETF) .LE. 1 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F8.2, A, A )' )
     1            'CHKRDFQ: Baseband frequency ', BBCFREQ(ICH),
     2            '.  Will be changed to 1040 by the VLBA on-line',
     3            ' system.'
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' ' 
               WRITE( MSGTXT, '( A, A )' )
     1            'CHKRDFQ: Correlation of that channel will be ',
     2            ' corrupted.'
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' ' 
               WRITE( MSGTXT, '( A, A, A )' )
     1            'CHKRDFQ:  This is normal for 16 chan, 1 pol, ',
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
      IF( DBE(KS) .EQ. 'RDBE_DDC' ) THEN
C
C        All bandwidths must be between 250 kHz and 128 MHz.
C
         DO ICH = 1, NCHAN(KS)
            IF( .NOT. ( DEQUAL( BBCBW(ICH), 128.0D0 ) .OR. 
     a          DEQUAL( BBCBW(ICH), 64.0D0 ) .OR. 
     1          DEQUAL( BBCBW(ICH), 32.0D0 ) .OR. 
     2          DEQUAL( BBCBW(ICH), 16.0D0 ) .OR. 
     3          DEQUAL( BBCBW(ICH), 8.0D0 ) .OR. 
     4          DEQUAL( BBCBW(ICH), 4.0D0 ) .OR. 
     5          DEQUAL( BBCBW(ICH), 2.0D0 ) .OR. 
     6          DEQUAL( BBCBW(ICH), 1.0D0 ) ) ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A, F8.3 )' )
     1            'CHKRDFQ: Bandwidth must be 1 to 128 MHz for ',
     2            'DBE=RDBE_DDC. Value specified is: ', 
     3            SIDEBD(ICH,KS)
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
         END DO
C
C        Baseband frequencies must be between 512 and 1024 MHz.
C        They can be set to within 1 Hz or less, but must be 
C        a multiple of 256E6/2**32 MHz =0.0596046 Hz. (I don't absolutely
C        guarantee that that isn't 256/2**31, but it doesn't really
C        matter here).  Values that are not integer Hz are a problem
C        because the phase doesn't repeat each second so it is difficult
C        to return to phase after going to another frequency.  Therefore
C        we are going to restrict the tuning increment to the smallest
C        value that is an even number of Hz, namely 15.625 kHz ( which
C        is 256E6/2**14.  That means the allowed values are N*125 kHz plus
C        0, 15.625, 31.250, 46.875, 62.500, 78.125, 93.750, or 109.375
C        kHz.
C
C        Once all frequencies are set, prevent basebands crossing 
C        the crossover points.
C
C        We have decided for now (Feb. 2011) to restrict tuning
C        to multiples of 15.625 kHz.  In fact, we strongly encourage
C        use of multiples of 250 kHz, so note that.
C
         DO ICH = 1, NCHAN(KS)
C
C           Check that it is in the IF.
C
            ISIDEBD = 1
            IF( SIDEBD(ICH,KS) .EQ. 'L' ) ISIDEBD = -1
            BB1 = BBCFREQ(ICH)
            BB2 = BBCFREQ(ICH) + ISIDEBD * BBCBW(ICH)
            IF( BB1 .LT. 512.0D0 .OR.
     1          BB1 .GT. 1024.0D0 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F8.2, A )' )
     1            'CHKRDFQ: Invalid BBSYN for DBE=RDBE_DDC: ', 
     2            BBCFREQ(ICH),
     3            '.  Must be between 512 and 1024 MHz.'
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
C
C           Check the other end of the sideband.  Just warn of the
C           issue in this case.  Only warn once if it is less than
C           1% of the band - it's probably because of trying to get
C           decent pcal frequencies.
C
            SLOP = MAX( 512.D0 - BB2, BB2 - 1024.D0 )
            IF( ( SLOP .GT. 0.D0 .AND. OBWARN ) .OR. 
     1            SLOP .GT. 0.01 * BBCBW(ICH) ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I3, A, F8.2, A )' )
     1           'CHKRDFQ:  Channel ', ICH, 
     2           ' with bandwidth ', BBCBW(ICH), ' MHz extends '
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F8.3, A )' )
     1            '          ', SLOP,
     2            ' MHz outside the IF band of 512-1024 MHz.'
               CALL WLOG( 1, MSGTXT )
               CALL WLOG( 1, '         That Part of the band '//
     1            'will be corrupted.' )
               IF( SLOP .LE. 0.01 * BBCBW(ICH) ) THEN
                  OBWARN = .FALSE.
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( 2A )' )
     1              '         This is small and may be ',
     2              'intentional for good pulse cal.'
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A )' )
     1              '         This warning will not be repeated.'
                  CALL WLOG( 1, MSGTXT )
               END IF
               SHOWID = .TRUE.
            END IF
C
C           Check for multiple of 15.625 kHz.  This test allows a 
C           bit of slop (about 100 mHz)
C
            IF( DMOD( BBCFREQ(ICH) + 1.D-7, 0.015625D0 ) .GT. 2.D-7
     1          .AND. NFWARN .LE. 16 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( 3A )' ) 'CHKRDFQ:  ',
     1           ' BBSYN for the RDBE_DDC must be an even multiple',
     2           ' of 15.625 kHz.'
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
C
C           Check for multiple of 250 kHz.
C
            ELSE IF( DMOD( BBCFREQ(ICH) + 1.D-7, 0.250D0 ) .GT. 2.D-7
     1          .AND. N2WARN .LE. 16 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( 3A )' ) 'CHKRDFQ:  ',
     1           ' It is recommended that BBSYN for the RDBE_DDC ',
     2           'be an even multiple of 250 kHz.'
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' '
               WRITE( MSGTXT, '( 2A, F15.6, A, I3, A, A )' ) 
     1            '          ',
     2            ' Value of: ', BBCFREQ(ICH), 
     3            ' MHz found in channel ', ICH, ' of setup ', 
     4            SETNAME(KS)
               CALL WLOG( 1, MSGTXT )
               IF( N2WARN .GE. 16 ) THEN
                  MSGTXT = 'CHKRDFQ:   Further warnings suppressed '
     1               //'- too many.'
                  CALL WLOG( 1, MSGTXT )                     
               END IF                  
               N2WARN = N2WARN + 1
               SHOWID = .TRUE.
            END IF
C
C           Test the crossover points for the DDC.  Allow a small
C           percentage crossover at the end of the band opposite
C           from the LO.  This may be hard to avoid with the highest
C           bandwidths and a desire for a decent pcal frequency.
C           The filter used will be the one that includes the band
C           edge (should check with Matthias).
C
            ISIDEBD = 1
            IF( SIDEBD(ICH,KS) .EQ. 'L' ) ISIDEBD = -1
            BB1 = BBCFREQ(ICH)
            BB2 = BBCFREQ(ICH) + ISIDEBD * BBCBW(ICH)
C
C           Get which polyphase filter the band will end up in.
C
            IPF = 0
            DO I = 1, 3
C
C              The executor chooses the PFB channel to use.  If 
C              the frequency falls right on a boundary, it goes
C              to the appropriate side for the sideband.
C
               IF( SIDEBD(ICH,KS) .EQ. 'U' ) THEN
                  IF( BB1 .GE. PLO(I) .AND. BB1 .LT. PHI(I) ) THEN
                     IPF = I
                  END IF
               ELSE
                  IF( BB1 .GT. PLO(I) .AND. BB1 .LE. PHI(I) ) THEN
                     IPF = I
                  END IF
               END IF
            END DO
            IF( IPF .EQ. 0 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, 
     1               '( A, I4, A, I4, A, F10.2, A )' )
     2            'CHKRDFQ: Baseband ', ICH, ' in setup ', KS, 
     3            ' has baseband frequency ', BB1,
     4            ' outside 512-1024 MHz.'
               CALL WLOG( 1, MSGTXT )
               SHOWID = .TRUE.
            ELSE
C
C              Now see if the band goes outside the filter.  Allow
C              a bit of tolerance.
C
               PBWARN = .FALSE.
               IF( SIDEBD(ICH,KS) .EQ. 'U' ) THEN
                  IF( BB2 .GT. PHI(IPF) + 0.02D0 * BBCBW(ICH) ) THEN
                     PBWARN = .TRUE.
                  END IF
               ELSE
                  IF( BB2 .LT. PLO(IPF) - 0.02D0 * BBCBW(ICH) ) THEN
                     PBWARN = .TRUE.
                  END IF
               END IF
               IF( PBWARN ) NPBW = NPBW + 1
               IF( PBWARN .AND. NPBW .LE. 10 ) THEN               
C
                  MSGTXT = ' '
                  WRITE( MSGTXT,
     1               '( A, I4, A, F10.4, A, F10.4, 2A )' )
     2               'CHKRDFQ: ***  Baseband ', ICH, 
     4               ' between IF freqs ', BB1, ' and ', BB2, ' MHz',
     5               ' spans a crossover'
                  CALL WLOG( 1, MSGTXT )
C
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( 2A )' )
     1               '          or goes outside 512-1024 MHz.  ',
     2               'This will produce corrupted data.'
                  CALL WLOG( 1, MSGTXT )
C
                  CALL WLOG( 1, '         Recall crossovers are '//
     1               'the boundaries between the polyphase filter ' )
                  CALL WLOG( 1, '         outputs in the initial '//
     2               'stage of processing in the RDBE.' )
C
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, F10.2, A, F10.2, A )' )
     1               '         The crossovers are at ', CR1, ' and ',
     2               CR2, ' MHz in the IF.'
                  CALL WLOG( 1, MSGTXT )
C
                  MSGTXT = ' '
                  SHOWID = .TRUE.
               ELSE IF( PBWARN .AND. NPBW .EQ. 11 ) THEN
                  CALL WLOG( 1, ' ' )
                  CALL WLOG( 1, 
     1             'CHKRDFQ:  More crossover warnings suppressed.' )
                  CALL WLOG( 1, ' ' )
               END IF
            END IF
         END DO
C
      END IF
C
C     Identify the setup if there was a problem.
C      
      IF( ERRS .OR. SHOWID ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, A, A, A )' )
     1     'CHKRDFQ: The above problem is for setup ', 
     2     SETNAME(KS)(1:LEN1(SETNAME(KS))),
     3     ' and at least station ', SETSTA(1,KS)
         CALL WLOG( 1, MSGTXT )
      END IF
C
      RETURN
      END
