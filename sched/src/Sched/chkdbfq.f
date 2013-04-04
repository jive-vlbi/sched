      SUBROUTINE CHKDBFQ( KS, BBCBW, BBCFREQ, ERRS )
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
C     it can get called from print routines etc.
C
C     Original based on CHKDBFQ, but modified by CR to the best of
C     current knowledge...
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
      LOGICAL           IFHIGH, IFLOW
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
      SAVE     nwarn
      SAVE     N40WARN, N528WARN, NFWARN, N2WARN, NPBW, OBWARN
      SAVE     LSETF
C
C      PLO and PHI are the ranges for the DDC initial polyphase filter.
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
            IF( BBCFREQ(ICH) .EQ. 1040.0 .AND. 
     1            N40WARN(ISETF) .LE. 1 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F8.2, A )' )
     1            'CHKDBFQ: Baseband frequency ', BBCFREQ(ICH),
     2            '.  Will produce poor data with the PFB.'
               CALL WLOG( 1, MSGTXT )
               N40WARN(ISETF) = N40WARN(ISETF) + 1
               SHOWID = .TRUE.
            END IF
            IF( BBCFREQ(ICH) .EQ. 528.0 .AND. 
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
C        All bandwidths must be between 1 and 16 MHz. 32 MHz will be
C        available in the future.
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
     1            'CHKDBFQ: Bandwidth must be 1 to 128 MHz for ',
     2            'DBE=DBBC_DDC. Value specified is: ', 
     3            SIDEBD(ICH,KS)
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
            IF( DEQUAL( BBCBW(ICH), 32.0D0 ) ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A, I3 )' )
     1            'CHKDBFQ: You have requested 32 MHz bandwidth for ',
     2            'DBE=DBBC_DDC. This is not available yet! Setup: ', KS
            END IF
         END DO
C
C        CR: Eventually a single 10-1024 MHz IF will become available
C        with a future firmware release. For now the following appears
C        to be correct.
C        Baseband frequencies must be between 10 and 512 MHz or between
C        512 and 1024 MHz. They appear to be fully flexible within that
C        range.
C
         IFHIGH = .FALSE.
         IFLOW = .FALSE.
         DO ICH = 1, NCHAN(KS)
C
C           Check that it is in the IF.
C
            ISIDEBD = 1
            IF( SIDEBD(ICH,KS) .EQ. 'L' ) ISIDEBD = -1
            BB1 = BBCFREQ(ICH)
            BB2 = BBCFREQ(ICH) + ISIDEBD * BBCBW(ICH)
            IF( BB1 .LT. 10.0D0 .OR.
     1          BB1 .GT. 1024.0D0 ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F8.2, A )' )
     1            'CHKDBFQ: Invalid BBSYN for DBE=DBBC_DDC: ', 
     2            BBCFREQ(ICH),
     3            '.  Must be between 10 and 1024 MHz.'
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            ELSE IF( BB1 .LT. 512 ) THEN
               IFLOW = .TRUE.
            ELSE 
               IFHIGH = .TRUE.
            END IF
C
C           Check the other end of the sideband.  Just warn of the
C           issue in this case.  Only warn once if it is less than
C           1% of the band - it's probably because of trying to get
C           decent pcal frequencies.
C
            SLOP = MAX( 10.D0 - BB2, BB2 - 1024.D0 )
            IF( ( SLOP .GT. 0.D0 .AND. OBWARN ) .OR. 
     1            SLOP .GT. 0.01 * BBCBW(ICH) ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I3, A, F8.2, A )' )
     1           'CHKDBFQ:  Channel ', ICH, 
     2           ' with bandwidth ', BBCBW(ICH), ' MHz extends '
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F8.3, A )' )
     1            '          ', SLOP,
     2            ' MHz outside the IF band of 10-1024 MHz.'
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
         END DO

         IF( IFHIGH .AND. IFLOW ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A, A )' )
     1         'CHKDBFQ: Illegal BBCSYN placement for DBE=DBBC_DDC. ',
     2         'All BBSYN must either be between 10 and 512 MHz', 
     3         '*or* between 512 and 1024 MHz. '
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
C
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
