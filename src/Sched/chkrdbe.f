      SUBROUTINE CHKRDBE( KS, ERRS )
Cf2py intent(in, out) ERRS
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
C     Add the dual RDBE case.  Nov. 8, 2012  RCW.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER           KS, ICH, IIF, MIF, NIF, KSTA, NNIF(4)
      INTEGER           LEN1
      LOGICAL           ERRS, SBWARN, IFNEW, OK4
      CHARACTER         USEDIFC(4)*2
      DATA              SBWARN / .TRUE. /
      SAVE              SBWARN
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKRDBE: Starting' )
C
C     ---------
C     Need to do something about the solar attenuators similar to 
C     the IFDIST entry for the VLBA DAR.
C     ---------
C
C     Get the station catalog number for the station.
C
      KSTA = ISETSTA(KS)
C
C     Set the usable number of IF (2 for RDBE, 4 for RDBE2)
C
      MIF = 2
      IF( DAR(KSTA) .EQ. 'RDBE2' .AND. FORMAT(KS) .EQ. 'VDIF' .AND.
     1    DBE(KS) .EQ. 'RDBE_DDC' ) MIF = 4
C
C     Check the format.  The current status as of Feb. 4, 2014 is:
C
C        PFB or DDC     NONE    ok
C        PFB  One RDBE  VDIF    Not implemented.
C        PFB  One RDBE  MARK5B  Ok
C        PFB  Two RDBE  Any     Can't do because of bit rate limitations.
C        DDC  All cases VDIF    Ok
C        DDC  One RDBE  MARK5B  No.  MARK5B/DDC support ended before Nov 2014.
C        DDC  Two RDBE  MARK5B  No.  Cannot be merged.
C
      IF( DBE(KS) .EQ. 'RDBE_PFB' .AND. ( FORMAT(KS) .NE. 'MARK5B' 
     1    .AND. FORMAT(KS) .NE. 'NONE' ) ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( 3A )' )
     1      'CHKRDBE:  Format ', FORMAT(KS), ' specified for setup ',
     2      SETNAME(KS)(1:LEN1(SETNAME(KS)))
         CALL WLOG( 1, MSGTXT )
         CALL WLOG( 1, '          It must be MARK5B for DBE=RDBE_PFB.' )
         ERRS = .TRUE.
      END IF
C
C     Now only need one DDC case and it does not depend on the number of
C     IFs.
C
      IF( DBE(KS) .EQ. 'RDBE_DDC' .AND.
     1    ( FORMAT(KS) .NE. 'VDIF' .AND. FORMAT(KS) .NE. 'NONE' ) ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( 3A )' )
     1      'CHKRDBE:  Format ', FORMAT(KS), ' specified for setup ',
     2      SETNAME(KS)(1:LEN1(SETNAME(KS)))
         CALL WLOG( 1, MSGTXT )
         CALL WLOG( 1, '          It must be VDIF or NONE' //
     1        'for DBE=RDBE_DDC.' )
         ERRS = .TRUE.
      END IF
C
      IF( VLBITP .AND. FORMAT(KS) .NE. 'NONE' ) THEN 
C
C        Make sure there are no more than 2 IFs requested for 
C        DAR=RDBE or 4 IFs for DAR=RDBE2.  Get a count of IFs and
C        number of channels per IF for later use.
C
         DO IIF = 1, 4
            USEDIFC(IIF) = ' '
            NNIF(IIF) = 0
         END DO
         NIF = 0
C
         DO ICH = 1, NCHAN(KS)
C
C           See if this channel's IF is one that is in lower 
C           numbered channels.
C
            IFNEW = .TRUE.
            IF( NIF .GE. 1 ) THEN
               DO IIF = 1, NIF
                  IF( IFCHAN(ICH,KS) .EQ. USEDIFC(IIF) ) THEN
                     IFNEW = .FALSE.
                     NNIF(IIF) = NNIF(IIF) + 1
                  END IF
               END DO
            END IF
C
C           If it is new, increment the count and add the IF to
C           the record of previously used ones.  Complain and die
C           if there are too many.
C           
            IF( IFNEW ) THEN
               NIF = NIF + 1
               IF( NIF .GT. MIF ) THEN
C
C                 Here we have a problem - too many IFs.
C
                  IF( MIF .EQ. 4 ) THEN
                     CALL WLOG( 1, 
     1                    'CHKRDBE: More than 4 IF''s requested '//
     2                    'for 2 RDBE station using the DDC and VDIF' )
                  ELSE
                     CALL WLOG( 1, 
     1                    'CHKRDBE: More than 2 IF''s requested '//
     2                    'for single RDBE station or PFB '//
     3                    'or format other than VDIF' )
                  END IF
                  ERRS = .TRUE.
               END IF
               NNIF(NIF) = 1
               USEDIFC(NIF) = IFCHAN(ICH,KS)
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
     2           ' for DBE=RDBE_PFB. Must be 64.0 Msamp/s.'
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
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( A, A )' )
     1                  '         Setup file is: ',
     2                  SETNAME(KS)(1:LEN1(SETNAME(KS)))
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
     3                  CORREL .EQ. 'VLBADIFX' .OR.
     4                  CORREL .EQ. 'LBA' ) ) THEN
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
C
C                    For FREQREF, we need the net sideband to know
C                    whether to add or subtract CORINV.
C
C                    CORINV is the amount that got added to FREQREF 
C                    to get the LO setting. For comparison with 
C                    channels that will get correlated against
C                    this one, subtract CORINV.  
C
C                    Note in all cases we will be increasing the 
C                    BBSYN frequency because we will be switching
C                    from BBC sideband 'U' to 'L'.   FREQREF can go 
C                    either way depending on the IF sideband.
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
C
C                    Get the new reference frequeny.  The direction
C                    of the shift is in the sign of CORINV.
C
                     FREQREF(ICH,KS) = FREQREF(ICH,KS) + CORINV(ICH,KS)
C
                  ELSE
C
C                    We have the wrong baseband sideband and a correlator
C                    that can't deal with it.
C
                     IF( SIDEBD(ICH,KS) .NE. 'U' ) ERRS = .TRUE.
C
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
C           NCHAN must be 8 or less with 2 RDBEs or 4 or less with
C           one.  MIF is already set depending on the capability.
C
            IF( MIF .EQ. 4 .AND. NCHAN(KS) .GT. 8 ) THEN
               MSGTXT = ' '              
               WRITE( MSGTXT, '( A, A, A, I4 )' )
     1           'CHKRDBE: For DAR=RDBE2, DBE=RDBE_DDC, and VDIF ',
     2           'format, NCHAN must <= 8.', 
     3           ' Setup specified ', NCHAN(KS)
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
            IF( MIF .EQ. 2 .AND. NCHAN(KS) .GT. 4 ) THEN
               CALL WLOG( 1, 
     1           'CHKRDBE: If there is a single RDBE using the DDC '//
     2           'personality, or the format is not VDIF,' )
               MSGTXT = ' '              
               WRITE( MSGTXT, '( A, I4 )' )
     1           '         NCHAN must be <= 4.  Setup specified.', 
     2           NCHAN(KS)
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
C
C           Check for invalid distributions of IFs for RDBE2 (MIF=4).
C           For NIF 3, all possible combinations that have NIF < 8
C           conform to the restrictions which say that one IF can
C           have up to 8 channels and the other 2 must be less than
C           or equal to 4 each (only one IF can go to both RDBE's).
C           If one IF has more than 4 channels, the largest possible 
C           number of channels the others can have is 3, so it's ok.
C           So no test beyond the total number test is needed.
C
C           If NIF is 4, there must be two pairs, each with less
C           than 4 channels (each such pair goes to an RDBE and 
C           no IF can go to both RDBEs).  Note that there can be
C           pairs with more than 4 as long as they are kept
C           separated.  There are only 3 possible combinations so
C           just make a brute force check.  That will be more
C           understandable.
C
            IF( NIF .EQ. 4 ) THEN
               OK4 = ( NNIF(1) + NNIF(2) .LE. 4 .AND. 
     1                 NNIF(3) + NNIF(4) .LE. 4 ) .OR. 
     2               ( NNIF(1) + NNIF(3) .LE. 4 .AND. 
     3                 NNIF(2) + NNIF(4) .LE. 4 ) .OR. 
     4               ( NNIF(1) + NNIF(4) .LE. 4 .AND. 
     5                 NNIF(2) + NNIF(3) .LE. 4 )
               IF( .NOT. OK4 ) THEN
                  CALL WLOG( 1, 
     1               'CHKRDBE:  When using 2 RDBEs, with 4 IFs, '//
     2               'it must be possible to pair the IFs so ' )
                  WRITE( MSGTXT, '( A, A, A )' )
     1               '          that each pair has less than 4 ',
     2               'channels total.  This is not the case for ',
     3               'setup ', SETNAME(KS)
                  ERRS = .TRUE.
               END IF
            END IF
C
C           Sample rate can have many values.
C           These may need to shift down by 1 if we do complex
C           sampling.
C
            IF( SAMPRATE(KS) .NE. 256.0 .AND. 
     a          SAMPRATE(KS) .NE. 128.0 .AND. 
     1          SAMPRATE(KS) .NE. 64.0 .AND.
     2          SAMPRATE(KS) .NE. 32.0 .AND.
     3          SAMPRATE(KS) .NE. 16.0 .AND.
     4          SAMPRATE(KS) .NE. 8.0 .AND.
     5          SAMPRATE(KS) .NE. 4.0 .AND.
     6          SAMPRATE(KS) .NE. 2.0 .AND.
     7          SAMPRATE(KS) .NE. 1.0 .AND.
     8          SAMPRATE(KS) .NE. 0.5 ) THEN
               MSGTXT = ' '              
               WRITE( MSGTXT, '( A, F8.3, A )' )
     1           'CHKRDBE: Invalid SAMPRATE specified: ', SAMPRATE(KS),
     2           ' for DBE=VLBA_DDC. Must be 0.5 to 256 Msamp/s.'
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
         CALL CHKRDFQ( KS, 0, BBFILT(1,KS), BBSYN(1,KS), ERRS )
C
      END IF
C
      RETURN
      END
