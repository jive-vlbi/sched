      SUBROUTINE CHKDBBC( KS, ERRS )
C
C     Routine for SCHED called by CHKSET that checks items related
C     to the use of the DBBC digital backend.  This is adapted from,
C     and very similar to CHKRDBE.
C
C     I'm going to need help from Europe because I can't find all
C     the specs for the DBBC.
C
C     The frequency and bandwidth checks are broken out to a 
C     subroutine (CHKDBFQ) so that it can also be called by FSFREQ
C     to check in-schedule changes of frequency and bandwidth.
C
C     The RDBE PFB personality can only do lower sideband.  But
C     DiFX can invert sidebands.  So, if an upper sideband is
C     found, add the bandwidth to the BBC frequency and invert
C     the sideband.  Oct 11, 2011.
C
C     Add the dual RDBE case.  Nov. 8, 2012  RCW.
C
C     Adapted from CHKRDBE.  Jan 20, 2013.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
      INCLUDE  'schfreq.inc'
C
      INTEGER     KS, ICH, JCH, IIF, MIF, NIF, KSTA, NNIF(4), I, IBBC
      INTEGER     MAXBBC, MAXIF, IC, LEN1
      PARAMETER   (MAXBBC=16, MAXIF=4)
      LOGICAL     ERRS, SBWARN, IFNEW, OK, SOMEBAD
C     Can have up to 4 IFs but actual number depends on DBBCVER
      INTEGER     IFBBC(MAXBBC,4)
      CHARACTER   USEDIFC(4)*2, MYDBBCVER*8
      CHARACTER   IFNAM(4)*2
      DATA        SBWARN / .TRUE. /
      SAVE        SBWARN
      DATA        (IFNAM(I),I=1,MAXIF) / 'A', 'B', 'C', 'D' /
C Note that IFNAM is also defined in BBCDBBC (should match!)
C MAXIF is the maximum possible number of IFs (4), MIF is the actual
C number of IFs in this version of the DBBC (which we figure out later)
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKDBBC: Starting' )
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
C     All constraints (including the number of IFs) depend on the
C     personality (DDC or PFB) and the DBBCVER (astro, geo or hybrid).
C     IFDBBC knows the rules...
      MYDBBCVER = DBBCVER(KSTA)
      CALL IFDBBC( MYDBBCVER, MAXBBC, 4, IFBBC, MIF )
C
C
      IF( VLBITP .AND. FORMAT(KS) .NE. 'NONE' ) THEN 
C
C        Make sure there are no more than the available number of
C        IFs requested.  Get a count of IFs and number of channels 
C        per IF for later use.
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
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I2, 2A )' )
     1               'CHKDBBC: More than ', MIF, ' IF''s requested ',
     2                    'for a DBBC station.'
                  CALL WLOG( 1, MSGTXT )
                  ERRS = .TRUE.
               END IF
               NNIF(NIF) = 1
               USEDIFC(NIF) = IFCHAN(ICH,KS)
            END IF
         END DO
C
C        Check the DBBC_PFB personality.
C        CR: The PFB code here is based on the RDBE code, with some
C        modifications based on a brief message from Gino Tuccari. 
C        This should be used with great caution...
C
         IF( DBE(KS) .EQ. 'DBBC_PFB' ) THEN
            IF( .NOT. MODETEST(KS) ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A, A, A )' )
     1            'CHKDBBC: You have specified DBE=DBBC_PFB ',
     2            'The PFB mode is not yet supported in the DBBC. ',
     3            'This schedule will probably fail. ',
     4            'Specify MODETEST if you are testing.'
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
C
C           NCHAN must be 16 (in initial version).
C
            IF( NCHAN(KS) .NE. 16 ) THEN
               MSGTXT = ' '              
               WRITE( MSGTXT, '( A, A, I4 )' )
     1           'CHKDBBC: For DBE=DBBC_PFB, NCHAN must be 16.', 
     2           ' Setup specified: ', NCHAN(KS)
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
C
C           Sample rate must be 64 Msamp/sec. A future firmware upgrade
C           will allow 2048 Mbps recording with 64 MHz baseband channels
C           (128 Msamp/sec).
C
            IF( SAMPRATE(KS) .NE. 64.0 ) THEN
               MSGTXT = ' '              
               WRITE( MSGTXT, '( A, F8.3, A )' )
     1           'CHKDBBC: Invalid SAMPRATE specified: ', SAMPRATE(KS),
     2           ' for DBE=DBBC_PFB. Must be 64.0 Msamp/s.'
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
C
C           Bits per sample must be 2 or 1.
C
            DO ICH = 1, NCHAN(KS)
               IF( BITS(ICH,KS) .NE. 2 .AND.  BITS(ICH,KS) .NE. 1 ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, A, I3 )' )
     1               'CHKDBBC: BITS must be 2 for DBE=DBBC_PFB. ',
     2               '  Value specified is: ', BITS(ICH,KS)
                  CALL WLOG( 1, MSGTXT )
                  ERRS = .TRUE.
               END IF
            END DO
C
C           Basebands are either all LSB or all USB (depending on the
C           Nyquist zone to be used). 1st Nyq is USB, 2nd LSB, 3rd USB,
C           4th LSB.
C
            DO ICH = 1, NCHAN(KS) - 1
               DO JCH = 2, NCHAN(KS)
                  IF( SIDEBD(ICH,KS) .NE. SIDEBD(JCH,KS) ) THEN
                     IF( SBWARN ) THEN
                        MSGTXT = ' '
                        WRITE( MSGTXT, '( A, A )' )
     1                     'CHKDBBC: SIDEBAND must be the same for ',
     2                     'all channels if  DBE=DBBC_PFB. '
                        CALL WLOG( 1, MSGTXT )
                        ERRS = .TRUE.
                        SBWARN = .FALSE.
                     END IF
                  END IF
               END DO
            END DO
C
C           Bandwidths and frequencies will be checked in CHKDBFQ.
C
C     ***********************      Still to be checked - IF, POL.
C
         END IF
C
C        ===  Now check the DDC personality. ===
C
         IF( DBE(KS) .EQ. 'DBBC_DDC' ) THEN
C
C
C
C           Sample rate can have many values.
C           These may need to shift down by 1 if we do complex
C           sampling.
C           Current version of DBBC only allows 1-32 MHz channels and
C           Nyquist sampling.
C
            IF( SAMPRATE(KS) .NE. 64.0 .AND.
     1          SAMPRATE(KS) .NE. 32.0 .AND.
     2          SAMPRATE(KS) .NE. 16.0 .AND.
     3          SAMPRATE(KS) .NE. 8.0 .AND.
     4          SAMPRATE(KS) .NE. 4.0 .AND.
     5          SAMPRATE(KS) .NE. 2.0 ) THEN
               MSGTXT = ' '              
               WRITE( MSGTXT, '( A, F8.3, A )' )
     1           'CHKDBBC: Invalid SAMPRATE specified: ', SAMPRATE(KS),
     2           ' for DBE=DBBC_DDC. Must be 2 to 64 Msamp/s.'
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
C
C           Bits per sample is 2, but 1 bit recording is
C           possible via channel selection. 
C
            DO ICH = 1, NCHAN(KS)
               IF( BITS(ICH,KS) .NE. 2 .AND. BITS(ICH,KS) .NE. 1) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, A, I3 )' )
     1               'CHKDBBC: BITS must be 2 or 1 for DBE=DBBC_DDC. ',
     2               '  Value specified is: ', BITS(ICH,KS)
                  CALL WLOG( 1, MSGTXT )
                  ERRS = .TRUE.
               END IF
            END DO
C
C  ******************  There are some "interesting" ifchan assignment
C            restrictions for the DBBC_DDC.  They are DBBCVER dependent.
C            IFDBBC knows the rules and set IFBBC above.
C
            DO ICH = 1, NCHAN(KS)
C              IFs are in range A-D, inputs in range 1-4. You may omit the
C              input number, but it's not encouraged.
               IF( ( IFCHAN(ICH,KS)(1:1) .NE. 'A' .AND.
     1               IFCHAN(ICH,KS)(1:1) .NE. 'B' .AND.
     2               IFCHAN(ICH,KS)(1:1) .NE. 'C' .AND.
     3               IFCHAN(ICH,KS)(1:1) .NE. 'D' )  .OR.
     4             ( IFCHAN(ICH,KS)(2:2) .NE. '1' .AND.
     5               IFCHAN(ICH,KS)(2:2) .NE. '2' .AND.
     6               IFCHAN(ICH,KS)(2:2) .NE. '3' .AND.
     7               IFCHAN(ICH,KS)(2:2) .NE. '4' .AND.
     8               IFCHAN(ICH,KS)(2:2) .NE. ' ' )) THEN
                  CALL WLOG ( 1, 'CHKDBBC: IFCHAN ''' // IFCHAN(ICH,KS)
     1              // ''' not A[1-4], B[1-4], C[1-4] or D[1-4]' )
                  ERRS = .TRUE.
               END IF
C
            END DO
C
C
C           Loop through the channels checking IF assignments.
C
            SOMEBAD = .FALSE.
            DO ICH = 1, NCHAN(KS)
C
C              See if this channel uses an allowed IF given the wiring
C              constraints embodied in the IFBBC array. Only the IF name can
C              be enforced, the input number is known only to the catalogue.
C              Omitting the input number is not an error, but will require
C              remedial action at the station after running drudg.
C
               OK = .FALSE.
               IBBC = BBC(ICH,KS)
               DO IIF = 1, MAXIF
                  IF( IFNAM(IIF) .EQ. IFCHAN(ICH,KS)(1:1) .AND.
     1                IFBBC(IBBC,IIF) .EQ. 1 ) OK = .TRUE.
               END DO
C
C              Give warnings if it was not allowed.
C
               IF( .NOT. OK ) THEN
                  SOMEBAD = .TRUE.
                  SETMSG = ' '
                  WRITE( SETMSG, '( 3A, I3, A, I3 )' )
     1                'CHKDBBC: Illegal IF input ', IFCHAN(ICH,KS),
     2                ' for DBBC, channel ', ICH, '  BBC ', IBBC
                  CALL WLOG( 1, SETMSG )
                  SETMSG = ' '
                  WRITE( SETMSG, '( 2A )' )
     1              '         Allowed IF index and first character',
     2              ' for this BBC are:'
                  IC = LEN1( SETMSG ) + 1
                  DO IIF = 1, MAXIF
                     IF( IFBBC(IBBC,IIF) .EQ. 1 ) THEN
                        WRITE( SETMSG(IC:IC+5), '( 1X, A1, I1, 3A1 )' )
     1                        '(', IIF, ',', IFNAM(IIF), ')'
                        IC = IC + 6
                     END IF
                  END DO
                  CALL WLOG( 1, SETMSG )
                  ERRS = .TRUE.
               END IF
C
            END DO
            IF( SOMEBAD ) THEN
               SETMSG = ' '
               WRITE( SETMSG, '( 2A )' )
     1             '        Be careful of special wiring restrictions ',
     2             'for these DARs.'
               CALL WLOG( 1, SETMSG )
            END IF
         END IF
C
C           Bandwidths and frequencies checked in CHKDBFQ
C
C        Check the frequencies and bandwidths.  This is pulled out
C        so that it can be used again later on frequencies set in-line
C        or using Doppler.
C
         CALL CHKDBFQ( KS, BBFILT(1,KS), BBSYN(1,KS), ERRS, .TRUE. )
C
      END IF
C
      RETURN
      END
