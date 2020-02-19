      SUBROUTINE CHKSET( KS )
C
C     Routine to check the validity of inputs from the SETUP file.  
C     This is called for setup group KS
C
C     Remove checks of tape recorders.  July 22, 2010  RCW.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
      INCLUDE  'schfreq.inc'
C
      INTEGER           KS, ICH, JCH, ISTA
      INTEGER           LEN1, LNAME, ITOUT
      LOGICAL           ERRS, SAMPWARN, OVERWARN, WARN2CM
      DOUBLE PRECISION  BBWIDI, BBWIDJ
      DOUBLE PRECISION  FLOWI, FLOWJ, FHIGHI, FHIGHJ
      SAVE              ITOUT, WARN2CM
      DATA              ITOUT / 1 /
      DATA              WARN2CM / .TRUE. /
C ----------------------------------------------------------------------
C     Get the schedule station number
C
      ISTA = ISCHSTA(ISETSTA(KS))
C
C     Debug help.
C
      IF( DEBUG ) THEN
         CALL WLOG( 0, 'CHKSET: Starting on '//SETSTA(1,KS)//
     1                   ' in:' )
         CALL WLOG( 0, '        ' //  SETNAME(KS) )
      END IF
C
C     Initializations.
C
      ERRS = .FALSE.
      LNAME = LEN1( SETNAME(KS) )
      SAMPWARN = .TRUE.
C
C     Warn if OBSTYP requests no recording while format suggests otherwise.
C     This might be legitimate for certain tests, so don't crash.
C
      IF( NOTAPE .AND. FORMAT(KS) .NE. 'NONE' ) THEN
         WRITE( MSGTXT, '( 4A )' ) 
     1       'CHKSET: *** WARNING - OBSTYPE=', OBSTYP,
     2       ' but setup has FORMAT=', FORMAT(KS)
         CALL WLOG( ITOUT, MSGTXT )
         MSGTXT = ' '
         WRITE( MSGTXT, '( A )' )
     1       '        No tapes will be recorded with this OBSTYPE.'
         CALL WLOG( ITOUT, MSGTXT )
         MSGTXT = ' '
         IF( OBSTYP .EQ. 'NONE' ) THEN
            CALL WLOG( ITOUT, 
     1       '        NONE is the default OBSTYPE.  ' //
     2       'Did you forget to specify it?' )
         ELSE
            WRITE( MSGTXT, '( A )' )
     1       '        Was this intended?'
            CALL WLOG( ITOUT, MSGTXT )
         END IF
         MSGTXT = ' '
         ITOUT = 0
      END IF
C
C     Don't check VLBI related parameters if this is VLA only.
C     Many of the parameters that are specific to certain types
C     of observatories will be checked in other routines like
C     CHKVLBA and CHKVLA.
C
      IF( .NOT. VLAONLY ) THEN
C
C        Check the Mark 2 format.
C
         IF( MARK2 .AND. FORMAT(KS) .NE. 'MARKII' ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A )' )
     1       'CHKSET: For OBSTYP=MKII, FORMAT must be MARKII, not: ',
     2        FORMAT(KS)
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
C        Prohibit frequency switching if VEX or VSOP (DRUDG) 
C        files are to be written.
C
         IF( ( DOVEX .OR. DOVSOP ).AND. FRSWITCH(KS) ) THEN
            CALL WLOG( 1, 'CHKSET:  Cannot frequency switch with VEX '//
     1          'or VSOP file.' )
            ERRS = .TRUE.
         END IF
C
C        Check generic parameters related to channels and DAR.
C
C        Be sure there are some channels.  Die immediately if not
C        to prevent DO loop over bad range.
C
         IF( NCHAN(KS) .LT. 1 ) THEN
            CALL WLOG( 1, 'CHKSET: Setup file must have at least 1 '
     1             // 'channel unless OBSTYPE=VLA.' )
            CALL ERRLOG( 'CHKSET: The error is in ' // 
     1                SETNAME(KS)(1:LNAME) )
         END IF
C
C        Now check the channels.
C
         OVERWARN = .FALSE.
         DO ICH = 1, NCHAN(KS)
C
C           Sideband.
C
            IF( SIDEBD(ICH,KS) .NE. 'U' .AND. 
     1          SIDEBD(ICH,KS) .NE. 'L' ) THEN
               CALL WLOG( 1, 'CHKSET: Sideband not U or L in ' // 
     1                       SETNAME(KS)(1:LNAME) )
               ERRS = .TRUE.
            END IF
C
C           Polarization.
C
            IF( POL(ICH,KS)(1:3) .NE. 'RCP' .AND.
     1          POL(ICH,KS)(1:3) .NE. 'LCP' .AND.
     2          POL(ICH,KS)(1:1) .NE. 'X' .AND.
     3          POL(ICH,KS)(1:1) .NE. 'Y' ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( 3A, I4, A )' )
     1            'CHKSET: In setup ', SETNAME(KS)(1:LEN1(SETNAME(KS))),
     2            ' polarization of chan ', ICH,
     3            ' not given or deduced (should be RCP, LCP, X or Y).'
               CALL WLOG( 1, MSGTXT )
               ERRS = .TRUE.
            END IF
C
C           Check to see if two polarizations are expected from the
C           same IF (I seen users do that one and the warning about
C           not all bandwidth being in the IF is not too clear.
C
            IF( ICH .GE. 2 .AND. ICH .LT. NCHAN(KS) ) THEN
               DO JCH = ICH, NCHAN(KS)
                  IF( POL(ICH,KS) .NE. POL(JCH,KS) .AND. 
     1                IFCHAN(ICH,KS) .EQ. IFCHAN(JCH,KS) ) THEN
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( 7A, I3, 3A, I3, A )' )
     1                  'CHKSET:  In setup ', SETNAME(KS)(1:LNAME),
     2                  ' IF channel ', IFCHAN(ICH,KS), 
     3                  ' is assigned to ', POL(ICH,KS), ' in chan ',
     4                  ICH, ' and to ', POL(JCH,KS), ' in chan ',
     5                  JCH, '  Not possible.'
                     CALL WLOG( 1, MSGTXT )
                     ERRS = .TRUE.
                  END IF
               END DO
            END IF
C
C           Be sure bandwidth is not more than half the sample rate.
C
            IF( VLBITP ) THEN
               IF( BBFILT(ICH,KS) .GT. 0.5001D0 * SAMPRATE(KS) ) THEN
                  CALL WLOG( 1, 'CHKSET: Bandwidth more than half of '//
     1              'sample rate' )
                  ERRS = .TRUE.
               END IF
               IF( BBFILT(ICH,KS) .LT. 0.4999D0 * SAMPRATE(KS) .AND. 
     1             SAMPWARN ) THEN
                  CALL WLOG( 0, 
     1                    'CHKSET note: Oversampling specified. ' )
                  SAMPWARN = .FALSE. 
               END IF
C
            END IF
C
C           Warn of overlapping channels.  Don't worry about overlaps
C           due to the round down to the nearest 10 kHz when the
C           62.5 and 125 kHz channels are used.
C
            IF( ICH .GT. 1 ) THEN
C
C              Get frequency range of channel ICH.
C
               BBWIDI = AINT( BBFILT(ICH,KS) / 10.0D0 ) * 10.0D0
               IF( NETSIDE(ICH,KS) .EQ. 'L' ) THEN
                  FLOWI  = FREQREF(ICH,KS) - BBWIDI
                  FHIGHI = FREQREF(ICH,KS)
               ELSE
                  FLOWI  = FREQREF(ICH,KS)
                  FHIGHI = FREQREF(ICH,KS) + BBWIDI
               END IF
C
C              Now compare with other lower numbered channels.
C
               DO JCH = 1, ICH - 1
C
C                 Require polarization match.
C
                  IF( POL(ICH,KS) .EQ. POL(JCH,KS) ) THEN
C
C                    Get frequency range of other channel.
C
                     BBWIDJ = AINT( BBFILT(JCH,KS) / 10.0 ) * 10.0 
                     IF( NETSIDE(JCH,KS) .EQ. 'L' ) THEN
                        FLOWJ  = FREQREF(JCH,KS) - BBWIDJ
                        FHIGHJ = FREQREF(JCH,KS)
                     ELSE
                        FLOWJ  = FREQREF(JCH,KS)
                        FHIGHJ = FREQREF(JCH,KS) + BBWIDJ
                     END IF
C
C                    Look for overlap.
C
                     IF( ( FLOWI.GE.FLOWJ .AND. FLOWI.LT.FHIGHJ ) .OR.
     1                   ( FHIGHI.GT.FLOWJ .AND. FHIGHI.LE.FHIGHJ ) ) 
     2                    THEN
                        OVERWARN = .TRUE.
                     END IF
C
                  END IF
               END DO
            END IF
C
         END DO
C
C        Write the overlapping channel warning.
C
         IF( OVERWARN ) THEN
            CALL WLOG( 1, 'CHKSET: Setup file: ' // 
     1         SETNAME(KS)(1:LEN1(SETNAME(KS))) //
     2         ' Has overlapping channels.  Intended?' )
         END IF
C
C        Check many items specific to certain DARs.
C        Note that the "geodetic" VLBA systems need some extra checks
C        beyond what the normal VLBA systems get thanks to incomplete
C        wiring.
C
         IF( DAR(ISETSTA(KS))(1:4) .EQ. 'RDBE' )  
     1         CALL CHKRDBE( KS, ERRS )
         IF( DAR(ISETSTA(KS))(1:4) .EQ. 'DBBC' )  
     1         CALL CHKDBBC( KS, ERRS )
         IF( DAR(ISETSTA(KS)) .EQ. 'WIDAR' ) 
     1         CALL CHKWIDAR( KS, ERRS )
         IF( DAR(ISETSTA(KS)) .EQ. 'VLBA'  .OR. 
     1       DAR(ISETSTA(KS)) .EQ. 'VLBAG' .OR.
     2       DAR(ISETSTA(KS)) .EQ. 'VLBA4' )  
     3         CALL CHKVDAR( KS, NBBC(ISETSTA(KS)), ERRS )
         IF( DAR(ISETSTA(KS)) .EQ. 'VLBA4' ) 
     1         CALL CHKV4DAR( KS, ERRS )
         IF( DAR(ISETSTA(KS)) .EQ. 'VLBAG' ) 
     1         CALL CHKGDAR( KS, ERRS )
         IF( DAR(ISETSTA(KS)) .EQ. 'MKIV'  ) 
     1         CALL CHK4DAR( KS, NBBC(ISETSTA(KS)), ERRS )
         IF( DAR(ISETSTA(KS)) .EQ. 'CDAS'  ) 
     1         CALL CHKCDAS( KS, ERRS )

C
C        Check various recorder related parameters.  Stripped of 
C        tape stuff July 22, 2010.  RCW
C
         IF( VLBITP ) THEN
C
C           Some disk items.
C
            IF( USEDISK(ISTA) ) THEN
               CALL CHKDISK( KS, ERRS )
            END IF
C
C           Check the bit rate per track and the tape speeds.    
C
            IF( ( RECORDER(ISETSTA(KS)) .EQ. 'VLBA' .OR. 
     1            RECORDER(ISETSTA(KS)) .EQ. 'MKIV' .OR.
     1            RECORDER(ISETSTA(KS)) .EQ. 'VLBA4' ) .AND.
     2            FORMAT(KS) .NE. 'NONE' ) THEN
               CALL CHKSPD( KS, ERRS )
            END IF
C
C           Force all channels to use the same number of bits.
C           note that this is not really required by the system,
C           but the correlator might have a fit with mixed mode data.
C
            DO ICH = 1, NCHAN(KS)
               IF( BITS(ICH,KS) .NE. BITS(1,KS) ) THEN
                  CALL WLOG( 1, 'CHKSET: All channels must use the same'
     1               // ' number of bits. ' )
                  ERRS = .TRUE.
               END IF
            END DO
C
C           Pulse cal specification SPCAL was set to right case 
C           and checked in RDSET.
C
         END IF
C
      END IF   ! not VLAONLY
C
C     Warn 2cm users about different standard frequencies.
C     Only worry about the VLBA and the narrower band systems.
C
      IF( WARN2CM .AND.
     1    FREQREF(1,KS) .GT. 15100.D0 .AND. 
     2    FREQREF(1,KS) .LT. 15500.D0 .AND. 
     3    SETSTA(1,KS)(1:4) .EQ. 'VLBA' .AND.
     4    TOTBPS(KS) .LT. 1000.0 ) THEN
         MSGTXT = 'CHKSET:  See sched.runlog for information' //
     1        ' on 2cm frequencies.'
         CALL WLOG( 1, ' ' )
         CALL WLOG( 1, MSGTXT )
         MSGTXT = ' '
         CALL WRTMSG( 0, 'CHKSET', 'warn2cm' )
         WARN2CM = .FALSE.
      END IF

C
C     Check some more VLBA specific parameters.
C
      IF( SETSTA(1,KS)(1:4) .EQ. 'VLBA' ) 
     1     CALL CHKVLBA( KS, ERRS )
C
C     Check the VLA parameters.
C
      IF( SETSTA(1,KS)(1:3) .EQ. 'VLA' ) 
     1     CALL CHKVLA( KS, ERRS )
C
C     Abort if problems were found.
C
      IF( ERRS ) THEN
         CALL WLOG( 1, 'CHKSET:  Freq groups used or checked: ' )
         DO ICH = 1, NCHAN(KS)
            IF( IFREQNUM(ICH,KS) .GT. 0 ) THEN
               WRITE( MSGTXT, '( A, I3, 2X, A, A )' ) ' Channel: ', 
     1             ICH, '  Frequency Group: ', 
     2              FRNAME(IFREQNUM(ICH,KS))
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' '
            END IF
         END DO
         CALL ERRSET( KS )
      END IF
C
      RETURN
      END
