      SUBROUTINE CHKSET( KS )
C
C     Routine to check the validity of inputs from the SETUP file.  
C     This is called for setup group KS
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
      INCLUDE  'schfreq.inc'
C
      INTEGER           KS, ICH, JCH, ISTA
      INTEGER           LEN1, LNAME, ITOUT
      LOGICAL           ERRS, SAMPWARN, OVERWARN
      CHARACTER         UPPCAL*4
      REAL              BBWIDI, BBWIDJ
      DOUBLE PRECISION  FLOWI, FLOWJ, FHIGHI, FHIGHJ
      SAVE              ITOUT
      DATA              ITOUT / 1 /
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
C     Warn if OBSTYP requests no tape while format suggests otherwise.
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
     1          POL(ICH,KS)(1:3) .NE. 'LCP' ) THEN
               CALL WLOG( 1, 'CHKSET: Polarization not given '//
     1             'or deduced (should be RCP or LCP).' )
               ERRS = .TRUE.
            END IF
C
C           Be sure bandwidth is not more than half the sample rate.
C
            IF( VLBITP ) THEN
               IF( BBFILT(ICH,KS) .GT. 0.5*SAMPRATE(KS) ) THEN
                  CALL WLOG( 1, 'CHKSET: Bandwidth more than half of '//
     1              'sample rate' )
                  ERRS = .TRUE.
               END IF
               IF( BBFILT(ICH,KS) .LT. 0.5*SAMPRATE(KS) .AND. 
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
               BBWIDI = AINT( BBFILT(ICH,KS) / 10.0 ) * 10.0 
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
     2                   OVERWARN = .TRUE.
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
            CALL WLOG( 0, 'CHKSET: Setup file: ' // SETNAME(KS) )
            CALL WLOG( 0, 
     1         '        Has overlapping channels.  Intended?' )
         END IF
C
C        Check many items specific to certain DARs.
C        Note that the "geodetic" VLBA systems need some extra checks
C        beyond what the normal VLBA systems get thanks to incomplete
C        wiring.
C
         IF( DAR(ISETSTA(KS)) .EQ. 'VLBA' .OR. 
     1       DAR(ISETSTA(KS)) .EQ. 'VLBAG'  .OR.
     2       DAR(ISETSTA(KS)) .EQ. 'VLBA4' )  
     3         CALL CHKVDAR( KS, NBBC(ISETSTA(KS)), ERRS )
         IF( DAR(ISETSTA(KS)) .EQ. 'VLBA4' ) CALL CHKV4DAR( KS, ERRS )
         IF( DAR(ISETSTA(KS)) .EQ. 'VLBAG' ) CALL CHKGDAR( KS, ERRS )
         IF( DAR(ISETSTA(KS)) .EQ. 'MKIV'  ) 
     1         CALL CHK4DAR( KS, NBBC(ISETSTA(KS)), ERRS )

C        Check various recorder related parameters.
C
         IF( VLBITP ) THEN
C
C           Some disk items.
C
            IF( USEDISK(ISTA) ) THEN
               CALL CHKDISK( KS, ERRS )
            END IF
C
C           Items specific to certain recorder types.
C
            IF( RECORDER(ISETSTA(KS)) .EQ. 'VLBA' ) THEN 
C
C              Special cases are EVN VLBA recorders and dar with
C              formatter upgraded, but the transport still not upgraded.
C
               IF( DAR(ISETSTA(KS)) .EQ. 'VLBA4' ) THEN 
                  CALL CHKV4REC( KS, ERRS, RECORDER(ISETSTA(KS)) )
               ELSE
                  CALL CHKVREC( KS, ERRS )
               ENDIF
            ENDIF
            IF( RECORDER(ISETSTA(KS)) .EQ. 'VLBA4' ) 
     1          CALL CHKV4REC( KS, ERRS, RECORDER(ISETSTA(KS)) )
            IF( RECORDER(ISETSTA(KS)) .EQ. 'MKIV' ) 
     1           CALL CHK4REC( KS, ERRS )
            IF( RECORDER(ISETSTA(KS)) .EQ. 'S2' ) 
     1           CALL CHKSREC( KS, ERRS )
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
C           Check the pulse cal specification.  Simple so far.
C
            UPPCAL = SPCAL(KS)
            CALL UPCASE( UPPCAL )
            IF( UPPCAL .NE. 'OFF' .AND. UPPCAL .NE. '1MHZ' .AND.
     1          UPPCAL .NE. '5MHZ' ) THEN
               CALL WLOG( 1, 'CHKSET: Invalid PCAL specification: '
     1               // SPCAL(KS) )
               ERRS = .TRUE.
            END IF
C
         END IF
C
      END IF   ! not VLAONLY
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
         IF( IFREQNUM(KS) .GT. 0 )
     1      CALL WLOG( 1, 'CHKSET:  Freq group used or checked: ' //
     2       FRNAME(IFREQNUM(KS)) )
         CALL ERRSET( KS )
      END IF
C
  990 CONTINUE
      RETURN
      END
