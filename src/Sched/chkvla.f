      SUBROUTINE CHKVLA( KS, ERRS )
Cf2py intent(in, out) ERRS
C
C     Subroutine for SCHED called by CHKSET that checks some
C     VLA specific setup file parameters.
C
C     If ERRS is returned as TRUE, CHKSET will give the setup
C     file name and then crash the program.
C
C     The VLA routines are getting major surgery for the switch from
C     the old VLA system to the new hardware.  The VLA now uses VEX for 
C     control and WIDAR for the DAR.  For now (Oct 2012), the VLA
C     is restricted to 4 channels.  Each must be from a separate
C     IF and they must be in polarization pairs.  The channels must
C     be net USB.  This is changing to 16 channels, four each from
C     each IF.  That is coming soon enough that this routine will
C     assume it is here (Dec. 28, 2012).
C
C     Conversion to new system started May 9, 2012 RCW.  
C     For the old code, look for versions of SCHED 10.1 or earlier.
C     More work in Oct. 2012.  RCW
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER     VMIF, MVFE
      PARAMETER   (VMIF=4)
      PARAMETER   (MVFE=10)
      CHARACTER   VIFNAME(VMIF)*1, VIFPOL(VMIF)*3
      CHARACTER   VBANDS(MVFE)*2, VBANDSL(MVFE)*2, VLBBANDS(MVFE)*4
      INTEGER     KS, ICH, JCH, NCIF(VMIF), IIF, NAC, NBD, ISCN
      INTEGER     ISETF, IB, LEN1
      LOGICAL     ERRS, GOTPAIR, IFOK
      DOUBLE PRECISION  VIFFLO(VMIF), VIFFHI(VMIF), FRMAX, FRMIN
      DOUBLE PRECISION  ACAVG, BDAVG
      DOUBLE PRECISION  VLFREQ(MVFE), VHFREQ(MVFE)
      DATA  VIFNAME  / 'A', 'B', 'C', 'D' /
      DATA  VIFPOL   / 'RCP', 'RCP', 'LCP', 'LCP' /
C
C     The following are the lower and upper frequencies for each band
C     in MHz.
C
      DATA  VLFREQ  / 0.D0, 200.D0, 900.D0, 2000.D0, 4000.D0, 8000.D0,
     1               12.D3, 18.D3, 26.5D3, 40.D3 /
      DATA  VHFREQ  / 200.D0, 900.D0, 2000.D0, 4000.D0, 8000.D0, 12.D3,
     2               18.D3, 26.5D3, 40.D3, 50.D3 /
C
C     The following are the official band names.
C
      DATA  VBANDS  / '4', 'P', 'L', 'S', 'C', 'X', 
     1                'Ku', 'K', 'Ka', 'Q' /
C
C       The following are used for testing because FE, on input, is
C       down-cased.  They will be converted to the above.
C
      DATA  VBANDSL / '4', 'p', 'l', 's', 'c', 'x', 
     1                'ku', 'k', 'ka', 'q' /
C
C       The following are the VLBA names.  If found, translate to VLA.
C
      DATA  VLBBANDS / 'NA', '50cm', '20cm', '13cm', '6cm', '4cm', 
     1                '2cm', '1cm', 'NA', '7mm' /


C  --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKVLA: Starting.' )
      ISETF = ISETNUM(KS) 
C
C     Get rid of the pre-EVLA inhibition against 24+ hr schedules due to 
C     no day number in the obs files.  Also remove "vlaover24" from 
C     messages.txt.  Dec. 28, 2012.  RCW
C
C     Check VLARFANT.  Not sure if this will do anything for VLA.
C     This parameter is not used for the new system, so ignore it.
C     Keep the code in case it comes back.
C
C      IF( VLARFANT .LT. 1 .OR. VLARFANT .GT. 28 ) THEN
C         SETMSG = ' '
C         WRITE( SETMSG, '( A, I7, A )' ) 'CHKVLA: VLARFANT ', 
C     1      VLARFANT, ' out of range 1 to 28.'
C         CALL WLOG( 1, SETMSG )
C         ERRS = .TRUE.
C      END IF
C
C      Check the format.  Must be VDIF.
C
       IF( FORMAT(KS)(1:4) .NE. 'VDIF' ) THEN
          CALL WLOG( 1, 'CHKVLA:  The recording format at the VLA '//
     1       'must be VDIF, not '//FORMAT(KS) )
          CALL ERRLOG( 'Change the FORMAT and try again.' )
       END IF
C
C     Check the maximum number of channels.
C     This is modified for the new 16 channel option  Dec. 28, 2012.
C     Embed the 16 channel check in separate 4 channel checks for each
C     IF.  If there are more than 16, it will fail that test.
C     Check the polarization spec while at it.
C     Also check the frequency range for each IF.  Assume upper sideband
C     here.  That will be checked later.
C
C     Initializations for summary info.
C
      DO IIF = 1, VMIF
         NCIF(IIF) = 0
         VIFFLO(IIF) = 1.D15
         VIFFHI(IIF) = 0.D0
         ACAVG = 0.D0
         BDAVG = 0.D0
         NAC = 0
         NBD = 0
      END DO
      FRMAX = 0.D0
      FRMIN = 1.D12
      DO ICH = 1, NCHAN(KS)
C
C        Initialize - bad IF spec until proven good!
C
         IFOK = .FALSE.
C
C        Get which IF this is.
C
         DO IIF = 1, VMIF
            IF( IFCHAN(ICH,KS) .EQ. VIFNAME(IIF) ) THEN
               NCIF(IIF) = NCIF(IIF) + 1
               IFOK = .TRUE.
C
C              Check polarization specification.
C
               IF( POL(ICH,KS) .NE. VIFPOL(IIF) ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, A, A, A3, A, A3, A, 2I5 )' )
     1              'CHKVLA: VLA IF ', IFCHAN(ICH,KS), 
     2              ' is ', VIFPOL(IIF), ' but ', POL(ICH,KS), 
     3              ' requested for setup, channel ', KS, ICH
                  CALL WLOG( 1, MSGTXT )
                  ERRS = .TRUE.
               END IF
C
C              Get the minimum and maximum frequencies for the IF.
C              Recall that the VLA is always upper sideband.
C
               VIFFLO(IIF) = MIN( VIFFLO(IIF), FREQREF(ICH,KS) )
               VIFFHI(IIF) = MAX( VIFFHI(IIF), 
     1                            FREQREF(ICH,KS) + BBFILT(ICH,KS) )
               FRMAX = MAX( VIFFHI(IIF), FRMAX )
               FRMIN = MAX( VIFFLO(IIF), FRMIN )
               IF( VIFNAME(IIF) .EQ. 'A' .OR. 
     1             VIFNAME(IIF) .EQ. 'C' ) THEN
                  NAC = NAC + 1
                  ACAVG = ACAVG + FREQ(ICH,KS) + 0.5D0 * BBFILT(ICH,KS)
               ELSE IF( VIFNAME(IIF) .EQ. 'B' .OR. 
     1                  VIFNAME(IIF) .EQ. 'D' ) THEN
                  NBD = NBD + 1
                  BDAVG = BDAVG + FREQ(ICH,KS) + 0.5D0 * BBFILT(ICH,KS)
               END IF
            END IF
         END DO
C
C        Complain about an unrecognized IF name.
C
         IF( .NOT. IFOK ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I4, 3A )' )
     1          'CHKVLA: VLA baseband channel ', ICH, ' has IFCHAN ',
     2          IFCHAN(ICH,KS), ' which is not a VLA IF.'
            ERRS = .TRUE.
         END IF
      END DO
      IF( NAC .GT. 0 ) ACAVG = ACAVG / NAC
      IF( NBD .GT. 0 ) BDAVG = BDAVG / NBD
C
C     Now complain if any IF has too many channels and do other 
C     checks of the IFs.
C
      DO IIF = 1, VMIF
         IF( NCIF(IIF) .GT. 4 ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A, I4, A, A )' )
     1          'CHKVLA: ', NCIF(IIF),
     2          ' channels were assigned to VLA IF ', VIFNAME(IIF),
     3          '  The maximum allowed is 4.'
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
C        Complain if frequency range is too high for IF's that got used.
C
         IF( NCIF(IIF) .GT. 0 .AND. 
     1       VIFFHI(IIF) - VIFFLO(IIF) .GT. 1024.D0 ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A, A, A )' )
     1          'CHKVLA: Frequency range of channels assigned to ',
     2          'VLA IF ', VIFNAME(IIF), 
     3          ' span over 1024 MHz (', VIFFHI(IIF) - VIFFLO(IIF),
     4          ' MHz).' 
            CALL WLOG( 1, MSGTXT )
            ERRS = .TRUE.
         END IF
C
C        Deal with the VLA receiver names.  And complain if they
C        don't seem right.  Base this on the lowest frequency
C        requested for the IF.  Given that the frequencies span 
C        all of the logical space, one should stop somewhere in this
C        loop.
C
         IF( VIFFLO(IIF) .GT. VHFREQ(MVFE) ) THEN
            FE(IIF,KS) = 'omit'
         ELSE 
            DO IB = 1, MVFE
               IF( VIFFLO(IIF) .GT. VLFREQ(IB) .AND. 
     1             VIFFLO(IIF) .LE. VHFREQ(IB) ) THEN
                  IF( FE(IIF,KS) .EQ. ' ' .OR. 
     1                FE(IIF,KS) .EQ. 'omit' .OR.
     2                FE(IIF,KS) .EQ. VBANDSL(IB) .OR.
     3                FE(IIF,KS) .EQ. VLBBANDS(IB) ) THEN
                     FE(IIF,KS) = VBANDS(IB)
                  ELSE IF( FE(IIF,KS) .NE. VBANDS(IB) ) THEN
                     MSGTXT = ' '
                     WRITE( MSGTXT, '( A, I5, 4A )' )
     1                  'CHKVLA: Apparently incorrect VLA FE name ',
     2                  'for setup:', KS, '.  Is: ', FE(IIF,KS),
     3                  '  Should be: ', VBANDS(IB)
                     CALL WLOG( 1, MSGTXT )
                     CALL ERRLOG( 'Fix VLA BAND name in setup'//
     1                   SETFILE(ISETF)(1:LEN1(SETFILE(ISETF))) )
                  END IF
               END IF
            END DO
         END IF
      END DO
C
C     Enforce AC > BD for F>18GHz and data requested from AC and BD.
C     Comment this out.  It only affects cases that won't be used for 
C     VLBI, like Ka band.  Leave the hook here in case we get Ka band
C     on the VLBA some day.
C
C      IF( FRMAX .GT. 18000.0 .AND. NAC .GT. 0 .AND. NBD .GT. 0 ) THEN
C         IF( BDAVG .GT. ACAVG ) THEN
C            CALL WLOG( 1, 'CHKVLA WARNING:  Above 18 GHz, '//
C     1               'it is best, and sometimes required, that the ' )
C            CALL WLOG( 1, MSGTXT )
C            CALL WLOG( 1, '                  VLA AC IFs be at '//
C     1               'higher frequencies than the BD IFs.' )
C            MSGTXT = ' '
C            WRITE( MSGTXT, '( 2A )' )
C     1                    '                  Please fix setup ', 
C     2         SETFILE(ISETF)(1:LEN1(SETFILE(ISETF)))
C            CALL WLOG( 1, MSGTXT )
C         END IF
C      END IF
C
C     Require dual polarization pairs.
C     Do this a bit crudely - go through all channels looking for the
C     partner.  This is twice as many checks as required, but it is
C     such a short loop it doesn't matter.
C     I am also not checking the frequency sets.  Maybe should.
C
      IF( NCHAN(KS) .GT. 1 ) THEN
         DO ICH = 1, NCHAN(KS)
            GOTPAIR = .FALSE.
            DO JCH = 1, NCHAN(KS)
               IF( ICH .NE. JCH .AND.
     1             FREQREF(ICH,KS) .EQ. FREQREF(JCH,KS) .AND.
     2             BBFILT(ICH,KS) .EQ. BBFILT(JCH,KS) .AND.
     3             POL(ICH,KS) .NE. POL(JCH,KS) .AND.
     4             NETSIDE(ICH,KS) .EQ. NETSIDE(JCH,KS) ) THEN
                  GOTPAIR = .TRUE.
               END IF
            END DO
            IF( .NOT. GOTPAIR ) THEN
               SETMSG = ' '
               WRITE( SETMSG, '( 2A, I4, A )' ) 
     1           'CHKVLA: At the VLA, baseband channels must be ', 
     2           'polarization pairs.  Channel ', ICH,
     3           ' does not have a partner.'
               CALL WLOG( 1, SETMSG )
               ERRS = .TRUE.
            END IF
         END DO      
      END IF
C
C     Require net upper sideband.
C
      DO ICH = 1, NCHAN(KS)
         IF( NETSIDE(ICH,KS) .NE. 'U' ) THEN
            SETMSG = ' '
            WRITE( SETMSG, '( 2A, I4, A )' ) 
     1        'CHKVLA: At the VLA, baseband channels must be ', 
     2        'net upper sideband.  Channel ', ICH,
     3        ' is not.'
            CALL WLOG( 1, SETMSG )
            ERRS = .TRUE.
         END IF
      END DO
C
C     Require that FE be set.
C
      DO ICH = 1, NCHAN(KS)
         IF( ( IFCHAN(ICH,KS).EQ.'A' .AND. FE(1,KS).EQ.'omit' ) .OR.
     1       ( IFCHAN(ICH,KS).EQ.'B' .AND. FE(2,KS).EQ.'omit' ) .OR.
     2       ( IFCHAN(ICH,KS).EQ.'C' .AND. FE(3,KS).EQ.'omit' ) .OR.
     3       ( IFCHAN(ICH,KS).EQ.'D' .AND. FE(4,KS).EQ.'omit' ) ) THEN
            SETMSG = ' '
            WRITE( SETMSG, '( 3A )' ) 
     1         'CHKVLA: FE not specified for IF ', IFCHAN(ICH,KS),
     2         ' at the VLA.'
            CALL WLOG( 1, SETMSG )
            ERRS = .TRUE.
         END IF
      END DO
C
C     Check VLAPTIME is 10 seconds or more.
C
      DO ISCN = SCAN1, SCANL
         IF( VLAPTIME(ISCN) .LT. 10 ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A, I7, A, I5 )' )
     1         'CHKVLA:  VLA phase time (VLAPTIME) should be 10 sec ',
     2         'or more.', VLAPTIME(ISCN), ' s found on scan ', ISCN
            CALL ERRLOG( MSGTXT )
         END IF
      END DO
C
C     Consider checking the frequency ranges at some point, but it is not
C     critical for now because the VLA can hit any frequency that the VLBA
C     can.  The main problems will occur with the sideband restrictions 
C     which will not be compatible with WIDAR (USB) and the PFB on the VLBA
C     when the IF is upper sideband.  We will need the sideband inverting 
C     mode for that.
C
      RETURN
      END
