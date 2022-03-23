      SUBROUTINE PCALFQ( PCOPT, KF, PCX1, PCX2, PCFR1, PCFR2 )
C
C     Routine to set the pcal tone commands for frequency set KF.
C     Called by RECONFIG
C     and WRTFREQ, perhaps others.  These pcal commands control
C     the detectors in the legacy system on the VLBA.  They appear
C     only in the crd files.  They may differ from what is in the 
C     Vex file.
C
C     If PCOPT = "SET", get the setup file values for setup group KS=KF.
C     This option is not yet used.  PCOPT is usually the pcal state
C     (1MHZ, 5MHZ, or OFF) which is what is was meant to be in the past.
C     But the pcal state is now an item in the frequency set and is
C     obtained there.
C
C     PCX1 and PCX1 are the detector/bit to use (eg. S2, M4 etc).
C     PCFR1 and PCFR2 are the tone frequency to use in kHz. 
C
C     This routine takes into account any FREQ and BW set in the scan
C     It also takes into account and CRD parameters that are meant
C     to only affect the VLBA crd files.
C
C     Aug. 2013.  Modifying the way we deal with PCALX.  This is
C     triggered by addition of infrastructure to allow the VLBA
C     legacy system BBCs to be set to different frequencies than
C     the main recording channels, mainly to allow reference pointing
C     on masers while using the RDBE_PFB which cannot do narrow 
C     bands or be finely tuned.  In the process, there will no longer
C     be pulse cal sets - they are merged with frequency sets.
C
C     The pcalx information for the tone detectors in the legacy 
C     system can be provided by the user in the setup.  But that 
C     information is likely to become inappropriate if any of the
C     scan parameters FREQ, BW, DOPPLER, CRDDOP, CRDFREQ, CRDBW, or
C     PCAL are given.  Any changes in these parameters triggers
C     a new (or previously used) frequency set.
C
C     In the new scheme, the pcalx assignments will be made on the
C     fly by this routine when they are needed for crd files, the .sum
C     file etc.  There is an assumption, enforced by the nature
C     of the code, that the pcx setup will be unique to a frequency
C     set.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER           KF, KS
      INTEGER           ICH, ID, IP, ICT, IDD
      INTEGER           NTONES(MCHAN)
      INTEGER           TONE1(MCHAN), TONE2(MCHAN), TONE3(MCHAN)
      INTEGER           INTBW, NTPB
      INTEGER           PCFR1(MAXPC), PCFR2(MAXPC)
      INTEGER           KSTA, KSCN, ISTA, CRSETC(MAXCHN)
      DOUBLE PRECISION  RTONE1
      DOUBLE PRECISION  BBCFREQ(MCHAN), BBCBW(MCHAN)
      DOUBLE PRECISION  TONEINT, LOSUM(MCHAN)
      CHARACTER         PCX1(MAXPC)*3, PCX2(MAXPC)*3
      CHARACTER         PCALC1*3, UPCAL*4, PCOPT*4
      INTEGER           CRDN, ICHS
      DOUBLE PRECISION  CRDF(MCHAN), CRDB(MCHAN), CRDLOSUM(MCHAN)
      CHARACTER         CRDS(MCHAN)*1, CNETSIDE*1
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'PCALFQ starting ' )
C
C     Allow a call that uses the setup file values rather than any
C     that might have been provided in the scan dependent variables
C     FREQ, BW, and DOPPLER, or the CRD equivalents.  Here we assume
C     that KF is the desired setup group.
C     Note added later:  Not used - not sure why this was provided.
C
      IF( PCOPT .EQ. 'SET' ) THEN
         KS = KF
         UPCAL = SPCAL(KS)
         CRDN = NCHAN(KS)
         DO ICH = 1, NCHAN(KS)
            LOSUM(ICH) = FREQREF(ICH,KS)
            BBCBW(ICH) = BBFILT(ICH,KS)
            CRDF(ICH) = LOSUM(ICH)
            CRDB(ICH) = BBCBW(ICH)
            CRDS(ICH) = SIDEBD(ICH,KS)
            CRDLOSUM(ICH) = LOSUM(ICH)
         END DO
      ELSE
C
C        Get information from the frequency set.
C        This includes the frequencies that we will actually
C        be using for the legacy system for which we are trying
C        to set the pulse cal detector values.
C
         KS = FSETKS(KF)
         UPCAL = FSPCAL(KF)
         CALL FSFREQ( KF, LOSUM, BBCFREQ, BBCBW,
     1         CRDN, CRDF, CRDB, CRDS, CRDLOSUM, CRSETC )
C
      END IF
      KSCN = FSETSCN(KF)
      KSTA = ISETSTA(KS)
      ISTA = ISCHSTA(KSTA)
C
C     Get the tone interval.  The case and validity of the PCAL 
C     commands was checked on input of PCAL in the setups and schedule.
C
      TONEINT = -1.D0
      IF( UPCAL .EQ. '1MHz' ) THEN
         TONEINT = 1.D0
      ELSE IF( UPCAL .EQ. '5MHz' ) THEN
         TONEINT = 5.D0
      ELSE IF( UPCAL .EQ. 'off' ) THEN
         TONEINT = 0.D0
      ELSE
C
C        Earlier tests should have failed so we shouldn't get here.
C
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, A, A, F7.3)' )
     1     'PCALFQ: Programming error with PCAL. PCAL=''', UPCAL,
     2     '''  TONEINT=', TONEINT 
         CALL ERRLOG( MSGTXT )
      END IF
C
C     If the first PCALX1 is set and the pcal setting is the same
C     as for the setup group, assume that all other PCALX values are
C     also set and don't need to change.  Just write out what came 
C     from the setup file. Otherwise, set the defaults.
C
      IF( PCALX1(1,KS) .NE. ' ' .AND. UPCAL .EQ. SPCAL(KS) ) THEN
C
         DO IP = 1, MAXPC
            PCX1(IP) = PCALX1(IP,KS)
            PCX2(IP) = PCALX2(IP,KS)
            PCFR1(IP) = PCALFR1(IP,KS)
            PCFR2(IP) = PCALFR2(IP,KS)
            IF( PCX1(IP) .EQ. ' ' ) PCX1(IP) = 'OFF'
            IF( PCX2(IP) .EQ. ' ' ) PCX2(IP) = 'OFF'
         END DO
C
      ELSE
C
C        Find the tone frequencies that might be of interest,
C        namely the lowest and highest frequency ones in the 
C        bandpass.  Count the tones of interest (0, 1, 2, or 3).
C        Zero will mean just do state counts.
C        Avoid tones at the band edges (0 and CRDB).
C        RTONE1 is the tone frequency in MHz (real).
C        TONE1 etc are the tone frequencies in kHz (integer).
C
C        For all this, use the CRD values as they represent the
C        signals that the detectors will see.
C
         CALL GETCRDN( KSCN, ISTA, CRDN, CRSETC )
C
         DO ICH = 1, CRDN
            NTONES(ICH) = 0
         END DO
C
C        There may be an offset between the output crd channels
C        and the setup channels.  ICH refers to the output to the
C        crd file.  ICHS refers to the setup file.  GETCRDN did
C        the work setting this up.
C
         IF( TONEINT .GT. 0.D0 ) THEN
            DO ICH = 1, CRDN
               ICHS = CRSETC(ICH)
               INTBW = 1000 * CRDB(ICH)
C
C              Get the first tone.
C              Need the net sideband.  CRDS is the BBC sideband
C              before any sideband inversion tricks were done.
C              Get a CNETSIDE which is the net sideband in the
C              BBC.  Presumably NETSIDE has reflected the 
C              inversions so can't be used directly.
C
               IF( CRDS(ICH) .EQ. SIDE1(ICHS,KS) ) THEN
                  CNETSIDE = 'U'
               ELSE
                  CNETSIDE = 'L'
               END IF
C
               IF( CNETSIDE.EQ.'U' .AND. CRDLOSUM(ICH).NE.0.D0 ) THEN
                  TONE1(ICH) = DNINT( 1.D3 * 
     1                   ( TONEINT - MOD( CRDLOSUM(ICH), TONEINT ) ) )
                  IF( TONE1(ICH) .EQ. 0 ) 
     1                   TONE1(ICH) = DNINT( TONEINT * 1000.D0 )
                  IF( TONE1(ICH) .LT. INTBW ) NTONES(ICH) = 1
                  RTONE1 = TONE1(ICH) / 1000.0D0
               ELSE IF( CNETSIDE .EQ. 'L' .AND. 
     1                  CRDLOSUM(ICH) .NE. 0.D0 ) THEN
                  TONE1(ICH) = 
     1                 DNINT( 1.D3 * MOD( CRDLOSUM(ICH), TONEINT ))
                  IF( TONE1(ICH) .EQ. 0 ) 
     1                   TONE1(ICH) = DNINT( TONEINT * 1000.D0 )
                  IF( TONE1(ICH) .LT. INTBW ) NTONES(ICH) = 1
                  RTONE1 = TONE1(ICH) / 1000.0D0
               ELSE
                  CALL WLOG( 1,
     1                 'PCALFQ: Invalid sideband or no LO sum.' )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( 3A, 2I5 )' )
     1                 '        BBC Net Sideband ', CNETSIDE,
     2                 ' in frequency set/channel: ', KS, ICH
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, F14.6 )' )
     1                 '        Legacy system LO sum: ', 
     2                 CRDLOSUM(ICH)
                  CALL WLOG( 1, MSGTXT )

                  CALL ERRLOG( '        when setting VLBA legacy '//
     1                  'system pulse cal detectors.' )
               END IF
C
C              Get the second tone.  The 0.005 prevents the tone from
C              ending up right on the band edge.  
C
               IF( NTONES(ICH) .EQ. 1 .AND. 
     1             CRDB(ICH) - RTONE1 .GT. TONEINT ) THEN
                  NTPB = ( CRDB(ICH) - RTONE1 - 0.005D0 ) / TONEINT
C
C                 Try to prevent getting too close to the upper edge 
C                 of the band where there is a strong filter roll off.
C
                  IF( TONE1(ICH) / 1000.0D0 + NTPB * TONEINT .GT.
     1                  0.85D0 * CRDB(ICH) ) THEN
                     IF( NTPB .LE. 8 .AND. NTPB .GE. 3 ) THEN
                        NTPB = NTPB - 1
                     ELSE IF( NTPB .GT. 8 ) THEN
                        NTPB = NTPB - 2
                     END IF
                  END IF
C
                  TONE2(ICH) = TONE1(ICH) + 
     1               NINT( NTPB * TONEINT * 1000.0D0 )
                  NTONES(ICH) = 2
C
C                 Get the third tone if there is room.
C
                  IF( NTPB .GT. 1 ) THEN
                     NTONES(ICH) = 3
                     TONE3(ICH) = TONE1(ICH) + NINT( TONEINT * 1.0D3 )
                  END IF                  
C
               END IF
            END DO
         END IF
C
C        Loop through the channels assigning the low tones.
C        ID is a detector count.  IP is the detection unit of
C        which there are MAXPC.  ICT is the channel.
C
         ID = 0
C
         DO ICH = 1, CRDN
            IF( NTONES(ICH) .GE. 1 .AND. ID .LT. MAXTON ) THEN
               ID = ID + 1
               IP = ( ID + 1 ) / 2
               ICT = 1 + MOD( ID + 1, 2 )
               WRITE( PCALC1, '(A,I2)' ) 'S', ICH
               IF( ICH .LT. 10 ) PCALC1 = PCALC1(1:1)//PCALC1(3:3)//' '
               IF( ICT .EQ. 1 ) THEN
                  PCX1(IP) = PCALC1
                  PCFR1(IP) = TONE1(ICH)
               ELSE
                  PCX2(IP) = PCALC1
                  PCFR2(IP) = TONE1(ICH)
               END IF
            END IF
         END DO
C
C        Now do the high frequency tones (tone2).
C
         DO ICH = 1, CRDN
            IF( NTONES(ICH) .GE. 2 .AND. ID .LT. MAXTON ) THEN
               ID = ID + 1
               IP = ( ID + 1 ) / 2
               ICT = 1 + MOD( ID + 1, 2 )
               WRITE( PCALC1, '(A,I2)' ) 'S', ICH
               IF( ICH .LT. 10 ) PCALC1 = PCALC1(1:1)//PCALC1(3:3)//' '
               IF( ICT .EQ. 1 ) THEN
                  PCX1(IP) = PCALC1
                  PCFR1(IP) = TONE2(ICH)
               ELSE
                  PCX2(IP) = PCALC1
                  PCFR2(IP) = TONE2(ICH)
               END IF
            END IF
         END DO
C
C        Now do the state counts.  If 2 bit, use a pair of detectors.
C        If this means skipping a detector, assign it to S1, freq 0.
C
         DO ICH = 1, CRDN
            IF( ID .LT. MAXTON ) THEN
               ID = ID + 1
               IP = ( ID + 1 ) / 2
               ICT = MOD( ID + 1, 2 ) + 1
               IF( BITS(ICH,KS) .EQ. 2 .AND. ICT .EQ. 2 ) THEN
                  PCX2(IP) = 'S1'
                  PCFR2(IP) = 0
                  ID = ID + 1
                  IP = IP + 1
                  ICT = 1
               END IF
               IF( ID .LE. MAXTON ) THEN
                  WRITE( PCALC1, '(A,I2)' ) 'S', ICH
                  IF(ICH .LT. 10) PCALC1 = PCALC1(1:1)//PCALC1(3:3)//' '
                  IF( ICT .EQ. 1 ) THEN
                     PCX1(IP) = PCALC1
                     PCFR1(IP) = 0
                  ELSE
                     PCX2(IP) = PCALC1
                     PCFR2(IP) = 0
                  END IF
                  IF( BITS(ICH,KS) .EQ. 2 ) THEN
                     PCX2(IP) = 'M'//PCALC1(2:3)
                     PCFR2(IP) = 0
                     ID = ID + 1
                  END IF
               END IF
            END IF
         END DO
C
C        Finally do the middle frequency tones (tone3).
C
         DO ICH = 1, CRDN
            IF( NTONES(ICH) .GE. 3 .AND. ID .LT. MAXTON ) THEN
               ID = ID + 1
               IP = ( ID + 1 ) / 2
               ICT = 1 + MOD( ID + 1, 2 )
               WRITE( PCALC1, '(A,I2)' ) 'S', ICH
               IF( ICH .LT. 10 ) PCALC1 = PCALC1(1:1)//PCALC1(3:3)//' '
               IF( ICT .EQ. 1 ) THEN
                  PCX1(IP) = PCALC1
                  PCFR1(IP) = TONE3(ICH)
               ELSE
                  PCX2(IP) = PCALC1
                  PCFR2(IP) = TONE3(ICH)
               END IF
            END IF
         END DO
C
C        If not all detectors have been used, turn off the rest.
C
         IF( ID .LT. MAXTON ) THEN
            DO IDD = ID + 1, MAXTON
               IP = ( IDD + 1 ) / 2
               ICT = MOD( IDD + 1, 2 ) + 1
               IF( ICT .EQ. 1 ) THEN
                  PCX1(IP) = 'OFF'
                  PCFR1(IP) = 0
               ELSE
                  PCX2(IP) = 'OFF'
                  PCFR2(IP) = 0
               END IF
            END DO
         END IF
C
      END IF
C
C     Guarantee uppercase.
C
      DO IP = 1, MAXPC
         CALL UPCASE( PCX1(IP) )
         CALL UPCASE( PCX2(IP) )
      END DO
C
      RETURN
      END
