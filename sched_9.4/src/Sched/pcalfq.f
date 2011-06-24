      SUBROUTINE PCALFQ( PCOPT, KF, PCX1, PCX2, PCFR1, PCFR2 )
C
C     Routine to set the pcal tone commands for frequency set KF
C     and pcal state PCOPT.  Called by GETPSET.
C     If PCOPT = "SET", get the setup file values for setup group KS=KF.
C     This option is not yet used.
C
C     PCX1 and PCX1 are the detector/bit to use (eg. S2, M4 etc).
C     PCFR1 and PCFR2 are the tone frequency to use in kHz. 
C
C     This routine takes into account any FREQ and BW set in the scan
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER       KF, KS
      INTEGER       ICH, ID, IP, ICT, IDD
      INTEGER       NTONES(MCHAN)
      INTEGER       TONE1(MCHAN), TONE2(MCHAN), TONE3(MCHAN)
      INTEGER       INTBW, NTPB
      INTEGER       PCFR1(MAXPC), PCFR2(MAXPC)
      DOUBLE PRECISION  RTONE1
      DOUBLE PRECISION  BBCFREQ(MCHAN), BBCBW(MCHAN)
      DOUBLE PRECISION  TONEINT, LOSUM(MCHAN)
      CHARACTER     PCX1(MAXPC)*3, PCX2(MAXPC)*3
      CHARACTER     PCALC1*3, UPCAL*4, PCOPT*4
C ----------------------------------------------------------------------
C     Allow a call that uses the setup file values.
C
      IF( PCOPT .EQ. 'SET' ) THEN
         KS = KF
         UPCAL = SPCAL(KS)
         DO ICH = 1, NCHAN(KS)
            LOSUM(ICH) = FREQREF(ICH,KS)
            BBCBW(ICH) = BBFILT(ICH,KS)
         END DO
      ELSE
C
C        Get information from the frequency set.
C
         KS = FSETKS(KF)
         UPCAL = PCOPT
         CALL FSFREQ( KF, LOSUM, BBCFREQ, BBCBW )
C
      END IF
C
C     Get the tone interval.
C
      CALL UPCASE( UPCAL )
      IF( UPCAL .EQ. '1MHZ' ) THEN
         TONEINT = 1.D0
      ELSE IF( UPCAL .EQ. '5MHZ' ) THEN
         TONEINT = 5.D0
      ELSE IF( UPCAL .EQ. 'OFF' ) THEN
         TONEINT = 0.D0
      ELSE
C
C        Earlier tests should have failed.
C
         CALL ERRLOG( 'PCALFQ: Programming error with PCAL.'//UPCAL ) 
      END IF
C
C     If the first PCALX1 is set, assume that all others are
C     set and just write out what came from the setup file.
C     Otherwise, set the defaults.
C
      IF( PCALX1(1,KS) .EQ. ' ' ) THEN
C
C        Find the tone frequencies that might be of interest,
C        namely the lowest and highest frequency ones in the 
C        bandpass.  Count the tones of interest (0, 1, 2, or 3).
C        Zero will mean just do state counts.
C        Avoid tones at the band edges (0 and BBCBW).
C        RTONE1 is the tone frequency in MHz (real).
C        TONE1 etc are the tone frequencies in kHz (integer).
C
         DO ICH = 1, NCHAN(KS)
            NTONES(ICH) = 0
         END DO
C
         IF( TONEINT .GT. 0.D0 ) THEN
            DO ICH = 1, NCHAN(KS)
               INTBW = 1000 * BBCBW(ICH)
C
C              Get the first tone.
C
               IF( NETSIDE(ICH,KS).EQ.'U'.AND.LOSUM(ICH).NE.0.D0 ) THEN
                  TONE1(ICH) = DNINT( 1.D3 * 
     1                   ( TONEINT - MOD( LOSUM(ICH), TONEINT ) ) )
                  IF( TONE1(ICH) .EQ. 0 ) 
     1                   TONE1(ICH) = DNINT( TONEINT * 1000.D0 )
                  IF( TONE1(ICH) .LT. INTBW ) NTONES(ICH) = 1
                  RTONE1 = TONE1(ICH) / 1000.0D0
               ELSE IF(NETSIDE(ICH,KS).EQ.'L' .AND. LOSUM(ICH).NE.0.D0) 
     1              THEN
                  TONE1(ICH) = DNINT( 1.D3 * MOD( LOSUM(ICH), TONEINT ))
                  IF( TONE1(ICH) .EQ. 0 ) 
     1                   TONE1(ICH) = DNINT( TONEINT * 1000.D0 )
                  IF( TONE1(ICH) .LT. INTBW ) NTONES(ICH) = 1
                  RTONE1 = TONE1(ICH) / 1000.0D0
               ELSE
                  CALL ERRLOG( 'PCALFQ: Invalid sideband or no LO sum' )
               END IF
C
C              Get the second tone.  The 0.005 prevents the tone from
C              ending up right on the band edge.  
C
               IF( NTONES(ICH) .EQ. 1 .AND. 
     1             BBCBW(ICH) - RTONE1 .GT. TONEINT ) THEN
                  NTPB = ( BBCBW(ICH) - RTONE1 - 0.005D0 ) / TONEINT
C
C                 Try to prevent getting too close to the upper edge 
C                 of the band where there is a strong filter roll off.
C
                  IF( TONE1(ICH) / 1000.0D0 + NTPB * TONEINT .GT.
     1                  0.85D0 * BBCBW(ICH) ) THEN
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
         DO ICH = 1, NCHAN(KS)
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
         DO ICH = 1, NCHAN(KS)
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
         DO ICH = 1, NCHAN(KS)
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
         DO ICH = 1, NCHAN(KS)
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
      ELSE
C
C        Use the values from the setup file.
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
      END IF
C
C     Guarantee uppercase so GETPSET does not have problems.
C
      DO IP = 1, MAXPC
         CALL UPCASE( PCX1(IP) )
         CALL UPCASE( PCX2(IP) )
      END DO
C
      RETURN
      END


