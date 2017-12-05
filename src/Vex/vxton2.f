      SUBROUTINE VXTON2( IPS, ISET, NLOCHN, LO, SIDE, BBCWID, TONINT, 
     1    NTONE, TNCHN, NTNCHN, ITNCHN )
C
C     Subroutine returns the number of tones in each channel
C     and the sequence they are. Tone zero is level statistics.
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 020896
C     revised to deal with PSETI and FSETI 180699, Huib
C     Removing pcal sets.  Sep. 2013  RCW.
C         Eventually get the desired tone numbers from new variables
C         in the setup file or frequency set.  But that's not done yet.
C
C     Inputs are
C         IPS: override ISET values with these if IPS > 0
C         NLOCHN: Number of frequency channels
C         LO:     Sum LO, double precision!
C         SIDE:   Net side band
C         BBCWID:  Filter width
C         TONINT: Interval for tones
C     Output
C         NTONE:   Number of idependent tone setting
C         TNCHN: list of NCHAN values, linking a channel to a tone def
C         NTNCHN(ICHAN): Number of tones detected per channel
C         ITNCHN(MAXTON,NLOCHN): indicating the tones to detect
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
C
C     Parameterize the number of detectors.  MAXPC, set in schset.inc,
C     is the maximum number of detectors, each of which has two 
C     channels.
C     MAXTON=16 is the maximum number of tones
C     MAXPC is 8 now, soon to be 16.
C
      INTEGER NLOCHN, NTNCHN(NLOCHN), ITNCHN(MAXTON,NLOCHN)
Cf2py intent(out) NTNCHN
Cf2py intent(out) ITNCHN
      INTEGER NTONE, TNCHN(NLOCHN)
Cf2py intent(out) NTONE
Cf2py intent(out) TNCHN
      DOUBLE PRECISION LO(NLOCHN), DTONINT
      REAL TONINT, BBCWID(NLOCHN)
      CHARACTER SIDE(NLOCHN)*1
C
      INTEGER    ID, IP, ICT, IDD, ICH, IDETECT, IPS, JCH, ISET
      INTEGER    TONE1(MCHAN), TONE2(MCHAN), TONE3(MCHAN)
      INTEGER    INTBW, NTPB
      INTEGER    PCF1(MAXPC), PCF2(MAXPC)
      INTEGER    NTNTMP(MAXCHN), ITNTMP(MAXTON,MAXCHN)
      REAL       RTONE1
      CHARACTER  PCX1(MAXPC)*3, PCX2(MAXPC)*3
      CHARACTER  PCALC1*3, TWOCHR*2, PXXS*3
      LOGICAL    UNIQUE, SAME
C ----------------------------------------------------------------------
C     Avoid variable type problems later.
C
      DTONINT = DBLE( TONINT )
C
C     Find the tone frequencies that might be of interest,
C     namely the lowest and highest frequency ones in the 
C     bandpass.  Count the tones of interest (0, 1, 2, or 3).
C     Zero will mean just do state counts.
C     Avoid tones at the band edges (0 and BBFILT).
C     RTONE1 is the tone frequency in MHz (real).
C     TONE1 etc are the tone frequencies in kHz (integer).
C
      DO ICH = 1, NLOCHN
         NTNTMP(ICH) = 0
      END DO
C
C     This routine can be used to calculate tones in IPS or ISET
C       RCW:  This routine is always called with IPS=-1.  PSX1 is
C             being dropped, so comment out the reference.
C     
C
C      IF( IPS.GT. 0 ) THEN
C         PXXS = PSX1(1,IPS) 
C      ELSE
         PXXS = PCALX1(1,ISET)
C      END IF
C
C     When not explicitly set, calculate
C
      IF( PXXS .EQ. ' ' ) THEN
         IF( DTONINT .GT. 0.D0 ) THEN
            DO ICH = 1, NLOCHN
               INTBW = 1000 * BBCWID(ICH)
C     
C           Get the first tone.
C
               IF( SIDE(ICH).EQ.'U'.AND.LO(ICH).NE.0.D0 ) THEN
                  TONE1(ICH) = DNINT( 1.D3 * 
     1                ( DTONINT - MOD( LO(ICH), DTONINT ) ) )
                  IF( TONE1(ICH) .EQ. 0 ) 
     1                TONE1(ICH) = DNINT( DTONINT * 1000.D0 )
                  IF( TONE1(ICH) .LT. INTBW ) NTNTMP(ICH) = 1
                  RTONE1 = TONE1(ICH) / 1000.0
               ELSE IF(SIDE(ICH).EQ.'L' .AND. LO(ICH).NE.0.D0) 
     1                THEN
                  TONE1(ICH) = DNINT( 1.D3 * MOD( LO(ICH), DTONINT ))
                  IF( TONE1(ICH) .EQ. 0 ) 
     1                TONE1(ICH) = DNINT( DTONINT * 1000.D0 )
                  IF( TONE1(ICH) .LT. INTBW ) NTNTMP(ICH) = 1
                  RTONE1 = TONE1(ICH) / 1000.0
               END IF
C     
C              Get the second tone.  The 0.005 prevents the tone from
C              ending up right on the band edge.
C
               IF( NTNTMP(ICH) .EQ. 1 .AND. 
     1             BBCWID(ICH) - RTONE1 .GT. TONINT ) THEN
                  NTPB = ( BBCWID(ICH) - RTONE1 - 0.005 ) / TONINT
                  TONE2(ICH) = TONE1(ICH) + 
     1                NINT( NTPB * TONINT * 1000.0 )
                  NTNTMP(ICH) = 2
C     
C                 Get the third tone if there is room.
C
                  IF( NTPB .GT. 1 ) THEN
                     NTNTMP(ICH) = 3
                     TONE3(ICH) = TONE1(ICH) + NINT( TONINT * 1000.0 )
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
         DO ICH = 1, NLOCHN
            IF( NTNTMP(ICH) .GE. 1 .AND. ID .LT. MAXTON ) THEN
               ID = ID + 1
               IP = ( ID + 1 ) / 2
               ICT = 1 + MOD( ID + 1, 2 )
               WRITE( PCALC1, '(A,I2)' ) 'S', ICH
               IF( ICH .LT. 10 ) PCALC1 = PCALC1(1:1)//PCALC1(3:3)//' '
               IF( ICT .EQ. 1 ) THEN
                  PCX1(IP) = PCALC1
                  PCF1(IP) = TONE1(ICH)
               ELSE
                  PCX2(IP) = PCALC1
                  PCF2(IP) = TONE1(ICH)
               END IF
            END IF
         END DO
C
C        Now do the high frequency tones (tone2).
C
         DO ICH = 1, NLOCHN
            IF( NTNTMP(ICH) .GE. 2 .AND. ID .LT. MAXTON ) THEN
               ID = ID + 1
               IP = ( ID + 1 ) / 2
               ICT = 1 + MOD( ID + 1, 2 )
               WRITE( PCALC1, '(A,I2)' ) 'S', ICH
               IF( ICH .LT. 10 ) PCALC1 = PCALC1(1:1)//PCALC1(3:3)//' '
               IF( ICT .EQ. 1 ) THEN
                  PCX1(IP) = PCALC1
                  PCF1(IP) = TONE2(ICH)
               ELSE
                  PCX2(IP) = PCALC1
                  PCF2(IP) = TONE2(ICH)
               END IF
            END IF
         END DO
C
C        Now do the state counts.  If 2 bit, use a pair of detectors.
C        If this means skipping a detector, assign it to S1, freq 0.
C
         DO ICH = 1, NLOCHN
            IF( ID .LT. MAXTON ) THEN
               ID = ID + 1
               IP = ( ID + 1 ) / 2
               ICT = MOD( ID + 1, 2 ) + 1
               IF( BITS(ICH,ISET) .EQ. 2 .AND. ICT .EQ. 2 ) THEN
                  PCX2(IP) = 'S1'
                  PCF2(IP) = 0
                  ID = ID + 1
                  IP = IP + 1
                  ICT = 1
               END IF
               IF( ID .LE. MAXTON ) THEN
                  WRITE( PCALC1, '(A,I2)' ) 'S', ICH
                  IF(ICH .LT. 10) PCALC1 = PCALC1(1:1)//PCALC1(3:3)//' '
                  IF( ICT .EQ. 1 ) THEN
                     PCX1(IP) = PCALC1
                     PCF1(IP) = 0
                  ELSE
                     PCX2(IP) = PCALC1
                     PCF2(IP) = 0
                  END IF
                  IF( BITS(ICH,ISET) .EQ. 2 ) THEN
                     PCX2(IP) = 'M'//PCALC1(2:3)
                     PCF2(IP) = 0
                     ID = ID + 1
                  END IF
               END IF
            END IF
         END DO
C
C        Finally do the middle frequency tones (tone3).
C
         DO ICH = 1, NLOCHN
            IF( NTNTMP(ICH) .GE. 3 .AND. ID .LT. MAXTON ) THEN
               ID = ID + 1
               IP = ( ID + 1 ) / 2
               ICT = 1 + MOD( ID + 1, 2 )
               WRITE( PCALC1, '(A,I2)' ) 'S', ICH
               IF( ICH .LT. 10 ) PCALC1 = PCALC1(1:1)//PCALC1(3:3)//' '
               IF( ICT .EQ. 1 ) THEN
                  PCX1(IP) = PCALC1
                  PCF1(IP) = TONE3(ICH)
               ELSE
                  PCX2(IP) = PCALC1
                  PCF2(IP) = TONE3(ICH)
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
                  PCX1(IP) = 'off'
                  PCF1(IP) = 0
               ELSE
                  PCX2(IP) = 'off'
                  PCF2(IP) = 0
               END IF
            END DO
         END IF
C
      ELSE 
C
C        Use the values from the PCal set or setup file
C        RCW:  Again, IPS always=-1 and PS variable are being
C        dropped, so comment them out.
C
C         IF( IPS .GT. 0 ) THEN
C            DO IP = 1, MAXPC
C               PCX1(IP) = PSX1(IP,IPS)
C               PCX2(IP) = PSX2(IP,IPS)
C               PCF1(IP) = PSFR1(IP,IPS)
C               PCF2(IP) = PSFR2(IP,IPS)
C               IF( PCX1(IP) .EQ. ' ' ) PCX1(IP) = 'off'
C               IF( PCX2(IP) .EQ. ' ' ) PCX2(IP) = 'off'
C            END DO
C         ELSE
            DO IP = 1, MAXPC
               PCX1(IP) = PCALX1(IP,ISET)
               PCX2(IP) = PCALX2(IP,ISET)
               PCF1(IP) = PCALFR1(IP,ISET)
               PCF2(IP) = PCALFR2(IP,ISET)
               IF( PCX1(IP) .EQ. ' ' ) PCX1(IP) = 'off'
               IF( PCX2(IP) .EQ. ' ' ) PCX2(IP) = 'off'
            END DO
C         END IF
C
      END IF
C
C     Temporarily, we need upper case for PCX1 and PCX1
C
      DO IP = 1, MAXPC
         CALL UPCASE( PCX1(IP) )
         CALL UPCASE( PCX2(IP) )
      END DO
C
C     Now write out the pcal commands (assuming setup information
C     of any sort was available.)
C
      DO ICH = 1, MAXCHN
         NTNTMP(ICH) = 0
      END DO
C
      DO IP = 1, MAXPC
         IF( PCX1(IP) .NE. 'OFF' ) THEN
            READ(PCX1(IP), '( 1X, A )' ) TWOCHR
            READ( TWOCHR, * ) ICH
            NTNTMP(ICH) = NTNTMP(ICH) + 1
            IF( PCF1(IP) .LT. 1E-5 ) THEN 
               IDETECT = 0
            ELSE
               IDETECT = INT(PCF1(IP)/(1E3*TONINT)) + 1
            END IF
            ITNTMP(NTNTMP(ICH),ICH) = IDETECT
         END IF
C
C        Second detector, ignore M-bit settings, covered by #1
C
         IF( PCX2(IP) .NE. 'OFF' .AND. PCX2(IP)(1:1) .NE. 'M' ) THEN
            READ(PCX2(IP), '( 1X, A )' ) TWOCHR
            READ(TWOCHR,*) ICH
            NTNTMP(ICH) = NTNTMP(ICH) + 1
            IF( PCF2(IP) .LT. 1E-5 ) THEN 
               IDETECT = 0
            ELSE
               IDETECT = INT(PCF2(IP)/(1E3*TONINT)) + 1
            END IF
            ITNTMP(NTNTMP(ICH),ICH) = IDETECT
         END IF
      END DO
C
C     Count and store the unique ones.
C
      NTONE = 0
      DO ICH = 1, NLOCHN
         IF( ICH .EQ. 1 ) THEN 
            UNIQUE = .TRUE.
         ELSE
            UNIQUE = .TRUE.
            DO JCH = 1, ICH-1
               IF( NTNTMP(ICH) .EQ. NTNTMP(JCH) ) THEN
                  SAME = .TRUE.
                  DO IP = 1, NTNTMP(ICH)
                     IF( ITNTMP(IP,ICH) .NE. ITNTMP(IP,JCH) ) 
     1                   SAME = .FALSE.
                  END DO
                  IF ( SAME ) THEN 
                     UNIQUE = .FALSE.
                     TNCHN(ICH) = TNCHN(JCH)
                  END IF
               END IF
            END DO
         END IF
         IF( UNIQUE )  THEN
            NTONE = NTONE + 1
            TNCHN(ICH) = NTONE
            NTNCHN(NTONE) = NTNTMP(ICH)
            DO IP = 1, NTNCHN(NTONE)
               ITNCHN(IP,NTONE) = ITNTMP(IP,ICH)
            END DO
         END IF
      END DO
C
      RETURN
      END
