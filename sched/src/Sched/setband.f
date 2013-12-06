      SUBROUTINE SETBAND( KS )
C
C     Routine for SCHED that sets the reference frequencies based on a
C     band specification.
C
C     For now, this routine does not know how to set frequencies
C     for the RDBE based systems.  That should change before long but
C     it will be a bit tricky because of the polyphase filters and 
C     I need to get a release out soon.  Note that this routine has
C     to do exactly the same thing for all setup groups in a setup file,
C     or you will have something that cannot be correlated.  That 
C     means it cannot just detect an RDBE based on the group's station.
C     Also worry about the 15.625 vs 10 kHz tuning steps.
C
C     This will work if most other parameters are also defaulting or
C     using values similar to the defaults.  But I imagine that
C     the user could easily concoct a setup file with things like
C     the IFCHAN specified that would lead to an invalid set of
C     frequencies from this routine.  Hopefully, the checking routines
C     later would stop such a schedule from being used.
C     
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER           KS, ICH, MBD, IB, I, ISETF, ICS, ICX, IBS, IBX
      INTEGER           ISTA, NBWARN
      PARAMETER         (MBD=15)
      DOUBLE PRECISION  CFREQ(MBD), FRQLOW(MCHAN), TEMP, TEMPS, TEMPX
      DOUBLE PRECISION  TOTBAND, CBW(MBD)
      CHARACTER         CBAND(MBD)*5
      LOGICAL           GOTNETF, GOTNETS, GOTBBF, GOTBBS, GOTLO1
      LOGICAL           USEJOD
C
C     Specify the bands.  Put any with specific bandwidths before the
C     default.  IBS and IBX are the index for S and X bands.  Put
C     them at 1 and 2 so I don't risk forgetting to change them if
C     bands are added.
C
      DATA  CBAND(1), CFREQ(1), CBW(1)  /  '13cm', 2295.49D0, 0.D0 /
      DATA  CBAND(2), CFREQ(2), CBW(2)  /  '4cm', 8415.49D0, 0.D0 /
C
      DATA  CBAND(3), CFREQ(3), CBW(3)  /  '90cm', 330.49D0, 0.D0 /
      DATA  CBAND(4), CFREQ(4), CBW(4)  /  '50cm', 610.98D0, 0.D0 /
      DATA  CBAND(5), CFREQ(5), CBW(5)  /  '21cm', 1465.49D0, 128.D0 /
      DATA  CBAND(6), CFREQ(6), CBW(6)  /  '21cm', 1435.49D0, 64.D0 /
      DATA  CBAND(7), CFREQ(7), CBW(7)  /  '21cm', 1416.49D0, 0.D0 /
C
C     The second 18cm band will be used when Jodrell, but not the VLA,
C     is present to avoid RFI.
C
      DATA  CBAND(8), CFREQ(8), CBW(8)  /  '18cm', 1658.49D0, 0.D0 /
      DATA  CBAND(9), CFREQ(9), CBW(9)  /  '18cm', 1653.99D0, 0.D0 /
      DATA  CBAND(10), CFREQ(10), CBW(10)  /  '6cm', 4990.49D0, 0.D0 /
C
C     The original 2cm band here is centered at 15285.49. 
C     The standard setup files and pointing files are centered at 
C     15360.99.  The VLA band is 14650-15325 for 0.9 sensitivity.
C     But the default VU band is 15339.90-15389.90.  Confusing.
C     Will have CHKSET write an informational note to 2cm user.
C
      DATA  CBAND(11), CFREQ(11), CBW(11)  /  '2cm', 15285.49D0, 0.D0 /
C
C     The second 1cm band avoids much of the water line.  I didn't
C     use a wavelength spec because 1cm is already too coarse (and
C     really 30 GHz).
C
      DATA  CBAND(12), CFREQ(12), CBW(12) / '1cm', 22235.49D0, 0.D0 /
      DATA  CBAND(13), CFREQ(13), CBW(13) / '24ghz', 23799.49D0, 0.D0 /
      DATA  CBAND(14), CFREQ(14), CBW(14) / '7mm', 43135.49D0, 0.D0 /
      DATA  CBAND(15), CFREQ(15), CBW(15) / 'sx', 0.D0, 0.D0 /
C
C     Keep the indices for the X and S band frequencies used for SX.
C
      DATA  IBS, IBX  /  1,  2  /
      DATA  NBWARN  / 0 /
C ----------------------------------------------------------------------
C     First see if the frequency is needed based on the first 
C     channel.
C
      ISETF = ISETNUM(KS)
      ICH = 1
      GOTNETF = FREQREF(ICH,KS) .GT. 0.D0
      GOTNETS = NETSIDE(ICH,KS) .EQ. 'U' .OR. NETSIDE(ICH,KS) .EQ. 'L'
      GOTLO1  = FIRSTLO(ICH,KS) .NE. NOTSET
      GOTBBF  = BBSYN(ICH,KS) .NE. 0.0D0
      GOTBBS  = SIDEBD(ICH,KS) .EQ. 'U' .OR. SIDEBD(ICH,KS) .EQ. 'L'
      IF( .NOT. GOTNETF .AND. .NOT. (
     1     GOTBBF .AND. GOTBBS .AND. GOTNETS .AND. GOTLO1 ) ) THEN
C
C        Need to set the frequencies.  Only do if can.  Let SETFREQ
C        worry about it if we can't.
C
         IF( BAND(KS) .NE. ' ' ) THEN
C
C           Abort if this is an RDBE.
C
            IF( DBE(KS) .EQ. 'RDBE_PFB' .OR. 
     1          DBE(KS) .EQ. 'RDBE_DDC' ) THEN
               CALL ERRLOG( 
     1            'SETBAND: SCHED cannot yet set frequencies ' //
     2            'from just a BAND specification for RDBE and '//
     3            'DBBC systems.' )
            END IF
C
C           Now to get down to business.
C
            IF( DUALPOL(KS) ) THEN
               TOTBAND = TOTBW(KS) / 2.0 
            ELSE
               TOTBAND = TOTBW(KS)
            END IF
C
C           Identify the band.  Need the total bandwidth for this.
C
            IB = 0
            DO I = 1, MBD
               IF( BAND(KS) .EQ. CBAND(I) .AND. (
     1             ABS( CBW(I) - TOTBAND ) .LT. 1.0 .OR. 
     2             CBW(I) .EQ. 0.0 ) ) THEN
                  IB = I
                  GO TO 100
               END IF
            END DO
            CALL ERRLOG( 'SETBAND:  Invalid band: '//BAND(KS) )
  100       CONTINUE
C
C           Detect if we need the special Jodrell case.
C
            IF( BAND(KS) .EQ. '18cm' .AND. TOTBAND .EQ. 32.0 ) THEN
C
C              For 32 MHz observations at 18 cm, there is an 
C              incompatibility between the VLA 50 MHz band that
C              should be used (1640-1690) and some bad RFI at
C              Jodrell just below 1670.  For this band, detect the
C              use of either station.  If Jodrell is there, but
C              the VLA is not, change to the special frequency
C              set for 18 cm - which would not otherwise have
C              been selected.  I don't plan to be fussy about 
C              whether the stations are used in this setup.  That
C              would take a search through the scans.
C
               USEJOD = .FALSE.
               DO ISTA = 1, NSTA
                  IF( STCODE(STANUM(ISTA)) .EQ. 'Jb' .OR. 
     1                STCODE(STANUM(ISTA)) .EQ. 'Jv' ) USEJOD = .TRUE.
               END DO
               DO ISTA = 1, NSTA
                  IF( STATION(STANUM(ISTA))(1:3) .EQ. 'VLA' ) 
     1                USEJOD = .FALSE.
               END DO
               IF( USEJOD ) IB = IB + 1
            END IF
C
            IF( BAND(KS) .EQ. 'sx' ) THEN
C
C              This is SX band.  Two center frequencies need to be 
C              used.
C
               IF( DUALPOL(KS) ) THEN
                  IF( NCHAN(KS) .LT. 4 ) 
     1               CALL ERRLOG( 'Too few channels for dual pol, ' //
     2                    'dual band observing - need 4.' )
                  TEMPS = CFREQ(IBS) - TOTBAND / 4.0D0
                  TEMPX = CFREQ(IBX) - TOTBAND / 4.0D0
                  FRQLOW(1) = TEMPS
                  ICX = 1 + NCHAN(KS) / 2
                  FRQLOW(ICX) = TEMPX
                  DO ICS = 2, NCHAN(KS)/2
C
C                    S band.
C
                     IF( POL(ICS,KS) .EQ. POL(1,KS) .OR. 
     1                   POL(ICS,KS) .EQ. POL(ICS-1,KS) ) 
     2                   TEMPS = TEMPS + BBFILT(ICS,KS)
                     FRQLOW(ICS) = TEMPS
C
C                    X band.
C
                     ICX = ICS + NCHAN(KS) / 2
                     IF( POL(ICX,KS) .EQ. POL(1,KS) .OR. 
     1                   POL(ICX,KS) .EQ. POL(ICX-1,KS) ) 
     2                   TEMPX = TEMPX + BBFILT(ICX,KS)
                     FRQLOW(ICX) = TEMPX

                  END DO
               ELSE
                  IF( NCHAN(KS) .LT. 2 ) 
     1               CALL ERRLOG( 'Too few channels for, ' //
     2                    'dual band observing - need 2.' )
                  TEMPS = CFREQ(IBS) - TOTBAND / 4.0D0
                  TEMPX = CFREQ(IBX) - TOTBAND / 4.0D0
                  DO ICS = 1, NCHAN(KS)/2
C
C                    S band
C
                     FRQLOW(ICS) = TEMPS
                     TEMPS = TEMPS + BBFILT(ICS,KS)
C
C                    X band
C
                     ICX = ICS + NCHAN(KS) / 2
                     FRQLOW(ICX) = TEMPX
                     TEMPX = TEMPX + BBFILT(ICX,KS)
                  END DO
               END IF

            ELSE
C
C              "Normal cases"
C
C              Do things slightly differently for the single and 
C              dual polarization cases.
C
               IF( DUALPOL(KS) ) THEN
                  TEMP = CFREQ(IB) - TOTBAND / 2.0D0
                  FRQLOW(1) = TEMP
                  DO ICH = 2, NCHAN(KS)
                     IF( POL(ICH,KS) .EQ. POL(1,KS) .OR. 
     1                   POL(ICH,KS) .EQ. POL(ICH-1,KS) ) 
     2                   TEMP = TEMP + BBFILT(ICH,KS)
                     FRQLOW(ICH) = TEMP
                  END DO
               ELSE
                  TEMP = CFREQ(IB) - TOTBAND / 2.0D0
                  DO ICH = 1, NCHAN(KS)
                     FRQLOW(ICH) = TEMP
                     TEMP = TEMP + BBFILT(ICH,KS)
                  END DO
               END IF
            END IF
C
C           Get the reference frequency for each channel.
C
            DO ICH = 1, NCHAN(KS)
               IF( NETSIDE(ICH,KS) .EQ. 'U' ) THEN
                  FREQREF(ICH,KS) = FRQLOW(ICH)
               ELSE IF( NETSIDE(ICH,KS) .EQ. 'L' ) THEN
                  FREQREF(ICH,KS) = FRQLOW(ICH) + BBFILT(ICH,KS)
               ELSE
                  CALL ERRLOG( 'SETBAND: Must have net sideband to '//
     1                'set channel frequencies from BAND.' )
               END IF
C
C              Round to nearest 10 kHz
C 
               FREQREF(ICH,KS) = IDNINT( FREQREF(ICH,KS) * 100.0D0 ) / 
     1                  100.0D0
C
            END DO
C
         END IF
C
C     Check for case where both BAND and setup info are provided. 
C     Warn user.
C
      ELSE IF( BAND(KS) .NE. ' ' .AND. NBWARN .LT. 1) THEN
         CALL WLOG( 1, 'SETBAND:  Both BAND and adequate information '
     1       //' to set frequencies were provided' )
         CALL WLOG( 1, '          BAND will be ignored. ' //
     1       ' Only one warning will be issued.' )
         CALL WLOG( 1, '          Setup file: '// SETNAME(KS) )
         NBWARN = NBWARN + 1
      END IF
C
      RETURN
      END
