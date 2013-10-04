      SUBROUTINE DOPFQ
C
C     Routine for SCHED that does Doppler calculations and sets 
C     frequencies for spectral line sources.  The calculated 
C     frequencies replace any values previously placed in the FREQ 
C     array, such as from FREQ specified for a scan.
C
C     It calls the closely related (almost copied) routine DOPCRD to
C     set frequencies for the BBCs when reference pointing under
C     circumstances when the main channels cannot be adjusted (PFB).
C
C     FREQ is the LO sum (band edge) given the sideband of the 
C     logical channel.  It may need to be adjusted if the station
C     is going to put out a sideband inverted from the logical
C     channel, as is sometimes required for the RDBE/PFB personality.
C     This routine only deals with logical channel values.
C
C     The velocities are obtained from the source catalog.
C
C     The rest frequencies were read in the LINEINIT records in the main
C     schedule input.
C
C     Use SLA lib routines to do the calculations.  For now, do the 
C     calculations for the center of the Earth for the middle of the
C     experiment.  TFIRST and TEND were calculated by SCHTIM, which
C     must be called earlier.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER          ISCN, YEAR, DAY, IGP, NGP, ICH
      INTEGER          ISETF, SBW, JCH, KS
      REAL             RA4, DEC4, SLA_RVLSRK, VELC, TIME4
      REAL             VSUN, VEARTH, TL
      DOUBLE PRECISION MSBW(MAXCHN,MAXSET)
      LOGICAL          WARN(MAXCHN), FWARN
      DOUBLE PRECISION TMID, TIME, VELTOT, VELSRC, CFREQ1, CFREQ2
      SAVE             WARN
C
C     Velocity of light m/s
C
      PARAMETER (VELC=2.99792458E8)
C
      DATA  WARN  / MAXCHN*.TRUE. /
      DATA  FWARN / .TRUE. /
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'DOPFQ: Starting. ' )
C
C     Later the minimum bandwidth in use for each setup file will be
C     needed.  Get it by searching the setup files.  The result should
C     be for each logical channel.
C
      DO ISETF = 1, NSETF
         DO ICH = 1, MAXCHN
            MSBW(ICH,ISETF) = 9999.0D0
         END DO
      END DO
      DO KS = 1, NSET
         ISETF = ISETNUM(KS)
         DO ICH = 1, NCHAN(KS)
            JCH = SGCHAN(ICH,KS)
            IF( JCH .NE. 0 ) THEN
               MSBW(ICH,ISETF) = MIN( MSBW(ICH,ISETF), BBFILT(JCH,KS) )
            END IF
         END DO
      END DO
C
C     Loop through scans looking for ones that need doppler calc.
C
      DO ISCN = SCAN1, SCANL
C
         ISETF = SETNUM(ISCN)
         IF( DOPCAL(ISCN) .AND. OKXC(ISETF) .AND. .NOT. VLAONLY ) THEN
C
C           Get the velocity of the Earth in the direction of the 
C           source.  It is the sum of the LSR velocity of the Sun 
C           and the solar system velocity of the Earth.  Get both 
C           from SLA_LIB routines.
C
C           Note that the midpoint of the experiment will be used for
C           the calculation.  Thus, despite redoing the calculation for
C           each scan, each source will use the same velocity for every
C           scan.
C
            TMID = (TFIRST + TEND) / 2.0
            CALL TIMEJ( TMID, YEAR, DAY, TIME )
            TIME4 = TIME / TWOPI
            RA4  = RA2000(IDOPSRC(ISCN))
            DEC4 = D2000(IDOPSRC(ISCN))
            VSUN = SLA_RVLSRK( RA4, DEC4 )
            RA4  = RAP(IDOPSRC(ISCN))
            DEC4 = DECP(IDOPSRC(ISCN))
            CALL SLA_ECOR( RA4, DEC4, YEAR, DAY, TIME4, VEARTH, TL )
C
C           Determine which group of lines is to be used.
C
            NGP = 0
            DO IGP = 1, NLGP
               IF( LINES(ISCN) .EQ. LINENAME(IGP) ) THEN
                  NGP = IGP
               END IF
            END DO
            IF( NGP .EQ. 0 )
     1          CALL ERRLOG( 'DOPFQ: Spectral line set '//LINES(ISCN)//
     2            ' not initialized.' )
C
C
C           This routine calculates the center frequency, then subtracts
C           half the bandwidth to get the LO sum.  The bandwidth 
C           subtracted is either that specified with the BW input
C           to the main routine, or the smallest seen in the setup file.
C
C           Do the Doppler for all possible channels.  In the inputs,
C           any channel for which there was no rest frequency, 
C           bandwidth, or velocity given, had that the missing 
C           number set to the value for channel 1.  
C
C           Assume that the velocity information applies to the
C           setup file "logical channel" (see SFINFO) which may be
C           different from the setup group channel.
C
C           Set FREQ to band edge.  The net sideband for the channel
C           was found by SBPAIR and is stored in SFSIDE, based on the
C           setup file.  Round the frequency to the nearest multiple
C           of DOPINCR (0.01 MHz for the VLBA legacy system).
C           If two channels share the same BBC, SAMEBBC will be set.  
C           In that case, use the frequency calculated for the first 
C           of those channels.
C
            IF( MSCHN(ISETF) .GT. MAXLCH ) THEN
               WRITE( MSGTXT, '( A, I3, A )' ) 
     1           'DOPFQ: With DOPCAL, NCHAN must be ', MAXLCH, 
     2             ' or less. ' 
               CALL ERRLOG( MSGTXT )
            END IF
C
C           Get the bandwidth to use.  Be sure it is unsigned.
C           BW is a scan dependent user input.
C
            DO ICH = 1, MSCHN(ISETF)
               IF( BW(ICH,ISCN) .EQ. 0.0 ) THEN
                  BW(ICH,ISCN) = MSBW(ICH,ISETF)
               END IF
               BW(ICH,ISCN) = ABS( BW(ICH,ISCN) )
C
C              Complain if no bandwidth is available.
C
               IF( BW(ICH,ISCN) .EQ. 0.0 ) THEN
                  CALL ERRLOG( 'DOPFQ: Bandwidths must be specified'//
     2              ' or in setup for Doppler calculations.' )
               END IF
            END DO
C
            DO ICH = 1, MSCHN(ISETF)
C
C              Check for same BBC.
C
               IF( SAMEBBC(ICH,ISETF) .NE. 0 ) THEN
                  FREQ(ICH,ISCN) = FREQ(SAMEBBC(ICH,ISETF),ISCN)
                  IF( WARN(ICH) ) THEN
                     WRITE( MSGTXT, '( A, I2, A, I2, A )' )
     1                  'DOPFQ Note: Setting freq of chan ', ICH, 
     2                  ' = freq of channel ',
     3                  SAMEBBC(ICH,ISETF), ' (shared BBC).'
                     CALL WLOG( 0, MSGTXT )
                     WARN(ICH) = .FALSE.
                  END IF
               ELSE
C
C                 Set up to add or subtract the bandwidth.  As a 
C                 software check, complain about unrecognized SFSIDE.
C
                  IF( SFSIDE(ICH,ISETF) .EQ. 'U' ) THEN
                     SBW = 1
                  ELSE IF( SFSIDE(ICH,ISETF) .EQ. 'L' ) THEN
                     SBW = -1
                  ELSE
                     CALL ERRLOG( 'DOPFQ: Unrecognized SFSIDE '//
     1                   SFSIDE(ICH,ISETF) )
                  END IF
C
C                 Now get the frequency.  If no velocity was given,
C                 use the setup frequency.  This was flagged with
C                 a "CONT" from RDSRC and a VLSR=-1.E9 from SRREAD.
C
                  IF( VLSR(ICH,IDOPSRC(ISCN)) .LT. -1.E8 ) THEN
                     FREQ(ICH,ISCN) = 0.D0
                  ELSE
C
C                    Actually calculate the frequency based on the
C                    VELREF and VELDEF requests.  Note that VLSR
C                    is now a bit of a misnomer.
C
C                    First convert a Z to a velocity. 
C
                     IF( VELDEF(IDOPSRC(ISCN)) .EQ. 'Z' ) THEN
                        VELSRC = VLSR(ICH,IDOPSRC(ISCN)) * VELC / 1.D3
                     ELSE
                        VELSRC = VLSR(ICH,IDOPSRC(ISCN))
                     END IF
C
C                    Now get the total velocity by adding in the 
C                    Earth's motions.  This is where the frame enters.
C
                     IF( VELREF(IDOPSRC(ISCN)) .EQ. 'L' ) THEN
                        VELTOT = VELSRC + VEARTH + VSUN
                     ELSE IF( VELREF(IDOPSRC(ISCN)) .EQ. 'H' ) THEN
                        VELTOT = VELSRC + VEARTH
                     ELSE IF( VELREF(IDOPSRC(ISCN)) .EQ. 'G' ) THEN
                        VELTOT = VELSRC
                     ELSE
                        CALL ERRLOG( 
     1                    'DOPFQ:  Bad VREF in source catalog: '//
     2                    VELREF(IDOPSRC(ISCN)) )
                     END IF
C
C                    Now calculate the frequency.
C
                     IF( VELDEF(IDOPSRC(ISCN)) .EQ. 'R' ) THEN
                        FREQ(ICH,ISCN) = RESTFREQ(ICH,NGP) * 
     1                     ( 1.D0 - VELTOT * 1.E3 / VELC ) - 
     2                     SBW * BW(ICH,ISCN) / 2.D0
                     ELSE IF( VELDEF(IDOPSRC(ISCN)) .EQ. 'O' .OR.
     1                        VELDEF(IDOPSRC(ISCN)) .EQ. 'Z' ) THEN
                        FREQ(ICH,ISCN) = RESTFREQ(ICH,NGP) / 
     1                     ( 1.D0 + VELTOT * 1.E3 / VELC ) - 
     2                     SBW * BW(ICH,ISCN) / 2.D0
                     ELSE
                        CALL ERRLOG( 
     1                    'DOPFQ:  Bad VDEF in source catalog: '//
     2                    VELDEF(IDOPSRC(ISCN)) )
                     END IF
C
C                    Round it to the required DOPINCR.  Note that 
C                    the LO equation is assumed to be 
C                    FREQ = N * DOPINCR(ISCN,1) + DOPINCR(ISCN,2)
C                    The ATCA needs the offset.
C                    Note that DOPINCR is in kHz while FREQ is in MHz.
C            
                     FREQ(ICH,ISCN) = 1.0D-3 * ( DOPINCR(ISCN,2) +
     1                   DOPINCR(ISCN,1) * DNINT( 
     2                   ( FREQ(ICH,ISCN) * 1.0D3 - DOPINCR(ISCN,2) )
     3                      / DOPINCR(ISCN,1) ) )
C            
C                    Check for excessive overlap with previous channels.
C            
                     IF( ICH .GE. 2 .AND. FWARN ) THEN
C            
C                       Get the center frequency of the current channel.
C            
                        IF( SFSIDE(ICH,ISETF) .EQ. 'U' ) THEN
                           CFREQ2 = FREQ(ICH,ISCN) + 0.5 * BW(ICH,ISCN)
                        ELSE
                           CFREQ2 = FREQ(ICH,ISCN) - 0.5 * BW(ICH,ISCN)
                        END IF
C            
C                       Compare with earlier channels of the same pol.
C            
                        DO JCH = 1, ICH - 1
                           IF( SFPOL(ICH,ISETF) .EQ. SFPOL(JCH,ISETF) ) 
     1                        THEN
C            
C                             Get center frequency of earlier channel.
C            
                              IF( SFSIDE(JCH,ISETF) .EQ. 'U' ) THEN
                                 CFREQ1 = FREQ(JCH,ISCN) + 
     1                                    0.5 * BW(JCH,ISCN)
                              ELSE
                                 CFREQ1 = FREQ(JCH,ISCN) - 
     2                                    0.5 * BW(JCH,ISCN)
                              END IF
C            
C                             Have a cow if the two are not separated
C                             by 0.8 of bandwidth.
C            
                              IF( ABS( CFREQ2 - CFREQ1 ) .LT. 0.8 * 0.5
     1                         * ( BW(JCH,ISCN) + BW(ICH,ISCN) ) ) THEN
                                 MSGTXT = ' ' 
                                 WRITE( MSGTXT,'( 3A )')
     1                            'DOPFQ: *** Frequencies of channels ',
     2                            'set by DOPPLER overlap more than ',
     3                            '20%.'
                                 CALL WLOG( 0, MSGTXT )
                                 CALL WLOG( 0, 
     1                              '           Was that intentional?' )
                                 FWARN = .FALSE.
                              END IF
                           END IF
                        END DO
                     END IF
C
C                    Leave a flag that we did doppler on this source and
C                    keep track of the maximum number of channels used.
C
                     DIDNDOP(IDOPSRC(ISCN)) = 
     1                    MAX( DIDNDOP(IDOPSRC(ISCN)), ICH )
                     DOPPED(SRCNUM(ISCN)) = .TRUE.
C
                  END IF
C
               END IF
            END DO
C
         ELSE IF( DOPCAL(ISCN) .AND. VLAONLY ) THEN
C
C           Warn that cannot do doppler adjustments on just VLA.
C           For VLAONLY schedules, SFINFO will set OKXC.
C           May be able to remove this restriction if VLA only schedules
C           become no different than any other single station obs.
C
            CALL ERRLOG( 'DOPFQ: Sched can not do Doppler ' //
     1             'adjustments for VLA only schedules.' )
         END IF
C
C        Balk at dopcal when different stations have incompatible
C        channelization (OKXC false).  "see above" refers to message
C        from SFINFO.  One way to get this is to have no setup.
C
         IF( DOPCAL(ISCN) .AND. ( .NOT. OKXC(ISETF) .OR. NOSET ) ) THEN
            IF( NOSET ) THEN
               CALL ERRLOG( 'DOPFQ: Doppler calculations requested '//
     1           'with no setup.  I cannot do that!' )
            ELSE
               CALL ERRLOG( 'DOPFQ: Doppler calculations requested '//
     1           'but have been disabled.  See above.' )
            END IF
         END IF
C
C        While at it, check that FREQ and BW were not specified when
C        the channelization is unclear.  Again the "see above" refers
C        to a message from SFINFO.
C
         IF( .NOT. DOPCAL(ISCN) .AND. .NOT. OKXC(ISETF) .AND.
     1     ( FREQ(1,ISCN) .NE. 0.D0 .OR. BW(1,ISCN) .NE. 0.0 ) ) THEN
C
            CALL WLOG( 1, 'DOPFQ: FREQ or BW requested but ' //
     1          'disabled.  See above.' )
            CALL ERRLOG( '        Fix channelization or don''t use '//
     1                  'FREQ or BW.' )
         END IF
      END DO
C
C     Call DOPCRD to set the legacy VLBA system frequencies for 
C     reference pointing when the main channels cannot be finely 
C     tuned.  Mainly for reference pointing while using the RDBE
C
      CALL DOPCRD
C
      RETURN
      END
