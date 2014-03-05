      SUBROUTINE DOPCRD
C
C     This is a varient on DOPFQ specifically for setting the frequencies
C     for the the legacy system, mostly on the VLBA, when doing
C     observations with the RDBE - especially with the PFB where fine
C     frequency tuning is not allowed.  It is mainly for use to arrange
C     for reference pointing on maser lines.
C
C     Called by DOPFQ.
C
C     The number of channels is specified by CRDNCH which is required
C     when CRDFREQ or CRDDOP (but not CRDBW) is specified.  Some channel
C     information is obtained from the setup file channels and one may
C     not wish to use just the first CRDNCH of those (think dual band
C     cases), especially with the PFB.  CRDCH1 allows the user to 
C     specify the first channel of the setup file to use for the crd
C     file channel 1.  The rest will be sequential from there when 
C     using CRDNCH.  CRDSETCH allows an arbitrary list of channels
C     to be specified.
C
C     CRDBW will be required user input if either CRDFREQ or CRDDOP
C     has been specified.  Specification of CRDFREQ and CRDDOP is
C     considered invalid as they stomp on each other.
C
C     If CRDDOP is specified, this routine sets CRDFREQ based on the
C     source velocity information, experiment timing, and CRDBW.  If 
C     multiple velocities are given for the source, they will be 
C     assumed to apply to the CRD channels, not necessarily the 
C     setup channels.  
C
C     CRDFREQ will only be used later in making the crd files 
C     when it has been set, either as an explicit input or here as a 
C     result of doppler calculations.  Otherwise the BBCs will be set to
C     values based on the RDBE channel settings.
C
C     The main purpose of this routine is to get away from needing a 
C     whole second schedule for the legacy system, which includes the 
C     control computer that deals with antenna control, when doing 
C     reference pointing.
C
C     The user inputs are:
C       CRDNCH(ISCN)      - Number of channels to specify to the BBCs.
C       CRDCH1(ISCN)      - Start setup file channel to get channel
C                           parameters such as IF, Pol, FIRSTLO etc.
C       CRDFREQ(ICH,ISCN) - LO sum for channel ICH in scan ISCN in 
C                           the legacy system.  Comparable to FREQ 
C                           for the main schedule.
C       CRDBW(ICH,ISCN)   - Bandwidth for the BBC channel/scan.
C       CRDDOP/CRDNODOP   - Comparable to DOPPLER/NODOP but for the BBCs.
C
C     In addition, CRDSIDE(ICH,KS) is saved for each setup group to 
C     preserve the original sideband specification prior to any changes
C     related to correlator sideband inversion (CORINV etc).
C
C     The rest of the setup will be the same as for the main channels,
C     including the IFCHAN, IF and RF sidebands, and all the external 
C     LO settings.  Also the spectral line to use is the same as for 
C     the main schedule.  The number of channels will be limited to 
C     8 compared to the usual PFB number of 16.  For just reference 
C     pointing, this could be just one, but I won't make it that 
C     restrictive.  Note that using the main channel sideband is likely 
C     to cause the use of lower sideband, but that should be ok.
C
C     PCALX frequency choices will be made in WRTFREQ.
C
C     The velocities are obtained from the source catalog.  Caution - the
C     velocities are by channel.  It is not particularly wise to try to
C     do Doppler setting for both the main channels and the pointing BBC
C     (CRD) channels.
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
      INTEGER          ISETF, SBW
      INTEGER          ICHS
      REAL             RA4, DEC4, SLA_RVLSRK, VELC, TIME4
      REAL             VSUN, VEARTH, TL
      LOGICAL          DEQUAL
      DOUBLE PRECISION TMID, TIME, VELTOT, VELSRC
      DOUBLE PRECISION DFREQ(MAXCHN), DBW(MAXCHN)
C
C     Velocity of light m/s
C
      PARAMETER (VELC=2.99792458E8)
C
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'DOPBBC: Starting. ' )
C
C     Loop through scans looking for ones that need doppler calc (CRDDOP).
C     Don't worry about OKXC etc for this one.
C
C     Note that the CRD parameters, except CRDNCH, were initialized as
C     part of the user input reading process for CRDFREQ and CRDBW and
C     as part of establishing the frequency sets for CRDSIDE.  CRDNCH
C     still needs to be set.  It will be used later as a flag concerning
C     whether to use the CRD parameters, or use the old scheme for
C     deriving the BBC frequencies from the main baseband data.
C
      DO ISCN = SCAN1, SCANL
         ISETF = SETNUM(ISCN)
C
C        Initialize the CRD variables for this scan.
C        Note that CRDSIDE is per setup group, not setup file.  It was
C        set in SETFCAT to equal SIDEBD, before sideband inversions but
C        after the normal frequency settings are done.
C
C        First check that inappropriate requests have not been made.
C
         IF( CRDDOP(ISCN) ) THEN
C
C           Don't attempt to set frequencies when there is no setup.
C
            IF( NOSET ) THEN
               CALL ERRLOG( 'DOPCRD: Doppler calculations requested '//
     1           'with no setup.  I cannot do that!' )
            END IF
C
C           Balk at CRDDOP when different stations have incompatible
C           channelization (OKXC false).  "see above" refers to message
C           from SFINFO.  One way to get this is to have no setup.
C
            IF( .NOT. OKXC(ISETF) ) THEN
               CALL ERRLOG( 'DOPCRD: Doppler calculations requested '//
     1           'but have been disabled.  See SFINFO message above.' )
            END IF
C
C           Don't allow both frequency setting and Doppler request.  
C           Assume a test against channel 1 is adequate.
C
            IF( CRDFREQ(1,ISCN) .NE. 0.D0 ) THEN
               CALL WLOG( 1, ' DOPCRD.  Both Doppler calculation ' //
     1             'and a specific frequency requested ' )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I5, A )' )
     1            '          for legacy system reference pointing in ',
     2            'scan ', ISCN
               CALL WLOG( 1, MSGTXT )
               CALL ERRLOG(  
     1             '          Please choose only one option.' )
            END IF
C
         END IF
C
C        Check that CRDFREQ and CRDBW were not specified when 
C        the channelization is unclear.  Again the "see above" refers
C        to a message from SFINFO.  Really this only catches CRDBW
C        because CRDDOP will be true if CRDFREQ not zero.
C
         IF( .NOT. CRDDOP(ISCN) .AND. .NOT. OKXC(ISETF) .AND.
     1     ( CRDFREQ(1,ISCN) .NE. 0.D0 .OR. 
     2     CRDBW(1,ISCN) .NE. 0.0 ) ) THEN
C
            CALL WLOG( 1, 'PBBCDOP: CRDFREQ or CRDBW requested but ' 
     1          // 'disabled.  See SFINFO message above.' )
            CALL ERRLOG( '        Fix channelization or don''t use '//
     1                  'CRDFREQ or CRDBW.' )
         END IF
C
C        Check that CRDNCH is viable - less than the number of BBCs
C        and the number of channels in the main setup (so we don't
C        try using some unset parameter like FIRSTLO).
C        Recall that INFDB forced that it be set by the user if 
C        CRDFREQ or CRDDOP were set, so we should not need to set it
C        or test for too low a value.
C
         IF( CRDNCH(ISCN) .GT. MIN( MAXCRD, MSCHN(ISETF) ) ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I3, A )' )
     1          'DOPCRD: CRDNCH must be less than', MAXCRD, 
     2          '(number of BBCs) and less than the '
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, 
     1          '        number of channels in the setup file.' )
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I5, A, I5 )' ) 

     2          ISCN
            CALL ERRLOG( MSGTXT )
         END IF
C
C        Be sure the required bandwidths were given.
C        If so. Transfer it to DBW for internal use in this routine.
C
         IF( CRDFREQ(1,ISCN) .GT. 0.D0 .OR. CRDDOP(ISCN) ) THEN
            DO ICH = 1, CRDNCH(ISCN)
               IF( CRDBW(ICH,ISCN) .LE. 0.D0 ) THEN
                  MSGTXT = ' ' 
                  WRITE( MSGTXT, '( 2A, I5, A, I5 )' )
     1               'DOPCRD: CRDFREQ, but not CRDBW, was specified ',
     2               'for BBC channel ', ICH,' in scan ', ISCN
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
                  CALL ERRLOG( '        Please specify CRDBW ' )
               ELSE
                  DBW(ICH) = CRDBW(ICH,ISCN)
               END IF
C
C              Complain if the bandwidth is not allowed (not subject to
C              the normal checking earlier because these are not the 
C              main setup channels).
C
               IF( .NOT. (
     1             DEQUAL( DBW(ICH), 0.0625D0 ) .OR. 
     2             DEQUAL( DBW(ICH), 0.125D0 ) .OR. 
     3             DEQUAL( DBW(ICH), 0.25D0 ) .OR. 
     4             DEQUAL( DBW(ICH), 0.5D0 ) .OR. 
     5             DEQUAL( DBW(ICH), 1.0D0 ) .OR. 
     6             DEQUAL( DBW(ICH), 2.0D0 ) .OR. 
     7             DEQUAL( DBW(ICH), 4.0D0 ) .OR. 
     8             DEQUAL( DBW(ICH), 8.0D0 ) .OR. 
     9             DEQUAL( DBW(ICH), 16.D0 ) ) ) THEN
                  MSGTXT = ' ' 
                  WRITE( MSGTXT, '( A, I5, A, I5, A, F6.2 )' )
     1               'DOPBBC:  Invalid CRDBW found in scan ', ISCN, 
     2               ' channel ', ICH, ' Value: ', DBW(ICH)
                  CALL WLOG( 0, MSGTXT )
                  CALL ERRLOG( '         Must be power of two between '
     1               // '0.0625 and 16 MHz' )
               END IF
C
            END DO
         END IF
C
C        Now deal with a Doppler request.
C
         IF( CRDDOP(ISCN) ) THEN
C
C           Get the velocity of the Earth in the direction of the 

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
     1          CALL ERRLOG( 'DOPCRD: Spectral line set '//LINES(ISCN)//
     2            ' not initialized.' )
C
C           This routine calculates the center frequency, then subtracts
C           (or adds) half the bandwidth to get the LO sum.  
C
C           Do the Doppler for all possible channels.  In the inputs,
C           any channel for which there was no rest frequency, 
C           bandwidth, or velocity given, had that the missing 
C           number set to the value for channel 1.  
C
C           Here the channel number is going to be the same as BBC number
C           as we won't be using both sidebands from a BBC.
C           Use any required parameters from the logical channels for the
C           setup file as previously established.
C
C           Set CRDFREQ to band edge.  The net sideband for the channel
C           was found by SBPAIR and is stored in SFSIDE, based on the
C           setup file.  This is being done before any sideband inversion
C           requests are made.  Round the frequency to the nearest 0.01 MHz.
C           Don't worry about channels sharing BBCs - that won't happen
C           in the cases where the CRD stuff applies as we are only doing
C           one per BBC.
C
C           Set the frequency for each channel.
C
C           At this point, we start using channel information from
C           the main setup.  Use CRDSETCH to map from crd channels to
C           setup file channels.  INFDB forced it to have been set when
C           using CRDDOP.  We might have used GETCRDN to get the mapping
C           CRSETC, but that routine needs a station, and this routine
C           is not for a specific station.
C
            DO ICH = 1, CRDNCH(ISCN)
C
C              Set up to add or subtract the bandwidth.  As a 
C              software check, complain about unrecognized SFSIDE.
C              Note that we will use the main setup file sideband.
C
C              Note that correlator inversions could cause an issue here,
C              but leave accounting for that to WRTFREQ.
C
C              SFSIDE will be indexed based on the setup file channels
C              (ICHS).  All the CRD parameters are for the crd channel 
C              index (ICH).  The mapping between the two is in CRDSETCH.
C              The user could have specified the mapping with CRDCH1 and
C              CRDNCH, but INFDB used those to fill in CRDSETCH.
C              For most projects, SCHED will set defaults (in GETCRDN)
C              if CRDSETCH is not filled.  But this routine is called
C              without a station where GETCRDN needs a station for other
C              setup information.  So INDFB enforces that CRDSETCH (
C              or CRDCH1) be given if CRDDOP is requested.  Here, the
C              only item from the setup that is used is the sideband.
C
               ICHS = CRDSETCH(ICH,ISCN)
               IF( SFSIDE(ICHS,ISETF) .EQ. 'U' ) THEN
                  SBW = 1
               ELSE IF( SFSIDE(ICHS,ISETF) .EQ. 'L' ) THEN
                  SBW = -1
               ELSE
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I3, A, A, A )' )
     1                'DOPCRD: Unrecognized SFSIDE(', ICHS, 
     1                ') = ''', SFSIDE(ICHS,ISETF) , ''''
                  CALL ERRLOG( MSGTXT )
               END IF
C
C              Now get the frequency.  If no velocity was given,
C              use the setup frequency.  This was flagged with
C              a "CONT" from RDSRC and a VLSR=-1.E9 from SRREAD.
C
               IF( VLSR(ICH,IDOPSRC(ISCN)) .LT. -1.E8 ) THEN
                  DFREQ(ICH) = 0.D0
               ELSE
C
C                 Actually calculate the frequency based on the
C                 VELREF and VELDEF requests.  Note that VLSR
C                 is now a bit of a misnomer.
C
C                 First convert a Z to a velocity. 
C
                  IF( VELDEF(IDOPSRC(ISCN)) .EQ. 'Z' ) THEN
                     VELSRC = VLSR(ICH,IDOPSRC(ISCN)) * VELC / 1.D3
                  ELSE
                     VELSRC = VLSR(ICH,IDOPSRC(ISCN))
                  END IF
C
C                 Now get the total velocity by adding in the 
C                 Earth's motions.  This is where the frame enters.
C
                  IF( VELREF(IDOPSRC(ISCN)) .EQ. 'L' ) THEN
                     VELTOT = VELSRC + VEARTH + VSUN
                  ELSE IF( VELREF(IDOPSRC(ISCN)) .EQ. 'H' ) THEN
                     VELTOT = VELSRC + VEARTH
                  ELSE IF( VELREF(IDOPSRC(ISCN)) .EQ. 'G' ) THEN
                     VELTOT = VELSRC
                  ELSE
                     CALL ERRLOG( 
     1                 'DOPCRD:  Bad VREF in source catalog: '//
     2                 VELREF(IDOPSRC(ISCN)) )
                  END IF
C
C                 Now calculate the frequency.
C
                  IF( VELDEF(IDOPSRC(ISCN)) .EQ. 'R' ) THEN
                     DFREQ(ICH) = RESTFREQ(ICH,NGP) * 
     1                  ( 1.D0 - VELTOT * 1.E3 / VELC ) - 
     2                  SBW * DBW(ICH) / 2.D0
                  ELSE IF( VELDEF(IDOPSRC(ISCN)) .EQ. 'O' .OR.
     1                     VELDEF(IDOPSRC(ISCN)) .EQ. 'Z' ) THEN
                     DFREQ(ICH) = RESTFREQ(ICH,NGP) / 
     1                  ( 1.D0 + VELTOT * 1.E3 / VELC ) - 
     2                  SBW * DBW(ICH) / 2.D0
                  ELSE
                     CALL ERRLOG( 
     1                 'DOPCRD:  Bad VDEF in source catalog: '//
     2                 VELDEF(IDOPSRC(ISCN)) )
                  END IF
C
C                 Round it to the required 0.01 MHz, which is good for
C                 the BBC's.  DOPINCR might have been set for the PFB
C                 or something else.  In other words, the the LO 
C                 equation is assumed to be FREQ = N * 0.01
C            
                  DFREQ(ICH) = 0.01D0 * 
     1                 DNINT( DFREQ(ICH) * 100.D0 )
C            
C                 Don't worry about overlapped channels.
C            
C                 Leave a flag that we did doppler on this source and
C                 keep track of the maximum number of channels used.
C
C                 Don't try to distinguish BBC and regular for this bit
C                 of bookkeeping.
C
                  DIDNDOP(IDOPSRC(ISCN)) = 
     1                 MAX( DIDNDOP(IDOPSRC(ISCN)), ICH )
                  DOPPED(SRCNUM(ISCN)) = .TRUE.
C
               END IF
            END DO
C
C           Took out warning related to VLAONLY observations.  We are
C           no longer supporting those.
C
C           Transfer the calculated frequency DFREQ to CRDFREQ
C           (the bandwidth is not changed in this routine).
C
            DO ICH = 1, CRDNCH(ISCN)
               CRDFREQ(ICH,ISCN) = DFREQ(ICH)
            END DO
C
         END IF
C
      END DO
C
      RETURN
      END
