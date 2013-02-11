      SUBROUTINE FMTVLBA( KS, TRKBPS, OK )
C
C     Routine for SCHED called by SETFMT that sets the VLBA format 
C     for setup group KS.  It will attempt to set FORMAT, SPEEDUP, 
C     FANOUT, and TAPEMODE.  OK will return .TRUE. if it succeeds.  
C     It will return .FALSE. if not, which means that it does not 
C     yet have enough information, which is only possible if it is
C     called with zero for the track bit rate (TRKBPS).  OK false
C     can also imply an error.
C
C     For "twohead" observations, some stations might not have enough
C     drives or heads.  Worry about that later once the formats are
C     set.  For now, it is appropriate just that we will try hard to
C     avoid using 2 "heads" if possible.  The TWOHEAD flag has already
C     been set based on the bit rate, as has the FASTTRK flag for
C     bit rates above 512 Mbps.  For VLBA, FASTTRK is not allowed 
C     because the electronics don't do 16 Mbps/track.
C
C     With tape, it was desirable to keep the number of tracks the 
C     same for all setups.  Specifically, for TWOHEAD observations,
C     scans with narrower bandwidths would still use 64 tracks.
C     With the disk systems, there is no need to keep the number
C     of tracks constant as it no longer complicates media managment.
C     Therefore relax any attempt to preserve the number of tracks
C     when there are no tapes.  Basically this routine is setting
C     the fanout as that, along with the sample rate essentially 
C     determines everything.  For disk, set the fanout to the
C     smallest allowed value as determined by the maximum track 
C     bit rate the formatter can handle.  On the VLBA, that is 8 Mbps.
C
      INCLUDE        'sched.inc'
      INCLUDE        'schset.inc'
C
      INTEGER        KS, TMODE, LEN1, NSTREAM, ALLTRK
      REAL           TRKBPS, MAXBR
      LOGICAL        OK
      CHARACTER      NEWFMT*8
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'FMTVLBA: starting.' )
C
      OK = .FALSE.
C
C     We're not going to get anywhere without the sample rate.  I
C     don't think this should happen, but just in case.
C
      IF( SAMPRATE(KS) .EQ. 0.0 ) THEN
         GO TO 999
      END IF
C
C     Set the maximum bit rate per track and the maximum number of
C     tracks MAXTRAK available for use.  Assume all stations can do 
C     TWOHEAD (2 drives or 2 heads - or simply 64 tracks).  It is 
C     later in SETFORM that the number of channels will be decreased 
C     for stations that don't have enough resources for TWOHEAD.
C
C     The meaning of some variables:
C     NSTREAM  - The number of bit streams.
C     TOTBPS   - The total bit rate.  Derived in SETFORM.  This will
C                be a bit less than WRTBPS, the actual bit rate 
C                because of headers, but is more useful for format 
C                picking.
C     MINTBPS  - The minimum track bit rate that can be used.
C     MAXTBPS  - The maximum track bit rate that can be used.
C     MINTRAK  - The minimum number of tracks required.
C     MAXTRAK  - The maximum number of tracks that can be used.
C     MAXBR    - Maximum track bit rate.
C
C     Set number of tracks.  64 for more than 256 Mbps, 32 otherwise.
C
      MAXBR = 8.0
      IF( ( ANYTAPE .AND. TWOHEAD ) .OR. TOTBPS(KS) .GT. 257.0 ) THEN
         ALLTRK = 64
      ELSE
         ALLTRK = 32
      END IF
C
      NSTREAM = NCHAN(KS) * BITS(1,KS)
      MINTBPS(KS) = MAX( 2.0, SAMPRATE(KS) / 4.0 )
      MAXTBPS(KS) = MIN( SAMPRATE(KS), MAXBR )
      MINTRAK(KS) = TOTBPS(KS) / MAXTBPS(KS)
      MAXTRAK(KS) = MIN( ALLTRK, NINT( TOTBPS(KS) / MINTBPS(KS) ) )
C
C     Don't allow more than 512 Mbps on these systems as it 
C     requires 16 Mbps per track, which the VLBA formatters can't do.
C
      IF( TOTBPS(KS) .GT. 512.0 ) THEN
         CALL WLOG( 1, 'FMTVLBA: Total bit rate too high in ' //
     1      'setup described below.  Max 512 for VLBA format.' )
         CALL ERRSET( KS )
      END IF
C
C     Do a quick sanity check on TAPEMODE(KS) to avoid needing
C     to do it several times later.
C
      IF( TAPEMODE(KS) .LT. 0 ) THEN
         CALL WLOG( 1, 'FMTVLBA: Negative TPMODE?  Not allowed.' )
         CALL ERRSET( KS )
      END IF
C
C     See if we've already set the format.  If so, the routine
C     is being used to check compatability.  Do so.
C
      IF( FANOUT(KS) .NE. 0.0 ) THEN
         OK =  TRKBPS .EQ. 0.0  .OR. ( TRKBPS .NE. 0.0 .AND. 
     1         TRKBPS .EQ. SAMPRATE(KS) / FANOUT(KS) )
         GO TO 999
      END IF
C
C     Sanity checks for the specified bit rate.
C
      IF( TRKBPS .NE. 0.0 ) THEN
         IF( TRKBPS .GT. MAXTBPS(KS) .OR. 
     1       TRKBPS .LT. MINTBPS(KS) ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F8.2, A, F8.2, A, F8.2 )' ) 
     1       'FMTVLBA: Track bit rate request ', TRKBPS, 
     2       ' outside the possible range for this setup: ',
     3       MINTBPS(KS), ' to ', MAXTBPS(KS)
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, '       setup file: ' // SETNAME(KS) )
            CALL WLOG( 1, '         station: '//
     1          STATION(ISETSTA(KS)) )
            CALL WLOG( 1, '         format: ' // FORMAT(KS) )
            CALL WLOG( 1, '         Probable programming error.' )
            OK = .FALSE.
            CALL ERRSET( KS )
         END IF
      END IF
C
C     Try to get the fanout from the inputs, or from the constraints.
C     
C     If the FORMAT is fully specified, that is all we need.
C     Note that FORMAT(1:4)='VLBA' or we wouldn't be in this routine.
C
      IF( LEN1( FORMAT(KS) ) .EQ. 7 ) THEN 
         IF( FORMAT(KS)(5:7) .EQ. '1:1' ) THEN
            FANOUT(KS) = 1.0
         ELSE IF( FORMAT(KS)(5:7) .EQ. '1:2' ) THEN
            FANOUT(KS) = 2.0
         ELSE IF( FORMAT(KS)(5:7) .EQ. '1:4' ) THEN
            FANOUT(KS) = 4.0
         ELSE IF( LEN1( FORMAT(KS) ) .NE. 4 ) THEN
            CALL WLOG( 1, 'FMTVLBA:  Bad VLBA format specified. '//
     1           ' Fan out spec not proper.' )
            CALL ERRSET( KS )
         END IF
C
      ELSE IF( LEN1( FORMAT(KS) ) .NE. 4 ) THEN
C
C        Protect against spurious format settings.
C
         CALL WLOG( 1, 'FMTVLBA:  Bad VLBA format specified. '//
     1            '  Invalid length. ')
         CALL ERRSET( KS )
      END IF
C
C     See if TAPEMODE(KS) will allow us to set format.
C
      IF( FANOUT(KS) .EQ. 0.0 .AND. TAPEMODE(KS) .NE. 0 ) THEN
C 
C        Complain about impossible cases.  Note we haven't decreased
C        the number of tracks yet for stations no equipped for two
C        drive operation.  Save that for after formats are set.
C
         IF( TOTBPS(KS) .GT. 128 .AND. TAPEMODE(KS) .NE. 1 ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A, I5 )' )
     1        'FMTVLBA: TPMODE should be 1 for more than 128 Mbps ',
     2        'with VLBA format.  Was set to: ', TAPEMODE(KS)
            CALL WLOG( 1, 
     1     '         TPMODE is the passes per head position.' )
            CALL ERRSET( KS )
         ELSE IF( TAPEMODE(KS) .GT. ALLTRK / MINTRAK(KS) ) THEN
            CALL WLOG( 1, 'FMTVLBA: Impossible TPMODE specified. ' )
            CALL WLOG( 1, 
     1          '         TPMODE is the passes per head position.' )
            CALL ERRSET( KS )
         ELSE
C
C           Ok, can conform to the request.  But don't overshoot
C           what is possible.
C
            FANOUT(KS) = MIN( 4.0, 
     1          FLOAT( ALLTRK / ( TAPEMODE(KS) * NSTREAM ) ) )
         END IF
C
      END IF
C
C     Set the format in some cases where there is no choice.
C     First if all available tracks are being used, the mode is
C     forced.  This happens with anything above 128 Mbps.
C     Also assume that all TWOHEAD tape observations use all 64 tracks 
C     (even if some setups have narrower bandwidths).  Note we 
C     have not yet docked channels from the single drive stations.  
C     We'll do that later to avoid logic complications now.
C     Use 129 as something a bit larger than 128.
C
      IF( FANOUT(KS) .EQ. 0.0 ) THEN
         IF( ( ANYTAPE .AND. TWOHEAD  ) .OR. TOTBPS(KS) .GT. 129.0 ) 
     1        THEN
            FANOUT(KS) = MAXTRAK(KS) * SAMPRATE(KS) / TOTBPS(KS)
         END IF
      END IF
C
C     If the sample rate was 32 Mbps, that constrains the fanout.
C
      IF( FANOUT(KS) .EQ. 0.0 .AND. 
     1    SAMPRATE(KS) .EQ. 32.0 ) THEN
         FANOUT(KS) = SAMPRATE(KS) / MAXBR
      END IF
C
C     The minimum track rate also forces the rate.  Use LE, but 
C     be aware that less than 2.0 is not allowed.
C
      IF( FANOUT(KS) .EQ. 0.0 .AND. 
     1    SAMPRATE(KS) .LE. 2.0 ) THEN
         FANOUT(KS) = 1.0
      END IF
C
C     If TRKBPS was set, use that.  Note that we checked
C     earlier that TRKBPS was not greater than the sample rate.
C     Will sanity check later.
C
      IF( FANOUT(KS) .EQ. 0.0 .AND. TRKBPS .GT. 0.0 ) THEN
         FANOUT(KS) = SAMPRATE(KS) / TRKBPS
      END IF
C
C     --------------------
C     The following may actually be all that is needed of this 
C     routine in the disk era unless there is a need to conform
C     to setup file input.  Set the fanout to the minumum
C     possible that the formatter can handle.  This is fine since
C     is no longer necessary to match the track bit rates at 
C     stations.
C
      IF( FANOUT(KS) .EQ. 0.0 ) THEN
         FANOUT(KS) = SAMPRATE(KS) / MAXTBPS(KS)
      END IF
C     ---------------------
C
      IF( FANOUT(KS) .EQ. 0.0 ) THEN
C
C        If got here, something was wrong with the last statments.
C
         OK = .FALSE.
C
      ELSE
C
C        Fill out the format from any FANOUT derived above.
C        Force the result to 1.0, 2.0, or 4.0.  Fractional values
C        can be derived above from non-power-of-two numbers of
C        channels.
C
         OK = .TRUE.
         IF( FANOUT(KS) .GT. 0.0 .AND. FANOUT(KS) .LT. 2.0 ) THEN
            NEWFMT = 'VLBA1:1'
            FANOUT(KS) = 1.0
         ELSE IF( FANOUT(KS) .GE. 2.0 .AND. FANOUT(KS) .LT. 4.0 ) THEN
            NEWFMT = 'VLBA1:2'
            FANOUT(KS) = 2.0
         ELSE IF( FANOUT(KS) .GE. 4.0 ) THEN
            NEWFMT = 'VLBA1:4'
            FANOUT(KS) = 4.0
         ELSE
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F8.3, A, F8.2 )' ) 
     1         'FMTVLBA: Invalid fanout ', FANOUT(KS), 
     2         'derived for setup described below. TRKBPS was ', 
     3         TRKBPS
            CALL WLOG( 1, MSGTXT )
            CALL ERRSET( KS )
         END IF
C
C        Make sure we haven't changed a fully specified format.
C        I really don't think I should get here.  Maybe this 
C        could be deleted.
C
         IF( LEN1(FORMAT(KS)) .GT. 4 .AND. 
     1       NEWFMT .NE. FORMAT(KS) ) THEN
            MSGTXT = ' ' 
            WRITE( MSGTXT, '( 4A )' )
     1        'FMTVLBA: Derived format ', NEWFMT, 
     2        ' does not match specified format ', FORMAT(KS)
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, '       Probable programming error.' )
         END IF
C
C        Set the format regardless of previous format.
C
         FORMAT(KS) = NEWFMT
C
C        Set tape mode if needed.  Otherwise check the preset
C        tapemode for reasonableness and complain if it is not
C        optimal.  TMODE is the TAPEMODE that eventually uses all
C        tracks for this schedule.  Recall TAPEMODE is the number
C        of passes per head position.
C
         TMODE = ALLTRK / ( NCHAN(KS) * BITS(1,KS) * FANOUT(KS) )
         IF( TMODE .LT. 1 ) THEN
            MSGTXT = ' ' 
            WRITE( MSGTXT, '( A, F8.2, A )' ) 
     1         'FMTVLBA: Derived fanout ', FANOUT(KS),
     2         ' too large for setup detailed below. '
            CALL WLOG( 1, MSGTXT )
         END IF
C
C        Set the TAPEMODE if not already.
C
         IF( TAPEMODE(KS) .EQ. 0 ) THEN
            TAPEMODE(KS) = TMODE
C
C        Now check the reasonableness of a preset TAPEMODE.
C
         ELSE IF( TAPEMODE(KS) .GT. TMODE ) THEN
            CALL WLOG( 1, 'FMTVLBA: Specified TPMODE impossible for '//
     1           'specified format.' )
            CALL WLOG( 0, 
     1        '         TPMODE is the passes per head position.' )
            CALL ERRSET( KS )
         ELSE IF( TAPEMODE(KS) .LT. TMODE ) THEN
            CALL WLOG( 0, 'FMTVLBA: Specified TPMODE not as high '//
     1         'as could be for specified format.' )
            CALL WLOG( 0, '         setup: '// SETNAME(KS) )
            CALL WLOG( 0, '         station: '// 
     1         STATION(ISETSTA(KS)) )
            CALL WLOG( 0, '         format: ' // FORMAT(KS) )
            CALL WLOG( 0, 
     1        '         TPMODE is the passes per head position.' )
         END IF
C
C        Set SPEEDUP and OK before exiting.  Record the track bit rate
C        used.   SPEEDUP is not really a proper concept in the disk
C        world, but calculate it anyway.  Places where it is used
C        are being removed.
C
         SPEEDUP(KS) = 8.0 * FANOUT(KS) / SAMPRATE(KS)
         TBPS(KS) = SAMPRATE(KS) / FANOUT(KS)
         OK = .TRUE.
C
      END IF
C
C     Line 999 is the jump point for some error points above and
C     for when the format does not need to be set.
C
  999 CONTINUE
      RETURN
      END
