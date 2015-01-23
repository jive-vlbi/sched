      SUBROUTINE FMTMKIV( KS, TRKBPS, OK )
C
C     Routine for SCHED called by SETFMT that sets the MKIV format 
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
C     bit rates above 512 Mbps.  For FASTTRK observations, use 
C     16 Mbps/track.  Some place, restrict FASTTRK to disk.
C
C     This routine is a very close copy of FMTVLBA, which was written
C     first.  
C
      INCLUDE        'sched.inc'
      INCLUDE        'schset.inc'
C
      INTEGER        KS, TMODE, LEN1, NSTREAM, ALLTRK, ISTA, KSTA
      REAL           TRKBPS, MAXBR
      LOGICAL        OK
      CHARACTER      NEWFMT*8
C ---------------------------------------------------------------------
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
C     TWOHEAD (2 drives or 2 heads - or 64 tracks).  It is later 
C     in SETFORM that the number of channels will be decreased for 
C     stations that don't have enough resources for TWOHEAD.

C     With tape, it was desirable to keep the number of tracks the 
C     same for all setups.  Specifically, for TWOHEAD observations,
C     scans with narrower bandwidths would still use 64 tracks.
C     With the disk systems, there is no need to keep the number
C     of tracks constant as it no longer complicates media managment.
C     Therefore relax any attempt to preserve the number of tracks
C     when there are no tapes.
C
C
C     The meaning of some variables:
C     NSTREAM   - The number of bit streams.
C     TOTBPS    - The nominal total bit rate.  Derived in SETFORM.  
C                 This will be the same as WRTBPS because of data 
C                 replacement headers.
C     MINTBPS   - The minimum track bit rate that can be used.
C     MAXTBPS   - The maximum track bit rate that can be used.
C     MINTRAK   - The minimum number of tracks required.
C     MAXTRAK   - The maximum number of tracks that can be used.
C     MAXBR     - Maximum track bit rate.
C
      MAXBR = 8.0
C
      IF( ( ANYTAPE .AND. TWOHEAD ) .OR. TOTBPS(KS) .GT. 257.0 ) THEN
         ALLTRK = 64
      ELSE
         ALLTRK = 32
      END IF
C
      IF( FASTTRK ) MAXBR = 16.0
      NSTREAM = NCHAN(KS) * BITS(1,KS)
      MINTBPS(KS) = MAX( 2.0, SAMPRATE(KS) / 4.0 )
      MAXTBPS(KS) = MIN( SAMPRATE(KS), MAXBR )
      MINTRAK(KS) = TOTBPS(KS) / MAXTBPS(KS)
      MAXTRAK(KS) = MIN( ALLTRK, NINT( TOTBPS(KS) / MINTBPS(KS) ) )
C
C     Get the station catalog number and schedule station number
C     for the first station associated with this setup.
C
      KSTA = ISETSTA(KS)
      ISTA = ISCHSTA(KSTA)
C
C     Allow up to 1024 Mbps on the Mark IV systems.  They will
C     use 16 Mbps per track using what otherwise looks like the
C     512 Mbps (twohead) mode.
C
      IF( TOTBPS(KS) .GT. 512.0 .AND. 
     1        .NOT. ( USEDISK(ISTA) .OR. 
     2        RECORDER(KSTA) .EQ. 'NONE' ) ) THEN
         CALL WLOG( 1, 'FMTMKIV: Total bit rate too high in ' //
     1      'setup described below.' )
         CALL WLOG( 1, '         Max 512 for MKIV format on tape.' )
         CALL ERRSET( KS )
      END IF
C
C     Do a quick sanity check on TAPEMODE(KS) to avoid needing
C     to do it several times later.
C
      IF( TAPEMODE(KS) .LT. 0 ) THEN
         CALL WLOG( 1, 'FMTMKIV: Negative TPMODE?  Not allowed.' )
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
     1       'FMTMKIV: Track bit rate request ', TRKBPS, 
     2       ' outside the possible range for this setup: ',
     3       MINTBPS(KS), ' to ', MAXTBPS(KS)
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, '       setup file: ' // SETNAME(KS) )
            CALL WLOG( 1, '         station: '//
     1          STATION(KSTA) )
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
C     Note that FORMAT(1:4)='MKIV' or we wouldn't be in this routine.
C
      IF( LEN1( FORMAT(KS) ) .EQ. 7 ) THEN 
         IF( FORMAT(KS)(5:7) .EQ. '1:1' ) THEN
            FANOUT(KS) = 1.0
         ELSE IF( FORMAT(KS)(5:7) .EQ. '1:2' ) THEN
            FANOUT(KS) = 2.0
         ELSE IF( FORMAT(KS)(5:7) .EQ. '1:4' ) THEN
            FANOUT(KS) = 4.0
         ELSE IF( LEN1( FORMAT(KS) ) .NE. 4 ) THEN
            CALL WLOG( 1, 'FMTMKIV:  Bad MarkIV format specified. '//
     1           ' Fan out spec not proper.' )
            CALL ERRSET( KS )
         END IF
C
      ELSE IF( LEN1( FORMAT(KS) ) .NE. 4 ) THEN
C
C        Protect against spurious format settings.
C
         CALL WLOG( 1, 'FMTMKIV:  Bad MarkIV format specified. '//
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
     1        'FMTMKIV: TPMODE should be 1 for more than 128 Mbps ',
     2        'with MarkIV format.  Was set to: ', TAPEMODE(KS)
            CALL WLOG( 1, 
     1     '         TPMODE is the passes per head position.' )
            CALL ERRSET( KS )
         ELSE IF( TAPEMODE(KS) .GT. ALLTRK / MINTRAK(KS) ) THEN
            CALL WLOG( 1, 'FMTMKIV: Impossible TPMODE specified. ' )
            CALL WLOG( 1, 
     1          '         TPMODE is the passes per head position.' )
            CALL ERRSET( KS )
         ELSE
C
C           Ok, can conform to the request.  But don't overshoot
C           what is possible.
C
            FANOUT(KS) = MIN( 4.0, 
     1         FLOAT( ALLTRK / ( TAPEMODE(KS) * NSTREAM ) ) )
         END IF
C
      END IF
C
C     Set the format in some cases where there is no choice.
C     First if the bit rate is above 128 Mbps, the number of
C     tracks is forced because all available tracks will be in
C     use.  In TWOHEAD tape modes all setups will use all tracks even 
C     though setups with less than 512 Mbps won't be
C     using the full possible bit rate per track.  This is because
C     we wouldn't be in that mode unless some other setup did use all, 
C     and we want all setups to use the same number of tracks.  This
C     is for tape modes only - for disks the number of tracks can vary.
C
C     Note we have not yet docked channels from the single drive 
C     stations.  We'll do that later to avoid logic complications now.
C
      IF( FANOUT(KS) .EQ. 0.0 ) THEN
         IF( ( ANYTAPE .AND. TWOHEAD ) .OR. TOTBPS(KS) .GT. 129.0 ) 
     1        THEN
            FANOUT(KS) = MAXTRAK(KS) * SAMPRATE(KS) / TOTBPS(KS)
         END IF
      END IF
C
C     If the sample rate was 32 Msamp/sec, that constrains the fanout
C     to 4 unless this is a FASTTRK observation (some scans have
C     1024 Mbps).  For FASTTRK, the fanout would have been set above
C     so ignore that case (FASTTRK => TWOHEAD)
C
      IF( FANOUT(KS) .EQ. 0.0 ) THEN
         IF( SAMPRATE(KS) .EQ. 32.0 ) THEN
            FANOUT(KS) = SAMPRATE(KS) / MAXBR
         END IF
      END IF
C
C     The minimum track rate also forces the rate.  Use LE, but 
C     be aware that less than 2.0 is not allowed.
C     Actually, I'm not sure this is true for Mark IV.  Such data
C     could not be processed on the Socorro processor.
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
C        If got here, the setting above failed.  Return.  Presumably
C        we'll get called again with a TRKBPS.
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
         IF( FANOUT(KS) .GT. 0.0 .AND. FANOUT(KS) .LT. 2.0 ) THEN
            NEWFMT = 'MKIV1:1'
            FANOUT(KS) = 1.0
         ELSE IF( FANOUT(KS) .GE. 2.0 .AND. FANOUT(KS) .LT. 4.0 ) THEN
            NEWFMT = 'MKIV1:2'
            FANOUT(KS) = 2.0
         ELSE IF( FANOUT(KS) .GE. 4.0 ) THEN
            NEWFMT = 'MKIV1:4'
            FANOUT(KS) = 4.0
         ELSE
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F8.3, A, F8.2 )' ) 
     1         'FMTMKIV: Invalid fanout ', FANOUT(KS), 
     2         ' derived for setup described below. TRKBPS was ', 
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
     1        'FMTMKIV: Derived format ', NEWFMT, 
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
     1         'FMTMKIV: Derived fanout ', FANOUT(KS),
     2         ' too large for setup detailed below. '
            CALL WLOG( 1, MSGTXT )
         END IF
C
C        Set the TAPEMODE if not already.
C        Don't use values other than 1 for disk - the pass concept
C        isn't there.
C
         IF( TAPEMODE(KS) .EQ. 0 ) THEN
            IF( USEDISK(ISTA) ) THEN
               TAPEMODE(KS) = 1
            ELSE
               TAPEMODE(KS) = TMODE
            END IF
C
C        Now check the reasonableness of a preset TAPEMODE.
C
         ELSE IF( TAPEMODE(KS) .GT. TMODE ) THEN
            CALL WLOG( 1, 'FMTMKIV: Specified TPMODE impossible for '//
     1           'specified format.' )
            CALL WLOG( 0, 
     1        '         TPMODE is the passes per head position.' )
            CALL ERRSET( KS )
         ELSE IF( TAPEMODE(KS) .LT. TMODE ) THEN
            CALL WLOG( 0, 'FMTMKIV: Specified TPMODE not as high '//
     1         'as could be for specified format.' )
            CALL WLOG( 0, '         setup: '// SETNAME(KS) )
            CALL WLOG( 0, '         station: '// 
     1         STATION(KSTA) )
            CALL WLOG( 0, '         format: ' // FORMAT(KS) )
            CALL WLOG( 0, 
     1        '         TPMODE is the passes per head position.' )
         END IF
C
C        Set SPEEDUP and OK before exiting.  Record the track bit rate
C        used.
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
