      SUBROUTINE SETFORM
C
C     Routine for SCHED, called by SETREC, to set the recording
C     formats for the setup groups.
C
C     Conform to the modern disk world.  Jan. 11, 2011. RCW.  This
C     mainly means stop worrying about matching many recording
C     parameters across stations.

C     Much of this routine might be unnecessary after the legacy 
C     VLBA and MKIV systems go away.  Save the recoding until 
C     then.

C     ************  Have not actually done the modifications for
C       the above two comments yet.  Things are working for now
C       and time is going to more critical changes.

C
C     The following applies to MarkIV and VLBA tape or Mark5A systems.
C     The fan out is not a concept for the digital backends.
C
C     The nastiest recording parameter choice is deciding on the
C     format to use, which amounts to choosing the fan out.  This
C     cannot be done individually per setup group because of
C     the following global constraints.
C
C     1.  Obsolete: Try to avoid changing the number of heads in use at a 
C         station between scans with VLBA and Mark IV systems.  This would
C         be ok with disk systems.  This constraint is driven by
C         the tape usage accounting nightmare that variations in 
C         head use would cause.
C
C     2.  Obsolete (don't think it ever was true for JIVE): 
C         For some correlators (JIVE, old Socorro), the track bit rate
C         should be the same for all stations in a scan - which here
C         means for all setup groups in a setup file.  This has the
C         additional impact of insuring a constant speedup factor.
C         Treat all correlators as if they have this constraint.
C         This does not affect the DiFX correlators.
C
C     3.  Obsolete.  It's not necessarly to worry about this with disk.
C         Don't use more than one drive or head unless necessary.
C         But if any scan at a station uses more than one, do so
C         for all scans (basically the same as the constant heads
C         constraint).  This should not affect disk recordings.
C
C     4.  For multiple drive/head observations, allow stations 
C         without full resources to record reduced bandwidth by 
C         reducing the number of channels.  Actually this can
C         be done for any setup where a station has inadequate
C         resources.  Just cut NCHAN for the setup group.  Do this
C         after setting all the formats to reduce confusion.  
C         This part obsolete:
C         But during the setting, try hard to stay on one drive or head.
C         The recording resources should no longer cause this, but
C         DAR's might.
C
C     5.  Obsolete
C         Eventually there will be systems (eg MARK5B) that don't
C         care about speed up factors and number of tracks.  I think
C         they will be easier.  Actually the disk systems and 
C         correlators remaining after FXCORR was turned off qualify.
C
C     Trying to conform to the above constraints is a messy problem.
C     Here is the scheme that will be used:
C
C     1.  Go through forcing any formats where there is no choice.
C         This is done in SETFMT and the routines it calls.
C         A.  Tape or Mark 5A bit rates of 256 Mbps or more.
C         B.  Samplerate of 32 Msamp/sec.
C         C.  Samplerate of 2 Msamp/sec or less.
C         D.  While at it, cut the bandwidth at stations that 
C             can't handle the request (limited heads/drives/bbcs)
C         E.  Stop if all formats are set.
C
C     2.  For each setup file, if any group has fixed bit rate/track,
C         force the others in the group.  Done in FSPREAD.
C
C     3.  Try to force all setups to use the same number of tracks.
C         This assumes all stations have the same number of tracks 
C         available.  This should be ok since the single drive/head
C         stations have not been dealt with yet.  Later they will
C         be forced to use no more than 32 tracks.  Done in FSPREAD.
C
C     4.  For all unfixed setup groups, try to find an appropriate
C         set of track bit rates that will give an average speedup
C         factor near 2.  Adjust to conform to the constant bit rate
C         per track in a setup file and constant number of tracks
C         at a station.  
C
C     This should produce a consistent set of setups that gives
C     a speed up of as close as possible to 2 for current standard
C     correlation.
C
C     Confused yet?  I hope I'm not still!
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
C
      INTEGER           KS, ISETF, ISTA, ISCN
      REAL              MAXBPS, HDFACT
      LOGICAL           OK, NEEDFMT(MSET)
      LOGICAL           ANYLEFT, ANYLFT2
C -------------------------------------------------------------------- 
      IF( DEBUG ) CALL WLOG( 0, 'SETFORM: Starting' )
C
C     MARKIII no longer supported.
C
      DO KS = 1, NSET
         IF( FORMAT(KS) .EQ. 'MARKIII' ) THEN
            CALL ERRLOG ( 
     1         'MARKIII format no longer in use.  Fix setup.' )
         END IF
      END DO
C
C     Initialize the format related variables.  This helps prevent
C     problems on restarts.  Initialize TAPEMODE to the setup file
C     TPMODE value.
C
      DO KS = 1, NSET
         SPEEDUP(KS) = 0.0
         FANOUT(KS) = 0.0
         TAPEMODE(KS) = TPMODE(KS)
      END DO

C
C     First, set as much of the format as possible from the 
C     information provided in the setup file and catalogs.
C
C     If the format was not specified at all, set the default
C     general type based on the type of DAR/DBE at the station where
C     this can be done as with most modern systems.
C     Actually, this is not always right, but, if not, the
C     user will need to be specific.
C     Note the ELSE option deals with the VLBA and MKIV MARK5A/B systems.
C
C     For the VLBA/RDBE, the default will be firmware dependent with
C     MARK5B used for the PFB and VDIF used for the DDC.  The DDC
C     is the only allowed firmware when actually using 2 RDBEs, but that
C     will be dealt with elsewhere.  Assume for now that the DBBC
C     will use MARK5B in all cases.
C
      DO KS = 1, NSET
         IF( FORMAT(KS) .EQ. ' ' ) THEN
            IF( DISK(ISETSTA(KS)) .EQ. 'MARK5C' .AND.
     1           ( DBE(KS) .EQ. 'RDBE_PFB' .OR.
     2             DBE(KS) .EQ. 'DBBC_PFB' .OR.
     3             DBE(KS) .EQ. 'DBBC_DDC' ) ) THEN
               FORMAT(KS) = 'MARK5B'
C
            ELSE IF( DISK(ISETSTA(KS)) .EQ. 'MARK5C' .AND.
     1             DBE(KS) .EQ. 'RDBE_DDC' ) THEN
               FORMAT(KS) = 'VDIF'
C
            ELSE IF( DISK(ISETSTA(KS)) .EQ. 'MARK5B' ) THEN
               FORMAT(KS) = 'MARK5B'
C
            ELSE IF( DAR(ISETSTA(KS)) .EQ. 'WIDAR' ) THEN
               FORMAT(KS) = 'VDIF'
C
            ELSE
               FORMAT(KS) = DAR(ISETSTA(KS))
            END IF
C
C           Don't get goofed up by some of the VLBA varients.
C
            IF( FORMAT(KS) .EQ. 'VLBAG' ) FORMAT(KS) = 'VLBA'
            IF( FORMAT(KS) .EQ. 'VLBA4' ) FORMAT(KS) = 'MKIV'
         END IF
      END DO
C
C     Flag VLBA and MKIV format setup groups.  This is used by
C     various other routines besides this one.  Get the total
C     bit rate nice and early.  Initialize RECUSED.
C     TOTBPS is the old version and is the nominal data rate.
C     For calculations of actual disk space used, the true bit
C     rate WRTBPS should be used as it includes the effects
C     of the headers.
C     The factors for the header contribution are as per email 
C     from W. Brisken on Jan. 8, 2013.  If the format is not a 
C     recognized one, assume the overhead is 1.0.  For VDIF, 
C     the overhead can depend on specific flavor.  Use the one 
C     that we're starting with (5032 => 32 byte header on 5000 
C     bytes data).
C
      DO KS = 1, NSET
         VLBAMKIV(KS) = FORMAT(KS)(1:4) .EQ. 'VLBA' .OR. 
     1                  FORMAT(KS)(1:4) .EQ. 'MKIV'
         RECUSED(KS) = .FALSE.
C
         IF( FORMAT(KS)(1:4) .EQ. 'VLBA' ) THEN
            HDFACT = 1.008
         ELSE IF( FORMAT(KS)(1:4) .EQ. 'MKIV' ) THEN
            HDFACT = 1.0
         ELSE IF( FORMAT(KS)(1:6) .EQ. 'MARK5B' ) THEN
            HDFACT = 1.0016
         ELSE IF( FORMAT(KS)(1:4) .EQ. 'VDIF' ) THEN
            HDFACT = 1.0064
         ELSE
            HDFACT = 1.0
         END IF
         TOTBPS(KS)  = NCHAN(KS) * SAMPRATE(KS) * BITS(1,KS)
         WRTBPS(KS)  = TOTBPS(KS) * HDFACT
      END DO
C
C     Go through scans and stations to determine which 
C     format groups were actually used for recording and to get
C     the maximum bit rate used in the experiment in VLBA and
C     MKIV formats.  Those are the ones that need correlated formats.
C     From those determine if two head or fast track (16 Mbps/track)
C     observations are needed for VLBA and MKIV systems.
C
      MAXBPS = 0
      DO ISCN = 1, NSCANS
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
               KS = NSETUP(ISCN,ISTA)
               ISETF = ISETNUM(KS)
               IF( ( VLBAMKIV(KS) .OR. FORMAT(KS) .EQ. 'MARKIII' ) .AND.
     1               .NOT. NOREC(ISCN) ) THEN
                  MAXBPS = MAX( MAXBPS, TOTBPS(KS) )
               END IF
               IF( .NOT. NOREC(ISCN) ) THEN
                  RECUSED(KS) = .TRUE.
               END IF
            END IF
         END DO
      END DO
      TWOHEAD = MAXBPS .GT. 256.0
      FASTTRK = MAXBPS .GT. 512.0
C
C     Warn user about TWOHEAD and set HEADMODE for the wide band
C     modes.
C
      CALL TWOHDSET
C
C     Go through all setup groups setting the format parameters
C     (FORMAT, SPEEDUP, FANOUT, and TAPEMODE) that can be set at
C     this time.  After this, NEEDFMT can be used to tell if 
C     this format still needs to be set.  For any setups where the
C     format details can be set independently of other setups, this
C     will finish the job.
C
      ANYLEFT = .FALSE.
      DO KS = 1, NSET
         CALL SETFMT( KS, 0.0, OK )
         NEEDFMT(KS) = .NOT. OK
         IF( NEEDFMT(KS) ) ANYLEFT = .TRUE.
      END DO
C
C     For some systems (VLBA, MkIV), the track bit rate must be
C     constant across a setup file and the number of channels
C     should be constant in time.  This allows even one forced
C     format to cause the rest to be set.  Set any formats that can
C     be set because of these constraints.  FSPREAD does one pass
C     of spreading the track bit rate across setup files, and then
C     spreading the number of tracks across setup files by station.
C
C     When making some disk related updates, I found that the 
C     last statement now in the IF was actually outside.  That set
C     ANYLEFT to the undefined ANYLFT2 and could go into FMTPICK
C     even when things were already set.  Not sure how often that
C     caused a problem.
C

C  **********  Do I need to avoid changing bit rates in some cases.
C     see end of old okmodes.f (in older versions only now.)
C     I've got emails out to ask.


      IF( ANYLEFT ) THEN
         CALL FSPREAD( NEEDFMT )
         ANYLFT2 = .FALSE.
         DO KS = 1, NSET
            IF( NEEDFMT(KS) ) ANYLFT2 = .TRUE.
         END DO
         ANYLEFT = ANYLFT2
      END IF
C
C     If the above still did not force the outcome for all setups,
C     try to pick an option that gives a speedup factor averaging
C     about 2.  This was painful to program and I hope I got it ok,
C     but keep an eye on it.  
C
C     In the era of disk, this is not needed and, in fact, it is
C     not likely that the program will enter this call.  I should
C     confirm that, and maybe ditch FMTPICK.
C
      IF( ANYLEFT ) THEN
         CALL FMTPICK( NEEDFMT )
      END IF
C
C     That should be the end of picking any recording formats.  Check.
C
      DO KS = 1, NSET
         IF( RECUSED(KS) .AND. NEEDFMT(KS) ) THEN
            CALL WLOG( 1, 'SETFORM:  Setup file below has format not '
     1          // 'fully specified when it should be.' )
            CALL ERRSET( KS )
         END IF
      END DO
C
C     Drop the number of channels in any formats for stations that don't
C     have adequate resources.
C
      DO KS = 1, NSET
         IF( VLBAMKIV(KS) .AND. RECUSED(KS) .AND. .NOT. NEEDFMT(KS) )
     1       THEN
C
C           Look for just too much bandwidth.
C
            IF( ( TBPS(KS) .GT. 8.0 .AND. TOTBPS(KS) .GT. 1024.0 ) .OR.
     1         ( TBPS(KS) .LE. 8.0 .AND. TOTBPS(KS) .GT. 512.0 ) ) THEN
               CALL WLOG( 1, 'SETFORM: Too much bandwidth requested.' )
               CALL ERRSET( KS )
            END IF
C
C           Look for inappropriate bit rates for the format.
C
            IF( TBPS(KS) .GT. 8.0 .AND. 
     1          FORMAT(KS)(1:4) .NE. 'MKIV' ) THEN
               CALL WLOG( 1, 'SETFORM: Attempted to set track bit ' //
     1             'rate higher than 8 Mbps for inappropriate format.' )
               CALL ERRSET( KS )
            END IF
C
C           Test to look for too much bandwidth for stations of reduced
C           capability (single drive) removed.  Was a tape only test.
C           July 23, 2010  RCW.
C
         END IF
      END DO
C
C     Now set formats for setups only used in non-recording scans.
C     These will basically be dummy values, but set anyway.  Try
C     to match the fan-outs for following scans.
C
      CALL SETNOREC( NEEDFMT )
C
C     This is by no means done.
C
  999 CONTINUE
      RETURN
      END

