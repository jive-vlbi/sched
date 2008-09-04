      SUBROUTINE TPSCH( ISCN, ISTA, LASTISCN )
C
C     Obsolete - only called if tapes in use.
C
C
C     Routine for SCHED called by TPTAPE that schedules VLBA and
C     Mark IV tapes.  This routine gets things started and does
C     most of the work for the non-automatic tape handling case.
C     TPAUTO is called part way through for the autotape case.  See
C     the lead comments for that routine for descriptions of the
C     desired behavior for automatic tape handling.
C
C     The routine is configured to do the next scan for any station
C     so that it could become part of the optimization proceedures
C     if it is desired to adjust lengths of, for example, first 
C     passes on a tape.  It can also be run twice on a scan, for
C     example when SCHOPT determines that a forced tape change is
C     desired.
C
C     The TAPEINI file provides the necessary information on the tape
C     conditions at the start of an experiment including the number of
C     the tape drive in use, the index position to start with, and the
C     length of the tapes in use at the station.  Note that the
C     defaults will be good for Mark III, but the detailed information
C     is needed for scheduling VLBA observations with staggared tape
C     starts and multiple experiments on a tape.
C     
C     The tape position will be indicated by the tape footage,
C     direction, and head index position.
C     
C     A tape will be reversed at the start of any scan that will
C     not finish before the end of tape is reached.
C     
C     The heads will be moved about in a manner appropriate to the
C     requested mode.
C     
C     See the help file for more information on restrictions.
C     
C     Wideband system.
C     
C     IPASS   is the current pass.
C     TPCDIR  is current tape direction ( 1=>forward, -1=>reverse )
C     TPCINDX is current head index position.
C     TPCFOOT is current tape footage.
C     TPCHEAD is current head group.
C     TPCDRIV is current drive.
C     TPCLEN  is length of tape at current station.
C     SCNFOOT is the feet of tape to be used by this scan
C     TPFOOT1 is the tape footage at the start of the scan.
C     TPFOOT2 is the tape footage at the end of the scan.
C     LSTOPJ  is the stop time of the last scan.
C     EXPTAPE is keeps track of the number of tapes used at the site
C                when autoallocate is requested.
C     LTSTOP  is the last time the tape was moving.
C     LTSCN   is the last scan the moved the tape.
C     
C     PRESTART is the time before the nominal start time that the
C              tape actually starts moving for this scan - usually
C              the stop time of a close previous scan when SCHED
C              does not want to stop the tape.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
C     TPTOL is the tolerance allowed for scans to run past the nominal
C     end of a Mark III or VLBA tape in feet.
C
C     SLOP: Before a turnaround, if within SLOP of the end, stay put to
C     allow a readback test.  If further in, space to end if possible.
C
      REAL               TPCLEN, SCNFOOT, TPCFOOT, SPEED
      REAL               TPTOL, SLOP
      INTEGER            ISCN, ISTA, LASTISCN(MAXSTA), KSCN
      INTEGER            LTPCDIR
      INTEGER            TPMODMIN(MAXSTA), JSCN, JS, LTSCN(MAXSTA)
      INTEGER            YEAR, DAY
      DOUBLE PRECISION   LSTOPJ, LTSTOP(MAXSTA), TAVAIL
      DOUBLE PRECISION   TCHGMIN, TIME, ONESEC, NEWTPS
      LOGICAL            FIRSTS, TOOLONG, TWARN, REQTAPE
      CHARACTER          TTIME*9, TFORM*15
      PARAMETER          (TCHGMIN = 15.D0/(24.D0*60.D0))
      PARAMETER          (ONESEC = 1.D0 / 86400.D0 )
      SAVE               TPMODMIN, TWARN, LTSCN, LTSTOP
      SAVE               LSTOPJ
C
C     Items that get packed into the TPDAT array.
C
      INTEGER          TPCDIR, TPCINDX, TPCHEAD, IPASS, TPCDRIV
      LOGICAL          DOTAPE, DOREW, DOFAST
C  ---------------------------------------------------------------------
      DOTAPE = .FALSE.
      FIRSTS = LASTISCN(ISTA) .EQ. 0
      IF( DEBUG .AND. FIRSTS ) CALL WLOG( 0, 'TPSCH: Station start: '
     1     // STATION(STANUM(ISTA)) )
      LS = NSETUP( ISCN, ISTA )
      TPCLEN  = TPLENG(ISTA)
C     
C     Do not try to space to end of tape.  With speed up factors
C     greater than one, this can mess up the ability to sync since
C     the correlator may not be able to get to the start of data.
C     
      SLOP  = TPCLEN * 1.0
      TPTOL = TPCLEN * 0.005
C     
C     On first scan, initialize everything.
C     
      IF( FIRSTS ) THEN
C     
         EXPTAPE(ISTA) = 0.0
         TPCDIR  = 1
         LTPCDIR = 1
         TPCFOOT = 0.0
         TPCDRIV = TPSDRIV(ISTA)
         LSTOPJ  = STOPJ(ISCN) - 0.5
         PASSES(ISTA) = 1
         LTSTOP(ISTA) = LSTOPJ
         LTSCN(ISTA)  = 0
         IF( TWOHEAD .AND. STNDRIV(STANUM(ISTA)) .GE. 2 ) THEN
            TAPES(ISTA) = 2
         ELSE
            TAPES(ISTA) = 1
         END IF
C     
C        Find minimum TAPEMODE for the Romney tape waster mode.
C        Look through the input scans 1-NSCANS.  At this stage,
C        this should be ok wrt optimizations that are going on.
C        STASCN for the first scans up to this point may have
C        been modified, but all future scans involving this station
C        should be represented.  Don't count non-recording scans.
C
C        I'm not sure all this is needed in view of the efforts
C        made earlier to ensure that all setups that produce 
C        recordings at a station have the same TAPEMODE.  But all
C        this gets irrelevant once we switch to disk, so don't
C        really worry about it for now.  The earlier efforts did
C        not worry about the non-recording scans.
C        (note added without code changes, June 2005  RCW)
C     
         TPMODMIN(ISTA) = 100
         DO JSCN = 1, NSCANS
            IF( STASCN(JSCN,ISTA) .AND. .NOT. NOREC(JSCN) ) THEN
               JS = NSETUP(JSCN,ISTA)
               TPMODMIN(ISTA) = MIN( TPMODMIN(ISTA), TAPEMODE(JS) )
            END IF
         END DO
         TWARN = .TRUE.
C     
C        Assume that a program starts at the beginning of tape
C        with a pass in the forward direction.  With the 'new'
C        track sequence, this means that the starting head
C        index position must be odd.  The number of passes is set
C        by the minimum TPMODE in the experiment.
C     
         IF( MOD( TPSINDX(ISTA), 2 ) .EQ. 0 ) THEN
            WRITE( MSGTXT, '( A )' )
     1          '  Start head index position must be '//
     2          'odd for forward pass.  Adding one.'
            CALL WLOG( 0, MSGTXT )
            IPASS = TPMODMIN(ISTA) * TPSINDX(ISTA) + 1
         ELSE
            IPASS = TPMODMIN(ISTA) * ( TPSINDX(ISTA) - 1 ) + 1
         END IF
C     
      ELSE
C     
C        Get the tape situation from the last scan.
C        Some are dummies.  Not called when FIRSTS.
C     
         CALL TPPACK( 'UNPACK', TPDAT(1,LASTISCN(ISTA),ISTA),
     1              DOTAPE, DOFAST, DOREW, IPASS, TPCDRIV, TPCDIR,
     2              TPCINDX, TPCHEAD )
         LSTOPJ = STOPJ(LASTISCN(ISTA))
         LTPCDIR = TPCDIR
         TPCFOOT = TPFOOT2(LASTISCN(ISTA),ISTA)
      END IF
C     
C     Now process this scan.
C     
C     Warn if TAPEMODE changes for the station.  Actually only comparing
C     with the minimum TPMODE, but that is ok.  Ignore FORMAT=NONE scans.
C     Do not ignore norecord scans that are not FORMAT=NONE because they
C     can mess up the tape autoallocation.
C     
      IF( TAPEMODE(LS) .NE. TPMODMIN(ISTA) .AND. TWARN .AND. 
     1    FORMAT(LS) .NE. 'NONE' ) THEN
         IF( AUTOALOC(ISTA) ) THEN
            CALL WLOG( 0,'TPSCH: TPMODE changing with autoallocation.' )
            CALL WLOG( 0, '       Tape use predictions will most likely'
     1             // 'be wrong (probably overestimated).' )
         ELSE
            CALL WLOG( 0, 'TPSCH: TPMODE changing.  Tape will not '//
     1                'be used efficiently. ' )
         END IF
         TWARN = .FALSE.
      END IF
C     
C     SPEED is the "normal" speed in inches per second 
C     It is derived from the setup file and the tapeini density
C     specification.
C     
      IF( DENSITY(ISTA).EQ. 'H' ) THEN
         SPEED = SPEEDH(LS)
      ELSE 
         SPEED = SPEEDL(LS)
      END IF
C     
C     Get the footage that will be used by this scan and determine
C     if the scan will fit on this pass.
C     
C     Allow a little slop - ie. allow a scan to run out of tape very
C     near the end.  This keeps the program out of trouble in cases
C     where no rewind or fastforward time is allowed at the ends of
C     passes.  Note that 135 ips is about 11 feet per second.
C     
C     If NORECORD was specified, assume that the tape will 
C     not move (although a REWIND or FASTFOR will be obeyed).
C     
      IF( NOREC(ISCN) ) THEN
         SCNFOOT = 0.0
         TOOLONG = .FALSE.
      ELSE
         SCNFOOT = ( STOPJ(ISCN) - STARTJ(ISCN) ) * 86400.0 * 
     1         SPEED / 12.0
         SCNFOOT = SCNFOOT + TPSTART(ISCN,ISTA) * 86400.0 *
     1           SPEED / 12.0 
         TOOLONG = ( TPCDIR .EQ. 1 .AND. 
     1      TPCFOOT + ( SCNFOOT - TPTOL ) .GT. TPCLEN ) .OR.
     2      ( TPCDIR .EQ. -1 .AND. TPCFOOT .LT. ( SCNFOOT - TPTOL ) )
      END IF
C
C     Now split off the automatic tape handling.
C
      IF( AUTOALOC(ISTA) ) THEN
C
         CALL TPAUTO( ISCN, ISTA, FIRSTS, IPASS, 
     1       TPMODMIN, SCNFOOT, TPCFOOT, TPCDIR ) 
C
      ELSE
C        
C        Deal with reversals and tape changes.  
C        
         DOTAPE = .FALSE. 
         DOREW  = .FALSE.
         DOFAST = .FALSE.
C        
C        Deal with a requested tape change and other reversals etc.
C        
         IF( TAPE(ISCN,ISTA) ) THEN
            DOTAPE = .TRUE.
            IPASS = 1
            TPCDIR = 1
            TPCFOOT = 0.0
C
         ELSE IF( REWIND(ISCN,ISTA) .OR. FASTF(ISCN,ISTA) .OR.
     1            REVERSE(ISCN,ISTA) .OR. TOOLONG ) THEN
C
            CALL TPREV( ISCN, ISTA, LSTOPJ, TPCLEN, TPMODMIN, 
     1                  TOOLONG, DOTAPE, DOREW, DOFAST,
     2                  IPASS, TPCDIR, TPCFOOT, SCNFOOT, SLOP, TPTOL )
C        
         END IF
C
C        If there will be a new tape or a change in direction, don't
C        try for continuous tape motion no matter how short the gap.
C        But for VEX, don't adjust TPSTART as all tapes must start
C        at the same time.
C
         IF( ( DOTAPE .OR. TPCDIR .NE. LTPCDIR ) .AND. 
     1      TPSTART(ISCN,ISTA) .NE. 0.D0 .AND. .NOT. DOVEX ) THEN
            NEWTPS = MIN( TPSTART(ISCN,ISTA), PRESTART(ISCN) )
            SCNFOOT = SCNFOOT - ( TPSTART(ISCN,ISTA) - NEWTPS ) * 
     1         86400.0 * SPEED / 12.0 
            TPSTART(ISCN,ISTA) = NEWTPS
         END IF
C        
C        Get heads index position and group for the scan.
C        
         CALL INDXHEAD( TPMODMIN(ISTA), IPASS, HEADMODE(ISTA), 
     1                  TPCINDX, TPCHEAD, SETNAME(LS) )
C        
C        Get footage before and after scan
C        
         TPFOOT1(ISCN,ISTA) = TPCFOOT
         TPFOOT2(ISCN,ISTA) = TPFOOT1(ISCN,ISTA) + SCNFOOT * TPCDIR
C        
C        Set the number of passes and tapes for the summary file and set
C        the new tape drive number.  Allow the schedule to stay on tape
C        drive TPSDRIV if STNDRIV=1.  For TWOHEAD mode (2 drives on
C        VLBA), don't switch drives (unless there are lots of them).
C        If we ever get two drive MKIV stations, this may need to 
C        change - but so will other things.
C        
         IF( DOTAPE ) THEN
            IF( TWOHEAD .AND. STNDRIV(STANUM(ISTA)) .GE. 2 ) THEN
               TAPES(ISTA) = TAPES(ISTA) + 2
            ELSE
               TAPES(ISTA) = TAPES(ISTA) + 1
            END IF
            PASSES(ISTA) = PASSES(ISTA) + 1
            TPCDRIV = TPCDRIV + 1
            IF( TWOHEAD ) THEN
               IF( TPCDRIV .GT. STNDRIV(STANUM(ISTA)) - 1 ) TPCDRIV = 1
            ELSE 
               IF( TPCDRIV .GT. STNDRIV(STANUM(ISTA)) ) TPCDRIV = 1
            END IF
            IF( STNDRIV(STANUM(ISTA)) .EQ. 1 ) 
     1         TPCDRIV = TPSDRIV(ISTA)
         ELSE IF( TPCDIR .NE. LTPCDIR ) THEN
            PASSES(ISTA) = PASSES(ISTA) + 1
         END IF
C        
C        Check for adequate time for a tape change.  Take into account
C        any previous NOREC scans.  We can't look ahead because of
C        the possibility that we are in an optimizing situation and
C        the future scans are not yet known.
C        Don't worry about this for multiple drive sites unless they
C        are in the wideband mode.
C        
         IF( DOTAPE .AND. 
     1         ( STNDRIV(STANUM(ISTA)) .LE. 1 .OR. TWOHEAD ) ) THEN
            IF( NOREC(ISCN) ) THEN
               TAVAIL = STOPJ(ISCN) - LTSTOP(ISTA)
            ELSE
               TAVAIL = STARTJ(ISCN) - LTSTOP(ISTA)
            END IF
            IF( TAVAIL .LT. TCHGMIN - ONESEC ) THEN
               CALL WLOG( 1, ' ' )
               CALL WLOG( 1,
     1           '***** WARNING ****** Inadequate tape change time at' )
C        
               MSGTXT = ' '
               CALL TIMEJ( STARTJ(ISCN), YEAR, DAY, TIME )
               TTIME = TFORM( TIME, 'T', 0, 2, 2, '::@' )
               WRITE( MSGTXT, '(3A, I3, 2A )' )
     1           '                     ', STANAME(ISTA), ' on day ', 
     2           DAY, ' at ', TTIME
               CALL WLOG( 1, MSGTXT )
C        
               MSGTXT = ' '
               IF( STNDRIV(STANUM(ISTA)) .LE. 1 ) THEN
                  WRITE( MSGTXT, '( A, F6.2, A )' )
     1              '                     Please allow ', 
     2              TCHGMIN * 24.D0 * 60.D0,
     3              ' minutes at single tape drive stations.'
               ELSE
                  WRITE( MSGTXT, '( A, F6.2, A )' )
     1              '                     Please allow ', 
     2              TCHGMIN * 24.D0 * 60.D0,
     3              ' minutes when using both drives.'
               END IF
               CALL WLOG( 1, MSGTXT )
C        
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, F6.2, A )' )
     1           '                     Schedule only has a ', 
     2           TAVAIL * 24.D0 * 60.D0, ' minute gap.'
               CALL WLOG( 1, MSGTXT )
C
               CALL WLOG( 1, '                     Ignore ' //
     1           'this if really using Mark5.' )
C        
            END IF
         END IF
C        
C        Put the tape change request earlier if possible.
C        Don't do when TAPESYNC is requested - this could interact
C        with SCHOPT in weird ways.
C        Use REQTAPE to avoid duplicate tape change requests.
C        
         REQTAPE = DOTAPE
         IF( DOTAPE .AND. .NOT. TAPESYNC .AND. 
     1       LTSCN(ISTA) .LT. ISCN-1 ) THEN
            DO KSCN = LTSCN(ISTA) + 1, ISCN - 1
               IF( STASCN(KSCN,ISTA) ) THEN
                  CALL TPPACK( 'PACK', TPDAT(1,KSCN,ISTA),
     1               REQTAPE, .FALSE., .FALSE., IPASS, TPCDRIV, TPCDIR,
     2               TPCINDX, TPCHEAD )
                  REQTAPE = .FALSE.
               END IF
            END DO
         ELSE IF( DOTAPE ) THEN
            REQTAPE = .TRUE.
         END IF
C        
C        Fill the TPDAT array.
C        
         CALL TPPACK( 'PACK', TPDAT(1,ISCN,ISTA),
     1             REQTAPE, DOFAST, DOREW, IPASS, TPCDRIV, TPCDIR,
     2             TPCINDX, TPCHEAD )
C        
C        Record this scan if it moved the tape.
C        
         IF( DOFAST .OR. DOREW .OR. DOTAPE .OR. .NOT. NOREC(ISCN) )
     1       THEN
            LTSTOP(ISTA) = STOPJ(ISCN)
            LTSCN(ISTA)  = ISCN
         END IF
C        
      END IF    ! AUTOALOC
C
      RETURN
      END







