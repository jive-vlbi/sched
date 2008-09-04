      SUBROUTINE TPS2( ISCN, ISTA, LASTISCN )
C
C     Obsolete - only called if tapes in use.
C
C     version with small change by Huib 21/12/00
C
C     Routine for SCHED called by TPTAPE that schedules S2 tapes.
C     The number of groups of drives is in TAPEMODE.
C     An individual S2 recorder only makes one pass.
C     The IPASS gives which group to be on.
C     Pay attention to REWIND, but not FASTFOR or REVERSE.  REWIND
C     will be equivalent to TAPE and will not actually trigger a
C     rewind, as such, but just a tape change.
C
C     The tape footages will be for start and stop of the "scan", 
C     TSTARTJ and TSTOPJ.  It will be arranged that the tape will
C     always be running at least 2.5 minutes before the start time
C     and 0.1 minutes after the stop time.  Gaps between scans
C     can be smaller than 3 minutes if the tape does not stop.
C
C     The routine is configured to do the next scan for any station
C     so that it could become part of the optimization proceedures
C     if it is desired to adjust lengths of, for example, first 
C     passes on a tape.  It also might be called twice for a scan
C     if coordinated tape changes are desired.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
C     IPASS   is the current pass.
C     TPCFOOT is current tape footage.
C     TPCDRIV is current drive.
C     TPCLEN  is length of tape at current station.
C     SCNFOOT is the feet of tape to be used by this scan
C     TPFOOT1 is the tape footage at the start of the scan.
C     TPFOOT2 is the tape footage at the end of the scan.
C     LSTOPJ  is the stop time of the last scan.
C     EXPTAPE is keeps track of the number of tapes used at the site.
C             Partial passes are not taken into account.
C     LTSTOP  is the last time the tape was moving.
C     LTSCN   is the last scan the moved the tape.
C     
C
      INTEGER            ISCN, ISTA, LASTISCN(MAXSTA)
C
      REAL               TPCLEN, SCNFOOT, TPCFOOT, SPEED
      INTEGER            LTPMODE(MAXSTA), KSCN
      INTEGER            TPMODMIN(MAXSTA), JSCN, JS, LTSCN(MAXSTA)
      INTEGER            YEAR, DAY
      DOUBLE PRECISION   LSTOPJ, LTSTOP(MAXSTA), TAVAIL, TCHGMIN, TIME
      DOUBLE PRECISION   ONESEC, TBEFORE, TAFTER
      LOGICAL            FIRSTS, TOOLONG, TWARN, REQTAPE
      CHARACTER          TTIME*9, TFORM*15
C
C     I'm taking a guess that 4 minutes are required for a tape
C     change.  3 minutes for the 2.5 min prestart and 0.1 min trailer
C     plus a minute to change all the tapes.
C
      PARAMETER          (TCHGMIN = 4.D0/(24.D0*60.D0))
      PARAMETER          (ONESEC = 1.D0 / 86400.D0 )
      PARAMETER          (TBEFORE = 2.5D0/(24.D0*60.D0))
      PARAMETER          (TAFTER = 0.1D0/(24.D0*60.D0))
      SAVE               LTPMODE, TPMODMIN, TWARN, LTSCN, LTSTOP
      SAVE               LSTOPJ
C
C     Items that get packed into the TPDAT array.
C
      INTEGER          TPCDIR, TPCINDX, TPCHEAD, IPASS, TPCDRIV
      LOGICAL          DOTAPE, DOREW, DOFAST
C
C  --------------------------------------------------------------------
      FIRSTS = LASTISCN(ISTA) .EQ. 0
      IF( DEBUG .AND. FIRSTS ) CALL WLOG( 0, 'TPS2: Station start.' )
C
      DOTAPE = .FALSE.
      LS = NSETUP( ISCN, ISTA )
      TPCLEN  = TPLENG(ISTA)
C     
C     On first scan, initialize everything.
C     
      IF( FIRSTS ) THEN
C     
         EXPTAPE(ISTA) = 0.0
         TPCFOOT = 0.0
         LSTOPJ  = STOPJ(ISCN) - 0.5
         LTPMODE(ISTA) = TAPEMODE(LS)
         TAPES(ISTA)  = 1
         PASSES(ISTA) = 1
         LTSTOP(ISTA) = LSTOPJ
         LTSCN(ISTA)  = 0
         IPASS        = 1
C     
C        Find minimum TAPEMODE for the Romney tape waster mode.
C        Look through the input scans 1-NSCANS.  At this stage,
C        this should be ok wrt optimizations that are going on.
C        STASCN for the first scans up to this point may have
C        been modified, but all future scans involving this station
C        should be represented.
C     
         TPMODMIN(ISTA) = 100
         DO JSCN = 1, NSCANS
            IF( STASCN(JSCN,ISTA) ) THEN
               JS = NSETUP(JSCN,ISTA)
               TPMODMIN(ISTA) = MIN( TPMODMIN(ISTA), TAPEMODE(JS) )
            END IF
         END DO
         TWARN = .TRUE.
C     
      ELSE
C     
C        After the first scan, get the tape situation from the last 
C        scan.  Some are dummies.
C     
         CALL TPPACK( 'UNPACK', TPDAT(1,LASTISCN(ISTA),ISTA),
     1              DOTAPE, DOFAST, DOREW, IPASS, TPCDRIV, TPCDIR,
     2              TPCINDX, TPCHEAD )
         LSTOPJ = STOPJ(LASTISCN(ISTA))
         TPCFOOT = TPFOOT2(LASTISCN(ISTA),ISTA)
      END IF
C     
C     Now process this scan.
C     
C     Warn if TAPEMODE changes for the station.
C     
      IF( TAPEMODE(LS) .NE. LTPMODE(ISTA) .AND. TWARN .AND. 
     1    FORMAT(LS) .NE. 'NONE' ) THEN
         CALL WLOG( 1, 'TPS2: TPMODE changing.  Tape will not '//
     1       'be used efficiently. ' )
         TWARN = .FALSE.
      END IF
C     
C     SPEED is the "normal" speed in inches per second 
C     It is derived from the setup file and the tapeini density
C     specification.
C     
      IF( DENSITY(ISTA) .EQ. 'H' ) THEN
         SPEED = SPEEDH(LS)
      ELSE 
         SPEED = SPEEDL(LS)
      END IF
C     
C     Get the footage that will be used by this scan and determine
C     if the scan will fit on this pass.
C
C     If NORECORD was specified, assume that the tape will not move.
C     
      IF( NOREC(ISCN) ) THEN
         SCNFOOT = 0.0
         TOOLONG = .FALSE.
      ELSE
C     
C        For S2, will need TBEFORE before and TAFTER after each
C        scan, unless the tape does not stop.  The previous scan
C        footage will actually be for the stop time of that scan
C        so there might be up to TBEFORE+TAFTER  of motion before the 
C        scan start.  Make sure there is room for 0.5 min after.
C        Don't worry here that the first scan only need be within
C        TBEFORE min of tape start - that scan will always fit.
C        SCNFOOT is the footage needed from the previous scan (not tape)
C        stop time to the scan stop time of the current scan.
C
         SCNFOOT = ( SPEED / 12.0 ) * 86400.D0 * ( DUR(ISCN) + 
     1      MIN( STARTJ(ISCN) - LTSTOP(ISTA), TBEFORE + TAFTER ) )
C     
C        Require that there be TAFTER time available after the last
C        scan.
C
         TOOLONG = TPCFOOT + SCNFOOT .GT. TPCLEN - 
     1       TAFTER* 86400.0 * SPEED / 12.0
C     
      END IF
C     
C     Deal with switch to next group of drives.
C     
      IF( TAPE(ISCN,ISTA) .OR. REWIND(ISCN,ISTA) .OR. TOOLONG ) THEN
         IPASS = IPASS + 1
         TPCFOOT = 0.0
         PASSES(ISTA) = PASSES(ISTA) + 1
      END IF
C     
C     Deal with tape changes (can be triggered by REWIND).
C     
      IF( TAPE(ISCN,ISTA) .OR. IPASS .GT. TPMODMIN(ISTA) ) THEN
         DOTAPE = .TRUE.
         IPASS = 1
         TAPES(ISTA) = TAPES(ISTA) + 1
         TPCFOOT = 0.0 
      ELSE
         DOTAPE = .FALSE.
      END IF
C     
C     Indicate which group of tapes to use.  This is simply done
C     by setting the current index to the pass. A different tape
C     drive is required when IPASS/TAPEMODE goes over STNDRIV/8 
C     (one standard S2 recerder has 8 transports) 
C     Here a "drive" is being treated as a rack of 8 transports
C     while the "index" position indicates where you are in the
C     rack.  It's like heads on the VLBA/MkIV drives, but each
C     head has it's own rack.
C     
      TPCDRIV =  1 + ( IPASS - 1 ) * STNDRIV(STANUM(ISTA)) / 
     1           ( TPMODMIN(ISTA) * 8 )
      TPCINDX = IPASS
C     
C     Get footage at the scan start and stop times.
C     The tape will actually start 2.5 minutes before or in the
C     previous scan or its 30 second tail, and stop 30 seconds
C     after the scan end (or keep going to the next scan).
C     
      IF( TPCFOOT .EQ. 0.0 ) THEN
         TPFOOT1(ISCN,ISTA) = 86400.0 * TBEFORE * SPEED / 12.0
      ELSE
         TPFOOT1(ISCN,ISTA) = TPCFOOT + ( 86400.0 * SPEED / 12.0 ) * 
     1      MIN( STARTJ(ISCN) - LTSTOP(ISTA), TBEFORE + TAFTER )
      END IF
C     
      TPFOOT2(ISCN,ISTA) = TPFOOT1(ISCN,ISTA) + 
     1        DUR(ISCN) * 86400.0 * SPEED / 12.0
C     
C     Check for adequate time for a tape change.  Take into account
C     any previous NOREC scans.  We can't look ahead because of
C     the possibility that we are in an optimizing situation and
C     the future scans are not yet known.
C     
      IF( DOTAPE ) THEN
         IF( NOREC(ISCN) ) THEN
            TAVAIL = STOPJ(ISCN) - LTSTOP(ISTA)
         ELSE
            TAVAIL = STARTJ(ISCN) - LTSTOP(ISTA)
         END IF
         IF( TAVAIL .LT. TCHGMIN - ONESEC .AND. 
     1       TPMODMIN(ISTA) .EQ. 1 ) THEN
            CALL WLOG( 1, ' ' )
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F6.2, 2A )' )
     1           'TPS2: *** WARNING *** ', TCHGMIN * 24.D0 * 60.D0, 
     2           ' minutes needed to change tapes at S2 stations ',
     3           ' without'
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, '                       data loss ' //
     1           'while using all tapes.' )
C     
            MSGTXT = ' '
            CALL TIMEJ( STARTJ(ISCN), YEAR, DAY, TIME )
            TTIME = TFORM( TIME, 'T', 0, 2, 2, '::@' )
            WRITE( MSGTXT, '(4A, I3, 3A, F6.2, A )' )
     1         '                     ', 'Change at ', STANAME(ISTA), 
     2         ' on day ', DAY, ' at ', TTIME, ' has only a ',
     3         TAVAIL * 24.D0 * 60.D0, ' minute gap.'
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1, ' ' )
C     
         END IF
      END IF
C     
C     Put the tape change request earlier if possible.
C     Don't do when TAPESYNC is requested - this could interact
C     with SCHOPT in weird ways.
C     Use REQTAPE to avoid duplicate tape change requests.
C     
      REQTAPE = DOTAPE
      IF( DOTAPE .AND. .NOT. TAPESYNC .AND. 
     1    LTSCN(ISTA) .LT. ISCN-1 ) THEN
         DO KSCN = LTSCN(ISTA) + 1, ISCN - 1
            IF( STASCN(KSCN,ISTA) ) THEN
               CALL TPPACK( 'PACK', TPDAT(1,KSCN,ISTA), REQTAPE, 
     1             .FALSE., .FALSE., IPASS, TPCDRIV, 1, TPCINDX, 1 )
               REQTAPE = .FALSE.
            END IF
         END DO
      END IF
C     
C     Fill the TPDAT array.
C     
      CALL TPPACK( 'PACK', TPDAT(1,ISCN,ISTA), REQTAPE, 
     1           .FALSE., .FALSE., IPASS, TPCDRIV, 1, TPCINDX, 1 )
C     
C     Record this scan if it moved the tape.
C     
      IF( DOTAPE .OR. .NOT. NOREC(ISCN) ) THEN
         LTSTOP(ISTA) = STOPJ(ISCN)
         LTSCN(ISTA)  = ISCN
      END IF
C     
      RETURN
      END







