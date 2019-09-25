      SUBROUTINE OPTTIM( LASTISCN, LASTSSCN, ISCN, ADJUST, 
     1                   USETIME, DOPRESC )
Cf2py intent(in) LASTISCN, LASTSSCN, ISCN, ADJUST, USETIME, DOPRESC
C
C     Routine that adjust the time of optimized scans.  It's main
C     use is in SCHOPT, but it is also called from GMKSCN and MAKESCN.
C
C     This is not meant to be the routine that gets the scan 
C     geometry - in fact it does not calculate geometry for DUR 
C     type scheduling (ie when ADJUST is false).  The scan geometry
C     should be calculated after OPTTIM is run because, if adjusting
C     times, it will fill the arrays with numbers meant mainly for
C     its internal use.
C
C     Note that, even with optimization, if an explicit start or
C     stop time was specified, it will be used.  ADJUST refers to
C     to whether the start time can be altered.  DURONLY also indicates
C     whether the start and/or stop can be adjusted.
C
C     If, and only if, DOPRESC is true, adjust the start time by 
C     PRESCAN (regardless of ADJUST), being careful not to overlap 
C     the previous scan.  This should only be true on the very last
C     call, or this routine tends to add extra offsets with each call.
C
C     If USETIME is true and all LASTISCNs for stations in the scan
C     are zero (indicating that this is their first scan), then use 
C     the scan time as is.  If false, use the experiment start time.
C
C     SSTIME is the time for which to do the geometry calculation.
C     it will be set to the last stop time for any station scheduled
C     to be in the scan.  Previously, this just used STARTJ(ISCN), but
C     that turned out to be too far off for dwell time scheduling.  
C     We got away with that for a long time because OPTTIM was called
C     in OPTDWELL (now gone) and later in SCHOPT) so it iterated to 
C     a good result.
C
C     LASTSSCN is the last scan for the station that is not an 
C     inserted scan (mainly pointing).  Respect GAP for these scans,
C     but not for LASTISCN.  In some calls, LASTISCN will be used
C     for LASTSSCN, but not in the final call for the overall schedule.
C
      INCLUDE 'sched.inc'
C
      INTEGER          LASTISCN(*), LASTSSCN(*)
      INTEGER          ISCN, ISTA, LSCN, I
      DOUBLE PRECISION LASTTIME, TIME1J, TIME2J, T_AVAIL, TIME1K
      DOUBLE PRECISION MAXLASTT, TOLER, TBEGSRT(MAXSTA+1), DTEMP
      DOUBLE PRECISION SSTIME
      LOGICAL          ADJUST, ALL0, USETIME, DOPRESC
      PARAMETER        (TOLER=ONESEC/1000.D0)
C --------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. 3 ) CALL WLOG( 0, 'OPTTIM: Starting.' )
      MAXLASTT = -99.D9
      LSCN = 0
C
C      Some debugging code that might be useful again some day
C      double precision start
C      integer          year1, day1
C      character        cstart*8, tform*8
C      call timej( startj(iscn), year1, day1, start )
C      cstart = tform( start, 'T', 0, 2, 2, '::@' )
C      if( iscn .gt. 170 .and. iscn .lt. 190 ) then
C       write(*,*) 'opttim:', iscn, duronly(iscn), int(gap(iscn)/onesec),
C     1     ' ',  adjust, ' ', cstart
C      end if
C
      IF( ADJUST .AND. DURONLY(ISCN) .NE. 2 .AND. DURONLY(ISCN) .NE. 3
     1    .AND. DURONLY(ISCN) .NE. 6 .AND. DURONLY(ISCN) .NE. 7 ) THEN
C
C        Get the start time based on previous scans and on slews.  This
C        can use dwell or dur.  Note that user input DWELL(1) (now in DUR 
C        logical DWELL set to whether or not it was used) was given to 
C        be the on-source time for input DWELL(2) (internal variable 
C        NOWAIT) antennas to get on source.  Input DWELL(3) (internal
C        variable MINDW) is the minimum on source time for all antennas.  
C        TIME1K collects the last antenna arrival time when worrying 
C        about allowing some to be late.
C
         TIME1J = -99.D9
         TIME1K = -99.D9
         ALL0 = .TRUE.
C
C        Initialize the sorted stop times array used when not waiting
C        for NOWAIT antennas.
C
         DO I = 1, MAXSTA+1
            TBEGSRT(I) = 0.D0
         END DO
C
C        Loop through the stations getting the most recent stop time
C        of a previous scan for the antennas in this scan.  This is
C        the earliest possible start time for the next scan.
C        Use the schedule scan time if there are no LASTISCN's.  That 
C        would normally be the first scan which will trigger ALL0 and 
C        be handled separately anyway.  Allow for GAP with LASTSSCN,
C        but not LASTISCN.
C
         SSTIME = 0.D0
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) .AND. LASTISCN(ISTA) .NE. 0) THEN
               SSTIME = MAX( SSTIME, STOPJ(LASTSSCN(ISTA)) + GAP(ISCN),
     1                       STOPJ(LASTISCN(ISTA)) )
            END IF
         END DO
C
C         IF( SSTIME .EQ. 0.D0 ) SSTIME = STARTJ(ISCN)
C        Use this instead.  But it's not used when LASTISCN is 0.
C
         IF( SSTIME .EQ. 0.D0 ) SSTIME = TFIRST
C
C        Loop through stations checking slews to get the time to start.
C
         DO ISTA = 1, NSTA
C
C           Now deal with scans that are not first.
C
            IF( STASCN(ISCN,ISTA) .AND. LASTISCN(ISTA) .NE. 0) THEN
C
C              Get the geometry for the earliest possible start time
C              which is SSTIME.
C              Note that, when setting T_AVAIL, STAGEO takes into 
C              account slews for DWELL type scheduling, but not 
C              when using DUR.  T_AVAIL is meant to be the earliest 
C              that this station can start the scan given the scheduling type.
C
               CALL STAGEO( ISCN, ISTA, SSTIME, 
     1                 LASTISCN(ISTA), LASTTIME, T_AVAIL, 'OPTTIM' )
C
C              With dwell scheduling, you can specify to wait for all
C              but the last NOWAIT antennas.  This helps deal with slow,
C              but sensitive antennas and with sources near zenith.
C              Only do the math if NOWAIT is one or more.
C
               IF( DWELL(ISCN) .AND. NOWAIT(ISCN) .GE. 1 ) THEN
                  IF( UP1(ISCN,ISTA) .EQ. ' ' .AND. 
     1                UP2(ISCN,ISTA) .EQ. ' ' .AND. 
     2                LASTISCN(ISTA) .NE. 0 ) THEN
C
                     T_AVAIL = MAX( T_AVAIL, STOPJ(LASTISCN(ISTA)),
     1                   STOPJ(LASTSSCN(ISTA)) + GAP(ISCN) )
C
C                    Add this station in its place in the list
C                    of start times sorted into inverse time order.
C                    Start by replacing the last element of TBEGSRT
C                    of interest if the new T_AVAIL is later.  Note
C                    that this works if NOWAIT(ISCN) = 0, although we
C                    will not get here in that case.
C
                     IF( T_AVAIL .GT. TBEGSRT(NOWAIT(ISCN)+1) ) THEN
                        TBEGSRT(NOWAIT(ISCN)+1) = T_AVAIL
C
C                       NOWAIT assured to be .GE. 1 above.  Push the new 
C                       time up the stack to put in it's proper place.
C                       The stack needs to be NOWAIT(ISCN) + 1 deep 
C                       because ultimately we will use that element.
C                       For simplicity, the new time is loaded into
C                       element NOWAIT(ISCN) + 2, then pushed up.
C
                        DO I = NOWAIT(ISCN), 1, -1
                           IF( TBEGSRT(I+1) .GT. TBEGSRT(I) ) THEN
                              DTEMP = TBEGSRT(I)
                              TBEGSRT(I) = TBEGSRT(I+1)
                              TBEGSRT(I+1) = DTEMP
                           END IF
                        END DO
                     END IF
C
C                    Also save the last time any station gets there.
C
                     TIME1K = MAX( TIME1K, T_AVAIL )
C
                  END IF
C
C
               ELSE IF( DWELL(ISCN) ) THEN
C
C                 Deal with Dwell scheduling without NOWAIT.  This is perhaps
C                 the dominent option.
C
                  TIME1J = MAX( TIME1J, T_AVAIL )
C
               ELSE
C
C                 Finally deal with duration scheduling.
C
                  TIME1J = MAX( TIME1J, LASTTIME )
C
               END IF
C
C              Save the last stop time of the previous scan seen at any 
C              station in the scan.  This will be used in applying GAP 
C              or dealing with non-dwell scheduling.
C
               IF( LASTTIME .GT. MAXLASTT ) THEN
                  MAXLASTT = LASTTIME
                  LSCN = LASTISCN(ISTA)
               END IF
               ALL0 = .FALSE.
            END IF
         END DO
C
C        We're not done yet for the case with DWELL and NOWAIT as we 
C        needed the sorted list of start times.  Now pick the one we want.
C        Note that TBEGSRT(1) will be non-zero if and only if we want 
C        to be doing this (or there were no stations in the scan) so 
C        we don't need to test DWELL etc.
C
         IF( TBEGSRT(1) .NE. 0.D0 ) THEN
            TIME1J = TBEGSRT(NOWAIT(ISCN)+1)
C
C           If that didn't produce a viable result, see if any other 
C           start times work (a possible cause would be that the number 
C           of stations in the scan was less than NOWAIT).  This will
C           be a rare case anyway so don't worry about the corner case 
C           where there ends up being only one station in the scan.
C        
            IF( TIME1J .EQ. 0.D0 ) THEN
               DO I = 1, NOWAIT(ISCN)
                  IF( TBEGSRT(I) .GT. 0.D0 ) THEN
                     TIME1J = TBEGSRT(I)
                  END IF
               END DO
            END IF
         END IF
C
C        Set time if this is the first scan for all antennas in 
C        the scan. I think this covers any remaining possible cases.
C        Recall that this need not be the first scan of the
C        experiment if all the included stations were not in
C        earlier scans.
C        This will also give a number for the case where no 
C        stations are up.
C
C        Jan. 30, 2012.  Change to use the first time of the project
C        for the start time when no stations were in a previous scan.
C        unless USETIME is true.  If it is true, use the time already
C        associated with the scan.  This is used by optimization 
C        routines, especially GEOCHK.
C
C        This change was provoked by undesired behavior when VLA dummy
C        scans were scheduled in parallel with other stations at the
C        start of an observation.
C
C        Feb. 8 , 2013.  No longer subtract GAP from MAXLASTT for this
C        first scan because it won't automatically be added back when
C        gap is used later.
C
         IF( ALL0 ) THEN
            IF( USETIME ) THEN
               TIME1J = STARTJ(ISCN)
               MAXLASTT = STARTJ(ISCN) - GAP(ISCN)
            ELSE 
               TIME1J = TFIRST
               MAXLASTT = TFIRST - GAP(ISCN)
            END IF
         END IF
C
C        If TIME1K did not get set, set it the same as TIME1J
C
         IF( TIME1K .LE. 0.D0 ) THEN
            TIME1K = TIME1J
         END IF
C
C        So far, the time has been set based on slew time.  Now treat
C        GAP as the minimum interval to the next scan, which may require
C        moving the start to later.  This also deals with a case where
C        TIME1J has not been set - like all antennas in the scan were
C        down.  Do the same for TIME1K.
C
C        Before Feb. 2014, GAP was zeroed after pointing scans were
C        inserted.  But this caused problems for schedules that had
C        some DWELL and some DUR/GAP scans as the DWELL scans caused
C        ADJUST to be set positive, but the intervals of the DUR/GAP
C        scans needed to be retained.  Those intervals are the
C        intervals to the last non-inserted scan (non-pointing scan). 
C        LASTSSCN was introduced to keep track of the of the original 
C        scans.
C
C        First set TIME1J and K to the earliest time the antenna can
C        do something.  Usually MAXLASTT is usually from LASTTIME from
C        STAGEO, which is normally STOPJ(LASTISCN).
C               
         TIME1J = MAX( TIME1J, MAXLASTT )
         TIME1K = MAX( TIME1K, MAXLASTT )
C
C        Now be sure we didn't get closer than GAP to the previous
C        regular (non-inserted) scan.  But don't do for pointing scans.
C
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) .AND. LASTSSCN(ISTA) .NE. 0 ) THEN
               TIME1J = MAX( TIME1J, STOPJ(LASTSSCN(ISTA)) + GAP(ISCN) )
               TIME1K = MAX( TIME1K, STOPJ(LASTSSCN(ISTA)) + GAP(ISCN) )
            END IF
         END DO
C
      ELSE
C
C        Deal with scans for which we are not allowed to adjust the 
C        start time.
C
C        Set the start time to the requested value.
C
         TIME1J = STARTJ(ISCN)
         TIME1K = STARTJ(ISCN)
C
      END IF
C
C     Set scan stop time.  This is easier.
C
C     First deal with the case of when we are allowed to adjust the
C     stop time.  This is when the STOP time was not explicitly set 
C     and has to be set with the help of the start time and duration.  
C     Note that the case of stop time implicit forced by a forced
C     start time and duration can be treated in the same way as the
C     flexible stop time - the forced start and dur ensure it comes
C     out right.  If the STOP time has been forced, just use it.
C     The value of DURONLY tells us which case we are in.
C    
C     At this point, also take into account MINDW - the minimum 
C     dwell time for all antennas.
C
      IF( DURONLY(ISCN) .LE. 3 ) THEN
         TIME2J = TIME1J + DUR(ISCN)
         IF( NOWAIT(ISCN) .GE. 1 .AND. MINDW(ISCN) .GT. 0.D0 ) THEN
            TIME2J = MAX( TIME2J, TIME1K + MINDW(ISCN) ) 
         END IF
      ELSE
         TIME2J = STOPJ(ISCN)
      END IF
C 
C     Prepare to deal with PRESCAN insertion and with a check for
C     overlapped scans.  Get the last stop time for stations in the
C     scan and preserve the scan number of that last stop time.
C     MAXLASTT and LSCN were derived before, but only for the
C     case when adjustments could be made.  Do here again to get
C     all scans.  At the previous use, there was concern about how 
C     to set first scans.  Here we are only going to worry about 
C     PRESCAN and overlaps so we be a bit simpler.
C
      MAXLASTT = -99.D9
      LSCN = 0
      DO ISTA = 1, NSTA
         IF( STASCN(ISCN,ISTA) .AND. LASTISCN(ISTA) .GT. 0 ) THEN
            IF( STOPJ(LASTISCN(ISTA)) .GT. MAXLASTT ) THEN
               MAXLASTT = STOPJ(LASTISCN(ISTA)) 
               LSCN = LASTISCN(ISTA)
            ENDIF
         END IF
      END DO
C
C     Adjust the start time for PRESCAN for all cases (ADJUST set
C     or not), but only if DOPRESC is true.  The DOPRESC is needed
C     to avoid scans marching along with successive calls to OPTTIM.
C     DOPRESC should only be set on the last call from SCHOPT.
C
C     Recall that PRESCAN is an historical parameter for moving the
C     start time away from where it might normally land.  Various
C     other parameters have taken it's place, but it might still be
C     useful for delaying scan starts for an extra amount of time when
C     using DWELL to absolutely insure there is no bad data.
C
C     Note that PRESCAN can be of either sign.  If it is negative,
C     it should not be allowed to back the scan up over the end of
C     the preceeding scan.  It differs from GAP in that it can be
C     shortened if it is longer than the interval between scans as
C     set so far and does not move the overall scan timing, just 
C     the start.
C
C     Don't mess up the UPTIME optimization mode in the process.  That
C     mode can have backward time jumps.
C
C     This works for the first scan when MAXLASTT is not set because
C     MAXLASTT will not be chosen over TIME1J+PRESCAN.
C
      IF( DOPRESC ) THEN
         IF( OPTMODE .NE. 'UPTIME' ) THEN
            TIME1J = MAX( MAXLASTT, TIME1J + PRESCAN(ISCN) )
         ELSE
           TIME1J = TIME1J + PRESCAN(ISCN)
         END IF
      END IF
C
C     Now do a sanity check.  Make sure we don't have overlapped scans.
C     Don't panic if that happens with OPTMODE=UPTIME as each source
C     will start at the experiment start time.
C
      IF( TIME1J .LT. MAXLASTT - TOLER .AND. 
     1    OPTMODE .NE. 'UPTIME' ) THEN
         WRITE( MSGTXT, '( 2A, I5 )' )
     1       'OPTTIM: Specified start time before last scan ',
     2       'stop time in scan: ', ISCN
         CALL WLOG( 1, MSGTXT )
         CALL PRTSCN( LSCN, 'OPTTIM' )
         CALL PRTSCN( ISCN, 'OPTTIM' )
         CALL ERRLOG( 'OPTTIM: Check scan times and days.' )
      END IF
C
C     Avoid crossing 0 hr when setting scan times.
C     This not needed any more.  But save the code in case
C     it ever comes up again.
C
C      DOUBLE PRECISION    SCDUR
C      IF( ADJUST ) THEN
C         IF( INT( TIME1J ) .NE. INT( TIME2J ) ) THEN
C            SCDUR = TIME2J - TIME1J
C            TIME1J = INT( TIME2J )
C            TIME2J = TIME1J + SCDUR
C         END IF
C      END IF
C
C     Now set the scan boundaries.  Once this was rounded to the 
C     nearest second, but that messes up LST schedules, especially
C     if they are meant for the VLA.
C
      STARTJ(ISCN) = TIME1J
      STOPJ(ISCN) = TIME2J
C
C     Be sure that the scan is at least 1 sec long.
C
      IF( STOPJ(ISCN) - STARTJ(ISCN) .LT. 0.D0 ) THEN
         WRITE( MSGTXT, '( A, I5 )' )
     1       'OPTTIM: Stop before Start in scan number', ISCN
         CALL WLOG( 1, MSGTXT )
         CALL PRTSCN( ISCN, 'OPTTIM' )
         CALL ERRLOG( 'OPTTIM: Check scan times and days.' )
      END IF
      IF( STOPJ(ISCN) - STARTJ(ISCN) .LT. ONESEC ) THEN
         WRITE( MSGTXT, '( A, I5, A )' )
     1       'OPTTIM: Adjusted times make scan number', ISCN, 
     2       ' shorter than one second long. '
         CALL WLOG( 1, MSGTXT )
         CALL PRTSCN( ISCN, 'OPTTIM' )
         CALL ERRLOG( 'Check scan times and especially PRESCAN.' )
      END IF
C
      RETURN
      END
