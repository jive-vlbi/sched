      SUBROUTINE OPTTIM( LASTLSCN, ISCN, ADJUST )
C
C     Routine that adjust the time of optimized scans.  
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
C     to whether the start time can be altered.  DURONLY determines
C     whether to adjust the stop time.
C
C     Adjust the start time by PRESCAN (regardless of ADJUST), being 
C     careful not to overlap the previous scan.
C
C     SSTIME is the time for which to do the geometry calculation.
C     it will be set to the last stop time for any station scheduled
C     to be in the scan.  Previously, this just used STARTJ(ISCN), but
C     that turned out to be too far off for dwell time scheduling.  
C     We got away with that for a long time because OPTTIM was called
C     in OPTDWELL (now gone) and later in SCHOPT) so it iterated to 
C     a good result.
C
      INCLUDE 'sched.inc'
C
      INTEGER          LASTLSCN(*), LASTISCN(MAXSTA)
      INTEGER          ISCN, ISTA, LSCN, I
      DOUBLE PRECISION LASTTIME, TIME1J, TIME2J, T_AVAIL, TIME1K
      DOUBLE PRECISION MAXLASTT, TOLER, TBEGSRT(MAXSTA+1), DTEMP
      DOUBLE PRECISION SSTIME
      LOGICAL          ADJUST, ALL0, USETIME
      PARAMETER        (TOLER=ONESEC/1000.D0)
C --------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. 3 ) CALL WLOG( 0, 'OPTTIM: Starting.' )
      MAXLASTT = -99.D9
      LSCN = 0
C
C     Transfer LASTLSCN to LASTISCN.  GEOCHK tries to tell this routine
C     to use the current scan time by setting all LASTISCNs to -1.
C     This used to be zero and interacted properly with the way this 
C     routine dealt with first scans.  But that proved to be flawed
C     and things got more complicated (see ALL0 segment below).
C
      USETIME = .FALSE.
      DO ISTA = 1, NSTA
         IF( LASTLSCN(ISTA) .EQ. -1 ) USETIME = .TRUE.
         LASTISCN(ISTA) = MAX( LASTLSCN(ISTA), 0 )
      END DO
C   
C
      IF( ADJUST ) THEN
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
C        of a previous scan for the antennas in this scan.
C        Use the schedule scan time if there are no LASTISCN's.  That 
C        would normally be the first scan which will trigger ALL0 and 
C        be handled separately anyway.  Allow for GAP.
C
         SSTIME = 0.D0
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) .AND. LASTISCN(ISTA) .NE. 0) THEN
               SSTIME = MAX( SSTIME, STOPJ(LASTISCN(ISTA)) + GAP(ISCN) )
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
                     T_AVAIL = MAX( T_AVAIL, 
     1                   STOPJ(LASTISCN(ISTA)) + GAP(ISCN) )
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
C        Jan. 23, 2012.  Change to use the first scan start time
C        even if this was not the first scan.  We are running into
C        problems attempting to schedule different antennas 
C        differently at the start of an observation.  The VLA dummy
C        scans are the stand-out culprit.  I think using this scan's
C        gap is appropriate.
C
C        But that had a further complication.  Routine GEOCHK which
C        is part of the DELZN section generation tries to tell this
C        routine to use the current scan time rather than the scan 1
C        time by setting all LASTISCNs to -1.  That was trapped
C        above and USETIME was set.
C
         IF( ALL0 ) THEN
            IF( USETIME ) THEN
               TIME1J = STARTJ(ISCN)
               MAXLASTT = STARTJ(ISCN) - GAP(ISCN)
            ELSE 
               TIME1J = STARTJ(1)
               MAXLASTT = STARTJ(1) - GAP(ISCN)
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
C        down.  Do the same for TIME1K
C
         TIME1J = MAX( TIME1J, MAXLASTT + GAP(ISCN) )
         TIME1K = MAX( TIME1K, MAXLASTT + GAP(ISCN) )
C    
C        Set the scan stop time based on the input scan duration.
C        But there are circumstances when the stop time is fixed
C        when the start time is not (STOP specified).  STOP is
C        also implicitly fixed with START+DUR, so the only case
C        when STOP can float is when START and STOP are not specified.
C
C        At this point, also take into account MINDW - the minimum 
C        dwell time for all antennas.
C
         IF( DURONLY(ISCN) .LE. 1 ) THEN
            TIME2J = TIME1J + DUR(ISCN)
            IF( NOWAIT(ISCN) .GE. 1 .AND. MINDW(ISCN) .GT. 0.D0 ) THEN
               TIME2J = MAX( TIME2J, TIME1K + MINDW(ISCN) ) 
            END IF
         ELSE
            TIME2J = STOPJ(ISCN)
         END IF
C
C     Deal with scans we're not allowed to adjust.
C
      ELSE
         TIME1J = STARTJ(ISCN)
         TIME2J = STOPJ(ISCN)
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) .AND. LASTISCN(ISTA) .GT. 0 ) THEN
               IF( STOPJ(LASTISCN(ISTA)) .GT. MAXLASTT ) THEN
                  MAXLASTT = STOPJ(LASTISCN(ISTA)) 
                  LSCN = LASTISCN(ISTA)
               ENDIF
            END IF
         END DO
C
C        Test bad specified times (overlapped scans).
C
         IF( TIME1J .LT. MAXLASTT - TOLER .AND. 
     1       OPTMODE .NE. 'UPTIME' ) THEN
            WRITE( MSGTXT, '( 2A, I5 )' )
     1          'OPTTIM: Specified start time before last scan ',
     2          'stop time in scan: ', ISCN
            CALL WLOG( 1, MSGTXT )
            CALL PRTSCN( LSCN )
            CALL PRTSCN( ISCN )
            CALL ERRLOG( 'OPTTIM: Check scan times and days.' )
         END IF
      END IF
C
C     Adjust the start time for PRESCAN for all cases.  
C     Note that PRESCAN can be of either sign.  If it is negative,
C     it should not be allowed to back the scan up over the end of
C     the preceeding scan.  It differs from GAP in that it can be
C     shortened if it is longer than the interval between scans as
C     set so far.  Recall that PRESCAN was meant for prestarting tapes
C     and is now considered an obsolete parameter.
C
C     Don't mess up the UPTIME optimization mode in the process.  That
C     mode can have backward time jumps.
C
      IF( OPTMODE .NE. 'UPTIME' ) THEN
         TIME1J = MAX( MAXLASTT, TIME1J + PRESCAN(ISCN) )
      ELSE
         TIME1J = TIME1J + PRESCAN(ISCN)
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
         CALL PRTSCN( ISCN )
         CALL ERRLOG( 'OPTTIM: Check scan times and days.' )
      END IF
      IF( STOPJ(ISCN) - STARTJ(ISCN) .LT. ONESEC ) THEN
         WRITE( MSGTXT, '( A, I5, A )' )
     1       'OPTTIM: Adjusted times make scan number', ISCN, 
     2       ' shorter than one second long. '
         CALL WLOG( 1, MSGTXT )
         CALL PRTSCN( ISCN )
         CALL ERRLOG( 'Check scan times and especially PRESCAN.' )
      END IF
C
      RETURN
      END

