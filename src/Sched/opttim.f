      SUBROUTINE OPTTIM( LASTISCN, ISCN, ADJUST )
C
C     Routine that adjust the time of optimized scans
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
      INTEGER          LASTISCN(MAXSTA), ISCN, ISTA, LSCN, I
      DOUBLE PRECISION LASTTIME, TIME1J, TIME2J, T_AVAIL
      DOUBLE PRECISION MAXLASTT, TOLER, TBEGSRT(MAXSTA), DTEMP
      DOUBLE PRECISION SSTIME
      LOGICAL          ADJUST, ALL0
      PARAMETER        (TOLER=ONESEC/1000.D0)
C --------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. 3 ) CALL WLOG( 0, 'OPTTIM: Starting.' )
      MAXLASTT = -99.D9
      LSCN = 0
C
      IF( ADJUST ) THEN
C
C        Get the start time based on previous scans and on slews.  This
C        can use dwell or dur.
C
         TIME1J = -99.D9
         ALL0 = .TRUE.
C
C        Initialize the sorted stop times array used when not waiting
C        for NOWAIT antennas.
C
         DO I = 1, NOWAIT(ISCN) + 1
            TBEGSRT(I) = 0.D0
         END DO
C
C        Loop through the stations getting the most recent stop time.
C        Use the schedule time if there are no LASTISCN's.  That would
C        normally be the first scan which will trigger ALL0 and be handled
C        separately anyway.  Allow for GAP.
C
         SSTIME = 0.D0
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) .AND. LASTISCN(ISTA) .NE. 0) THEN
               SSTIME = MAX( SSTIME, STOPJ(LASTISCN(ISTA)) + GAP(ISCN) )
            END IF
         END DO
         IF( SSTIME .EQ. 0.D0 ) SSTIME = STARTJ(ISCN)
C
C        Loop through stations checking slews to get the time to start.
C
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) .AND. LASTISCN(ISTA) .NE. 0) THEN
C
C              Note that OPTGEO does or does not take into account 
C              slews, depending on scheduling type (DUR or DWELL), 
C              when setting T_AVAIL
C
               CALL OPTGEO( ISCN, ISTA, SSTIME, 
     1                      LASTISCN(ISTA), LASTTIME, T_AVAIL )
C
C              With dwell scheduling, you can specify to wait for all
C              but the last NOWAIT antennas.  This help deal with slow,
C              but sensitive antennas and with sources near zenith.
C              Only do the math if NOWAIT is zero.
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
C                       Now, if NOWAIT is more than zero, push the new 
C                       time up the stack to put in it's proper place.
C
                        IF( NOWAIT(ISCN) .GE. 1 ) THEN
                           DO I = NOWAIT(ISCN), 1, -1
                              IF( TBEGSRT(I+1) .GT. TBEGSRT(I) ) THEN
                                 DTEMP = TBEGSRT(I)
                                 TBEGSRT(I) = TBEGSRT(I+1)
                                 TBEGSRT(I+1) = DTEMP
                              END IF
                           END DO
                        END IF
                     END IF
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
C              Save the last stop time seen at any station in the scan.
C              This will be used in applying GAP or dealing with non-dwell
C              scheduling.
C
               IF( LASTTIME .GT. MAXLASTT ) THEN
                  MAXLASTT = LASTTIME
                  LSCN = LASTISCN(ISTA)
               END IF
               ALL0 = .FALSE.
            END IF
         END DO
C
C        We're not done yet for the case with DWELL and NOWAIT as we needed
C        the sorted list of start times.  No pick the one we want.
C        Note that TBEGSRT(1) will be non-zero if and only if we want to be
C        doing this so we don't need to test DWELL etc.
C
         IF( TBEGSRT(1) .NE. 0.D0 ) THEN
            TIME1J = TBEGSRT(NOWAIT(ISCN)+1)
C
C           See if any other start times work (like the number of 
C           stations in the scan was less than NOWAIT).
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
C        Set time if this is the first scan for all antennas.
C        I think this covers any remaining possible cases.
C
         IF( ALL0 ) THEN
            TIME1J = STARTJ(ISCN)
            MAXLASTT = STARTJ(ISCN) - GAP(ISCN)
         END IF
C
C        So far, the time has been set based on slew time.  Now treat
C        GAP as the minimum interval to the next scan, which may require
C        moving the start to later.
C
         TIME1J = MAX( TIME1J, MAXLASTT + GAP(ISCN) )
C    
C        Set the scan stop time based on the input scan duration.
C        But there are circumstances when the stop time is fixed
C        when the start time is not (STOP specified).  STOP is
C        also implicitly fixed with START+DUR, so the only case
C        when STOP can float is when START and STOP are not specified.
C
         IF( DURONLY(ISCN) .LE. 1 ) THEN
            TIME2J = TIME1J + DUR(ISCN)
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

