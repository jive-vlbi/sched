      SUBROUTINE WRAP( ISCN, LSCN, ISTA, T_EST1 )
C
C     Routine for SCHED that determines where in the full range of
C     azimuth for ALTAZ antennas the antenna will be.  Typically there
C     will be a range of azimuths where there are two possible antenna
C     positions (cable wrap) and which is in use depends on the 
C     recent history of scans.  This routine attempts to duplicate
C     what will happen at the antenna.  It has to worry about the
C     full time of the scan (by looking at both ends).
C
C     This routine assumes that the antenna takes the shortest time
C     slew to get to the next source without trying to optimize when
C     to slew relative to a series of scans.  The algorithm is simple
C     to try to increase the chances that it will reproduce what 
C     happens at the antennas, the antennas having been encouraged to
C     also keep it simple.  The algorithm does check that the azimuth
C     of the new source both at the start of the slew and at the end
C     of the scan are within the limits.
C
C     When called during the process of setting scan times, the 
C     stop time may get adjusted again later.  For ultimate
C     accuracy, there should be a second pass after the scan times
C     are close.  Note that using the position at the start of the 
C     slew may cause quite a large time offset if the station has 
C     not been used in a previous scan.  So that might not be
C     optimal, but I think that is what the antennas do too.
C
C     This routine will be called by STAGEO for use while optimizing
C     the pointing to take into account slew times and in SCHSRC
C     so that AZ1 and AZ2 in the main schedule variables have the
C     wrap position.  Then SLEW does not need to worry about wraps.
C     Note that STAGEO gets called by STNGEO for all antennas regardless
C     of whether they are in the scan to get the geometry in case 
C     of need to know what would have happened.  But when considering
C     the wrap, only consider the last scan the station was in.
C
C     There is a nasty case that is encountered most obviously in 
C     egplan.key where each source is "tracked" for 24 hours.  The
C     source spends a lot of time down, during which it moves a 
C     long distance in Az while not participating in scans.  The 
C     first scan after the rise has its slew parameters calculated
C     at the end of the last scan (as would happen on the antenna so
C     we can't change this).  For the wrap calculation, SCHED and 
C     the on-line system look at the azimuth at the start of the 
C     slew (end of last scan) and at the end of the actual scan,
C     which can be very far apart and have very different azimuths.
C     As far as the VLBA on-line system is concerned, there really
C     is such a long scan, although it is a "setup" scan with no
C     recording.  The effect is that CURAZ1 below will have a value
C     in the northwest (northern sources) and CURAZ2 will be in the
C     northeast.  
C
C     There are also a variety of issues when a phase reference pair
C     goes near the zenith.  See AUTODOWN for dealing with those.
C     To duplicate what happens at the VLBA antennas, the Az of the
C     new source is needed at the time the slew starts (STOPJ(LSCN)) 
C     and at the end of the scan.  What constitutes the end of the
C     scan is a bit "interesting".  For the VLBA legacy on-line system,
C     a scan is always in progress.  It can be the normal (probably 
C     recording) scan, or it can be the setup scan.  The setup scan 
C     fills the time between recordings during which the antenna might
C     be slewing, or might be sitting waiting.  The on-line system does
C     not distinguish the type of scan, so the position used for the 
C     end of scan for slew calculations may be the end of the setup,
C     which corresponds to the start of the recording scan.  This is 
C     not optimal and can lead to slews being needed at the start of 
C     the recording scan when there was plenty of time in the setup 
C     scan.
C
C     VLBA:
C
C     It was realized rather late (Jan. 8, 2014) that the algorithm
C     here is not quite the same as what is used on the VLBA.  The
C     algorithm here is the desired one and will be kept.  But the
C     setup scan confuses the issue on the VLBA.  The legacy on-line 
C     system only sees a scan.  It does not distinguish setup and 
C     recording scans.  So when it looks at the stop time, it is the 
C     end of the setup scan, which corresponds to the start of recording.  
C     Thus it will not detect a pending limit hit during recording until
C     the start of recording.  This can cause a slew during recording
C     even when there was a nice long setup during which it could 
C     have been done.
C
C     The legacy system is no longer controlling recording so there is
C     a bit more freedom to pick a solution to this issue than there 
C     was before.  In writing the crd file, I plan to eliminate the
C     setup scan if AZ1 and AZ2 flank the boundary between the ccw
C     overlap region and the normal region - ie 90 deg azimuth for
C     the VLBA - and the final AZ2 of the preceeding scan was greater
C     than an arbitrary number like 250 deg Az.  This is the case
C     where SCHED may have chosen the CCW wrap based on the limit
C     crossing during the scan, but the on-line system might have
C     gone to near the CW limit based on the end of the setup scan.
C     I won't worry about scans at the other limit crossing at 270 deg
C     because that limit will never be crossed while tracking - 
C     negative azimuth rates are only seen at other azimuths.  The
C     code for this will be entirely confined to subroutine VLBA.
C
C     Note that predictions will not be perfect because the on-line 
C     system makes its choices based on the azimuth after the pointing
C     equation is applied and SCHED does not know those equations.  The
C     azimuth encoder offset can be pretty large.  SKED will deal with
C     this by having exclusion zones.  I am avoiding that so far.  The
C     best solution is for the antennas to follow the wrap instructions
C     from the schedulers.
C
C     Over-the-top.
C
C     Some antennas have the ability to go beyond 90 deg El.  SCHED
C     does not yet include that as an option.  Someday, it probably
C     should.
C
      INCLUDE 'sched.inc'
C
      INTEGER        ISCN, LSCN, ISTA, KSTA, NT1
      INTEGER        IWRAP, NWRAP1, NWRAP2, IMSLEW, IMSLEW2, IMSLEWE
      REAL           CURAZ1, CURAZ2, LASTAZ, MINSLEW, MINSLEW2
      REAL           CURHA1, CUREL1, CURPA1, AZSLEW, MINSLEWE
      DOUBLE PRECISION  CURLST1, STTIME, T_EST1
C ------------------------------------------------------------------
      KSTA = STANUM(ISTA)
C
C     Don't do anything if the mount is not ALTAZ.
C
      IF( MOUNT(KSTA) .NE. 'ALTAZ' ) RETURN
C
C     First get the previous position.  If the source was down,
C     assume that it was tracking at the station elevation limit. 
C     If this is the first scan for the station, assume that it
C     is at the low point of the wrap range.  This used to be the
C     mid point, but that option was causing problems at Effelsberg.
C
      IF( LSCN .NE. 0 ) THEN
         LASTAZ = AZ2(LSCN,ISTA)
      ELSE
         LASTAZ = AX1LIM(1,KSTA)
      END IF
C
C     Get the antenna position required to observe the new source
C     at the time of the start of the slew (end of previous scan).
C     This is where the calculation is done on the VLBA to determine
C     the wrap condition.  Also get the azimuth at the nominal end 
C     of the scan.  The on-line system also looks at that in 
C     deciding about a wrap.  Note that, while the on-line system
C     knows the true end of scan time, at the time this routine is
C     run, that may not be so clear as the dwell adjustments have 
C     not yet been done.  For now, assume that this will not cause
C     problems often enough to be a concern.
C
C     Azimuths from SCHGEO will be in range 0-360 and will be 
C     adjusted as required below for the wrap condition.
C
C     For LSCN=0 and not the first scan (say other stations have
C     started), we basically have no idea when the scan will finally
C     be scheduled.  It may depend on the history for other stations
C     in the scan.  And it may deviate from the currently set scan
C     times because, in the optimazation process where scans are skipped,
C     scan times can keep incrementing even though none are accepeted, 
C     until one is finally accepted.  I think for all this, I will
C     just assume that the calling routine has a better idea of when
C     the scan will start than this routine has, so I'll use T_EST1.
C
      IF( SCANL .EQ. 0 ) THEN
C
C        No scans scheduled yet.
C
         STTIME = TFIRST
C
      ELSE IF( LSCN .NE. 0 ) THEN
C
C        This station has previous scans.  Pick up from last.
C
         STTIME = STOPJ(LSCN)
C
      ELSE
C
C        First scan for this station, but not for experiment.  Assume the
C        calling routine had an idea what it was doing.
C        An alternative that worked for pointing cases: STTIME=STARTJ(SCANL)
C        But that doesn't work well when different antennas are being scheduled
C        separately.
C
         STTIME = T_EST1
C
      END IF
      CALL SCHGEO( ISCN, ISTA, STTIME, CURHA1,
     1             CUREL1, CURAZ1, CURLST1, CURPA1 )
      CURAZ2 = AZ2(ISCN,ISTA)
C
C     Adjust CURAZ2 to be the azimuth the telescope would end
C     up at if tracking the source starting at CURAZ1.  
C     Unfortunately this is not quite so simple in cases where
C     the source goes very near the zenith during the scan - or
C     since the stop of the last scan.  So we need to do this in
C     a declination dependent manner.  I think we don't need
C     to distinguish hemispheres.
C     First put both AZ values in 0-360 deg.
C
      CURAZ1 = MOD( CURAZ1, 360.0 )
      CURAZ2 = MOD( CURAZ2, 360.0 )
      IF( CURAZ1 .LT. 0.0 ) CURAZ1 = CURAZ1 + 360.0
      IF( CURAZ2 .LT. 0.0 ) CURAZ2 = CURAZ2 + 360.0
C
C     Deal with situations where a source crosses the line
C     between 0 and 360 deg azimuth and azimuths confined to
C     0 - 360 deg won't be on the same wrap.  The line is
C     crossed at upper culmination (0hr hour angle) when 
C     the source declination is higher than than the 
C     station latitude.  It is crossed 12 hours later 
C     at lower culmination when the source declination is 
C     higher than the negative of the latitude (imagine a 
C     station directly opposite on the Earth where 
C     upper culmination would occur at this time).  When a 
C     source crosses the line at 0hr HA, subtract 360 
C     from CURAZ2.  When it happens at 12hr HA, add 360 
C     to CURAZ2.
C
      IF( DECP(SRCNUM(ISCN)) .GT. LAT(STANUM(ISTA)) .AND.
     1    CURHA1 .LE. 0.0 .AND. HA2(ISCN,ISTA) .GT. 0.0 )
     2   CURAZ2 = CURAZ2 - 360.0
C
      IF( DECP(SRCNUM(ISCN)) .GT. -1.0 * LAT(STANUM(ISTA)) .AND.
     1    CURHA1 .GT. 0.0 .AND. HA2(ISCN,ISTA) .LE. 0.0 )
     2   CURAZ2 = CURAZ2 + 360.0
C
C     Find the range of possible wraps assuming that CURAZ1 is in 0-360.
C       Note:  For AX1LIM(1,KSTA) > 0 or AX1LIM(2,KSTA) < 360,
C       more wraps than necessary are checked, but these will be rare
C       cases and it is better to keep the code simple.
C
      NWRAP1 = INT( AX1LIM(1,KSTA) / 360.0 ) - 1
      NWRAP2 = INT( AX1LIM(2,KSTA) / 360.0 )
C
C     Now find the shortest slew consistent with the AZ limits.
C     Initialize IMSLEW in order to detect if there is no
C     acceptable wrap state.  IMSLEW2 will detect a usable
C     wrap state for the start of scan only.  That will be 
C     used if there is nothing acceptable for both.  If there
C     is nothing at all, just use the input positions.
C
      MINSLEW = 99999.0
      IMSLEW = -99
      DO IWRAP = NWRAP1, NWRAP2
         IF( CURAZ1 + IWRAP * 360.0 .GE. AX1LIM(1,KSTA) .AND.
     1       CURAZ1 + IWRAP * 360.0 .LE. AX1LIM(2,KSTA) .AND.
     2       CURAZ2 + IWRAP * 360.0 .GE. AX1LIM(1,KSTA) .AND.
     3       CURAZ2 + IWRAP * 360.0 .LE. AX1LIM(2,KSTA) ) THEN
            AZSLEW = ABS( CURAZ1 + IWRAP * 360.0 - LASTAZ )
            IF( AZSLEW .LT. MINSLEW ) THEN
               MINSLEW = AZSLEW
               IMSLEW = IWRAP
            END IF
         END IF
      END DO
C
C     Set the Az values to the best wrap.  But only if a 
C     wrap worked.
C
      IF( IMSLEW .NE. -99 ) THEN
         CURAZ1 = CURAZ1 + IMSLEW * 360.0
         CURAZ2 = CURAZ2 + IMSLEW * 360.0
C
      ELSE
C
C        This is the case where the wrap didn't work.  For 
C        example, Haystack in the current station catalog
C        (Dec. 2010) has az limits of 0 and 360, so a northern
C        scan cannot track through zero, and such scans will
C        have problems.  I presume that the antenna will try
C        to get to the next source at its position at the end
C        of the previous scan (ie CURAZ1).  So determine the
C        wrap for that position.
C
C        This can also happen if the estimated start time is
C        way off (like end of previous station's scans when 
C        scheduling antennas separately in the old scheme.
C
C        When we don't have an acceptable combination, a 
C        common wrap for start and end is not viable by definition.
C        Then assume a wrap will happen during the scan.
C
         MINSLEW2 = 99999.0
         IMSLEW2 = -99
         DO IWRAP = NWRAP1, NWRAP2
            IF( CURAZ1 + IWRAP * 360.0 .GE. AX1LIM(1,KSTA) .AND.
     1          CURAZ1 + IWRAP * 360.0 .LE. AX1LIM(2,KSTA) ) THEN
               AZSLEW = ABS( CURAZ1 + IWRAP * 360.0 - LASTAZ )
               IF( AZSLEW .LT. MINSLEW2 ) THEN
                  MINSLEW2 = AZSLEW
                  IMSLEW2 = IWRAP
               END IF
            END IF
         END DO
C
         IF( IMSLEW2 .NE. -99 ) THEN
C
C           Get the minimum slew to the end of scan.
C           This might even help fix (get a non-slewing end of scan) 
C           the azimuths when a very bad start time gave a 
C           messed up projected start position.  At least it 
C           shouldn't give illegal azimuths at the end of scan.
C
            MINSLEWE = 99999.0
            IMSLEWE = -99
            DO IWRAP = NWRAP1, NWRAP2
               IF( CURAZ2 + IWRAP * 360.0 .GE. AX1LIM(1,KSTA) .AND.
     1             CURAZ2 + IWRAP * 360.0 .LE. AX1LIM(2,KSTA) ) THEN
                  AZSLEW = ABS( CURAZ2 + IWRAP * 360.0 - CURAZ1 )
                  IF( AZSLEW .LT. MINSLEWE ) THEN
                     MINSLEWE = AZSLEW
                     IMSLEWE = IWRAP
                  END IF
               END IF
            END DO
            IF( IMSLEWE .NE. -99 ) THEN
               CURAZ2 = CURAZ2 + IMSLEWE * 360.0
            ELSE
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I4, A, A, A )' )
     1            'WRAP:  Could not get wrap for end of scan ', ISCN, 
     2            ', station: ', STANAME(ISTA),
     3            '  Program logic problem?' 
               CALL WLOG( 1, MSGTXT )
            END IF
         ELSE
C           
C           Leave CURAZ1 and CURAZ2 as is, but complain.
C
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I4, A, A )' )
     1         'WRAP:  Could not get wrap for scan ', ISCN, 
     2         ', station: ', STANAME(ISTA),
     3         '  Program logic problem?' 
            CALL WLOG( 1, MSGTXT )
         END IF
      END IF
C
C     Get AZ1 and AZ2 on the right wrap.  Don't recalculate
C     their values, but rather make sure they are on the same
C     wrap as CURAZ1 and CURAZ2.  Specifically don't use CURAZ1
C     because it was recalculated for the end of the previous
C     scan, not the start of the current scan in order to 
C     duplicate what will be done at the antennas, at least on
C     the VLBA.
C
      AZ1(ISCN,ISTA) = AZ1(ISCN,ISTA) + 
     1     360.0 * NINT( ( CURAZ1 - AZ1(ISCN,ISTA) ) / 360.0 )
      AZ2(ISCN,ISTA) = AZ2(ISCN,ISTA) + 
     1     360.0 * NINT( ( CURAZ2 - AZ2(ISCN,ISTA) ) / 360.0 )
C
C     Note that there are situations where CURAZ1 and AZ1 can
C     be very different (such as a planning schedule when
C     CURAZ1 is calculated for about the time the source set
C     and AZ1 is for around when it rises).  But it should be
C     appropriate to put AZ1 on the same wrap as CURAZ1 
C     because CURAZ1 and CURAZ2 have been forced to be on the
C     same wrap and AZ1 is between.  
C
C     The one corner case left over is when there was no 
C     acceptable wrap for both CURAZ1 and CURAZ2 (eg HS across 
C     0 AZ) and AZ1 is on the same wrap as AZ2 because it came
C     well after CURAZ1.  Then the Azimuths should be moved
C     into the acceptable range. So if AZ1 is outside the 
C     allowed range, force AZ1 to the good range and move
C     AZ2 by the same amount.
C
      IF( AZ1(ISCN,ISTA) .LT. AX1LIM(1,KSTA) ) THEN
         NT1 = INT( ( AX1LIM(1,KSTA) - AZ1(ISCN,ISTA) ) / 360.0 ) + 1
         AZ1(ISCN,ISTA) = AZ1(ISCN,ISTA) + NT1 * 360.0
         AZ2(ISCN,ISTA) = AZ2(ISCN,ISTA) + NT1 * 360.0
      ELSE IF( AZ1(ISCN,ISTA) .GT. AX1LIM(2,KSTA) ) THEN
         NT1 = INT( ( AZ1(ISCN,ISTA) - AX1LIM(2,KSTA) ) / 360.0 ) + 1
         AZ1(ISCN,ISTA) = AZ1(ISCN,ISTA) - NT1 * 360.0
         AZ2(ISCN,ISTA) = AZ2(ISCN,ISTA) - NT1 * 360.0
      END IF
C
C     This was all a lot messier than I like and it still may not 
C     be perfect for antennas that don't use an algorithm very
C     similar to the VLBA.  Some day, I hope we can specify the
C     wrap in the VEX file and have it used at the antennas, so
C     we don't have to guess what the antenna will do.
C
      RETURN
      END
