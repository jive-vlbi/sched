      SUBROUTINE SLEW( ISCN, LSCN, ISTA )
C
C     This routine calculates the slew distances and time in
C     getting from scan LSCN to the current scan ISCN.  It is 
C     always called after a call to SCHSRC which sets the geometry
C     for the scan and determines the wrap.  Someday, maybe is should
C     be moved into SCHSRC, but for now, keep it out.
C
C     This routine is called from STAGEO, SUVOPT, and PLOTSTA, 
C     all of which call SCHSRC shortly before calling SLEW.
C
C     Units are degrees for Az, El, X, and Y.  Ha is in hours.  Dec
C     is radians on input, but the DEC4 parameters are degrees.
C     Slew time is in fractional days at the end.
C
C     It turns out that there is a "retarded" effect (like retarded
C     baselines) that becomes significant with long slews on slow
C     antennas.  It can change the slew time by several, to more than
C     ten, seconds.  It is a result of the fact that the Earth rotates
C     causing the Az and El to change during the slew.  This routine,
C     up to Dec. 2010, did not take that effect into account and just
C     used the positions calculated earlier by SCHSRC for the start
C     of the next observation.  It has now been modified to calculate
C     the slew time, then get the geometry for the calculated arrival
C     time, and then recalculate the slew time.  One iteration like
C     that should be sufficient.  Keep the same wrap for azimuth as
C     in the nominal data, and don't modify the nominal scan data.
C     Those are for the start time - common to all antennas - while 
C     the position at the end of the slew is very specific to this
C     antenna.
C
C     For now, this routine does not reconsider the wrap state at
C     the antenna.  There will be rare cases where doing the 
C     position calculations for the improved times in the second pass
C     would cause a wrap to be required that was not detected by 
C     SCHSRC.  We ignore that possiblility now.
C
C     Note that valid mount types are checked in HORCHK.
C
      INCLUDE 'sched.inc'
C
      INTEGER            ISCN, KSCN, LSCN, ISTA, KSTA, PASS, KS
      REAL               LASTAX1, LASTAX2, CURAX1, CURAX2
      LOGICAL            KSUSED, OKLEV
      DOUBLE PRECISION   AX1SLEW, AX2SLEW, DMINSU, TARRIVE
      REAL               TACC1, TACC2, DACC1, DACC2, TSLW1, TSLW2
      REAL               RATE1, RATE2, AX1ADU, AX2ADU
      REAL               LDEC4, DEC4
      REAL               AHA, AEL, AAZ, APA, AZDIFF
      DOUBLE PRECISION   ALSTTIM
C --------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. 3 ) CALL WLOG( 0, 'SLEW: Starting.' )
C
      KSTA = STANUM(ISTA)
C
C     Do the calculation for an actual move.
C
C     First get the previous position. 
C     
      IF( LSCN .EQ. 0 ) THEN
C     
C        First scan.
C     
C        Usually VLBI runs are not back to back and the antennas are
C        given a bit of time to get to source before the start.  So
C        assume that the telescope is at the beginning-of-scan position
C        at the start of the first scan.  This is a change as of Dec. 12
C        2010.
C
         LDEC4 = DECP(SRCNUM(ISCN)) / RADDEG
         CALL ANTPOS( KSTA, ISCN, ' ', LDEC4,
     1           AZ1(ISCN,ISTA), EL1(ISCN,ISTA), HA1(ISCN,ISTA), 
     2           LASTAX1, LASTAX2 )
C     
      ELSE
C     
C        Get position at end of scan LSCN.  Note wrap position in Az
C        should have been established already by routine WRAP.
C        This is the position at the start of the slew.
C     
         LDEC4 = DECP(SRCNUM(LSCN)) / RADDEG
         CALL ANTPOS( KSTA, LSCN, UP2(LSCN,ISTA), LDEC4,
     1           AZ2(LSCN,ISTA), EL2(LSCN,ISTA), HA2(LSCN,ISTA),
     2           LASTAX1, LASTAX2 )
C     
      END IF
C
C     Now get the position when the antenna arrives on-source for the
C     current scan.
C
C     Go through the next bit of code twice to iterate the slew time
C     based on the arrival time at the new source.  This is what
C     I call a "retarded slew" calculation.  It takes into account
C     the fact that the Earth rotates, and source positions in units
C     like Az and El change during the slew.  This effect can cause
C     errors of 10 sec or more in the slew time calculation with slow
C     antennas.  Note that the slew time calculation done with this
C     iteration is almost independent on the estimated scan start
C     time - it is based on where the antenna was at the end of the
C     previous scan and the coordinates of the destination source,
C     along with the slew rates.
C
      DO PASS = 1, 2
C
         IF( PASS .EQ. 1 ) THEN
C
C           On PASS 1, use the positions for the new source at the 
C           nominal start time of the scan.  These were calculated
C           by calls to SCHGEO and WRAP before SLEW was called
C     
            DEC4 = DECP(SRCNUM(ISCN)) / RADDEG
            CALL ANTPOS( KSTA, ISCN, UP1(ISCN,ISTA), DEC4,
     1           AZ1(ISCN,ISTA), EL1(ISCN,ISTA), HA1(ISCN,ISTA),
     2           CURAX1, CURAX2 )
C
         ELSE IF( LSCN .NE. 0 ) THEN
C
C           Recalculate the slew based on the geometry at the
C           actual arrival time.
C           First get that arrival time based on the first pass 
C           calculation.  The first pass number has not yet
C           had the settle time added or been converted to days.
C           We don't want the settle time here.  Do the day
C           conversion from seconds.
C
            TARRIVE = STOPJ(LSCN) + 
     1                TSLEW(ISCN,ISTA) / ( 1440.0D0 * 60.D0 )
C
C           Get the geometry.  Note that ISCN is only used to get
C           the station and source coordinates.  SCHGEO does not
C           modify any of the values in the arrays of scan data.
C
            CALL SCHGEO( ISCN, ISTA, TARRIVE, 
     1                   AHA, AEL, AAZ, ALSTTIM, APA )
C
C           If necessary, shift the azimuth to the wrap given
C           by the previous call to WRAP (before SLEW was called).
C
C            write(*,*) '   slew test 1: ', iscn, ista,
C     1              '  el: ', el1(iscn,ista), ael, 
C     2              '  az: ', az1(iscn,ista), aaz
            AZDIFF = AAZ - AZ1(ISCN,ISTA)
            AZDIFF = NINT( AZDIFF / 360.0 ) * 360.0            
            AAZ = AAZ - AZDIFF
C            write(*,*) 'slew test out ', iscn, ista, ' modified az: ',
C     1              aaz, azdiff
C
C           Now get the position in the telescope's native axes.
C           Can get into trouble with the limits consistency check if
C           the antenna is near its limits and the original estimate
C           was on one side and the new is on the other side.
C
            CALL ANTPOS( KSTA, ISCN, UP1(ISCN,ISTA), DEC4,
     1           AAZ, AEL, AHA, CURAX1, CURAX2 )
C
         ELSE
C
C           First scan.  Just keep the CURAX1 and CURAX2 from pass 1.
C
         END IF
C        
C        
C        Get the slew time in minutes.  All rates assumed to be deg/min.
C        Acceleration is assumed to be in deg/s/s.
C        Consider the two cases of when the antenna reaches full speed
C        and when it has to start slowing before reaching full speed.
C        TACC and DACC are the total time and distance spent both
C        accelerating and decelerating.
C        Note that the equation for DACC does not show two factors of
C        2 that cancel, one for both acceleration and deceleration, and
C        the other from D=0.5at^2.
C        Likewise, the equation for TSLEW in the case of not reaching
C        full speed is really T = 2sqrt(2(D/2)/a).
C        TACC1 and TACC2 are in seconds.
C        RATE1 and RATE2 are deg/sec  (trying to use seconds for all).
C        
C        The above was for the original implementation of acceleration.
C        It assumed acceleration and deceleration were the same.  But some
C        antennas, including the DSN, have different rates.
C        
C        First the slew distance.
C        
         AX1SLEW = ABS( CURAX1 - LASTAX1 )
         AX2SLEW = ABS( CURAX2 - LASTAX2 )
C        
C        Then how long does it take and how far does it go to get to
C        full speed.
C        
         RATE1 = AX1RATE(KSTA) / 60.0
         RATE2 = AX2RATE(KSTA) / 60.0
         AX1ADU = 1.0 / AX1ACC(1,KSTA) + 1.0 / AX1ACC(2,KSTA)
         AX2ADU = 1.0 / AX2ACC(1,KSTA) + 1.0 / AX2ACC(2,KSTA)
         TACC1 = RATE1 * AX1ADU
         TACC2 = RATE2 * AX2ADU
         DACC1 = RATE1**2 * AX1ADU * 0.5
         DACC2 = RATE2**2 * AX2ADU * 0.5
C        
C        Then get the actual time, with the method depending on whether
C        full speed was reached.
C        
         IF( AX1SLEW .LT. DACC1 ) THEN
            TSLW1 = SQRT( 2.0 * AX1SLEW * AX1ADU )
         ELSE
            TSLW1 = TACC1 + ( AX1SLEW - DACC1 ) / RATE1
         END IF
         IF( AX2SLEW .LT. DACC2 ) THEN
            TSLW2 = SQRT( 2.0 * AX2SLEW * AX2ADU )
         ELSE
            TSLW2 = TACC2 + ( AX2SLEW - DACC2 ) / RATE2
         END IF
C        
C        Use the largest.
C        
         TSLEW(ISCN,ISTA) = MAX( TSLW1, TSLW2 )
C
      END DO
C        
C     When there is no source change, there can be confusion.
C     Presumably the slew time is zero, assuming tracking continues.  
C     But when there is a gap between scans, the sky rotates a bit, 
C     and this routine used to come up with a non-zero slew time 
C     (second or two, typically, maybe more if there are GAPs scheduled).
C     That can be pretty confusing in the printouts.  So, set the 
C     actual slew time, before TSETTLE etc, to zero in this case.  But
C     don't always set it to zero.  If a wrap is needed, a long slew
C     may be required and would be reflected in the above calculations.
C     So only set TSLEW to zero if it is already small - implying no wrap.
C
      IF( LSCN .NE. 0 ) THEN
         IF( SRCNUM(LSCN) .EQ. SRCNUM(ISCN) .AND.
     1        TSLEW(ISCN,ISTA) .LT. 20.D0 * ONESEC ) THEN
            TSLEW(ISCN,ISTA) = 0.D0
         END IF
C   
C        Add the settling time for the station.  
C
         TSLEW(ISCN,ISTA) = TSLEW(ISCN,ISTA) + TSETTLE(KSTA)
      END IF
C
C     Make sure that the "slew" time does not drop below the minimum
C     setup time.
C
      DMINSU = MINSETUP(KSTA)
      TSLEW(ISCN,ISTA) = MAX( TSLEW(ISCN,ISTA), DMINSU )
C
C     Be sure there is time for the "set and remember" level setting
C     operation on the VLBA or VLA (or any other antenna with 
C     TLEVSET set non-zero).  There are special problems doing this
C     because SLEW is called often during calculations for trial 
C     scans that will not be part of the final schedule (pointing, 
C     DELZN, etc).  Those trials do need to know if the extra time 
C     will be added.  But having added it is no assurance that the 
C     station no longer needs the time - the scan that added it may 
C     not get used in the final schedule.  I don't see a clean 
C     solution to this, so, each time the routine is called, look 
C     back for a previous incidence of the setup.  Crude, but 
C     computers are fast now.
C
C     It turns out there is another issue.  Users will sometimes
C     schedule scans that are explicitly meant for the level setting,
C     especially on the VLA.  Try to sense when that happens, and
C     don't add the time for the level setting if it can be done
C     during the scan.
C
C     Check if this scan is the first with this setup.
C
      KS = NSETUP(ISCN,ISTA)
      KSUSED = .FALSE.
      KSCN = ISCN - 1
      DO WHILE( .NOT. KSUSED .AND. KSCN .GE. SCAN1 )
         IF( STASCN(KSCN,ISTA) .AND. NSETUP(KSCN,ISTA) .EQ. KS )
     1        THEN
            KSUSED = .TRUE.
         END IF
         KSCN = KSCN - 1
      END DO
C
C     Also see if this could be a level setting scan.
C
      OKLEV = NOREC(ISCN) .AND. 
     1        SNGL( DUR(ISCN) * 86400.D0 ) .GE. TLEVSET(KSTA) - 1
      IF( STANAME(ISTA)(1:3) .EQ. 'VLA' ) THEN
         OKLEV = OKLEV .AND. 
     1           VLAPEAK(ISCN) .NE. 'ADJUST' .AND. 
     2           VLAMODE(ISCN) .NE. 'VA'
      ELSE
         OKLEV = OKLEV .AND. DOPEAK(ISCN) .LT. 1
      END IF
C
C     Now if it is first, and the scan itself can't serve, then
C     claim the slew has to be long enough to accommodate the leve
C     setting.
C
      IF( .NOT. KSUSED .AND. .NOT. OKLEV ) THEN
C
C        Change index on TLEVSET from ISTA to KSTA Jan. 7, 2012 RCW.
C
         IF( TLEVSET(KSTA) .GT. TSLEW(ISCN,ISTA) ) THEN
            TSLEW(ISCN,ISTA) = TLEVSET(KSTA)
         END IF
      END IF
C
C     Convert TSLEW from seconds to fractional days.
C
      TSLEW(ISCN,ISTA) = TSLEW(ISCN,ISTA) / ( 1440.0D0 * 60.D0 )
C
      RETURN
      END
