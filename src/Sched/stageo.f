      SUBROUTINE STAGEO( ISCN, ISTA, T_EST1, LSCN,
     1                   LASTTIME, T_AVAIL, CALLER )
Cf2py intent(in) ISCN, ISTA, T_EST1, LSCN, CALLER
Cf2py intent(out) LASTTIME, T_AVAIL
C
C     General purpose geometry routine for calculation of
C     the geometry for a station in a scan.  It is used a lot
C     in the SCHED optimization routines.  This routine will
C     not reset scan parameters such as STARTJ or STASCN, 
C     but it will fill in the geometry variables based on
C     the input times and antenna participation.
C
C     For calculation of the geometry of all stations in a
C     scan, use SCNGEO, which calls this routine.  Note that
C     it calls this routine whether or not the station is
C     actually included in the scan in case it is desired
C     to see where the antenna would be pointed if it were
C     included.
C     
C     This routine used (pre-Dec 2010) to be called OPTGEO.
C     I (RCW) renamed it to make its role more clear.
C
C     ISCN and ISTA are the input scan and station numbers.
C     T_EST1 is the estimated start time for the scan.
C     LSCN is input giving the scan number for the last scan
C       that the station was in.
C     LASTTIME is output giving the last time the station was
C       scheduled for something.
C     T_AVAIL is the output time when the station can observe this
C       source.  For DWELL type scheduling, this will be when the
C       antenna is on source.  For DUR type scheduling, it will
C       be the end of the previous scan.  For this reason, T_AVAIL
C       should not be used later for the final setting of TONSRC.  
C       STAGEO does set TONSRC correctly before setting T_AVAIL.
C
C       The geometric parameters are in the HA1, EL1 etc arrays.
C       This is set to the next even second.
C
C     CALLER is the name of the calling routine.  This routine 
C       is called from so many places that, during debugging, 
C       it can help a lot to know who called it.
C
C     If this is the first scan (LSCN=0), return 0.D0 for
C     LASTTIME and T_AVAIL.  The calling routine should not use
C     these times.
C
      INCLUDE 'sched.inc'
C
      INTEGER          ISCN, ISTA, LSCN
      DOUBLE PRECISION T_EST1, T_EST2, T_AVAIL, LASTTIME
      CHARACTER        HORCHK*1, CALLER*(*)
C ----------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. 3 ) THEN
          MSGTXT = ' '
          WRITE( MSGTXT, '( A, A )' )
     1        'STAGEO: starting. Called by ', CALLER
          CALL WLOG( 0, MSGTXT )
      END IF
C      write(*,*) 'stageo: called by ', caller
C
C     Get geometric parameters for this station and this scan.
C
      T_EST2 = T_EST1 + DUR(ISCN)
C
C     The calls to SCHGEO, WRAP, and HORCHK used to be in SCHSRC.  
C     I absorb them into this routine for ease of comprehension of 
C     the overall geometry software.
C        Old call:  CALL SCHSRC( LSCN, ISCN, ISTA, T_EST1, T_EST2 )
C
C     Get geometry at start and end of scan.  
C
      CALL SCHGEO( ISCN, ISTA, T_EST1, HA1(ISCN,ISTA), 
     1             EL1(ISCN,ISTA), AZ1(ISCN,ISTA),
     2             LST1(ISCN,ISTA), PA1(ISCN,ISTA) )
C
      CALL SCHGEO( ISCN, ISTA, T_EST2, HA2(ISCN,ISTA),
     1             EL2(ISCN,ISTA), AZ2(ISCN,ISTA),
     2             LST2(ISCN,ISTA), PA2(ISCN,ISTA) )
C
C     Adjust azimuth for cable wrap for ALTAZ antennas.  Note that
C     WRAP uses the geometry for the end of the last scan the 
C     station was in plus the geometry of the new source at the same
C     time, which is what the antennas do.  It also uses the 
C     geometry at the nominal end of the scan to check need for a
C     wrap.  The latter may not be right before the final scan times
C     are set.  T_EST1 is used as and estimated slew start time when
C     LSCN is zero.
C
      CALL WRAP( ISCN, LSCN, ISTA, T_EST1 )
C
C     Check if rising, setting, below horizon, etc.  Should be called
C     before SLEW because of the consistency check in ANTPOS which is
C     called by SLEW.
C
      UP1(ISCN,ISTA) = HORCHK( STANUM(ISTA), HA1(ISCN,ISTA), 
     1           AZ1(ISCN,ISTA), EL1(ISCN,ISTA), SRCNUM(ISCN) )
      UP2(ISCN,ISTA) = HORCHK( STANUM(ISTA), HA2(ISCN,ISTA), 
     1           AZ2(ISCN,ISTA), EL2(ISCN,ISTA), SRCNUM(ISCN) )
C
C     Get the slew time.
C
      CALL SLEW( ISCN, LSCN, ISTA )
C
C     Get the last observing time and first available time for this
C     antenna.  Also get the time the antenna will be on source.  The
C     zeros for the first scan basically mean that the antenna is
C     ready any time.
C
      IF( LSCN .EQ. 0 ) THEN
C
C        First scan for this antenna.
C
         LASTTIME = 0.D0
         T_AVAIL = 0.D0
         TONSRC(ISCN,ISTA) = STARTJ(ISCN)
C
      ELSE
         LASTTIME = STOPJ(LSCN)
         TONSRC(ISCN,ISTA) = STOPJ(LSCN) + TSLEW(ISCN,ISTA)
C
C        Get when the antenna will be available.  This part is
C        mainly for the optimization routines.  It is only set beyond
C        LASTTIME if the antenna is up throughout the scan - ie it is
C        useful for an optimized schedule.
C
         IF( UP1(ISCN,ISTA) .EQ. ' ' .AND. UP2(ISCN,ISTA) .EQ. ' ' ) 
     1       THEN
C
C           Source is up.  Get the earliest time that the source is 
C           available at that antenna.  Honor requests to not take 
C           slew into account.  Won't get here for ISCN=1.
C
            IF( ( DURONLY(ISCN) .EQ. 1 .OR. DURONLY(ISCN) .EQ. 4 ) 
     1          .AND. DWELL(ISCN) ) THEN
               T_AVAIL = TONSRC(ISCN,ISTA)
            ELSE
               T_AVAIL = LASTTIME
            END IF
         ELSE
C
C           Source not up.  Antenna still available at last scan stop.
C
            T_AVAIL = LASTTIME
C
         END IF
C
      END IF
C
      RETURN 
      END
