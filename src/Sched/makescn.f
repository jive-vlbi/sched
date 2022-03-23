      SUBROUTINE MAKESCN( LASTISCN, ISCN, JSCN, ISRC, SRCN, TAPPROX, 
     1                    MINEL, NGOOD, OKSTA, USETIME )
C
C     Routine for SCHED, initially used in ADDGEO, to make a scan
C     on a source based on a template scan and a source name.
C     The scan times coming out of this are good enough to use
C     in SCHOPT after SCHOPT has nominally done it's final tweaks.
C
C     LASTISCN is the scan number of the last scan the station was in.
C     ISCN is the scan number being constructed.
C     JSCN is the template scan.  It will be in the original
C        range of 1 to NSCANS.  Pay attention to its STASCN.
C     ISRC is the source number in the main catalog.
C     SRCN is the name of the source to use.
C     TAPPROX is an input approximate time for the scan.  Used for 
C         initial geometry calculations.  Also, if there was no
C         preceeding scan, this becomes the start time of the scan.
C     MINEL is the required minimum elevation for the source to
C         be counted as up (SCNSRC will be set according to 
C         being above the antenna horizons).
C     NGOOD (output) is the nubmer of antennas for which the 
C         source is above MINEL and the station horizon
C     OKSTA is an output array of logicals indicating the station is
C         up for this scan, including above the horizon.
C     USETIME true means force use of TAPPROX as the scan time rather
C         than the experiment start time when none of the stations
C         involved has been in a previous scan.
C         
C
      INCLUDE 'sched.inc'
C
      INTEGER            ISCN, JSCN, ISRC, ISTA, NGOOD, LASTISCN(*)
      REAL               MINEL
      LOGICAL            OKSTA(*), USETIME
      DOUBLE PRECISION   TAPPROX, LASTTIME, T_AVAIL
      CHARACTER          SRCN*12
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'MAKESCN starting.' )
C
C     Copy the template scan.
C
      CALL SCNDUP ( ISCN, JSCN, .FALSE., 'MAKESCN' )
C
C     Insert the source.
C
      SRCNUM(ISCN) = ISRC
      SCNSRC(ISCN) = SRCN
C
C     Loop through stations to see which ones are up at TAPPROX.
C     Also see how many are above the minimum elevation.
C     This pass determines the approximate geometry - enough to 
C     edit stations and count the good ones.
C
      NGOOD = 0
      DO ISTA = 1, NSTA
         OKSTA(ISTA) = .FALSE.
         IF( STASCN(JSCN,ISTA) ) THEN
            CALL STAGEO( ISCN, ISTA, TAPPROX, LASTISCN(ISTA),  
     1          LASTTIME, T_AVAIL, 'MAKESCN' )
            IF( UP1(ISCN,ISTA) .EQ. ' ' .AND. 
     1          UP2(ISCN,ISTA) .EQ. ' ' .AND. 
     2          EL1(ISCN,ISTA) .GT. MINEL .AND.
     3          EL2(ISCN,ISTA) .GT. MINEL ) THEN
               OKSTA(ISTA) = .TRUE.
               NGOOD = NGOOD + 1
            END IF
         END IF
      END DO
C
C     Set the scan stations list based on the results from STAGEO.
C
      DO ISTA = 1, NSTA
         STASCN(ISCN,ISTA) = STASCN(JSCN,ISTA) .AND. OKSTA(ISTA)
      END DO
C
C     Get the final timing and geometry.
C     Consider whether to make the SCNGEO call optional depending 
C     on how long the geo source selection takes.  It lengthens 
C     the time in this routine for minor geometry tweaks not needed
C     while optimizing, but wanted for finally pushing the scans 
C     out the door.  OPTTIM is needed regardless to set start and 
C     stop times.  The restriction to a minimum number of good antennas
C     will be done elsewhere.
C
      IF( NGOOD .GE. 1 ) THEN
         STARTJ(ISCN) = TAPPROX
C
C        Adjust the scan times based on a geometry calculation.
C        This reruns the above STAGEO so it could possibly be made
C        more efficient some day.  Don't apply PRESCAN.  Assume
C        we can assume LASTISCN and LASTSSCN are the same for this
C        call - there is no confusion with preceding inserted pointing.
C        is that always true?
C
         CALL OPTTIM( LASTISCN, LASTISCN, ISCN, .TRUE., 
     1                USETIME, .FALSE. )
C
C        Determine the geometry, slew times, and time of arrival 
C        on source based on the final times.  NGOOD is 
C        redetermined based on the final times.
C
         CALL SCNGEO( LASTISCN, NGOOD, ISCN )
C
      END IF
C
      RETURN
      END
