      SUBROUTINE GMKSCN( LASTISCN, ISCN, JSCN, ISRC, SRCN, TAPPROX, 
     1                    MINEL, NGOOD, OKSTA, SSTASCN, GMODE )
C
C     This is varient of MAKESCN to be used in the process of the
C     geodetic scan insertion.  It calls MAKESCN, but also can
C     remove stations, or even inhibit use of the new scan, based
C     on slew times.  It was needed several times in MAKESEG and
C     ADDGEO, so I have consolidated it here.
C
C     GMODE = 'SET' means set STASCN and other parameters based on
C             the geometry and on JSCN.
C
C     GMODE = 'FORCE' means set STASCN = SSTASCN and set the other
C             indicators (NGOOD, OKSTA) based on SSTASCN.  Basically
C             in these cases, this routine has been called earlier
C             and the selections done based on that.  Now we basically
C             want MAKESCN but with STASCN forced to the previously
C             derived values.  That forcing avoids unexpected edge
C             effects with slight time shifts.
C
      INCLUDE 'sched.inc'
C
C     Call arguments, mostly for MAKESCN
C 
      INTEGER            LASTISCN(*), ISCN, JSCN, ISRC, NGOOD
      CHARACTER          SRCN*12, GMODE*5
      REAL               MINEL
      DOUBLE PRECISION   TAPPROX
      LOGICAL            OKSTA(*), SSTASCN(*)
C
C     Internal variables.
C
      INTEGER            ISTA, NSUM
      DOUBLE PRECISION   LASTTIME, T_AVAIL, TONSUM
C ------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'GMKSCN starting.' )
C
C     The first part is lifted from MAKESCN, then modified to deal
C     with SSTASCN.
C
      NGOOD = 0
C
C     Copy the template scan.
C
      CALL SCNDUP ( ISCN, JSCN, .FALSE. )
C
C     Insert the source.
C
      SRCNUM(ISCN) = ISRC
      SCNSRC(ISCN) = SRCN
C
C     Deal with the request to set make a scan but leave off 
C     slow slew stations.
C
      IF( GMODE .EQ. 'SET' ) THEN
C
C        Loop through stations to see which ones are up at TAPPROX.
C        Also see how many are above the minimum elevation.
C        This pass determines the approximate geometry - enough to 
C        edit stations and count the good ones. 
C
         DO ISTA = 1, NSTA
            OKSTA(ISTA) = .FALSE.
            IF( STASCN(JSCN,ISTA) ) THEN
               CALL OPTGEO( ISCN, ISTA, TAPPROX, LASTISCN(ISTA),  
     1             LASTTIME, T_AVAIL )
               IF( UP1(ISCN,ISTA) .EQ. ' ' .AND. 
     1             UP2(ISCN,ISTA) .EQ. ' ' .AND. 
     2             EL1(ISCN,ISTA) .GT. MINEL .AND.
     3             EL2(ISCN,ISTA) .GT. MINEL ) THEN
                  OKSTA(ISTA) = .TRUE.
                  NGOOD = NGOOD + 1
               END IF
            END IF
         END DO
C
C        Set the scan stations list based on the results from OPTGEO.
C
         DO ISTA = 1, NSTA
            STASCN(ISCN,ISTA) = STASCN(JSCN,ISTA) .AND. OKSTA(ISTA)
         END DO
C
C        Get the final timing and geometry.  OPTTIM sets the scan
C        timing and SCNGEO gets the geometry, slew times, on-source
C        times etc.  
C        The restriction to a minimum number of good antennas
C        will be done elsewhere.
C      
         IF( NGOOD .GE. 1 ) THEN
            STARTJ(ISCN) = TAPPROX
C      
C           Adjust the scan times based on a geometry calculation.
C           This reruns the above OPTGEO so it could possibly be made
C           more efficient some day.
C      
            CALL OPTTIM( LASTISCN, ISCN, .TRUE. )
C      
C           Determine the geometry, slew times, and time of arrival 
C           on source based on the final times.  NGOOD is 
C           redetermined based on the final times.
C      
            CALL SCNGEO( LASTISCN, NGOOD, ISCN )
         END IF
C
C        Try to prevent long waits for small numbers of
C        antennas.  Get the average TONSRC for stations
C        that were in the preceding scan (Could be distorted
C        badly if those that could slew during the last scan
C        are counted).  Then look at any station that is
C        later than the average by 30 seconds or more.
C        If the elevation at that station is high, eliminate
C        the station (adjust STASCN, NGOOD, OKSTA).  Note
C        that the final times will change, but this routine
C        will only be called in "SET" mode by routines (MAKESEG)
C        that will call it again in "FORCE" mode before scan
C        parameters are finalized.
C
C        If the source is at low elevation at
C        the slow station, block use of the scan here in 
C        the hope that it will be picked up later.  Do that
C        by setting NGOOD=0.
C
C        Don't bother with these calculations if the source 
C        won't be selected anyway (NGOOD too small regardless).
C        Also don't do it for the first scan of an experiment.
C
         IF( NGOOD .GE. OPMIAN(JSCN) .AND. 
     1       ISCN .GT. SCAN1 ) THEN
C
C           Initialize the averaging accumulators.
C
            TONSUM = 0.D0
            NSUM = 0
C
C           Get the average start time.
C
            DO ISTA = 1, NSTA
               IF( STASCN(ISCN,ISTA) .AND. 
     1             STASCN(ISCN-1,ISTA) ) THEN
                  TONSUM = TONSUM + TONSRC(ISCN,ISTA)
                  NSUM = NSUM + 1
               END IF
            END DO
            IF( NSUM .GT. 0 ) THEN
               TONSUM = TONSUM / NSUM
            ELSE
               CALL ERRLOG( 'GMKSCN: NSUM zero.  Programming error' )
            END IF
C
C           Deal with late stations.
C
            DO ISTA = 1, NSTA
               IF( STASCN(ISCN,ISTA) ) THEN
                  IF( TONSRC(ISCN,ISTA) .GT. 
     1               TONSUM + 30.D0 * ONESEC ) THEN
C
C                    Low elevation scan for the slow antenna.
C
                     IF( EL1(ISCN,ISTA) .LT. GEOLOWEL ) THEN
                        NGOOD = 0
C
C                    High elevation scan for the slow antenna.
C                    Remove the station from the scan.
C
                     ELSE
                        NGOOD = NGOOD - 1
                        STASCN(ISCN,ISTA) = .FALSE.
                        OKSTA(ISTA) = .FALSE.
                        IF( GEOPRT .GE. 2 ) THEN
                           MSGTXT = ' '
                           WRITE( MSGTXT, '( A, I3, A, 2I5, 3A )' ) 
     1                        'GMKSCN: Dropping station ',
     2                        ISTA, '  Scan ', ISCN, SCAN1, 
     3                        '  Geosrc: ', SRCN, ' for long slew.'
                        END IF
                     END IF
                  END IF
               END IF
            END DO
         END IF
C
C        Set the output equivalent to STASCN.
C
         DO ISTA = 1, NSTA
            SSTASCN(ISTA) = STASCN(ISCN,ISTA)
         END DO
C
C     ==========================================
C     Now deal with the FORCE case.  Make STASCN match
C     SSTASCN and don't reset it based on the geometry.
C     Set NGOOD and OKSTA based on SSTASCN.
C
      ELSE IF( GMODE .EQ. 'FORCE' ) THEN
         DO ISTA = 1, NSTA
            STASCN(ISCN,ISTA) = SSTASCN(ISTA)
            IF( SSTASCN(ISTA) ) THEN
               OKSTA(ISTA) = .TRUE.
               NGOOD = NGOOD + 1
            ELSE
               OKSTA(ISTA) = .FALSE.
            END IF
         END DO
C
C        Get the geometry with the same calls as used for the
C        GMODE=SET version.
C
         IF( NGOOD .GE. 1 ) THEN
            STARTJ(ISCN) = TAPPROX
            CALL OPTTIM( LASTISCN, ISCN, .TRUE. )
            CALL SCNGEO( LASTISCN, NGOOD, ISCN )
         END IF

      ELSE
         CALL ERRLOG( 'GMKSCN: Bad GMODE.  Programming error.' )
      END IF

      RETURN
      END
