      SUBROUTINE GMKSCN( LASTISCN, ISCN, JSCN, ISRC, SRCN, TAPPROX, 
     1                 MINEL, KEEPSTA, NGOOD, OKSTA, SSTASCN, GMODE )
Cf2py intent(in) LASTISCN, ISCN, JSCN, ISRC, SRCN, TAPPROX, MINEL, KEEPSTA
Cf2py intent(in) GMODE
Cf2py intent(out) NGOOD
C OKSTA is an out argument, but make it (in, out) to define its size
Cf2py intent(in, out) OKSTA, SSTASCN
C
C     This is varient of MAKESCN to be used in the process of the
C     geodetic scan insertion.  Besides what MAKESCN does, it can
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
C     KEEPSTA can be set to an antenna that should not be dropped
C             for long slews.
C
      INCLUDE 'sched.inc'
C
C     Call arguments, mostly for MAKESCN
C 
      INTEGER            LASTISCN(*), ISCN, JSCN, ISRC, NGOOD
      INTEGER            KEEPSTA
      CHARACTER          SRCN*12, GMODE*5
      REAL               MINEL
      DOUBLE PRECISION   TAPPROX
      LOGICAL            OKSTA(*), SSTASCN(*)
C
C     Internal variables.
C
      INTEGER            ISTA, INSCN, ARRSEQ(MAXSTA), K, ITEMP
      DOUBLE PRECISION   LASTTIME, T_AVAIL, TMED
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
      CALL SCNDUP ( ISCN, JSCN, .FALSE., 'GMKSCN' )
C
C     Insert the source.
C
      SRCNUM(ISCN) = ISRC
      SCNSRC(ISCN) = SRCN
C
C     Deal with a request to make a scan but determine and leave off 
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
               CALL STAGEO( ISCN, ISTA, TAPPROX, LASTISCN(ISTA),  
     1             LASTTIME, T_AVAIL, 'GMKSCN' )
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
C        Set the scan stations list based on the results from STAGEO.
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
C           This reruns the above STAGEO so it could possibly be made
C           more efficient some day.  Assume LASTISCN and LASTSSCN
C           are the same in circumstances where GMKSCN is used, or
C           at least that such an assumption will not get us in trouble.
C           The difference relates to whether or not the scan is 
C           preceded by automatically inserted pointing scans.
C      
            CALL OPTTIM( LASTISCN, LASTISCN, ISCN, .TRUE., 
     1                   .FALSE., .FALSE. )
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
C           Get the time to compare with TONSRC to determine if an
C           antenna gets there late.  Naively this could be the 
C           average time of arrival of the antennas that were in 
C           the previous scan (ISCN-1).  I tried 
C           that.  But if there is a scan with no antennas in the
C           previous scan, this creates trouble.  I hit that at the
C           start of a geodetic sequence following a pointing scan
C           at the GBT.  So instead try something else.  For the 
C           moment, it will be the third last arrival time.
C
            INSCN = 0
            DO ISTA = 1, NSTA
               ARRSEQ(ISTA) = 0
            END DO
            DO ISTA = 1, NSTA
               IF( STASCN(ISCN,ISTA) ) THEN
                  INSCN = INSCN + 1
C
C                 Put current station at end of list, then sort to
C                 right place.
C
                  ARRSEQ(INSCN) = ISTA
                  IF( INSCN .GE. 2 ) THEN
                     DO K = INSCN, 2, -1
                        IF( TONSRC(ISCN,ARRSEQ(K)) .GT. 
     1                      TONSRC(ISCN,ARRSEQ(K-1)) ) THEN
                           ITEMP = ARRSEQ(K-1)
                           ARRSEQ(K-1) = ARRSEQ(K)
                           ARRSEQ(K) = ITEMP
                        END IF
                     END DO
                  END IF
               END IF
            END DO
            IF( INSCN .GT. 0 ) THEN
               TMED = TONSRC(ISCN,ARRSEQ(MIN(INSCN,3)))
            ELSE
               CALL ERRLOG( 'GMKSCN: INSCN zero.  Programming error' )
            END IF
C
C       Various debugging printouts I don't want to retype if I need them
C       again.
C            write(*,*) 'gmkscn median', iscn, inscn, ' ', srcn, 
C      1        (tonsrc(iscn,arrseq(1))-55452.D0)/onesec, 
C      2        (tmed-55452.D0)/onesec, 
C      1        (tonsrc(iscn,arrseq(1))-55452.D0)/onesec - 
C      2        (tmed-55452.D0)/onesec, 
C      3        arrseq(1), arrseq(2), arrseq(3)
C
C      Note that the following debugging print is specific to an 
C      11 station observation so it is not subject to GEOPRT.
C       write(*,'(A,A,11l2,11F6.0,3X,11F6.1)') 'gmkscn --', srcn,
C     1     (stascn(iscn,ista),ista=1,nsta),
C     2     ((tonsrc(iscn,ista)-tonsum)/onesec,ista=1,nsta),
C     3     (el1(iscn,ista),ista=1,nsta)
C
C
C           Deal with late stations.
C           But don't drop the antenna that is really needs improvement.
C
            DO ISTA = 1, NSTA
               IF( STASCN(ISCN,ISTA) .AND. ISTA .NE. KEEPSTA .AND.
     1             LASTISCN(ISTA) .NE. 0 ) THEN
                  IF( TONSRC(ISCN,ISTA) .GT. 
     1                TMED + GEOSLOW * ONESEC ) THEN
C
C                    Low elevation scan for the slow antenna.
C
                     IF( EL1(ISCN,ISTA) .LT. GEOLOWEL ) THEN
                        NGOOD = 0
                        IF( GEOPRT .GE. 2 ) THEN
                           MSGTXT = ' '
                           WRITE( MSGTXT,* ) 
     1                         '**gmkscn: Dropping scan - '//
     2                         'low el for slow ant',
     3                         ISTA, ISCN, '   ', SRCN, 
     4                         EL1(ISCN,ISTA)
                           CALL WLOG( 0, MSGTXT )
                        END IF
C
C                    High elevation scan for the slow antenna.
C                    Remove the station from the scan.
C
                     ELSE
                        NGOOD = MAX( NGOOD - 1, 0 )
                        STASCN(ISCN,ISTA) = .FALSE.
                        OKSTA(ISTA) = .FALSE.
                        IF( GEOPRT .GE. 2 ) THEN
                           MSGTXT = ' '
                           WRITE( MSGTXT, '( A, I3, A, 2I5, 3A )' ) 
     1                        '++gmkscn: Dropping station ',
     2                        ISTA, '  Scan ', ISCN, SCAN1, 
     3                        '  Geosrc: ', SRCN, ' for long slew.'
                           CALL WLOG( 0, MSGTXT )
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
C        GMODE=SET version.  Assume that it is ok to use
C        LASTISCN for LASTSSCN.  This certainly true if the
C        preceding scan is not an inserted pointing scan.
C
         IF( NGOOD .GE. 1 ) THEN
            STARTJ(ISCN) = TAPPROX
            CALL OPTTIM( LASTISCN, LASTISCN, ISCN, .TRUE., 
     1                   .FALSE., .FALSE. )
            CALL SCNGEO( LASTISCN, NGOOD, ISCN )
         END IF

      ELSE
         CALL ERRLOG( 'GMKSCN: Bad GMODE.  Programming error.' )
      END IF
      RETURN
      END
