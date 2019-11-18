      SUBROUTINE SCNGEO( LASTISCN, NGOOD, ISCN )
Cf2py intent(in) LASTISCN, ISCN
Cf2py intent(out) NGOOD
C
C     Subroutine for SCHED that gets the geometry of a scan for all
C     stations.  It is basically a station loop for STAGEO
C     (nee OPTGEO) which does the calculations for one station.
C     STAGEO gets items like the az and el, slew times etc for each
C     station.  
C
C     SCNGEO also counts the number of stations that are up which might 
C     be used to eliminate the scan later.
C
C     SCNGEO is called by SCHOPT and by routines involved in adding scans
C     for pointing or DELZN (ADDPEAK, GMKSCN, MKSCN, SRINSERT).
C
      INCLUDE      'sched.inc'
C
      INTEGER      LASTISCN(MAXSTA), NGOOD, ISCN, ISTA   
      DOUBLE PRECISION  T_AVAIL, LASTTIME
C ---------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LT. SCAN1 + 5 ) 
     1   CALL WLOG( 0, 'SCNGEO starting' ) 
C
C     Loop over stations.
C
      NGOOD = 0
      DO ISTA = 1, NSTA
C
C        Get the geometry for all stations.  Occasionally tables
C        are printed that show the elevations, for example, of stations
C        including those that are down.  So calculate that for all
C        stations, but then only use the stations in the scan to 
C        calculate things like slews.
C
C        Get the geometry at each antenna for this source, whether or not
C        it will observe.  The output arguments for STAGEO (LASTTIME
C        and T_AVAIL) are not used for this application).
C
         CALL STAGEO( ISCN, ISTA, STARTJ(ISCN), LASTISCN(ISTA),
     1              LASTTIME, T_AVAIL, 'SCNGEO' )
C
C        Count the number of stations in the scan that will get good
C        data.
C
         IF( STASCN(ISCN,ISTA) ) THEN
C     
C           Count the number of antennas that are up.  Will use later
C           to decide whether to keep the scan.   For this
C           to matter, non-default OPMINANT and OPMINEL would
C           have to be specified. 
C     
            IF( EL1(ISCN,ISTA) .GE. OPMINEL(ISCN) .AND. 
     1          EL2(ISCN,ISTA) .GE. OPMINEL(ISCN) .AND.
     2          UP1(ISCN,ISTA) .EQ. ' ' .AND. 
     3          UP2(ISCN,ISTA) .EQ. ' ' ) THEN
               NGOOD = NGOOD + 1
            END IF
C     
         ELSE
C     
C           Clear the UP indicators for scans that are not used to keep
C           from cluttering the summary etc.
C           Note that AUTODOWN has not yet been called.  UP indicators
C           for stations scheduled to be in the scan, but removed by 
C           AUTODOWN, will be left intact.
C     
            UP1(ISCN,ISTA) = ' '
            UP2(ISCN,ISTA) = ' '
C     
         END IF
      END DO
C
      RETURN
      END
