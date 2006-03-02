      SUBROUTINE SCNGEO( LASTISCN, NGOOD, ISCN )
C
C     Subroutine for SCHED that gets the geometry of an observation.
C     It gets items like the az and el, slew times etc for each
C     station.  It also counts the number of stations that are up
C     which might be used to eliminate the scan later.
C
      INCLUDE      'sched.inc'
C
      INTEGER      LASTISCN(MAXSTA), NGOOD, ISCN, ISTA   
C ---------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LT. SCAN1 + 5 ) 
     1   CALL WLOG( 0, 'SCNGEO starting' ) 
C
C     Loop over stations.
C
      NGOOD = 0
      DO ISTA = 1, NSTA
C
C        Only bother with stations that are in the scan.
C
         IF( STASCN(ISCN,ISTA) ) THEN
C
C           Get the geometry at each antenna.
C
            CALL SCHSRC( LASTISCN(ISTA), ISCN, ISTA, 
     1                   STARTJ(ISCN), STOPJ(ISCN) )
C     
C           Get the time the antenna got to source.
C     
            IF( LASTISCN(ISTA) .EQ. 0 ) THEN
               TONSRC(ISCN,ISTA) = STARTJ(ISCN)
            ELSE
               CALL SLEW( ISCN, LASTISCN(ISTA), ISTA )
               TONSRC(ISCN,ISTA) = STOPJ(LASTISCN(ISTA)) + 
     1              TSLEW(ISCN,ISTA)
            END IF
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
C     
            UP1(ISCN,ISTA) = ' '
            UP2(ISCN,ISTA) = ' '
C     
         END IF
      END DO
C
      RETURN
      END





