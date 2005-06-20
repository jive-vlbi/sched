      SUBROUTINE OPTGEO( ISCN, ISTA, T_EST1, LSCN,
     1                   LASTTIME, T_AVAIL)
C
C     General purpose geometry routine for optimazition mode 
C     subroutines in SCHED.
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
C       be the end of the previous scan.  For this reason, this
C       should not be used for the final setting of TONSRC.
C
C       The geometric parameters are in the HA1, EL1 etc arrays.
C       This is set to the next even second.
C
C     If this is the first scan (LSCN=0), return 0.D0 for
C     LASTTIME and T_AVAIL.  The calling routine should not use
C     these times.
C
      INCLUDE 'sched.inc'
C
      INTEGER          ISCN, ISTA, LSCN
      DOUBLE PRECISION T_EST1, T_EST2, T_AVAIL, LASTTIME
C ----------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. 3 ) CALL WLOG( 0, 'OPTGEO: Starting.' )
C
C     Get geometric parameters for this station and this scan.
C
      T_EST2 = T_EST1 + DUR(ISCN)
      CALL SCHSRC( LSCN, ISCN, ISTA, T_EST1, T_EST2 )
      CALL SLEW( ISCN, LSCN, ISTA )
C
C     Get the last observing time and first available time for this
C     antenna.
C
      IF( LSCN .NE. 0 ) THEN
         LASTTIME = STOPJ(LSCN)
         IF( UP1(ISCN,ISTA) .EQ. ' ' .AND. UP2(ISCN,ISTA) .EQ. ' ' ) 
     1       THEN
C
C           Source is up.  Get the earliest time that the source is 
C           available at that antenna.  Honor requests to not take 
C           slew into account.  Won't get here for ISCN=1.
C
            IF( ( DURONLY(ISCN) .EQ. 1 .OR. DURONLY(ISCN) .EQ. 4 ) 
     1          .AND. DWELL(ISCN) ) THEN
               T_AVAIL = LASTTIME + TSLEW(ISCN,ISTA)
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
      ELSE
C
C        First scan for this antenna.
C
         LASTTIME = 0.D0
         T_AVAIL = 0.D0
      END IF
C
      RETURN 
      END
