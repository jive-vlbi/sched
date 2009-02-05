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
      INCLUDE 'sched.inc'
C
      INTEGER          LASTISCN(MAXSTA), ISCN, ISTA, LSCN
      DOUBLE PRECISION LASTTIME, TIME1J, TIME2J, T_AVAIL
      DOUBLE PRECISION MAXLASTT, TOLER
      LOGICAL          ADJUST, ALL0
      PARAMETER        (TOLER=ONESEC/1000.D0)
C --------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. 3 ) CALL WLOG( 0, 'OPTTIM: Starting.' )
      MAXLASTT = -99.D9
      LSCN = 0
C
      IF( ADJUST ) THEN
C
C        Get the start time based on previous scans and on slews.
C
         TIME1J = -99.D9
         ALL0 = .TRUE.
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) .AND. LASTISCN(ISTA) .NE. 0) THEN
C
C              Note we are using OPTGEO in the way in which T_AVAIL
C              does or does not take into account slews, depending
C              on scheduling type.

               CALL OPTGEO( ISCN, ISTA, STARTJ(ISCN), 
     1                      LASTISCN(ISTA), LASTTIME, T_AVAIL )
               TIME1J = MAX( TIME1J, T_AVAIL )
               IF( LASTTIME .GT. MAXLASTT ) THEN
                  MAXLASTT = LASTTIME
                  LSCN = LASTISCN(ISTA)
               END IF
               ALL0 = .FALSE.
            END IF
         END DO
C
C        Set time if this is the first scan for all antennas.
C
         IF( ALL0 ) THEN
            TIME1J = STARTJ(ISCN)
            MAXLASTT = STARTJ(ISCN) - GAP(ISCN)
         END IF
C
C        If DWELL is false, start the scan at the end of the
C        most recent scan.
C
         IF( .NOT. DWELL(ISCN) ) THEN
            TIME1J = MAXLASTT
         END IF
C
C        Treat GAP as the minimum interval to the next scan.
C
         TIME1J = MAX( TIME1J, MAXLASTT + GAP(ISCN) )
C    
C        Set the scan stop time based on the input scan duration.
C        But there are circumstances when the stop time is fixed
C        when the start time is not (STOP specified).  STOP is
C        also implicitly fixed with START+DUR, so the only case
C        when STOP can float is when START and STOP are not specified.
C        Allow a bit of rounding error.
C
         IF( DURONLY(ISCN) .LE. 1 ) THEN
            TIME2J = TIME1J + DUR(ISCN)
         ELSE
            TIME2J = STOPJ(ISCN)
         END IF
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
C     the preceeding scan.
C
C     Don't mess up the UPTIME optimization mode in the process.  That
C     mode can have backward time jumps.  Also catch cases where 
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

