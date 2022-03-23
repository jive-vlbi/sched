      SUBROUTINE SCH24
C
C     Adjust start and stop times to avoid times of 24:00:00
C     This can happen when a time is between 23:59:59 and 24:00:00.
C     Also avoid having stop times right on 00:00:00 as per the
C     older version, although I'm not sure this is necessary.
C
C     Try not to introduce a quick recording stoppage that can throw
C     disks out of sync as per request from Brisken (Nov 2006 RCW)
C     Previously a 2 second setup scan could appear.  And don't 
C     introduce variation in TPSTART where it is not allowed (DOVEX).
C     This routine will only worry about these timing issues near 
C     midnight.  Trust other parts of SCHED to avoid other short gaps.
C
C     Warn of scans having negative duration (possible when 
C     specifying stop times and forgetting to change day number).
C     Old warning about crossing 0 hr UT commented.
C
C     Old prohibition about crossing midnight removed.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER           ISCN, ISTA, LSCN(MAXSTA)
      INTEGER           SY, SD, TY, TD
      DOUBLE PRECISION  ST, TL
      DOUBLE PRECISION  TDIFF, TWOSEC
      DOUBLE PRECISION  JDAY, RECSTRT, MINTPS
      PARAMETER         (TWOSEC = 2.D0 * ONESEC )
      CHARACTER         TFORM*8, CSTART*8, CSTOP*8
C---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SCH24 starting' )
C
      DO ISTA = 1, NSTA
         LSCN(ISTA) = 0
      END DO
C
      DO ISCN = 1, NSCANS
C
C        Move any stop time that is within plus or minus one 
C        second of midnight to one second before midnight.
C
         JDAY = DNINT( STOPJ(ISCN) )
         TDIFF = DABS( JDAY - STOPJ(ISCN) )
         IF( TDIFF .LT. ONESEC ) THEN
            STOPJ(ISCN) = JDAY - ONESEC 
         END IF
C	 	 
C        Move a scan start time that is within the one second 
C        before midnight to midnight.
C	 	 
         TDIFF = DMOD( STARTJ(ISCN), 1.D0 )
         IF( TDIFF .GT. 1.D0 - ONESEC ) 
     1      STARTJ(ISCN) = DNINT( STARTJ(ISCN) )
C
C        Move the media start time for each station to midnight 
C        if it is within the preceeding second by adjusting
C        TPSTART.
C
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
               RECSTRT = STARTJ(ISCN) - TPSTART(ISCN,ISTA)
               TDIFF = DMOD( RECSTRT, 1.D0 )
               IF( TDIFF .GT. 1.D0 - ONESEC ) THEN
                  TPSTART(ISCN,ISTA) = STARTJ(ISCN) - DNINT( RECSTRT )
               END IF
            END IF
         END DO
C
C        Adjust the TPSTARTS if needed to avoid media stoppages of
C        less than 2 seconds.  Note that, while this is meant to cover
C        any problems introduced above, it will work over the full
C        time range.  Don't just restrict this to stop times near 
C        midnight because the possible STARTJ change, combined with 
C        TPSTART, could affect a scan transition somewhat earlier.
C
C        This tangled with the effort in settps to prevent long
C        recording scans where 2 second gaps are introduced.  Change
C        settps to a 3 second gap.  This comment here so this 
C        connection is not forgotten.
C
         MINTPS = -1.D0 * UNSET
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) .AND. LSCN(ISTA) .GT. 0 ) THEN
               RECSTRT = STARTJ(ISCN) - TPSTART(ISCN,ISTA)
               IF( DABS( RECSTRT - STOPJ(LSCN(ISTA)) ) .LT. TWOSEC )
     1              THEN
                  TPSTART(ISCN,ISTA) = 
     1                STARTJ(ISCN) - STOPJ(LSCN(ISTA)) 
               END IF
               MINTPS = MIN( MINTPS, TPSTART(ISCN,ISTA) )
            END IF
         END DO
C
C        Now undo any unequal TPSTARTS when that is not allowed.
C
         IF( DOVEX .AND. MINTPS .NE. -1.D0 * UNSET ) THEN
            DO ISTA = 1, NSTA
               IF( STASCN(ISCN,ISTA) ) THEN
                  TPSTART(ISCN,ISTA) = MINTPS
               END IF
            END DO
         END IF
C
C        Check for negative duration scan.
C
         IF( STARTJ(ISCN) .GT. STOPJ(ISCN) ) THEN
            CALL TIMEJ( STARTJ(ISCN), SY, SD, ST )
            CALL TIMEJ( STOPJ(ISCN), TY, TD, TL )
            CSTART = TFORM( ST, 'T', 0, 2, 2, '::@' )
            CSTOP = TFORM( TL, 'T', 0, 2, 2, '::@' )
            WRITE(6, '( 3A, I4, /, 3A, I4 )' )
     1               ' Start: ', CSTART, ' on Day ', SD, 
     2               ' Stop:  ', CSTOP,  ' on Day ', TD
            CALL ERRLOG ( ' This is a scan of negative duration!' )
         END IF
C
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
               LSCN(ISTA) = ISCN
            END IF
         END DO
      END DO
C
      RETURN
      END
