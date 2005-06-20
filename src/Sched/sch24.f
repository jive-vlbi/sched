      SUBROUTINE SCH24
C
C     Adjust start and stop times to avoid times of 24:00:00
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
      INTEGER           ISCN, ISTA
      INTEGER           SY, SD, TY, TD
      DOUBLE PRECISION  ST, TL
      DOUBLE PRECISION  TDIFF, ONESEC, JDAY
      PARAMETER         (ONESEC = 1.D0 / 86400.D0)
      CHARACTER         TFORM*8, CSTART*8, CSTOP*8
C---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SCH24 starting' )
C
      DO ISCN = 1, NSCANS
C
         JDAY = DNINT( STOPJ(ISCN) )
         TDIFF = DABS( JDAY - STOPJ(ISCN) )
         IF( TDIFF .LT. ONESEC ) STOPJ(ISCN) = JDAY - ONESEC 
C
         TDIFF = DMOD( STARTJ(ISCN), 1.D0 )
         IF( TDIFF .GT. 1.D0 - ONESEC ) 
     1      STARTJ(ISCN) = DNINT( STARTJ(ISCN) )
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
C
C              Adjust TPSTART if it might cause a midnight start - ie
C              startj-tpstart is in the last second of the day.
C
               TDIFF = DMOD( STARTJ(ISCN) - TPSTART(ISCN,ISTA), 1.D0 )
               IF( TDIFF .GT. 1.D0 - ONESEC ) THEN
                  TPSTART(ISCN,ISTA) = 
     1                  TPSTART(ISCN,ISTA) - ( 1.D0 - TDIFF )
               END IF
            END IF
         END DO
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
      END DO
C
      RETURN
      END
