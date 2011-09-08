      SUBROUTINE PROTECT
C
C     Make a listing in the summary file of the times when
C     a couple of stations can be taken from the project.
C     This allows users to protect critical calibration scans
C     or other critical scans.
C
C     This is mainly to support scheduling of the USNO observations
C     on the VLBA.  These observations will take 1.5 hr per day on
C     the PT-MK baseline.  The USNO will have some flexibility in
C     schedule time - something like 6 hours.  This routine will
C     try to enforce providing adequate unprotected opportunities.
C
C     PESTART and PESTOP are the start and stop times of intervals
C     when preemption is ok.  Such intervals will be no less than
C     1.5 hours long.  This might need to be a parameter some day.
C
      INCLUDE 'sched.inc'
C
      INTEGER  MPROT
      PARAMETER  (MPROT=50)
      INTEGER            IP, NP, ISCN
      DOUBLE PRECISION   PESTART(MPROT), PESTOP(MPROT)
      DOUBLE PRECISION   EXSTART, EXSTOP
C  -----------------------------------------------------------------
C deal with scans out of time order.
C
C     Collect the experiment start and stop time for the listing.
C
      EXSTART = 1.D10
      EXSTOP = 0.D0
      DO ISCN = SCAN1, SCANL
         EXSTART = MIN( EXSTART, STARTJ(ISCN) )
         EXSTOP = MAX( EXSTOP, STOPJ(ISCN) )
      END DO
C
C     Initialize the time ranges.  Assume the whole time range is
C     ok to start with.
C
      NP = 1
      PESTART(1) = EXSTART
      PESTOP(1) = EXSTOP
      DO IP = 2, MPROT
         PESTART(IP) = 0.D0
         PESTOP(IP) = 0.D0
      END DO
C
C     Now look for blocked times.  Also check the PREEMPT input.
C
      DO ISCN = SCAN1, SCANL
         IF( PREEMPT(ISCN) .EQ. 'NO' ) THEN

      WRITE(*,*) 'protect: PROJECTUS INTERRUPTUS'

         ELSE IF( PREEMPT(ISCN) .NE. 'OK' ) THEN
            MSGTXT = 'PROTECT:  Invalid input PREEMPT='''//
     1               PREEMPT(ISCN)//'''.  Must be OK or NO.'
            CALL ERRLOG( MSGTXT )
         END IF

      END DO
C
C      END DO
C
      RETURN
      END
