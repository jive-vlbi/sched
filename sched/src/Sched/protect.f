      SUBROUTINE PROTECT( IUNIT )
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
C     when preemption is not ok.  Such intervals have intervals between
C     unpreemptable scans of less than MINTPE long (set to 2 hr).  
C
C     The algorithm would be fairly simple if it weren't that SCHED 
C     does not demand that scans be in time order.  Only each station
C     needs to be in time order, but subarrays can make a hash out
C     of the ordering.
C
C     Allow a first and last preemptable block that are shorter as
C     the USNO observation could start earlier or stop later than
C     the project.
C
      INCLUDE 'sched.inc'
C
      INTEGER            MPROT
      DOUBLE PRECISION   MINTPE, MAXBLOCK
      PARAMETER          (MPROT = 50)
      PARAMETER          (MINTPE = 1.6666D0 / 24.D0 )      
      PARAMETER          (MAXBLOCK = 4.D0 / 24.D0 )
      INTEGER            IP, NP, ISCN, JSCN, IUNIT, ISTA
      INTEGER            SY, SD, TY, TD, LEN1
      INTEGER            IOERR, VLBOPE
      LOGICAL            GOTPTMK, TOOLONG, IGWARN, EXISTS
      LOGICAL            SCNPTMK(MAXSCN)
      DOUBLE PRECISION   ST, TL
      DOUBLE PRECISION   PESTART(MPROT), PESTOP(MPROT)
      DOUBLE PRECISION   EXSTART, EXSTOP, BLOCK1, BLOCKL
      CHARACTER          TFORM*8, CSTART*8, CSTOP*8
      CHARACTER          PREFILE*80, OPSTAT*4, OPTEXT*255
C  -----------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'PROTECT starting' )

C *********************************
C  Automatically protect the DELZN segments if the user has not
C  specified any PREEMPTs
C *********************************
C
C     Check if PT or MK are included in the project.  If not, 
C     make a quick note and RETURN.  Also get presence of the
C     stations on a scan by scan basis.
C
      GOTPTMK = .FALSE.
      DO ISTA = 1, NSTA
         IF( STANAME(ISTA) .EQ. 'VLBA_PT' .OR. 
     1       STANAME(ISTA) .EQ. 'VLBA_MK' ) THEN
            GOTPTMK = .TRUE.
         END IF
      END DO
      IF( .NOT. GOTPTMK ) THEN
         MSGTXT = 'PROTECT:  No PT or MK, so will not check that '//
     1       'preemptable times are present.'
         CALL WLOG( 1, MSGTXT )
         RETURN
      END IF
      DO ISCN = SCAN1, SCANL
         SCNPTMK(ISCN) = .FALSE.
         DO ISTA = 1, NSTA
            IF( ( STANAME(ISTA) .EQ. 'VLBA_PT' .OR. 
     1          STANAME(ISTA) .EQ. 'VLBA_MK' ) .AND.
     2          STASCN(ISCN,ISTA) ) THEN
               SCNPTMK(ISCN) = .TRUE.
            END IF
         END DO
      END DO
C
C     Open the .preempt file.  Construct the name, then check if it
C     exists.  Finally open it with VLBOPE.
C
      WRITE( PREFILE, '( A,A )' )  EXPCODE(1:LEN1(EXPCODE)), '.PREEMPT'
      CALL DWCASE( PREFILE )
      INQUIRE( FILE=PREFILE, EXIST=EXISTS )
      IF( EXISTS .AND. OVERWRIT ) THEN
         OPSTAT = 'OLD'
      ELSE IF( EXISTS ) THEN
         WRITE( MSGTXT, '( A, A, A )' )  'PROTECT: ', 
     1       PREFILE(1:LEN1(PREFILE)), ' already exists.'
         CALL WLOG( 1, MSGTXT )
         CALL ERRLOG( 
     1      'PROTECT: You need to delete old output files '//
     2      'or use OVERWRIT.')
      ELSE
         OPSTAT = 'NEW'
      END IF
      IOERR = VLBOPE( IPRE, PREFILE, 'TEXT', OPSTAT, OPTEXT )
      IF( IOERR .NE. 1 ) CALL ERRLOG( ' PROTECT open problem:'//OPTEXT )
C
C     Collect the experiment start and stop time and the first
C     and last times of protected data.
C
      EXSTART = 1.D10
      EXSTOP = 0.D0
      DO ISCN = SCAN1, SCANL
         EXSTART = MIN( EXSTART, STARTJ(ISCN) )
         EXSTOP = MAX( EXSTOP, STOPJ(ISCN) )
      END DO
      BLOCK1 = EXSTOP
      BLOCKL = EXSTART
      DO ISCN = SCAN1, SCANL
         IF( PREEMPT(ISCN) .EQ. 'NO' .AND. SCNPTMK(ISCN) ) THEN
            BLOCK1 = MIN( BLOCK1, STARTJ(ISCN) )
            BLOCKL = MAX( BLOCKL, STOPJ(ISCN) )
         END IF
      END DO
C
C     Initialize the time ranges.  Assume the whole time range is
C     ok to start with.
C
      DO IP = 1, MPROT
         PESTART(IP) = EXSTART
         PESTOP(IP) = EXSTOP
      END DO
C
C     Now look for blocks of preemptable times in a manner that works
C     with scans not in time order.
C
C     First see if there is a block at the start of non-zero
C     length.
C
      NP = 0
      IF( BLOCK1 .GT. EXSTART ) THEN
         PESTART(1) = EXSTART
         PESTOP(1) = BLOCK1
         NP = 1
      END IF
C
C     Sanity check PREEMPT, but only on the scans that matter.
C
      DO ISCN = SCAN1, SCANL
         IF( PREEMPT(ISCN) .NE. 'OK' .AND. 
     1       PREEMPT(ISCN) .NE. 'NO' ) THEN
            MSGTXT = 'PROTECT:  Invalid input PREEMPT='''//
     1         PREEMPT(ISCN)//'''.  Must be OK or NO.'
            CALL ERRLOG( MSGTXT )
         END IF
      END DO
C
C     Now look for other good periods based on scan times.
C     Assume that any available block other than the first (which
C     has already been specified) will start with the end of
C     a protected scan.  Then see if such a block is long enough.
C     If not, ignore it.  If so, add it to the list.  Note that
C     there will be no further attempts to start a block in that
C     gap because, by definition, there aren't any. 
C     It is just possible, in very weird circumstances, that 
C     finding the open periods this way will result in blocks out 
C     of time order.  In this process, ignore scans without PT or MK.
C     Don't do this for the last scan (in terms of time) as it 
C     cannot start a valid interval.
C
      DO ISCN = SCAN1, SCANL
         IF( PREEMPT(ISCN) .EQ. 'NO' .AND. SCNPTMK(ISCN) .AND.
     1       STOPJ(ISCN) .NE. EXSTOP ) THEN
            NP = NP + 1
            PESTART(NP) = STOPJ(ISCN)
            DO JSCN = SCAN1, SCANL
C
C              Look only at blocked scans that include PT and MK.  
C              Also don't look at the current scan (ISCN).
C
               IF( JSCN .NE. ISCN .AND.
     1             PREEMPT(JSCN) .EQ. 'NO' .AND. SCNPTMK(JSCN) ) THEN
C
C                 Find if this scan makes the gap after PESTART too
C                 short.  If so, this PESTART cannot be used so jump
C                 to the next ISCN.  It must be a scan that does
C                 extend to after PESTART to be of interest.
C
                  IF( STOPJ(JSCN) .GT. PESTART(NP) ) THEN
                     IF( STARTJ(JSCN) .LT. PESTART(NP) + MINTPE ) THEN
C
C                       Available period is too short.
C
                        NP = NP - 1
                        GO TO 100
                     ELSE
C
C                       Set the end of the available period to the
C                       earliest of the start of the test scan or the
C                       current stop time of the interval.
C
                        PESTOP(NP) = MIN( STARTJ(JSCN), PESTOP(NP) )
                     END IF
                  END IF
               END IF
            END DO
         END IF
C
C        Jump here when this scan stop can't be used as the start
C        of a preemptable period.
C
  100    CONTINUE
      END DO
C
C     Now see if there is a short, but finite period of preemptable
C     scans at the end.
C
      IF( ABS( PESTOP(NP) - EXSTOP ) .GT. ONESEC .AND. 
     1    ABS( BLOCKL - EXSTOP ) .GT. ONESEC ) THEN
         NP = NP + 1
         PESTART(NP) = BLOCKL
         PESTOP(NP) = EXSTOP
      END IF       
C
C     Write the periods and complain if there are inadequate times
C     to run the EOP project.
C
      WRITE( IUNIT, '( A )' ) ' '
      MSGTXT = 'ALLOWED PREEMPTION TIMES:'
      WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
      WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
C
      WRITE( IUNIT, '( A )' ) ' '
      WRITE( IPRE, '( A )' ) ' '
C
      MSGTXT = ' '
      WRITE( MSGTXT, '( A, F4.1, A, A, A )' ) 
     1    ' Daily USNO EOP observations of ', MINTPE * 24.D0, 
     2    ' hr on one baseline (usually PT-MK) will be made.'
      WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
      WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
C
      MSGTXT = 
     1   ' These will preempt other projects for those two stations.'
      WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
      WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
C
      MSGTXT = 
     1    ' Usually this will happen between 14:00 and 22:00 UT.'
      WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
      WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
C
      MSGTXT = ' Gaps between projects will be used when possible.'
      WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
      WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
C
      MSGTXT = ' Input parameter PREEMPT can be used to protect '//
     1    'important scans - see manual.'
      WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
      WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
C
C     Deal with entire project protected.
C
      IGWARN = .FALSE.
      IF( NP .EQ. 0 ) THEN
C
         MSGTXT = ' *******'
         WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
         WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
         CALL WLOG( 1, MSGTXT )
C
         MSGTXT = ' ******* This schedule has no times that ' //
     1     'can be preempted for EOP observations.'
         WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
         WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
         CALL WLOG( 1, MSGTXT )
C
C        Deal with short projects.
C
         IF( EXSTOP - EXSTART .LE. MAXBLOCK ) THEN
C
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F5.1, A )' ) 
     1          ' ******* But the project is less than ', 
     2          MAXBLOCK * 24.D0, 
     2          ' hours long so this may not be a'
            WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            CALL WLOG( 1, MSGTXT )
C
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A)' ) 
     1          ' ******* problem unless 2 such runs are scheduled ',
     2          'back to back.'
            WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            CALL WLOG( 1, MSGTXT )
C
            MSGTXT = ' ******* If that happens, your protection '//
     2          'requests may be ignored.'
            WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            CALL WLOG( 1, MSGTXT )
C
            MSGTXT = ' ******* See .sum file or .preempt file'//
     1           ' for details.'
            CALL WLOG( 1, MSGTXT )
C
            MSGTXT = ' *******'
            WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            CALL WLOG( 1, MSGTXT )
C
         ELSE
C
C           Now the bad case of a long project.
C
            IGWARN = .TRUE.
         END IF
      ELSE
C
         MSGTXT = ' Unprotected time ranges that could be used are: '
         WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
         WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
C
         DO IP = 1, NP
            CALL TIMEJ( PESTART(IP), SY, SD, ST )
            CALL TIMEJ( PESTOP(IP), TY, TD, TL )
            CSTART = TFORM( ST, 'T', 0, 2, 2, '::@' )
            CSTOP = TFORM( TL, 'T', 0, 2, 2, '::@' )
            MSGTXT = ' '
            WRITE( MSGTXT, 
     1          '( I3, 4X, A, I3.3, A, A8, A, I3.3, A,  A8 )' ) 
     2          IP, '(', SD, ') ', CSTART, '  to  (', TD, ') ', CSTOP
            WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
         END DO
C
C        Write the time range of the observations.
C
         CALL TIMEJ( EXSTART, SY, SD, ST )
         CALL TIMEJ( EXSTOP, TY, TD, TL )
         CSTART = TFORM( ST, 'T', 0, 2, 2, '::@' )
         CSTOP = TFORM( TL, 'T', 0, 2, 2, '::@' )
         MSGTXT = ' '
         WRITE( MSGTXT, 
     1       '( A, 4X, A, I3.3, A, A8, A, I3.3, A,  A8 )' ) 
     2       ' Project start and stop: ', '(', SD, ') ', 
     3       CSTART, '  to  (', TD, ') ', CSTOP
         WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
         WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
C
C        Check for adequate options.  Look for, and complain about, 
C        protected periods of MAXBLOCK hours or more.  This assumes
C        time order, but that should be the case given how the blocks
C        were derived.
C
         TOOLONG = .FALSE.
         IF( PESTART(1) - EXSTART .GT. MAXBLOCK ) TOOLONG = .TRUE.
         IF( EXSTOP - PESTOP(NP) .GT. MAXBLOCK ) TOOLONG = .TRUE.
         IF( NP .GE. 2 ) THEN
            DO IP = 2, NP
               IF( PESTART(NP) - PESTOP(NP-1) .GT. MAXBLOCK ) 
     1            TOOLONG = .TRUE.
            END DO
         END IF
C
C        Complain about long blocked periods.
C
         IF( TOOLONG ) THEN
C
C
            MSGTXT = ' *******'
            WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            CALL WLOG( 1, MSGTXT )
C
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F5.1, A )' )
     1         ' ******* There is at least one block of over ', 
     2         MAXBLOCK * 24.D0, 
     3         ' hr when your PREEMPT specification does not'
            WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            CALL WLOG( 1, MSGTXT )
C
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, F5.1, A, A )' )
     1         ' ******* allow ', MINTPE * 24.D0,
     2         ' hr EOP observations on ',
     3         'one baseline (PT-MK usually) to be inserted.'
            WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            CALL WLOG( 1, MSGTXT )
C
            MSGTXT = ' '
            WRITE( MSGTXT, '( A )' )
     1         ' ******* See details in the .sum or .preempt file.'
            WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
            CALL WLOG( 1, MSGTXT )
C
            IGWARN = .TRUE.
         END IF
      END IF
C
C        Warn user his/her instructions may be ignored.
C
      IF( IGWARN ) THEN 
C
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, A )' )
     1      ' ******* Your requests to protect scans will be ',
     2      'ignored.'
         WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
         WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
         CALL WLOG( 1, MSGTXT )
C
         MSGTXT = ' '
         WRITE( MSGTXT, '( A )' )
     1      ' ******* See the SCHED manual info on parameter PREEMPT.'
         WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
         WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
         CALL WLOG( 1, MSGTXT )
C
         MSGTXT = ' *******'
         WRITE( IUNIT, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
         WRITE( IPRE, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
         CALL WLOG( 1, MSGTXT )
      END IF
C
      RETURN
      END
