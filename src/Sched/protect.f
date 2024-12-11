      SUBROUTINE PROTECT( IUNIT )
C
C     Called by SUMOPE during writing of the .sum file.  Must be
C     called before the main scan listings and the generation of
C     the crd and VEX files.
C
C     Make a listing in the summary file (IUNIT) of the times when
C     a couple of stations can be taken from the project.
C     This allows users to protect critical calibration scans
C     or other critical scans.
C
C     Also deal with specification of "extra" scans whose scheduling
C     is optional.  Only allow that at the ends of the project.
C
C     The preempt concept is  mainly to support scheduling of the 
C     USNO observations on the VLBA.  These observations will take 
C     1.5 hr per day on the PT-MK baseline.  The USNO will have 
C     some flexibility in schedule time - something like 6 hours.  
C     This routine will try to enforce providing adequate 
C     unprotected opportunities.
C
C     PESTART and PESTOP are the start and stop times of intervals
C     when preemption is ok.  Such intervals have intervals between
C     unpreemptable scans longer than MINTPE long (set to 2 hr).  
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
      INTEGER            SY, SD, TY, TD, CBY, CBD, CEY, CED, LEN1
      INTEGER            IOERR, VLBOPE, CORESC1, CORESCL
      LOGICAL            GOTPTMK, TOOLONG, IGWARN, EXISTS
      LOGICAL            SCNPTMK(MAXSCN), ADDEND
      DOUBLE PRECISION   ST, TL, CBT, CEL
      DOUBLE PRECISION   PESTART(MPROT), PESTOP(MPROT)
      DOUBLE PRECISION   BLOCK1, BLOCKL
      CHARACTER          TFORM*8, CSTART*8, CSTOP*8
      CHARACTER          CBSTART*8, CESTOP*8
      CHARACTER          PREFILE*80, OPSTAT*4, OPTEXT*255
      CHARACTER          LINE1*80, LINE2*80, LINE3*80, LINE4*80 
      CHARACTER          LINE5*80, LINE6*80, LINE7*80
C  -----------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'PROTECT starting' )
C
C     First check PREEMPT for valid arguments.
C
      DO ISCN = SCAN1, SCANL
         IF( PREEMPT(ISCN) .NE. 'OK' .AND. 
     1       PREEMPT(ISCN) .NE. 'NO' .AND.
     2       PREEMPT(ISCN) .NE. 'EXTRA' ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A, A, I5, A )' )
     1         'PROTECT:  Invalid input PREEMPT=''',
     2         PREEMPT(ISCN)(1:LEN1(PREEMPT(ISCN))), ''' on scan ', 
     3         ISCN, '.  Must be OK, NO, or EXTRA.'
            CALL ERRLOG( MSGTXT )
         END IF
      END DO
C
C     If the the VLBA is not in use and no PREEMPT=EXTRA scans were
C     specified, get out of here silently.  This routine will 
C     only cause confusion.
C
      IF( .NOT. GOTVLBA .AND. .NOT. FUZZY ) RETURN
C
C     Recall that the default PREEMPT is OK for normal scans
C     and NO for DELZN scans (GEOLEN .NE. 0).  That was set
C     in SCHIN.  FUZZY was also set there if any PREEMPT=EXTRA scans
C     were seen.
C
C     Check if PT or MK are included in the project.  If not, 
C     record that fact and don't worry about preemptable scans.
C     But will still need to worry about EXTRA scans if FUZZY.
C
      GOTPTMK = .FALSE.
      DO ISTA = 1, NSTA
         IF( STANAME(ISTA) .EQ. 'VLBA_PT' .OR. 
     1       STANAME(ISTA) .EQ. 'VLBA_MK' ) THEN
            GOTPTMK = .TRUE.
         END IF
      END DO
      IF( .NOT. GOTPTMK ) THEN
         CALL WLOG( 1, 'PROTECT:  No PT or MK, so will not check '//
     1       'that times preemptable for USNO are present.' )
         IF( .NOT. FUZZY ) RETURN
      END IF
C
C     Determine scan by scan if PT or MK are present.
C
      DO ISCN = SCAN1, SCANL
         SCNPTMK(ISCN) = .FALSE.
         IF( GOTPTMK ) THEN
            DO ISTA = 1, NSTA
               IF( ( STANAME(ISTA) .EQ. 'VLBA_PT' .OR. 
     1             STANAME(ISTA) .EQ. 'VLBA_MK' ) .AND.
     2             STASCN(ISCN,ISTA) ) THEN
                  SCNPTMK(ISCN) = .TRUE.
               END IF
            END DO
         END IF
      END DO
C
C     By this point, either PT and MK are present, or EXTRA scans were
C     specified.  Open the .preempt file.  Construct the name, then 
C     check if it  exists.  Finally open it with VLBOPE.
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
C     Collect the first and last times of protected data (the core), 
C     and the times of the first and last scans of the core 
C     (not PREEMPT(ISCN)=EXTRA).  The overall experiment first and 
C     last times are already in TFIRST and TEND.
C
      CORESC1 = 100000
      CORESCL = 1
      COREBEG = 1.D10
      COREEND = 0.D0
      DO ISCN = SCAN1, SCANL
         IF( PREEMPT(ISCN) .NE. 'EXTRA' ) THEN
            COREBEG = MIN( COREBEG, STARTJ(ISCN) )
            COREEND = MAX( COREEND, STOPJ(ISCN) )
            CORESC1 = MIN( CORESC1, ISCN )
            CORESCL = MAX( CORESCL, ISCN )
         END IF
      END DO
C
C     Don't allow all EXTRA scans.  Also don't allow 
C     EXTRA scans in the core range.
C
      IF( COREBEG .EQ. 1.D10 ) THEN
         CALL ERRLOG( 'PROTECT:  You must have some scans with '//
     1        'PREEMPT not set to EXTRA.' )
      END IF
      DO ISCN = CORESC1, CORESCL
         IF( PREEMPT(ISCN) .EQ. 'EXTRA' ) THEN
            CALL WLOG( 1, 'PROTECT: PREEMPT=EXTRA is only allowed in '//
     1         'blocks at the start and end.' )
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I5, A )' )
     1         '         Scan ', ISCN, ' is in the core and is EXTRA.'
            CALL WLOG( 1, MSGTXT )
            CALL ERRLOG( 
     1           'Please change PREEMPT for the offending scan.' )
         END IF
      END DO
C
C     Get BLOCK1 and BLOCKL, the first and last times for protected
C     scans.
C
      BLOCK1 = TEND
      BLOCKL = TFIRST
      DO ISCN = SCAN1, SCANL
         IF( PREEMPT(ISCN) .EQ. 'NO' .AND. SCNPTMK(ISCN) ) THEN
            BLOCK1 = MIN( BLOCK1, STARTJ(ISCN) )
            BLOCKL = MAX( BLOCKL, STOPJ(ISCN) )
         END IF
      END DO
C
C     Initialize the time ranges of the individual preemptable periods.  
C     Assume the whole time range is ok to start with.
C
      DO IP = 1, MPROT
         PESTART(IP) = TFIRST
         PESTOP(IP) = TEND
      END DO
C
C     Now look for blocks of preemptable times in a manner that works
C     with scans not in time order.
C
C     First see if there is a block at the start of non-zero
C     length.
C
      NP = 0
      IF( BLOCK1 .GT. TFIRST ) THEN
         PESTART(1) = TFIRST
         PESTOP(1) = BLOCK1
         NP = 1
      END IF
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
     1       STOPJ(ISCN) .NE. TEND ) THEN
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
C     scans at the end.  If the final period is long, then PESTOP(NP)
C     should equal TEND and a segment won't be added.
C
      ADDEND = .FALSE.
      IF( NP .EQ. 0 ) THEN
         ADDEND =  ABS( BLOCKL - TEND ) .GT. ONESEC
      ELSE 
         ADDEND =  ABS( PESTOP(NP) - TEND ) .GT. ONESEC .AND. 
     1             ABS( BLOCKL - TEND ) .GT. ONESEC 
      END IF
      IF( ADDEND ) THEN
         NP = NP + 1
         PESTART(NP) = BLOCKL
         PESTOP(NP) = TEND
      END IF       
C
C     Write the periods and complain if there are inadequate times
C     to run the EOP project.  This included a fair amount of text
C     which is being written to both the .sum file and the .preempt files.
C
      LINE1 = ' Dynamically scheduled VLBA projects can have '//
     1        'optional scans at'
      LINE2 = ' the start and end designated by PREEMPT=''EXTRA''.  '//
     1        'These can be'
      LINE3 = ' used or not used to help with efficient meshing '//
     1        'with previous'
      LINE4 = ' and following projects.  Only the core time range '//
     1        'will be used '
      LINE5 = ' for the initial dynamic project selection.  DOSCANS'//
     1        'can be used to '
      LINE6 = ' restrict the scan range sent to the .vex, .oms, '//
     1        'crd., sch., and'
      LINE7 = ' .flag files. '
C
      WRITE( IUNIT, '( A, 10( /, A ) )' )
     1        ' ', ' ', 'ALLOWED PREEMPTION TIMES', ' ', 
     2        LINE1, LINE2, LINE3, LINE4, LINE5, LINE6, LINE7
      WRITE( IPRE, '( A, 8( /, A ) )' )
     1        'ALLOWED PREEMPTION TIMES', ' ', 
     2        LINE1, LINE2, LINE3, LINE4, LINE5, LINE6, LINE7
C
      LINE1 = ' The schedule can also designate which scans can be '//
     1        'preempted at'
      LINE2 = ' PT and MK for daily USNO EOP observations using '//
     1        'PREEMPT=''OK'' '
      LINE3 = ' or ''NO''.  For details, see the manual discussion '//
     1        'of PREEMPT.'
      WRITE( IUNIT, '( A, 4( /, A ) )' ) ' ', LINE1, LINE2, LINE3, ' '
      WRITE( IPRE, '( A, 4( /, A ) )' ) ' ', LINE1, LINE2, LINE3, ' '
C
C     Deal with the case where the entire project is protected.  Complain,
C     but then be forgiving if the project is short.  This information also
C     goes to the screen and logfile.
C
      IGWARN = .FALSE.
      IF( NP .EQ. 0 ) THEN
C
         LINE1 = ' ******* This schedule has no times that ' //
     1     'can be preempted for EOP observations.'
         WRITE( IUNIT, '( A, / ,A )' ) ' *******', LINE1
         WRITE( IPRE, '( A, /, A )' ) ' *******', LINE1
         CALL WLOG( 1, ' *******' )
         CALL WLOG( 1, LINE1 )
C
C        Deal with short projects.
C
         IF( TEND - TFIRST .LE. MAXBLOCK ) THEN
C
            LINE1 = ' '
            WRITE( LINE1, '( A, F5.1, A )' ) 
     1          ' ******* But the project is less than ', 
     2          MAXBLOCK * 24.D0, ' hours long so this may not be a'
            LINE2 = ' ******* problem unless 2 such runs are '//
     1          'scheduled back to back.'
            LINE3 = ' ******* If that happens, your protection '//
     1          'requests may be ignored.'
            LINE4 = ' *******'
            WRITE( IUNIT, '( A, /, A, /, A, /, A )' ) 
     1          LINE1, LINE2, LINE3, LINE4
            WRITE( IPRE, '( A, /, A, /, A, /, A )' ) 
     1          LINE1, LINE2, LINE3, LINE4
            CALL WLOG( 1, LINE1 )
            CALL WLOG( 1, LINE2 )
            CALL WLOG( 1, LINE3 )
            CALL WLOG( 1, LINE4 )
C
         ELSE
C
C           Now flag the case of a long project with no preemption times.
C
            IGWARN = .TRUE.
C
         END IF
      ELSE
C
C        This is the case where there are some available blocks.
C
         LINE1 = ' Time ranges available for USNO daily EOP '//
     1        'observations at PT and MK:'
         WRITE( IUNIT, '( A )' ) LINE1
         WRITE( IPRE, '( A )' ) LINE1
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
C        Check for adequate options.  Look for, and complain about, 
C        protected periods of MAXBLOCK hours or more.  This assumes
C        time order, but that should be the case given how the blocks
C        were derived.
C
         TOOLONG = .FALSE.
         IF( PESTART(1) - TFIRST .GT. MAXBLOCK ) TOOLONG = .TRUE.
         IF( TEND - PESTOP(NP) .GT. MAXBLOCK ) TOOLONG = .TRUE.
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
            LINE1 = ' *******'
            LINE2 = ' '
            WRITE( LINE2, '( A, F5.1, A )' )
     1         ' ******* There is at least one block of over ', 
     2         MAXBLOCK * 24.D0, 'hr'
            LINE3 = ' '
            WRITE( LINE3, '( A, F5.1, A )' )
     1         ' ******* when your PREEMPT specification does not '//
     2         'allow ', MINTPE * 24.D0, ' hr'
            LINE4 = ' ******* EOP observations on '//
     1         'one baseline (PT-MK usually) to be inserted.'
            LINE5 = ' ******* See details in the .sum or .preempt file.'
            WRITE( IUNIT, '( A, 4( /, A ) )' )
     1          LINE1, LINE2, LINE3, LINE4, LINE5
            WRITE( IPRE, '( A, 4( /, A ) )' )
     1          LINE1, LINE2, LINE3, LINE4, LINE5
            CALL WLOG( 1, LINE1 )
            CALL WLOG( 1, LINE2 )
            CALL WLOG( 1, LINE3 )
            CALL WLOG( 1, LINE4 )
            CALL WLOG( 1, LINE5 )
C
            IGWARN = .TRUE.
         END IF
      END IF
C
C     Warn user his/her instructions may be ignored.
C
      IF( IGWARN ) THEN 
         LINE1 = 
     1     ' ******* Your requests to protect scans will be ignored.'
         LINE2 = 
     1     ' ******* See the SCHED manual info on parameter PREEMPT.'
         LINE3 = ' *******'
         WRITE( IUNIT, '( A, /, A, /, A )' ) LINE1, LINE2, LINE3
         WRITE( IPRE, '( A, /, A, /, A )' ) LINE1, LINE2, LINE3
         CALL WLOG( 1, LINE1 )
         CALL WLOG( 1, LINE2 )
         CALL WLOG( 1, LINE3 )
      END IF
C
C     Write the time ranges of the EXTRA and CORE scans.
C
      LINE1 = ' Project times summary:'
      CALL TIMEJ( TFIRST, SY, SD, ST )
      CALL TIMEJ( TEND, TY, TD, TL )
      CALL TIMEJ( COREBEG, CBY, CBD, CBT )
      CALL TIMEJ( COREEND, CEY, CED, CEL )
      CSTART = TFORM( ST, 'T', 0, 2, 2, '::@' )
      CSTOP = TFORM( TL, 'T', 0, 2, 2, '::@' )
      CBSTART = TFORM( CBT, 'T', 0, 2, 2, '::@' )
      CESTOP = TFORM( CEL, 'T', 0, 2, 2, '::@' )
C
      WRITE( IUNIT, '( A, /, A )' ) ' ', LINE1
      WRITE( IPRE, '( A, /, A )' ) ' ', LINE1
      IF( TFIRST .LT. COREBEG ) THEN
         WRITE( LINE2, 
     1     '( A, 4X, A, I3.3, A, A8, A, I3.3, A,  A8 )' ) 
     2     ' Extra scans at start:    ', '(', SD, ') ', 
     3     CSTART, '  to  (', CBD, ') ', CBSTART
         WRITE( IUNIT, '( A )' ) LINE2
         WRITE( IPRE, '( A )' ) LINE2
      END IF
      WRITE( LINE3,
     1    '( A, 4X, A, I3.3, A, A8, A, I3.3, A,  A8 )' ) 
     2    ' Core start and stop:     ', '(', CBD, ') ', 
     3    CBSTART, '  to  (', CED, ') ', CESTOP
      WRITE( IUNIT, '( A )' ) LINE3
      WRITE( IPRE, '( A )' ) LINE3
      IF( TEND .GT. COREEND ) THEN
         WRITE( LINE4, 
     1      '( A, 4X, A, I3.3, A, A8, A, I3.3, A,  A8 )' ) 
     2      ' Extra scans at end:      ', '(', CED, ') ', 
     3      CESTOP, '  to  (', TD, ') ', CSTOP
         WRITE( IUNIT, '( A )' ) LINE4
         WRITE( IPRE, '( A )' ) LINE4
      END IF
      WRITE( IUNIT, '( A )' ) ' '
C
C     If DOSCANS was specified, give the time range.
C     Reuse some temporary variables from above.
C
      IF( DOSCANS(1) .NE. 0 ) THEN
         WRITE( LINE1, '( A, I5, A, I5 )' ) 
     1       ' DOSCANS was set to only pass scans ', 
     2       DOSCANS(1), ' to ', DOSCANS(2)
         CALL TIMEJ( STARTJ(DOSCANS(1)), SY, SD, ST )
         CALL TIMEJ( STOPJ(DOSCANS(2)), TY, TD, TL )
         CSTART = TFORM( ST, 'T', 0, 2, 2, '::@' )
         CSTOP = TFORM( TL, 'T', 0, 2, 2, '::@' )
         WRITE( LINE2, 
     1     '( A, 4X, A, I3.3, A, A8, A, I3.3, A,  A8 )' ) 
     2     ' DOSCANS time range:      ', '(', SD, ') ', 
     3     CSTART, '  to  (', TD, ') ', CSTOP
         WRITE( IUNIT, '( A, /, A)' ) LINE1, LINE2
         WRITE( IPRE, '( A, /, A)' ) LINE1, LINE2
      END IF
C
      RETURN
      END
