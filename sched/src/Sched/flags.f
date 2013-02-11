      SUBROUTINE FLAGS
C
C     Routine for SCHED that makes an AIPS UVFLG input table to
C     flag the time at each station from the start of tape motion
C     for a scan to the time the antenna arrives on source, assuming
C     that is a positive interval.  
C
      INCLUDE    'sched.inc'
C
      INTEGER    ISCN, ISTA, LEN1, IOERR, VLBOPE
      INTEGER    YEAR, DOY, MONTH, DAY, JD, IL, NL
      DOUBLE PRECISION  STOP, TLOOP, TINC, HLST
      REAL       HHA, HEL, HAZ, HPA
      CHARACTER  FLAGFILE*40, OPTEXT*255, OPSTAT*4
      CHARACTER  PDATE*80, MNAME*3, DNAME*3, HORCHK*1
      LOGICAL    EXISTS
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'FLAGS starting' )
C
C     Construct the name of the flag file.
C
      WRITE( FLAGFILE, '(A,A)' )  EXPCODE(1:LEN1(EXPCODE)), '.FLAG'
      CALL DWCASE( FLAGFILE )
      IF( DEBUG ) 
     1   CALL WLOG( 0, 'FLAGS: Opening apriori flagging file: ' //
     2      FLAGFILE(1:LEN1(FLAGFILE)) )
C
C     Find out if the .flag file already exists.  Don't worry about
C     not writing over it.  But leave the code there in case I change
C     my mind.
C
      INQUIRE( FILE=FLAGFILE, EXIST=EXISTS )
      IF( EXISTS .AND. OVERWRIT ) THEN
         OPSTAT = 'OLD'
      ELSE IF( EXISTS ) THEN
C         CALL WLOG( 1, 'FLAGS: '//PRTFILE//' already exists.' )
C         CALL ERRLOG( 'FLAGS: You need to delete old output files' )
         OPSTAT = 'OLD'
      ELSE
         OPSTAT = 'NEW'
      END IF
C
C     Announce your intentions.
C
      CALL WLOG( 0, 'FLAGS:   Writing apriori flagging file ' //
     1     FLAGFILE(1:LEN1(FLAGFILE)) )
C
C     OPEN summary file and write station info to it.  
C
      IOERR = VLBOPE( IFLAG, FLAGFILE, 'TEXT', OPSTAT, OPTEXT )
      IF( IOERR .NE. 1 ) CALL ERRLOG( ' Open problem:'//OPTEXT )
C
C     Get the date
C
      CALL TIMEJ( STOPJ(1), YEAR, DOY, STOP )
      MONTH = 1
      DAY = DOY
      CALL TDATECW( YEAR, MONTH, DAY, JD, MNAME, DNAME )
      WRITE( PDATE, '( A, I3, A, A3, I4, 1X, A3, I5, A, I6 )' )
     1         'Day ', DOY, ' is ',  DNAME, DAY, MNAME, YEAR,
     2    '   MJD ', INT( JD - 2400000.5 )
C
C     Write a header.
C
      WRITE( IFLAG, '( 2A,/, A,/, 2A,/, 2A,/, 2A, /, A,/, A,/, A )' )
     1   '!  Apriori edit data for ',  EXPCODE(1:LEN1(EXPCODE)), 
     2   '!  The format is appropriate for UVFLG in AIPS.',
     3   '!  There is one flag per station per scan covering time ',
     4   'from start of tape ', 
     5   '!    motion to antenna arrival on source, ',
     6   'if that is a positive interval.',
     7   '!  These flags are not needed when there are monitor ',
     8   'data flags (eg VLBA).',
     9   '! ',
     a   'opcode = ''FLAG''',
     b   'dtimrang = 1  timeoff = 0 '
C
C     Now get the flags.
C
      DO ISTA = 1, NSTA
         DO ISCN = SCAN1, SCANL
            IF( STASCN(ISCN,ISTA) ) THEN
C
C              Flag for slewing while tape writing.
C              Note that the reason should be < 24 characters.
C
C              Allow for some rounding tolerance now that recordings
C              can start right at TONSRC.  Before dealing with this,
C              a lot of flags with the same start and stop times 
C              were introduced.  Looking in some detail showed 
C              that TONSRC-(STARTJ-TPSTART) was often between .15 and
C              .2 seconds.  Not sure why.  For now, extend the flag
C              1 second early and 0.5 second late.  That seems to have
C              the effect of extending the flag one second beyond the
C              VEX on-source start time for most scans when this antenna
C              is limiting the slew.  That's probably an effect of the
C              .15 second offset and is likely to be what you actually want.
C              Note that, for stations with USEONSRC true, the flag
C              could well start quite a while before the start of recording.
C              but that should be harmless.
C
               IF( TONSRC(ISCN,ISTA) - ( STARTJ(ISCN) - 
     1             TPSTART(ISCN,ISTA) ) .GT. 0.1D0 * ONESEC ) THEN
                  CALL FLAGWRT( IFLAG, STCODE(STANUM(ISTA)), 
     1                   STARTJ(ISCN) - TPSTART(ISCN,ISTA) - ONESEC,
     2                   TONSRC(ISCN,ISTA) + 0.5D0 * ONESEC,
     3                   'Slewing expected.' )
               END IF
C
C              Flags for horizons.
C              First case - down at both ends of scan.
C
               IF( UP1(ISCN,ISTA) .NE. ' ' .AND. 
     1             UP2(ISCN,ISTA) .NE. ' ' ) THEN
                  CALL FLAGWRT( IFLAG, STCODE(STANUM(ISTA)),
     1                   STARTJ(ISCN) - TPSTART(ISCN,ISTA), 
     2                   STOPJ(ISCN),
     3                   'Slew limits or horizon' )
C
               ELSE IF( UP2(ISCN,ISTA) .NE. ' ' ) THEN
C
C                 If setting, get set time to nearest 30 seconds.
C                 It seems non-integer loop indices are being eliminated
C                 in future fortrans so try to get rid of them.
C                 The loop will end at or before TSTOP.
C
                  TINC = 30.D0 / 86400.D0
                  NL = ( STOPJ(ISCN) - STARTJ(ISCN) ) / TINC
                  DO IL = 1, NL
                     TLOOP = STOPJ(ISCN) - IL * TINC
                     CALL SCHGEO( ISCN, ISTA, TLOOP, 
     1                      HHA, HEL, HAZ, HLST, HPA )
                     IF( HORCHK( STANUM(ISTA), HHA, HAZ, HEL, 
     1                   SRCNUM(ISCN) ) .EQ. ' ' ) THEN
                        CALL FLAGWRT( IFLAG, STCODE(STANUM(ISTA)),
     1                      TLOOP, STOPJ(ISCN), 'Source set.' )
                        GO TO 100
                     END IF
                  END DO
C
C                 If still here, source set near start.  Flag scan.
C
                  CALL FLAGWRT( IFLAG, STCODE(STANUM(ISTA)),
     1                   STARTJ(ISCN) - TPSTART(ISCN,ISTA), 
     2                   STOPJ(ISCN),
     3                   'Source set near start' )
C
               ELSE IF( UP1(ISCN,ISTA) .NE. ' ' ) THEN
C
C                 If rising, get rise time to nearest 30 seconds.
C
                  TINC = 30.D0 / 86400.D0
                  NL = ( STOPJ(ISCN) - STARTJ(ISCN) ) / TINC
                  DO IL = 1, NL
C                  DO TLOOP = STARTJ(ISCN) - TPSTART(ISCN,ISTA) + TINC, 
C     1                STOPJ(ISCN), TINC
                     TLOOP = STARTJ(ISCN) + IL * TINC
                     CALL SCHGEO( ISCN, ISTA, TLOOP, 
     1                      HHA, HEL, HAZ, HLST, HPA )
                     IF( HORCHK( STANUM(ISTA), HHA, HAZ, HEL, 
     1                   SRCNUM(ISCN) ) .EQ. ' ' ) THEN
                        CALL FLAGWRT( IFLAG, STCODE(STANUM(ISTA)),
     1                      STARTJ(ISCN) - TPSTART(ISCN,ISTA), TLOOP, 
     2                      'Source rise.' )
                        GO TO 100
                     END IF
                  END DO
C
C                 If still here, source rose right at end.  Flag scan.
C
                  CALL FLAGWRT( IFLAG, STCODE(STANUM(ISTA)),
     1                   STARTJ(ISCN) - TPSTART(ISCN,ISTA), 
     2                   STOPJ(ISCN),
     3                   'Source rise near end' )

               END IF
C
C              Jump here when done checking for rise or set.
C
  100          CONTINUE
C
            END IF
         END DO
         WRITE( IFLAG, '( 1X )' )
      END DO
C
      CLOSE( UNIT=IFLAG )
      RETURN
      END



