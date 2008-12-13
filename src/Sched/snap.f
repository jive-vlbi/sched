      SUBROUTINE SNAP( ISCN, ISTA, FIRSTS )
C
C     Writes a SNAP schedule.
C
C     The following note was found here Nov. 4, 1992.  A quick test
C     with hlpspect.ky suggests that it is ok.  I don't recall the 
C     problem.
C          *********   not working.  fix it.  test with hlpspect.ky.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER           ISCN, ISTA, LEN1, YEAR, DAY, I, J
      INTEGER           IFREQ, FR1, FR2, BW1, BW2, N, NCHAR
      REAL              REPOCH, LBW(MAXCHN)
      DOUBLE PRECISION  START, STOP, LFREQ(MAXCHN)
      LOGICAL           FIRSTS, DOWRT, DOTAPE
      CHARACTER         EPOCH*8, TFORM*15, RASCH*15, DECSCH*15
      CHARACTER         TSTART*15, TSTOP*15, PRTSRC*12
      CHARACTER         OUTFRQ(MAXCHN)*9, OUTBW(MAXCHN)*9
      SAVE              LFREQ, LBW
C  ------------------------------------------------------------------
      IF( DEBUG ) WRITE(*,*) 'SNAP starting'
C
C     Write header for experiment. 
C
      IF(FIRSTS) THEN
         WRITE( IULOC, '( 3A, /, 3A )' ) 
     1      '"Schedule for ', STATION(STANUM(ISTA)), '"', 
     2      '"', EXPT(1:LEN1(EXPT)), '"'
         IF( GOTFREQ ) THEN
            WRITE( IULOC, '( A, /, A, /, A, /, A )' )
     1      '"  ************************************* "',
     1      '"Frequencies changed in schedule."',
     2      '"Edit in correct VC numbers and subtract first LO."',
     3      '"  ************************************* "'
         END IF
         DO I = 1, MAXCHN
            LFREQ(I) = 0.D0
            LBW(I) = 0.0
         END DO
         DO I = 1, MCOVER
            J = LEN1( COVER(I) )
            WRITE( IULOC, '( A )' ) '" '//COVER(I)(1:J)//' "'
         END DO
      END IF
C
C     Write final instructions if schedule done.
C
      IF( ISCN.EQ.-999 ) THEN
         WRITE (IULOC,'(''"End of '',A,''"''/''STOP'')')
     1      EXPT(1:LEN1(EXPT))
C
      ELSE
C
C        Write any requested comment.
C
         IF( ANNOT(ISCN) .NE. ' ' ) THEN
            WRITE( IULOC, '(''"'',A,''"'')') 
     1           ANNOT(ISCN)(1:LEN1(ANNOT(ISCN)))
         END IF
C
C        Get Day and Year to get approximate epoch.  Use START time as
C        that is also needed for first scan setup.  Do not call TIMEJ
C        for stop time until after first scan is set up.
C
         CALL TIMEJ( STARTJ(ISCN), YEAR, DAY, START )
C
C        Get RA, DEC, and epoch for schedule.
C
         IF( C1950(SRCNUM(ISCN)).EQ.'*' .OR. 
     1       CONTROL(STANUM(ISTA)) .EQ. 'SN50' ) THEN
            RASCH = TFORM( RA1950(SRCNUM(ISCN)), 'T', 0, 2, 6, '@@@' )
            DECSCH = TFORM( D1950(SRCNUM(ISCN)), ' ', 1, 2, 5, '@@@' )
            EPOCH = ',1950.0'
         ELSE IF( C2000(SRCNUM(ISCN)) .EQ. '*' ) THEN
            RASCH = TFORM( RA2000(SRCNUM(ISCN)), 'T', 0, 2, 6, '@@@' )
            DECSCH = TFORM( D2000(SRCNUM(ISCN)), ' ', 1, 2, 5, '@@@' )
            EPOCH = ',2000.0'
         ELSE
            REPOCH = YEAR + DAY/365.25D0
            RASCH = TFORM( RAP(SRCNUM(ISCN)), 'T', 0, 2, 6, '@@@' )
            DECSCH = TFORM( DECP(SRCNUM(ISCN)), ' ', 1, 2, 5, '@@@' )
            WRITE( EPOCH, '('','',F7.2)' ) REPOCH
         END IF
C
C        Get the source name to use.  The maximum length in SNAP is
C        10.  If the name is longer than 10 characters, use the
C        short version, which is 8 characters now.
C
         PRTSRC = SCNSRC(ISCN)
         NCHAR = LEN1( PRTSRC )
         IF( NCHAR .GT. 10 ) THEN
            IF( SOUR8(SRCNUM(ISCN)) .NE. ' ' ) THEN
               PRTSRC = SOUR8(SRCNUM(ISCN))
            ELSE
               CALL ERRLOG( ' Need 8 or less character alias for '//
     1                PRTSRC//' for SNAP at'//STATION(STANUM(ISTA)) )
            END IF
         END IF
         NCHAR = LEN1( PRTSRC )
C
C        Go to source.
C
         IF( DECSCH(1:1) .EQ. '-' ) THEN 
            WRITE( IULOC, '( ''SOURCE='', A, '','', A10, '','', A10,A)')
     1         PRTSRC(1:NCHAR), RASCH, DECSCH, EPOCH
         ELSE
            WRITE( IULOC, '( ''SOURCE='', A, '','', A10, '','', A9, A)')
     1         PRTSRC(1:NCHAR), RASCH, DECSCH(2:10), EPOCH
         END IF
C
C        For first scan, request tape be mounted and wait for start time.
C        Call to TIMEJ for start but not stop should have happened above.
C
         IF( FIRSTS ) THEN
            WRITE( IULOC, '(''NTAPE'')' )
            TSTART = TFORM( START, 'T', 0, 2, 2, '@@@' )
            WRITE( IULOC, '( ''WAIT='', I3.3, A6)') DAY, TSTART 
            IF( .NOT. NOTSYS(ISCN) ) WRITE( IULOC, '( ''TSYSS'', /, ' //
     1         '  ''WX'' )')
         END IF
C
C        Change tape if requested. 
C
         DOTAPE = MOD( TPDAT(1,ISCN,ISTA), 10 ) .GT. 0
         IF( DOTAPE .AND. .NOT.FIRSTS ) WRITE(IULOC,'(''NTAPE'')')
C
C        Test if there are any new frequencies or bandwidths.
C        ****  Note that these are the "logical" channels which may
C              not be the same as what is in the setup file.
C
         IF( GOTFREQ ) THEN
C
C
            DOWRT = .FALSE.
            DO IFREQ = 1, MSCHN(SETNUM(ISCN))
C
C              Check if setup file channels correspond to logical chans.
C
               IF( .NOT. OKXC(SETNUM(ISCN)) .OR.
     1              SFCHAN(IFREQ,SETNUM(ISCN)) .NE. IFREQ ) THEN
C
C                 Issue a warning if there are channelization 
C                 complications.
C
                  CALL WLOG( 0, 'SNAP: Frequencies put in SNAP file ' //
     1                 ' are for setup file logical channels.' )
                  CALL WLOG( 0, '      It appears likely that they ' //
     1                 'are not for the right channel at this station.')
                  CALL WLOG( 0, '      Avoid problems by avoiding ' //
     1                 'DOPPLER, FREQ, and BW.' )
                  CALL WLOG( 0, '      NOTE: Support for SNAP in ' //
     1                 'SCHED is being phased out.' )
C
               END IF
C
               IF( FREQ(IFREQ,ISCN) .NE. LFREQ(IFREQ) ) THEN 
                  IF( FREQ(IFREQ,ISCN) .EQ. 0.D0 ) THEN
                     OUTFRQ(IFREQ) = '  Default'
                  ELSE
                     WRITE( OUTFRQ(IFREQ), '(F9.2)' ) FREQ(IFREQ,ISCN)
                  END IF
                  DOWRT = .TRUE.
                  LFREQ(IFREQ) = FREQ(IFREQ,ISCN)
               END IF
               IF( BW(IFREQ,ISCN) .NE. LBW(IFREQ) ) THEN 
                  IF( BW(IFREQ,ISCN) .EQ. 0.0 ) THEN
                     OUTBW(IFREQ) = '  Default'
                  ELSE
                     WRITE( OUTBW(IFREQ), '(F9.4)' ) BW(IFREQ,ISCN)
                  END IF
                  DOWRT = .TRUE.
                  LBW(IFREQ) = BW(IFREQ,ISCN)
               END IF
            END DO
C
C           Write frequency and bandwidth requests.  Don't pass blanks.
C
            IF( DOWRT ) THEN
               DO IFREQ = 1, MSCHN(SETNUM(ISCN))
                  DO N = 1, 9
                     IF( OUTFRQ(IFREQ)(10-N:10-N) .NE. ' ' ) 
     1                    FR1 = 10 - N
                     IF( OUTBW(IFREQ)(10-N:10-N) .NE. ' ' ) 
     1                    BW1 = 10 - N
                     IF( OUTFRQ(IFREQ)(N:N) .NE. ' ' ) FR2 = N
                     IF( OUTBW(IFREQ)(N:N) .NE. ' ' ) BW2 = N         
                  END DO
                  WRITE( IULOC, '( A, A, A, A )' ) 'VCnn=',
     1              OUTFRQ(IFREQ)(FR1:FR2), ',', OUTBW(IFREQ)(BW1:BW2)
                  WRITE( IULOC, '( A )' )
     1              'VCnn=ALARM'
               END DO
            END IF
         END IF
C
C        Stay on source until end.  Times of 24:00:00 were precluded by
C        code in SCHTIM.
C
         CALL TIMEJ( STOPJ(ISCN), YEAR, DAY, STOP )
         TSTOP = TFORM( STOP, 'T', 0, 2, 2, '@@@' )
         WRITE( IULOC, '( ''!'', I3.3, A6 )' ) DAY, TSTOP 
         IF( .NOT. NOTSYS(ISCN) ) WRITE( IULOC, '( ''TSYSS'', /,
     1          ''WX'' )')
         IF( ( TANTS1(ISTA) .AND. TANT1(ISCN) ) .OR. 
     1       ( TANTS2(ISTA) .AND. TANT2(ISCN) ) ) 
     2       WRITE( IULOC, '( ''TANT'' )' )
C
      END IF
C
      RETURN
      END
