      SUBROUTINE PRTSCH( ISCN, ISTA, FIRSTS )
C
C     Subroutine for SCHED that prints the operator schedule.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER            ISCN, ISTA, ILINE, IPAGE, LEN1, LASTDY
      INTEGER            KY, KM, JD
      INTEGER            LTEST, KD1, KD2, NCHL, LNCHL
      INTEGER            DAY1, DAY2, DAY3, YEAR1, YEAR2, YEAR3
      INTEGER            IFREQ
      INTEGER            TEARLY, TDWELL, NFLINES
      INTEGER            KF, LKF
      REAL               LBW(MCHAN), SBW(MCHAN), SBBC(MCHAN)
      DOUBLE PRECISION   LFREQ(MCHAN), SFREQ(MCHAN), START, STOP 
      DOUBLE PRECISION   TSTART, RTCORR
      LOGICAL            FIRSTS, DOWRTF, DOWRTB
      CHARACTER          TFORM*15, SHORTN*10
      CHARACTER          CLST1*9, CLST2*9, FF*1
      CHARACTER          CSTART*15, CSTOP*15, CTSTART*15, CTCORR*15
      CHARACTER          OUTFRQ(MCHAN)*9, OUTBW(MCHAN)*9
      CHARACTER          PRTSRC*12, DIRECT*1, OUTBBC(MCHAN)*9
      CHARACTER          MNAME1*3, MNAME2*3, DNAME1*3, DNAME2*3
      SAVE               ILINE, IPAGE, LASTDY, LNCHL, LFREQ, LBW
C
C     Tape information from TPDAT.
C
      INTEGER          TPPASS, TPDIR, TPINDX, TPHEAD, TPDRIV, ISETF
      LOGICAL          DOTAPE, DOFAST, DOREW
C -----------------------------------------------------------------
      IF( DEBUG .AND. FIRSTS ) THEN
         CALL WLOG( 0, 'PRTSCH: Making operator schedule file for' //
     1      STANAME(ISTA) )
      END IF
C
      NCHL = NCHAN(LS)
      ISETF = ISETNUM(LS)
C
C     Reset line (ILINE) and page (IPAGE) counters on first scan.
C
      FF = CHAR(12)       !  Form feed.
      IF( FIRSTS ) THEN
         ILINE = 999
         IPAGE = 1
         LASTDY = -999
         LNCHL = 0
         LKF = 0
         DO IFREQ = 1, MCHAN
            LFREQ(IFREQ) = 0.D0
            LBW(IFREQ) = 0.0
         END DO
C
C        Write cover page.
C
         WRITE( IPRT, '( A, /,1X,/, 5A /, 2A, /, 2A, /, 2A )' )
     1      '  COVER INFORMATION ',
     2      ' Station:    ', STANAME(ISTA),
     3      '  (Code ', STCODE(STANUM(ISTA)), ') ', 
     4      ' Experiment: ', EXPT(1:LEN1(EXPT)),
     5      ' Exp. Code:  ', EXPCODE            
         WRITE( IPRT, '( 1X, /, 2(A,/), 1X,/, A, /, 1X,/, 4(A,/), ' //
     1      '1X,/, 4(A,/), 1X,/, A,/, 1X,/, 4(A,/) )' )
     2      COVER
C
C        Write cover letter if one was given.
C
         IF( COVERLET ) THEN
            WRITE( IPRT, '( 1X, /, A, /, 1X )' )
     1          'COVER LETTER:'
            CALL WRTCOV( IPRT )
         END IF
      END IF
C
C     Get time information for the scan.
C
      CALL TIMEJ( STARTJ(ISCN), YEAR1, DAY1, START )
      CALL TIMEJ( STOPJ(ISCN), YEAR2, DAY2, STOP )
      CALL TIMEJ( STARTJ(ISCN) - TPSTART(ISCN,ISTA), 
     1            YEAR3, DAY3, TSTART )
      CALL TIMEJ( TCORR(ISCN,ISTA), 
     1            YEAR3, DAY3, RTCORR )
C
C     Get the dwell time and the margin of arrival time.
C
      TEARLY = IDNINT( ( STARTJ(ISCN) - TONSRC(ISCN,ISTA) ) * 86400.D0 )
      IF( UP1(ISCN,ISTA) .EQ. ' ' .AND. UP2(ISCN,ISTA) .EQ. ' ' ) THEN
         IF( TEARLY .GE. 0 ) THEN
            TDWELL = IDNINT( ( STOPJ(ISCN) - STARTJ(ISCN) ) * 86400.D0 )
         ELSE
            TDWELL = IDNINT( ( STOPJ(ISCN)-TONSRC(ISCN,ISTA) )*86400.D0)
         END IF
      ELSE
         TDWELL = 0
      END IF
C
C     Extract the tape commands from PTDAT.
C
      CALL TPPACK( 'UNPACK', TPDAT(1,ISCN,ISTA), DOTAPE, DOFAST,
     1             DOREW, TPPASS, TPDRIV, TPDIR, TPINDX, TPHEAD )
      IF( TPDIR .EQ. 1 ) THEN
         DIRECT = 'F'
      ELSE IF( TPDIR .EQ. -1 ) THEN
         DIRECT = 'R'
      END IF
      IF( NOREC(ISCN) ) DIRECT = 'S'
C
C     Test if there are any new frequencies or bandwidths.
C
      KF = FSETI(ISCN,ISTA)
      DOWRTF = .FALSE.
      DOWRTB = .FALSE.
      IF( KF .NE. LKF ) THEN
         CALL FSFREQ( KF, SFREQ, SBBC, SBW )
         NCHL = NCHAN( FSETKS(KF) )
         IF( NCHL .NE. LNCHL ) THEN
            DOWRTF = .TRUE.
            DOWRTB = .TRUE.
         END IF
         DO IFREQ = 1, NCHL
            WRITE( OUTFRQ(IFREQ), '(F9.2)' ) SFREQ(IFREQ)
            WRITE( OUTBBC(IFREQ), '(F9.2)' ) SBBC(IFREQ)
            WRITE( OUTBW(IFREQ), '(F9.2)' ) SBW(IFREQ)
            IF( SFREQ(IFREQ) .NE. LFREQ(IFREQ) ) DOWRTF = .TRUE.
            IF( SBW(IFREQ) .NE. LBW(IFREQ) ) DOWRTB = .TRUE.
         END DO
C
C        Save these parameters for comparison next time.
C
         LKF = KF
         LNCHL = NCHL
         DO IFREQ = 1, NCHL
            LFREQ(IFREQ) = SFREQ(IFREQ)
            LBW(IFREQ) = SBW(IFREQ)
         END DO         
C
C        Get the number of lines it will take to print frequencies
C        or bandwidths at 6 per line.
C
         NFLINES = 1 + ( NCHL - 1 ) / 6 
C
      ELSE
         NFLINES = 0
      END IF
C
C     Page if necessary.  LTEST is where the line counter will be after
C     this scan.  Test it against LINEPG.
C
      LTEST = ILINE + 2
      IF( DAY2 .NE. LASTDY )     LTEST = LTEST + 2
      IF( ANNOT(ISCN) .NE. ' ' ) LTEST = LTEST + 2
      IF( DOTAPE )               LTEST = LTEST + 2
      IF( DOWRTF .OR. DOWRTB )   LTEST = LTEST + 1
      IF( DOWRTF )               LTEST = LTEST + 2 * NFLINES
      IF( DOWRTB )               LTEST = LTEST + NFLINES
C
      IF( LTEST .GT. LINEPG ) THEN
         ILINE = 9
         IPAGE = IPAGE + 1
C
C        Write page header.
C
         WRITE( IPRT, '( A1, A,A, A,A,A, T70, A,I3, /, 15X,A )' ) 
     1      FF, 'Schedule for ', STANAME(ISTA),
     2      '  (Code ', STCODE(STANUM(ISTA)), ') ', 
     3      'Page ', IPAGE,     EXPT(1:LEN1(EXPT))
C
         WRITE( IPRT, '( 2A, /, 2A, /, 2A, /, 2A, /, A )' )
     1      '  UP:  D => Below limits;  H => Below horizon mask.',
     2      '  blank => Up.',
     3      '  Early: Seconds between end of slew and start. ',
     4      '  Dwell: On source seconds. ',
     5      '  Tapes: Drive, Index, Start footage ',
     6      '/ Direction, Head Group, End footage.',
     7      '  TPStart:  Tape motion start time.',
     8      '  Frequencies are LO sum (band edge).',
     9      '  SYNC: Time correlator is expected to sync up.'
C
         IF( AUTOALOC(ISTA) ) THEN
            WRITE( IPRT, '( A, A )' )
     1         '     Automatic tape allocation specified.  Tape ' //
     2         'positions are just estimates.'
            ILINE = ILINE + 1
         END IF
C
         WRITE( IPRT, '( 88(''-''), /, 2A, /, 2A, /, 88(''-'') )' )
     1      'Start UT  Source               Start / Stop           ',
     2      '      Early    Tapes      TPStart',
     3      'Stop UT                  LST      EL    AZ   HA  UP   ',
     4      'ParA  Dwell  (see above)   SYNC'
C                         
         LASTDY = -999
      END IF
C
C     Write day number at top of page or when day changes.
C     Use modified CIT routine, TDATECW, to get month name and day name.
C
      IF( DAY2 .NE. LASTDY ) THEN
         LASTDY = DAY2
         KY = YEAR2
         KM = 1
         KD2 = DAY2
         CALL TDATECW( KY, KM, KD2, JD, MNAME2, DNAME2 )
         IF( DAY1 .EQ. DAY2 ) THEN
            WRITE( IPRT, '( 2X, /, A, A3, I4, 1X, A3, I5, A, I4, A )' )
     1         ' --- ', DNAME2, KD2, MNAME2, YEAR2, '   Day', DAY2, 
     2         ' ---'
         ELSE
            KY = YEAR1
            KM = 1
            KD1 = DAY1
            CALL TDATECW( KY, KM, KD1, JD, MNAME1, DNAME1 )
            WRITE( IPRT, '( 2X, /, A, A3, I4, 1X, A3, I5, A, I4, ' //
     1          ' A, A3, I4, 1X, A3, I5, A, I4, A )' )
     2          ' --- Start: ', DNAME1, KD1, MNAME1, YEAR1, '   Day', 
     3          DAY1, ' -- Stop: ', DNAME2, KD2, MNAME2, YEAR2, 
     4          '   Day', DAY2, ' ---'
         END IF
         ILINE = ILINE + 2
      END IF
C
C     Add comment
C
      IF( ANNOT(ISCN) .NE. ' ' ) THEN
         WRITE (IPRT,'(2X,/,'' ---------- '',A,'' ----------'')')
     1      ANNOT(ISCN)(1:LEN1(ANNOT(ISCN)))
         ILINE = ILINE + 2
      END IF
C
C     Tape changes
C
      IF( DOTAPE ) THEN 
         WRITE (IPRT,'(2X,/,''-----------  Tape Change ------------'')')
         ILINE = ILINE + 2
      END IF
C
C     Write new frequencies and bandwidths.  Assume less than 36 chan.
C
      IF( DOWRTF .OR. DOWRTB ) THEN
         WRITE( IPRT, '( 2X )' )
      END IF
      IF( DOWRTF ) THEN
         WRITE( IPRT, '( A, 6A, 6( /, 23X, 6A ) )' )
     1      ' Next scan frequencies:', (OUTFRQ(IFREQ),IFREQ=1,NCHL)
         WRITE( IPRT, '( A, 6A, 6( /, 23X, 6A ) )' )
     1      ' Next BBC frequencies: ', (OUTBBC(IFREQ),IFREQ=1,NCHL)
         ILINE = ILINE + 1 + 2 * NFLINES
      END IF
      IF( DOWRTB ) THEN
         WRITE( IPRT, '( A, 6A, 6( /, 23X, 6A ) )' )
     1      ' Next scan bandwidths: ', (OUTBW(IFREQ),IFREQ=1,NCHL)
         ILINE = ILINE + 1 + NFLINES
      END IF
C
C     Schedule file output
C
      CLST1 = TFORM( LST1(ISCN,ISTA), 'T', 0, 2, 2, '  @' )
      CLST2 = TFORM( LST2(ISCN,ISTA), 'T', 0, 2, 2, '  @' )
C
C     Short name, Up-Down comment, and tape stuff.
C
      IF( LEN1(SCNSRC(ISCN)) .GT. 8 .AND. 
     1    SOUR8(SRCNUM(ISCN)) .NE. ' ' ) THEN
         SHORTN = '='//SOUR8(SRCNUM(ISCN))
      ELSE
         SHORTN = '---'
      END IF
C
C     Write scan info.
C
      IF( LEN1( SCNSRC(ISCN) ) .LE. 11 )  THEN
         PRTSRC = ' '//SCNSRC(ISCN)
      ELSE
         PRTSRC = SCNSRC(ISCN)
      END IF
      CSTART = TFORM( START, 'T', 0, 2, 2, '  @' )
      CSTOP = TFORM( STOP,  'T', 0, 2, 2, '  @' )
      IF( NOREC(ISCN) ) THEN
         CTSTART = 'Stopped '
         CTCORR  = ' '
      ELSE
         CTSTART = TFORM( TSTART, 'T', 0, 2, 2, '  @' )
         CTCORR = TFORM( RTCORR, 'T', 0, 2, 2, '  @' )
      END IF

C
      WRITE( IPRT, '( 2X, /, A8, 1X, A, T24, A8, 2F6.1, F5.1, 2X,  ' //
     1     ' A, F7.1, I6, 3X, I1, I3, I7, 2X, A8 )' )
     2     CSTART, PRTSRC, CLST1, 
     3     EL1(ISCN,ISTA), AZ1(ISCN,ISTA), HA1(ISCN,ISTA), 
     4     UP1(ISCN,ISTA), PA1(ISCN,ISTA), TEARLY, 
     5     TPDRIV, TPINDX, NINT( TPFOOT1(ISCN,ISTA) ), CTSTART
C
      WRITE( IPRT, '( A8, 2X, A, T24, A8, 2F6.1, F5.1, 2X, ' // 
     1     ' A, F7.1, I6, 3X, A, I3, I7, 2X, A8 )' ) 
     2     CSTOP, SHORTN, CLST2, 
     3     EL2(ISCN,ISTA), AZ2(ISCN,ISTA), HA2(ISCN,ISTA), 
     4     UP2(ISCN,ISTA), PA2(ISCN,ISTA), TDWELL,
     5     DIRECT, TPHEAD, NINT( TPFOOT2(ISCN,ISTA) ),
     6     CTCORR
C
      ILINE = ILINE + 3
C
C     Print sun warning if needed.
C
      CALL SUNWARN( ISCN, 10.0, ILINE, IPRT )
C
      RETURN
      END
