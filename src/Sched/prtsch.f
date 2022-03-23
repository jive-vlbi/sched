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
      INTEGER            IFREQ, I
      INTEGER            TEARLY, TDWELL, NFLINES, CRSETC(MAXCHN)
      INTEGER            KF, LKF, GBY1, GBY2, LGBY, ICH1
      DOUBLE PRECISION   LBW(MCHAN), SBW(MCHAN), SBBC(MCHAN)
      DOUBLE PRECISION   LFREQ(MCHAN), SFREQ(MCHAN), START, STOP 
      DOUBLE PRECISION   TSTART, RTCORR, LSTOPJ, DSTART
      LOGICAL            FIRSTS, DOWRTF, DOWRTB
      INTEGER            CRDN
      DOUBLE PRECISION   CRDF(MCHAN), CRDB(MCHAN), CRDLOSUM(MCHAN)
      CHARACTER          CRDS(MCHAN)*1
      CHARACTER          TFORM*15, SHORTN*10
      CHARACTER          CLST1*9, CLST2*9, FF*1
      CHARACTER          CSTART*15, CSTOP*15, CTSTART*15, CTCORR*15
      CHARACTER          PRTSRC*12
      CHARACTER          MNAME1*3, MNAME2*3, DNAME1*3, DNAME2*3
      SAVE               ILINE, IPAGE, LASTDY, LNCHL, LFREQ, LBW
      SAVE               LGBY, LKF
      SAVE               LSTOPJ
C -----------------------------------------------------------------
      IF( DEBUG .AND. FIRSTS ) THEN
         CALL WLOG( 0, 'PRTSCH: Making operator schedule file for' //
     1      STANAME(ISTA) )
      END IF
C
      NCHL = NCHAN(NSETUP(ISCN,ISTA))
C
C     Reset line (ILINE) and page (IPAGE) counters on first scan.
C
      FF = CHAR(12)       !  Form feed.
      IF( FIRSTS ) THEN
         LGBY = 0
         ILINE = 999
         IPAGE = 1
         LASTDY = -999
         LSTOPJ = 0.D0
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
     2      (COVER(I)(1:MAX(1,LEN1(COVER(I)))), I=1,MCOVER)
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
C     Test if there are any new frequencies or bandwidths.
C
      KF = FSETI(ISCN,ISTA)
      DOWRTF = .FALSE.
      DOWRTB = .FALSE.
      IF( KF .NE. LKF ) THEN
         CALL FSFREQ( KF, SFREQ, SBBC, SBW,
     1         CRDN, CRDF, CRDB, CRDS, CRDLOSUM, CRSETC )
         NCHL = NCHAN( FSETKS(KF) )
         IF( NCHL .NE. LNCHL ) THEN
            DOWRTF = .TRUE.
            DOWRTB = .TRUE.
         END IF
         DO IFREQ = 1, NCHL
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
C        or bandwidths at 6 per line.  Note that this might not be
C        exact as it depends on how many digits are needed per freq.
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
      IF( DOWRTF .OR. DOWRTB )   LTEST = LTEST + 1
      IF( DOWRTF )               LTEST = LTEST + 2 * NFLINES
      IF( DOWRTB )               LTEST = LTEST + NFLINES
C
      IF( LTEST .GT. LINEPG ) THEN
         ILINE = 8
         IPAGE = IPAGE + 1
C
C        Write page header.
C
         WRITE( IPRT, '( A1, A,A, A,A,A, T70, A,I3, /, 15X,A )' ) 
     1      FF, 'Schedule for ', STANAME(ISTA),
     2      '  (Code ', STCODE(STANUM(ISTA)), ') ', 
     3      'Page ', IPAGE,     EXPT(1:LEN1(EXPT))
C
         WRITE( IPRT, '( 2A, /, 2A, /, A, /, 2A, /, A )' )
     1      '  UP:  D => Below limits;  H => Below horizon mask;',
     2      '  W => still slewing at end;  blank => Up.',
     3      '  Early: Seconds between end of slew and start. ',
     4      '  Dwell: On source seconds. ',
     5      '  Disk: GBytes recorded to this point.',
     6      '  TPStart:  Recording start time.',
     7          '  Frequencies are LO sum (band edge).',
     8      '  SYNC: Time correlator is expected to sync up.'
C
         WRITE( IPRT, '( 88(''-''), /, 2A, /, 2A, /, 88(''-'') )' )
     1      'Start UT  Source               Start / Stop           ',
     2      '      Early    Disk   TPStart',
     3      'Stop UT                  LST      EL    AZ   HA  UP   ',
     4      'ParA  Dwell   GBytes    SYNC'
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
     1      ANNOT(ISCN)(1:MAX(1,LEN1(ANNOT(ISCN))))
         ILINE = ILINE + 2
      END IF
C
C     Write new frequencies and bandwidths.
C     Allow more digits in the frequency specifications.  Only print
C     them all when needed.  To align columns, assume SFREQ(1) has the
C     number of digits normally needed for the frequency.
C
      IF( DOWRTF .OR. DOWRTB ) THEN
         WRITE( IPRT, '( 2X )' )
      END IF
C
      ICH1 = LEN1( ' Next scan frequencies:' )
      CALL LSTFREQ( IPRT, SFREQ, SBBC, SBW, ICH1, NCHL, ILINE,
     1       100, 8, DOWRTF, DOWRTB,
     2       ' Next scan frequencies:',
     3       ' Next BBC frequencies:',
     4       ' Next scan bandwidths:' )
C
C     Schedule file output
C
      CLST1 = TFORM( LST1(ISCN,ISTA), 'T', 0, 2, 2, '  @' )
      CLST2 = TFORM( LST2(ISCN,ISTA), 'T', 0, 2, 2, '  @' )
C
C     Short name, Up-Down comment, and disk stuff.
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
C
C        Get the media start time.  This is no longer so simple because
C        the VLA and VLBA systems believe the good data time from the
C        Vex file.  Other systems still use the old start time.
C        USEONSRC is the flag for which to use.
C


C      For exploring the start times and tcorr.
C      write(*,*) 'prtsch: ', iscn, ista, ' ', stcode(stanum(ista)) ,
C     1    ' ', useonsrc(stanum(ista)), ' ', prestart(iscn),
C     2    startj(iscn), tpstart(iscn,ista) / onesec, 
C     2    ( tonsrc(iscn,ista) - startj(iscn) ) / onesec,
C     3    ( tcorr(iscn,ista) - startj(iscn) ) / onesec


         IF( USEONSRC(STANUM(ISTA)) ) THEN
            DSTART = MAX( ( STARTJ(ISCN) - TPSTART(ISCN,ISTA) ), 
     1           TONSRC(ISCN,ISTA) )
         ELSE
            DSTART = ( STARTJ(ISCN) - TPSTART(ISCN,ISTA) )
         END IF
C
         IF( ABS( LSTOPJ - DSTART ) .LT. ONESEC ) THEN
            CTSTART = 'No stop'
         ELSE
            CALL TIMEJ( DSTART, YEAR3, DAY3, TSTART )
            CTSTART = TFORM( TSTART, 'T', 0, 2, 2, '  @' )
         END IF
C
C        Get the correlator sync time.
C
         CALL TIMEJ( TCORR(ISCN,ISTA), YEAR3, DAY3, RTCORR )
         CTCORR = TFORM( RTCORR, 'T', 0, 2, 2, '  @' )
C
         LSTOPJ = STOPJ(ISCN)
      END IF
C
C     Use the GBYTE position for disks instead of the tape
C     footage.
C
      IF( USEDISK(ISTA) ) THEN
         GBY1 = LGBY
         GBY2 = NINT( GBYTES(ISCN,ISTA) )
      ELSE
         GBY1 = 0.0
         GBY2 = 0.0
      END IF
      LGBY = GBY2
C
C     Write the scan lines.
C
      WRITE( IPRT, '( 2X, /, A8, 1X, A, T24, A8, 2F6.1, F5.1, 2X,  ' //
     1     ' A, F7.1, I6, 1X, I8, 3X, A8 )' )
     2     CSTART, PRTSRC, CLST1, 
     3     EL1(ISCN,ISTA), AZ1(ISCN,ISTA), HA1(ISCN,ISTA), 
     4     UP1(ISCN,ISTA), PA1(ISCN,ISTA), TEARLY, 
     5     GBY1, CTSTART
C
      WRITE( IPRT, '( A8, 2X, A, T24, A8, 2F6.1, F5.1, 2X, ' // 
     1     ' A, F7.1, I6, 1X, I8, 3X, A8 )' ) 
     2     CSTOP, SHORTN, CLST2, 
     3     EL2(ISCN,ISTA), AZ2(ISCN,ISTA), HA2(ISCN,ISTA), 
     4     UP2(ISCN,ISTA), PA2(ISCN,ISTA), TDWELL,
     5     GBY2, CTCORR
C
      ILINE = ILINE + 3
C
C     Print sun warning if needed.
C
      CALL SUNWARN( ISCN, 10.0, ILINE, IPRT )
C
      RETURN
      END
