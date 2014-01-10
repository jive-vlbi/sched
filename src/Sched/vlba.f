      SUBROUTINE VLBA( ISCN, ISTA, FIRSTS )
C
C     Writes a basic VLBA control schedule.  The setup must be added.  
C
C     If the control file for the recorder at the VLA is being written, 
C     do not write many of the unneeded VLBA equipment parameters.  
C
C     Do not attempt to avoid writing the unneeded VLA tape control 
C     file when the wideband tape recorder is not being used.  The
C     user can just ignore it.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER           ISCN, ISTA, QUOUT, KSTA
      INTEGER           LASTDY, DOY1, DOY2
      INTEGER           PTADD, LKS, LSTA
      INTEGER           LSCN, LEN1, KS, KR, LKR
      DOUBLE PRECISION  LSTOP
      LOGICAL           FIRSTS, FRS, DOSET, LPTVLB
      LOGICAL           WRTSET, CRLWARN, SLEWPBM
      CHARACTER         VLBAD1*9, VLBAD2*9
      CHARACTER         FRSDUR*17, TSTART*9, TSTOP*9
      CHARACTER         LCRDLINE*80
C
C     Save variables.
C
      SAVE              LSTA, LKS, LKR, LASTDY, LPTVLB, LSCN
      SAVE              LSTOP, CRLWARN, LCRDLINE
C
      DATA          CRLWARN  / .TRUE. /
C
      DATA          LSTA, LKS, LKR, LSCN / 0, 0, 0, 0 /
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'VLBA: Starting' )
      IF( ISTA .NE. LSTA ) LSCN = 0
C
C     Wrap up the schedule on the last call.
C
      IF( ISCN .EQ. -999 ) THEN
         CALL VLBAEND( ISTA, LASTDY, LSCN )
      ELSE
C        Deal with a regular scan.
C
C        On the first call for the station, initialize some variables
C        and call VLBAINI to start writing the control file.
C
         IF( FIRSTS ) THEN
            LASTDY = -999
            LPTVLB = .FALSE.
            LSTOP = 0.D0
            CALL VLBAINI( ISCN, ISTA )
         END IF
C
C        Detremine if a setup scan will be needed - true if writing tape
C        and there is more than a 1 second gap between scans.
C
         DOSET =  VLBITP .AND. ( FIRSTS .OR. 
     1      ( STARTJ(ISCN) - TPSTART(ISCN,ISTA) - LSTOP ) .GT. ONESEC )
C
C        But don't do a setup scan if there is a reasonable chance that
C        the on-line system and SCHED will disagree on the wrap.  See
C        the discussion in the comments in wrap.f for details.  Added
C        Jan. 9, 2014  RCW.
C
         KSTA = STANUM(ISTA)
         IF( LSCN .NE. 0 ) THEN
            SLEWPBM = AZ1(ISCN,ISTA) .LE. AX1LIM(2,KSTA) - 360.0 .AND.
     1                AZ2(ISCN,ISTA) .GT. AX1LIM(2,KSTA) - 360.0 .AND.
     2                AZ2(LSCN,ISTA) .GT. 250.0 
            MSGTXT = ' '
            IF( DOSET .AND. SLEWPBM ) THEN
               WRITE( MSGTXT, '( A, I5, A, A, A )' )
     1            'VLBA:  Not writing setup file for scan ', ISCN,
     2            ', station ', STANAME(ISTA), 
     3            ' to avoid potential incorrect wrap.'
               CALL WLOG( 1, MSGTXT )
            END IF
         ELSE
            SLEWPBM = .FALSE.
         END IF
         DOSET = DOSET .AND. .NOT. SLEWPBM
C
C        Get the scan started.  VLBAST deals with scan times, source
C        information, caltime, and pointing requests.
C
         CALL VLBAST( ISCN, ISTA, FIRSTS, LSTOP,
     1          TSTART, TSTOP, DOSET, DOY1, DOY2, VLBAD1, VLBAD2 )
C
C        Detect a new setup file.  WRTFREQ will deal with new
C        frequency groups.
C
         KS = NSETUP(ISCN,ISTA)
         KR = FSETI(ISCN,ISTA)
         WRTSET = FIRSTS .OR. ISTA .NE. LSTA .OR. KS .NE. LKS .OR.
     1         KR .NE. LKR
         LKS = KS
         LKR = KR
         LSTA = ISTA
C
C        Write most of setup information.  Call every scan since
C        some parameters can be changed by the main schedule.
C
         CALL VLBASU( ISCN, ISTA, FIRSTS, FRS, WRTSET )
C
C        Write the arbitrary line - used for developing new features 
C        in the on-line system.
C
         IF( CRDLINE(ISCN) .NE. ' ' .AND. 
     1       ( CRDLINE(ISCN) .NE. LCRDLINE .OR. FIRSTS ) ) THEN
            WRITE( IUVBA, '(A)' ) CRDLINE(ISCN)(1:LEN1(CRDLINE(ISCN)))
            IF( CRLWARN ) THEN
               CALL WLOG( 1, 'WARNING: Test parameter CRDLINE being' //
     1          ' used.  Be sure you know what you are doing.' )
               CRLWARN = .FALSE.
            END IF
         END IF

         LCRDLINE = CRDLINE(ISCN)
C
C        Do a setup scan for VLBA observations when there
C        is a gap between scans.  Also do one at the start of any
C        experiment to force a scan header in the monitor data at the
C        experiment start time.
C
         IF( DOSET .OR. FIRSTS ) THEN
C
C           Turn recording off for the setup scan.
C
            CALL VLBADK( ISCN, ISTA, .TRUE. )
C
C           Stop time and !NEXT! for setup scan.
C           Write a dur=0 into setup scan for frequency switched
C           observations so that SETUP is not only 15s long.
C           Change date if necessary.
C
            IF( DOY1 .NE. LASTDY ) THEN
               WRITE( IUVBA, '(''  date = '', A9 )' ) VLBAD1
               LASTDY = DOY1
            END IF
            IF( FRS .OR. LPTVLB ) THEN
               FRSDUR = '   dur=0  !NEXT!'
            ELSE
               FRSDUR = '   !NEXT!'
            END IF
            WRITE( IUVBA, '( 3A )' ) 'stop=', TSTART, FRSDUR
C
         END IF
C
C        ---------   Setup scan done.  -----------
C
C        Write qualifier if there is a setup scan or freq. switching.
C
         IF( DOSET .OR. FIRSTS .OR. FRS ) THEN
            IF( FRS ) THEN
               QUOUT = 1
            ELSE
               QUOUT = QUAL(ISCN)
            END IF
            WRITE( IUVBA, '( A, I3 )' ) 'qual=', QUOUT
         END IF
C
C        Ask for the pointing to be peaked up if requested.  Only
C        do the for full VLBA systems.  This used to be done during
C        a setup scan if one happened.  But the on-line system has
C        a 40 second timeout to get to source, plus if DWELL is used,
C        time may be short.  Therefore put it during the main scan.
C
         IF( .NOT. VLBADAR(STANUM(ISTA)) ) THEN
            IF( DOPEAK(ISCN) .GT. NCHAN(KS) ) THEN
               WRITE( MSGTXT, '( A, I4 )' ) 
     1             'VLBA: Invalid channel specified for PEAK: ', 
     2             DOPEAK(ISCN)
               CALL ERRLOG( MSGTXT )
            ELSE IF( DOPEAK(ISCN) .GT. 0 ) THEN
               WRITE( IUVBA, '( A, I2 )' ) 'peakchan = ', DOPEAK(ISCN)
            END IF
         END IF
C
C        Turn recording on or off.
C
         CALL VLBADK( ISCN, ISTA, .FALSE. )
C  
C        Change day number if necessary.
C
         IF( DOY2 .NE. LASTDY .AND. ROTPAT .EQ. 0 ) THEN
            WRITE( IUVBA, '(''date='', A9 )' ) VLBAD2
            LASTDY = DOY2
         END IF
C
C        Finish off scan.  Just give stop time and !NEXT! for 
C        most scans, but deal with the special sequence requests
C        for pointing, Ta, or frequency switching if necessary.
C
C        PTADD is the amount to add to slew time for pointing.
C
         IF( .NOT. FIRSTS ) THEN
            PTADD = ( STARTJ(ISCN) - LSTOP ) * 3600.D0 * 24.D0
         ELSE
            PTADD = 0
         END IF
C
         IF( PNTVLBA(ISCN) ) THEN
            IF( ROTPAT .EQ. 0 ) THEN
               CALL PTVLBA( ISCN, PTADD, TSTOP, KS )
            ELSE
               CALL ROTVLBA( ISCN, ISTA, PTADD, LASTDY )
            END IF
         ELSE IF( TANVLBA(ISCN) ) THEN
            CALL TAVLBA( ISCN, PTADD, TSTOP, KS )
         ELSE IF( DOPN3DB(ISCN) ) THEN
            CALL PN3DB( ISCN, PTADD, TSTOP, KS )
         ELSE IF( FRS ) THEN
            CALL FSVLBA( TSTOP, KS )
         ELSE
            IF( LPTVLB ) THEN
               WRITE( IUVBA, '( 3A )' )
     1          'stop=', TSTOP, '   dur=0s  !NEXT!'
            ELSE
               WRITE( IUVBA, '( 3A )' )
     1          'stop=', TSTOP, '   !NEXT!'
            END IF
         END IF
         LPTVLB = PNTVLBA(ISCN) .OR. TANVLBA(ISCN)
C
C        Record the last scan processed for use in the final call.
C        Do at the end of the scan loop so it can be used as a
C        lastscan number.  Recall that the test for this station
C        being in the scan happens in STAFILES, several layers up
C        from here.
C
         LSCN = ISCN
C
C        Also set this scan's stop time to be the "last" stop time.
C
         LSTOP = STOPJ(ISCN)
C
      END IF      ! Not final wrapup.
C
      IF( DEBUG ) CALL WLOG( 0, 'VLBA: Ending' )
      RETURN
      END





