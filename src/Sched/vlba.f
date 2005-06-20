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
      INTEGER           ISCN, ISTA, QUOUT
      INTEGER           LASTDY, DOY1, DOY2
      INTEGER           TPCINDX, PTADD, LLS, LSTA
      INTEGER           LSCN
      DOUBLE PRECISION  ONESEC, LSTOP
      LOGICAL           FIRSTS, FRS, DOSET, LPTVLB
      LOGICAL           WRTSET, POSTPASS
      CHARACTER         VLBAD1*9, VLBAD2*9, REWSP*7
      CHARACTER         FRSDUR*17, TSTART*9, TSTOP*9
      PARAMETER         (ONESEC = 1.D0 / 86400.D0)
      INTEGER           TPCDIR, TPCHEAD, TPCDRIV
      LOGICAL           DOTAPE, PASSOK
C
C     Save variables.
C
      SAVE              POSTPASS, LSTA, LLS, LASTDY, LPTVLB, LSCN
      SAVE              LSTOP, TPCDRIV, PASSOK
C
C     Parameter to turn on or off the POSTPASS commands at the end of
C     tapes.  This can be changed if operations changes its mind
C     about the necessity of postpasses.
C
      DATA          POSTPASS / .TRUE. /
C
      DATA          LSTA, LLS / 0, 0 /
C --------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. 3 ) CALL WLOG( 0, 'VLBA: Starting' )
C
C     Wrap up the schedule on the last call.
C
      IF( ISCN .EQ. -999 ) THEN
         CALL VLBAEND( ISTA, POSTPASS, LASTDY, TPCDRIV, LSCN, PASSOK )
      ELSE
C
C        Deal with a regular scan.
C        Record the last scan processed for use in the final call.
C
         LSCN = ISCN
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
C        and there is more than a 10 second gap between scans.
C
         DOSET =  VLBITP .AND. ( FIRSTS .OR. 
     1      ( STARTJ(ISCN) - TPSTART(ISCN,ISTA) - LSTOP ) .GT. ONESEC )
C
C        Get tape information.  Deal with tape changes including
C        postpasses etc. VLBITP just implies wide band recording.
C        Some "tape" stuff is needed for Mark 5A too.
C
         IF( VLBITP )
     1      CALL VLBATI( ISCN, ISTA, FIRSTS, DOTAPE, POSTPASS, REWSP, 
     2                DOSET, TPCDRIV, TPCDIR, TPCHEAD, TPCINDX, PASSOK, 
     3                LASTDY, LSTOP )
C
C        Get the scan started.  VLBAST deals with scan times, source
C        information, caltime, and pointing requests.
C
         CALL VLBAST( ISCN, ISTA, FIRSTS, LSTOP,
     1          TSTART, TSTOP, DOSET, DOY1, DOY2, VLBAD1, VLBAD2 )
C
C        Detect a new setup file.
C
         WRTSET = FIRSTS .OR. ISTA .NE. LSTA .OR. LS .NE. LLS 
         LLS = LS
         LSTA = ISTA
C
C        Write most of setup information.  Call every scan since
C        some parameters can be changed by the main schedule.
C
         CALL VLBASU( ISCN, ISTA, FIRSTS, FRS, WRTSET, TPCHEAD )
C
C        Turn off the unused tape drive.  This is in the same routine
C        used earlier for tape changes, but it is better to issue
C        this first command after the setups are established.
C
         IF( VLBITP .AND. USETAPE(ISTA) .AND.
     1       FIRSTS .AND. .NOT. AUTOALOC(ISTA) ) THEN
            CALL VLBACHG( ISCN, ISTA, FIRSTS, DOTAPE, POSTPASS, 
     1                 LASTDY, LSTOP, TPCDRIV, PASSOK )
         END IF
C
C        Do a setup scan for Mark III or VLBA observations when there
C        is a gap between scans.  Also do one at the start of any
C        experiment to force a scan header in the monitor data at the
C        experiment start time.
C
         IF( DOSET .OR. FIRSTS ) THEN
C
C           Write tape motion commands for Mark III or VLBA.
C           If AUTOALLOCATE requested, give only tape and write 
C           command.
C
            CALL VLBATP( ISCN, ISTA, REWSP, TPCDRIV, 
     2                      TPCINDX, TPCDIR, .TRUE. )
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
            IF( DOPEAK(ISCN) .GT. NCHAN(LS) ) THEN
               WRITE( MSGTXT, '( A, I4 )' ) 
     1             'VLBA: Invalid channel specified for PEAK: ', 
     2             DOPEAK(ISCN)
               CALL ERRLOG( MSGTXT )
            ELSE IF( DOPEAK(ISCN) .GT. 0 ) THEN
               WRITE( IUVBA, '( A, I2 )' ) 'peakchan = ', DOPEAK(ISCN)
            END IF
         END IF
C
C        Tape controls for wide band tapes.  
C
         CALL VLBATP( ISCN, ISTA, REWSP, TPCDRIV, TPCINDX, TPCDIR,
     1                   .FALSE. )
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
               CALL PTVLBA( ISCN, PTADD, TSTOP )
            ELSE
               CALL ROTVLBA( ISCN, ISTA, PTADD, LASTDY )
            END IF
         ELSE IF( TANVLBA(ISCN) ) THEN
            CALL TAVLBA( ISCN, PTADD, TSTOP )
         ELSE IF( DOPN3DB(ISCN) ) THEN
            CALL PN3DB( ISCN, PTADD, TSTOP )
         ELSE IF( FRS ) THEN
            CALL FSVLBA( TSTOP )
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
C        Now set this scan's stop time to be the "last" stop time.
C
         LSTOP = STOPJ(ISCN)
C
      END IF      ! Not final wrapup.
C
      RETURN
      END





