      SUBROUTINE STMSG
C
C     Routine for SCHED to write some useful information when it starts
C     up.  Also make some programming, especially parameter, checks.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'schpeak.inc'
      INCLUDE 'plot.inc'
      INCLUDE 'rdcat.inc'
      CHARACTER  OPSTAT*4, OPTEXT*255
      LOGICAL    EXISTS
      INTEGER    IERR, VLBOPE, LEN1
C ---------------------------------------------------------------------
C     Open the run log.  Note cannot use ERRLOG if there is a problem
C     because the log file won't be open.
C
      READLOG = .FALSE.
      LOGFILE = 'sched.runlog'
      INQUIRE( FILE=LOGFILE, EXIST=EXISTS )
      IF( EXISTS ) THEN
         OPSTAT = 'OLD'
      ELSE
         OPSTAT = 'NEW'
      END IF
      IERR = VLBOPE( ILOG, LOGFILE, 'TEXT', OPSTAT, OPTEXT )
      IF( IERR .NE. 1 ) THEN 
         CALL WLOG( 1, 'STMSG: CANNOT OPEN LOG FILE ' // LOGFILE )
         CALL WLOG( 1, '       Error text: ' // OPTEXT )
         CALL ERROR( ' Check why log file cannot be opened' )
      END IF
C
      CALL WLOG( 1, ' ' )
      MSGTXT = ' '
      WRITE( MSGTXT, '( A, F6.2, 2X, A)' ) 
     1   '    Welcome to program SCHED.  Version: ', 
     2            VERNUM, VERSION(1:LEN1(VERSION))
      CALL WLOG( 1, MSGTXT )
      CALL WLOG( 1, ' ' )
C
      CALL WLOG( 1, 'The manual is at ' //
     1    'http://www.aoc.nrao.edu/software/sched/index.html' )
      CALL WLOG( 1, 'Unix users should set $SCHED to the base ' //
     1    'area where SCHED is installed.' )
C
C     Announce the run log.
C
      CALL WLOG( 1, 'Most run time messages will be in '
     1       //LOGFILE )
C
C     Initialize the frequency catalog version in case it isn't used.
C
      FREQVER = 'None used'
C ---------------------------------------------------------------------
C     Give some instructions.
C
      CALL WLOG( 1, ' ' )
      CALL WLOG( 1, 'Some useful commands to give now if running ' //
     1             'interactively:' )
      CALL WLOG( 1, '   SCHedule=<filename>    :  Specify input file.' )
      CALL WLOG( 1, '   PLOT                   :  '//
     1              'Invokes uv, xy, rd, and uptime plotting.' )
      CALL WLOG( 1, '   FREQLIST=lowF,highF    :  Make frequency list'//
     1          ' (MHz).  Then exit.' )
      CALL WLOG( 1, '   EXIT                   :  Leave program.' )
      CALL WLOG( 1, '   /                      :  End of inputs - '//
     1              'run program (or EXIT).' )
      CALL WLOG( 1, ' ' )
C
C ---------------------------------------------------------------------
C     Programming checks:
C
C     Since this is the first SCHED subroutine, use it to check
C     that some parameters from different includes that must be
C     related.  Since requiring sched.inc to come before schset.inc,
C     the number of tests is down.
C
      IF( MAXSTA .GT. PSTMAX ) THEN
         CALL ERRLOG( 'STMSG: Programming error.  MAXSTA .GT. PSTMAX' )
      END IF
C
      IF( MAXSRC .GT. PSOMAX ) THEN
         CALL ERRLOG( 'STMSG: Programming error.  MAXSRC .GT. PSOMAX' )
      END IF
C
      IF( MVEL .LT. MAXCHN ) THEN
         CALL ERRLOG( 'STMSG: Programming error.  MVEL .LT. MAXCHN' )
      END IF

C
C     Warn myself to reset the number of stations and/or scans from
C     configuration search versions to normal versions before a 
C     release.
C
      IF( MAXSTA .GT. 50 ) THEN
         CALL WLOG( 1, ' -------------------------------------' )
         WRITE( MSGTXT, '( A, I5, A )' ) 
     1         'STMSG: *****  MAXSTA greater than 50. (', MAXSTA, 
     2         ')' 
         CALL WLOG( 1, MSGTXT )
         MSGTXT = ' '
         CALL WLOG( 1, '       Reduce MAXSTA before a release' )
         CALL WLOG( 1, ' -------------------------------------' )
      END IF
      IF( MAXSCN .LT. 3000 ) THEN
         CALL WLOG( 1, ' -------------------------------------' )
         WRITE( MSGTXT, '( A, I5, A )' ) 
     1         'STMSG: *****  MAXSCN less than 3000.(', MAXSCN, 
     2         ')' 
         CALL WLOG( 1, MSGTXT )
         MSGTXT = ' '
         CALL WLOG( 1, '       Increase MAXSCN before a release' )
         CALL WLOG( 1, ' -------------------------------------' )
      END IF
C
C ----------------------------------------------------------------------
C     Now to the job at hand.
      RETURN
      END
