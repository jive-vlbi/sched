      SUBROUTINE DELSCR( RESTART )
C
C     Delete any scratch files.  I did this instead of implementing
C     STATUS=SCRATCH in the VLBOPE, but maybe that should be changed
C     some day.  I'm generally a bit reluctant to mess with the Cit
C     routines.
C
C     Also close the log file, since this is the end of the program.
C
      INCLUDE     'sched.inc'
C
      LOGICAL     EXISTS, RESTART
      INTEGER     IOERR, VLBOPE, LEN1
C  -------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'DELSCR starting.' )
C
C     Cover letter scratch file.
C
      INQUIRE( FILE='schedcov.tmp', EXIST=EXISTS )
      IF( EXISTS ) THEN
         IOERR = VLBOPE( ICOV, 'schedcov.tmp', 'TEXT', 'OLD', MSGTXT )
         IF( IOERR .NE. 1 ) THEN
            CALL WLOG( 1, MSGTXT )
            CALL ERRLOG( 'WRTCOV: Cannot open '//
     1       'cover letter scratch file.  This should not happen.' )
         ELSE
            CLOSE( UNIT=ICOV, STATUS='DELETE' )
         END IF
      END IF         
C
C     Close the log file.  Use PUTOUT because these don't need to be
C     in the logfile.
C
      IF( .NOT. RESTART ) THEN
         IF( READLOG ) THEN
            CALL PUTOUT( ' ' )
            CALL PUTOUT( ' *** There are special messages in ' // 
     1           LOGFILE )
            CALL PUTOUT( '     Please read them.' )
            CALL PUTOUT( ' ' )
         ELSE
            CALL PUTOUT( ' REMINDER - see ' // 
     1             LOGFILE(1:LEN1(LOGFILE)) //
     2             ' for most run time messages.' )
         END IF
C
         CLOSE( UNIT=ILOG )
C
      END IF
C
      RETURN
      END

