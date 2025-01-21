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

C            CALL PUTOUT( ' +-----------------------+')
C            CALL PUTOUT( ' | Version 11.8 Advisory |')
C            CALL PUTOUT( ' +-----------------------+')
C            CALL PUTOUT( ' -')
C            CALL PUTOUT( " - Sched v11.7 incorporates the position " // 
C     1                   "changes from 11.5 to 11.6.  ")
C            CALL PUTOUT( " - If you are in a multi epoch observation" //
C     1                   " you may wish to continue using sched 11.5.")
C            CALL PUTOUT( " - Additionally Sched 11.7 uses a new  " //
C     1                   "default srcfile (if no srcfile is set in ")
C            CALL PUTOUT( " - your key file). To ensure " //
C     1                   "sched 11.5 is run by VLBA")
C            CALL PUTOUT( " - operations, please include in the .key " //
C     1                   "file a line similar to:")
C            CALL PUTOUT( " -")
C            CALL PUTOUT( " -     note1 = 'Please schedule with " //
C     1                   "Sched 11.5'")
C            CALL PUTOUT( " -")
C            CALL PUTOUT( " - All other projects should see improved " //
C     1                   "results with this new version of sched. ")

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

