      SUBROUTINE WLOG( TERM, MESSAGE )
C
C     Routine for SCHED that writes messages to the run log
C     and, optionally, to the terminal.
C
C     It should replace the old routine PUTOUT in most cases.
C
C     TERM should be 0 (write to log file only) or 1 (write to 
C     logfile and terminal).  Anything else is an error.
C
      INCLUDE  'sched.inc'
      INTEGER    TERM
      CHARACTER  MESSAGE*(*)
      INTEGER    LEN1, NCH
C-----------------------------------------------------------------------
C     Programming check:
C
      IF( TERM .NE. 0 .AND. TERM .NE. 1 ) THEN
         WRITE( ILOG, '( A, I10, /, A )' ) 
     1       'WLOG:  TERM not 0 or 1. Programming error.  TERM = ',
     2       TERM, ' The message being logged is:'
         WRITE( *, '( A, I10, /, A )' ) 
     1       'WLOG:  TERM not 0 or 1. Programming error.  TERM = ', 
     2       TERM, ' The message being logged is:'
      END IF
C
C
C     Get the length of the character string.  Don't get into trouble
C     with blanks.
C
      NCH = MAX( LEN1( MESSAGE ), 1 )
C
C     Write the string to the terminal if in DEBUG mode.  If there
C     is a crash, the log file doesn't necessarily get written.
C
      IF( DEBUG ) WRITE( *, '(A)' ) MESSAGE(1:NCH)
C
C     Write the string to the log file.
C
      WRITE( ILOG, '(A)' ) MESSAGE(1:NCH)
C
C     If TERM is 1, write to the screen too.
C     This should be discouraged except for error messages that 
C     explain an abort.
C
      IF( TERM .NE. 0 .AND. .NOT. DEBUG ) THEN
         WRITE (*,'(A)') MESSAGE(1:NCH)
      END IF
C
      RETURN
      END
