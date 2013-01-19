      SUBROUTINE ERRLOG( MESSAGE )
C
C     Routine for SCHED that deals with an error abort.  This is
C     to be used where ERROR used to be used.  It writes to
C     logfile, closes the logfile, and calls ERROR.
C
      INCLUDE  'sched.inc'
      CHARACTER   MESSAGE*(*)
C -----------------------------------------------------------------
C     Alert the user that something is very wrong.
C
      CALL WLOG( 1, ' ' )
      CALL WLOG( 1, MESSAGE )
      CALL WLOG( 1, ' ' )
      CALL WLOG( 1, '+++ERROR+++ SCHED terminating' )
      CALL WLOG( 1, '   The above is a fatal error.' )
      CALL PUTOUT( '   There may be much more information in '//
     1     LOGFILE  )
C
C     Then call error.
C
      CALL ERROR( 'SCHED terminating.  Fix input and restart.' )
C
C     Actually will never get back here, but for show:
C
      RETURN
      END
