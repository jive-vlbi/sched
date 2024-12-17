      SUBROUTINE PLOTTER( MKFILES, RESTART )
C
C     This routine, which should be in file schplot.stub, is
C     a substutute for all the SCHED plotting routines for
C     use by sites that do not have PGPLOT.  It simply returns
C     reasonable values for the call arguments and prevents
C     the linker from complaining.
C
      LOGICAL   MKFILES, RESTART
C ----------------------------------------------------------------------
      MKFILES = .TRUE.
      RESTART = .FALSE.
C
      CALL PUTOUT( 'PLOTTER STUB: SCHED was compiled without '//
     1    'plotting capability.' )
C
      RETURN
      END
