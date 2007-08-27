      SUBROUTINE DEFSET
C
C     Routine for SCHED that sets defaults for setup files.
C     Extracted from GETSET  28 Jan 2003.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER    KS
C ---------------------------------------------------------------------
C     Expand the setups, set defaults, and check.
C
C     Expand out the setup groups so there is one per input station.
C     Also fill in the station list if none was given (full default).  
C     If stations were given, remove any that are not needed for 
C     this schedule.  After this, all remaining setups are used.
C
      CALL SETEXPND
C
C     Set a lot of defaults for all input groups.
C
      CALL SETDEFS
C
C     Check many of the input numbers.  Do for all setup groups that
C     were actually used.
C     OKMODES checks that the mode is one approved for use at the 
C     VLBA correlator.
C
      DO KS = 1, NSET
         CALL CHKSET( KS )
         CALL OKMODES( KS )
      END DO
C
C     Check things that shouldn't vary between groups in a setup file.
C
      CALL CHKSFIL
C
C     Get logical channels as needed by the dopplar and in-line freq
C     options to know how to associate channels in those specs with
C     setup file channels.
C
      CALL SFINFO
C
C     Detect channels assigned to the same BBC so DOPFQ can give them
C     the same frequency.  Also get setup file channel net sidebands.
C     Do some consistency on these issues.
C
      CALL SBPAIR
C
C     Check that some Socorro correlator restrictions have not been
C     violated.
C
      CALL CHKSOC
      CALL CHKJIVE
C
C     Finished.
C
      RETURN
      END
