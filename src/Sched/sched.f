      PROGRAM SCHED
C
C     A VLBI scheduling program.  It can also be used to
C     schedule some VLA projects.
C
C     Sched is written and maintained by primarily by R. Craig Walker.
C     VEX format output sections were written originally by Huib 
C     vanLangevelde, were maintained by Cormac Reynolds and are now 
C     maintained by Antonios Politides.  Most of the interactive plot
C     section was written by Franco Tinarelli of Bologna.
C
C     See the manual for information and history.
C
C     Include all major .inc files here to 'SAVE' everything.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
      INCLUDE   'schfreq.inc'
      INCLUDE   'srlist.inc'
C
      LOGICAL   MKFILES, RESTART
      INTEGER   ISET
      LOGICAL   FMTNONE, ALLNONE
C --------------------------------------------------------------------
C     Current version (up to 40 characters).
C
      CALL VERSCHED( VERNUM, VERSION )
C
C     Write some information to the screen at startup and cross check
C     some parameter settings in different includes.
C
      CALL STMSG
C
C     Initializations.
C
      RESTART = .FALSE.
C
C     Let user restart from plotter interactive input.
C     Note global defaults will not be reset.
C
  100 CONTINUE
C
C        Initializations for items not available to INPUT.
C
         NSET = 0    !  Number of setup groups.
         SRLN = 0    !  Number of sources in full source list.
C
C        Get all user and catalog input.
C
         CALL INPUT
C
C        Fill in defaults etc not provided by users.
C        Do the setup file checking.
C
         CALL DEFAULTS
C
C        Obtain source coordinates for epochs not provided. 
C
         CALL SCHPRE
C
C        Do some scan checks
C
         CALL CHKSC1
C
C        Optimization, scan geometry, tape positions etc.
C        After this call, use scans SCAN1-SCANL.
C
         CALL SCHOPT
C
C        Do doppler calculations for spectral lines if requested.
C
         CALL DOPFQ
C
C        Get sun distance from each source for later flags.
C
         CALL GETSUN
C
C        A last chance to find problems.
C
         CALL CHKSCN
C
C        Write the summary file.
C
         CALL SCHSUM( RESTART )
C
C        Go to the interactive plotting.  But first flush the buffers
C        of the log file so it can be read properly while plotting.
C
         CALL FLUXH( ILOG, LOGFILE )
         CALL PLOTTER( MKFILES, RESTART )
C
C        If a restart was requested during plotting, do it.
C
         IF( RESTART ) THEN
            CALL DELSCR( RESTART )
            CALL WLOG( 0, ' ' )
            CALL WLOG( 0, 
     1         ' ===================  RESTART  ===================== ' )
            CALL WLOG( 0, ' ' )
            GO TO 100
         END IF
C
C     -----  End of restart loop  -----
C
C     Make station output files if requested.  This won't be allowed
C     (MKFILES will be .FALSE.) if any restarts were done.
C     It also won't be allowed if the UPTIME mode is in use.
C
      IF( MKFILES .AND. OPTMODE .NE. 'UPTIME' .AND. .NOT. NOSET ) THEN
C
C        Make the VEX file if needed.
C
C        For the moment (9feb2001), the VEX writer cannot deal with
C        FORMAT=NONE.  Therefore block it, but give Huib a back
C        door for testing.
C        Modified Oct 27, 2008 to pass schedules that only have some
C        format NONE scans.  Those scans will simply be left out of
C        the Vex file.  Don't pass schedules that are all format NONE
C        but want VEX.  This will change more in the near future as we
C        figure out how to do pointing runs with VEX.
C
C        As the switch to the DiFX correlator occurs, most interferometry
C        observations will use VEX.  But block VEX output when doing
C        OBSTYPE='CONFIG' where there may be a special binary with MSET
C        = MAXMOD set too small to actually use.
C
         IF( DOVEX ) THEN
            FMTNONE = .FALSE.
            ALLNONE = .TRUE.
            DO ISET = 1, NSET
               IF( USED(ISET) .AND. FORMAT(ISET) .EQ. 'NONE' ) 
     1              FMTNONE = .TRUE.
               IF( USED(ISET) .AND. FORMAT(ISET) .NE. 'NONE' ) 
     1              ALLNONE = .FALSE.
            END DO
C            IF( OVERRIDE .OR. .NOT. FMTNONE ) THEN
            IF( ( OVERRIDE .OR. .NOT. ALLNONE ) .AND. 
     1            OBSTYP .NE. 'CONFIG' ) THEN
               CALL VXWRT
            ELSE
               WRITE( MSGTXT, '(A,A)' )
     1           'SCHED: Cannot mix VEX and OBSTYPE=CONFIG ',
     2           ' or FORMAT=NONE for all scans in this release.'
               CALL ERRLOG( MSGTXT )
            END IF
         END IF
C
C        Make the DRUDG file for VSOP.  This is still just a stub.
C        The VSOP group never provided the subroutines.
C
         IF( DOVSOP ) CALL VSOPWRT
C
C        Make the flagging file.
C
         CALL FLAGS
C
C        Make all station specific files.
C
         CALL STAFILES
C
      END IF
C
C     Delete any scratch files, close the log file, and end.
C
      CALL DELSCR( .FALSE. )
      CALL PUTOUT( ' -------  Finished  ----------- ' )
      STOP
      END
