      SUBROUTINE PLOTTER( MKFILES, RESTART )
C
C     Routine for sched that initialize the X11 envinronment
C     and call the Control Panel.
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      LOGICAL     MKFILES, RESTART
      INTEGER     STAT
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL PUTOUT( 'PLOTTER: Starting. ' )
C
      IF( .NOT. PLOT ) THEN
         MKFILES = .TRUE.
      ELSE
C
C        First check if some parameters in differents include files
C        are the same
C
         CALL PLINCK
C
C        Try to open an X11 device to plot the Control Pannel.
C        If an error occours switch to terminal input
C
         STAT = 0
C
         IF ( .NOT. RESTART ) THEN
             CALL PLINIT( STAT )
             IF( STAT.LT.0 ) THEN
                CALL PUTOUT( ' PLOT: no XWindow capability ' )
                PMTTYT = .TRUE.
                CALL PLOTTY( MKFILES, RESTART, .FALSE. ) 
                RETURN
             ENDIF
         ELSE
C
C            If Terminal Mode already selected go to specific
C            routine
C
             IF( PMTTYT ) THEN 
                CALL PLOTTY( MKFILES, RESTART, PMTTYT )
                RETURN
             ELSE
                CALL PLREST
             END IF
         END IF
C
C        Write help about sliders in the startup terminal
C        window
C
         CALL PLHELP
C
C        Go to Menu Panel actions control 
C
         CALL PLMAIN( MKFILES, RESTART )
C
      END IF
C
      RETURN
      END
