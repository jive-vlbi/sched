      SUBROUTINE PLMAIN( MKFILES, RESTART )
C
C     Routine for sched that selects plot type.
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      LOGICAL     MKFILES, POPEN, RESTART, DIDRST
      INTEGER     STAT, ID
C
      DATA        DIDRST  / .FALSE. /
C ----------------------------------------------------------------------
C
C     Initializations for after each restart.
C
      RESTART = .FALSE.
C
C     Set the defaults for Plot
C
      POPEN = .FALSE.
C
C     This is the main loop.
C
 400  CALL PGSLCT( PPOPEN )
      CALL PLMENU( ID )
C
C     PLOT Button
C
      IF( ID .EQ. 1 ) THEN
         CALL PLPLOT( '/XW', POPEN, 0 )
C
C     CLOSE Button
C
      ELSE IF( ID .EQ. 2 ) THEN
         IF( POPEN ) THEN
            CALL PGSLCT( PPNWIN )
            CALL PGCLOS
            POPEN = .FALSE.
            PPNWIN = 0
         ENDIF
C
C     FILES Button: Save Image 
C
      ELSE IF( ID .EQ. 301 ) THEN
         CALL PLPLOT( PFLFIL(1), .FALSE., 1 )
C
C     FILES Button: Save Inputs 
C
      ELSE IF( ID .EQ. 302 ) THEN
         CALL PLFSAV( PFLFIL(3) )
C
C     FILES Button: Loads Inputs 
C
      ELSE IF( ID .EQ. 303 ) THEN
         CALL PLFLOD( PFLFIL(3), STAT )
C
C     OPTION Button: Save Configuration 
C
      ELSE IF( ID .EQ. 601 ) THEN
         CALL PLOPIO( 2, STAT )
C
C     OPTION Button: Load Configuration 
C
      ELSE IF( ID .EQ. 602 ) THEN
         CALL PLOPIO( 1, STAT )
         IF( STAT .GT. 0 ) CALL PLOPIO( 0, STAT )
C
C     OPTION Button: Set Default Configuration 
C
      ELSE IF( ID .EQ. 603 ) THEN
         CALL PLOPIO( 0, STAT )
C
C     RESTART Button
C
      ELSE IF( ID .EQ. 7 ) THEN
         RESTART = .TRUE.
         DIDRST  = .TRUE.
         MKFILES = .FALSE.
         IF( POPEN ) THEN
            CALL PGSLCT( PPNWIN )
            CALL PGCLOS
            POPEN = .FALSE.
            PPNWIN = 0
         ENDIF
         RETURN
C
C     FINISH Button
C
      ELSE IF( ID .EQ. 8 ) THEN
         MKFILES = .TRUE.
         IF( MISSING ) THEN
            MKFILES = .FALSE.
            CALL PUTOUT( 'PLMAIN: Will not write output files '//
     1                   ' because of missing information (cover?)' )
         END IF
         IF( DIDRST ) THEN
            MKFILES = .FALSE.
            CALL PUTOUT( 'PLMAIN: Will not write output files' //
     1                   ' after RESTART.  Parameters might be wrong.' )
            CALL PUTOUT( 'PLMAIN: Rerun SCHED with input file.' )
         END IF
         GOTO 990
C
C     TERMINAL Button
C
      ELSE IF( ID .EQ. 9 ) THEN
         POPEN = .FALSE.
         CALL PLEND
         CALL PGEND
         CALL PLOTTY( MKFILES, RESTART, DIDRST ) 
         GOTO 990
C
C     EXIT Button
C     Also guarantee that files are not written in OBSTYPE=CONFIG
C
      ELSE IF ( ID .EQ. 10 .OR. OBSTYP .EQ. 'CONFIG' ) THEN
         MKFILES = .FALSE.
         GOTO 990
      END IF
C
      GOTO 400
C
 990  CONTINUE
      CALL PGEND
      RETURN
C
      END
