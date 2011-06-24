      SUBROUTINE INPUT
C
C     Get all user and catalog input for SCHED.
C
C     This concentrates on getting the input information.  Setting
C     defaults and checking the data should come later.
C
C     Be careful about inverting any call orders as one routine may
C     depend on results from previous ones.
C
      INCLUDE      'sched.inc'
      INCLUDE      'schpeak.inc'
C
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'INPUT starting' )
C
C     Read the main user input file.  For now, the station catalog
C     needs to be read there too, but try to move it out.
C     This does pick up any inline catalog stuff.
C
      CALL SCHIN
C
C     Get reference pointing instructions.  This should come before
C     ACCSRC because it specifies some sources that need to be 
C     kept when the catalogs are read.  
C
      CALL RDPEAK( IUPK )
C
C     Collect the names of all sources used in the schedule.  Include
C     sources that might get used in reference pointing.  This 
C     determines which sources to keep from the catalogs.
C
      CALL ACCSRC( .TRUE. )
C
C     Get the sources needed from the main catalog and the 
C     second catalog.  Initialize the VERSIONs before going
C     into the routine to keep the logic relatively simple.
C
      SRVER = ' --- '
      SRVER2 = ' --- '
      CALL SRREAD( IUSRC, SRCFILE, .TRUE., .TRUE., '1' )
      IF( SRCFILE2 .NE. 'NONE' ) THEN
         CALL SRREAD( IUSRC, SRCFILE2, .TRUE., .TRUE., '3' )
      END IF
C
C     Get the pointing catalog sources.
C
      IF( DOPOINT ) THEN
         CALL SRREAD( IUSRC, PSRCFILE, .TRUE., .TRUE., '2' )
      END IF
C
C     Note that the schedule sources list (srcname) will not
C     get built until SRFINISH is run.
C
C     Get the setup files if needed.
C
      IF( .NOT. NOSET ) CALL GETSET
C
C     Get the frequency catalog if needed.
C
      IF( .NOT. NOSET ) CALL GETFREQ
C
C     Now write the number of input scans to get beyond all
C     the user feedback about catalog reads.
C
      MSGTXT = ' '
      WRITE( MSGTXT, '( A, I5, A )' ) 'INPUT:   Found ', NSCANS, 
     1   ' input scans. '
      CALL WLOG( 0, MSGTXT )
C
      RETURN
      END
