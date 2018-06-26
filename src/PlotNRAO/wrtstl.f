      SUBROUTINE WRTSTL
C
C     For the configuration search function of SCHED.
C     When writing out a plot file (eps or some such), also write
C     a comma separated value (.csv) and a SCHED station format
C     (.dat) file using the current station positions, which may
C     have been dragged and dropped.  The .csv file can be read
C     into other programs, notably Google Earth, for alternative
C     plotting.  The .dat file can be used to save the moved 
C     positions for future processing in SCHED.
C
C     This is called from PLUVNAM, which writes the panel with
C     the station information on the uv plot file.  Note you
C     will not get this output unless that type of plot is being
C     written.  The .csv and .dat files will have the same base
C     name as the plot files, so they are easily associated.
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      INTEGER    FLEN, LEN1, PCH, IC, IER, VLBOPE, ISTA, ISCAT
      CHARACTER  CSVFILE*80, DATFILE*80, RESULT*255
      CHARACTER  CHLAT*9, CHLONG*10, TFORM*15
C
C  -----------------------------------------------------------------
C
      FLEN = LEN1( PFLFIL(2) )
      DO IC = FLEN, 1, -1
         IF( PFLFIL(2)(IC:IC) .EQ. '.' ) THEN
            PCH = IC - 1
            GO TO 100
         END IF
      END DO
  100 CONTINUE
      CSVFILE = PFLFIL(2)(1:PCH) // ".csv"
      DATFILE = PFLFIL(2)(1:PCH) // ".dat"
C
C     Do a bit of hoop jumping to delete an existing file of the same
C     name, if one exists (first VLBOPE succeeds).  First, try to open
C     as OLD.  If that works (IER=1), there is an old file.  Delete it.
C     Then open the new file.  Unfortunately there doesn't seem to be 
C     a STATUS for make new or replace.
C
C     First the CSV file.
      IER = VLBOPE( ICSV, CSVFILE, 'TEXT', 'OLD', RESULT )
      IF( IER .EQ. 1 ) THEN
         CLOSE( ICSV, STATUS = 'DELETE' )
         CALL WLOG( 1, 'WRTSTL: Deleted old .csv file' )
      ELSE
         MSGTXT = ' ' 
         MSGTXT = 'WRTSTL: Above error messages normal if .csv '//
     1       'file doesn''t already exist.'
         CALL WLOG( 1, MSGTXT )
         CALL WLOG( 1, 'If it does, it would be deleted.' )
      END IF
      IER = VLBOPE( ICSV, CSVFILE, 'TEXT', 'NEW', RESULT )
      IF( IER .EQ. 1 ) THEN
         CALL WLOG( 1, 'WRTSTL: Opened new .csv file' )
      ELSE
         CALL WLOG( 1, RESULT(1:LEN1(RESULT) ) )
         CALL ERRLOG( 'WRTSTL: Open of .csv file failed' )
      END IF
C
C     SCHED station catalog
C
      IER = VLBOPE( IDAT, DATFILE, 'TEXT', 'OLD', RESULT )
      IF( IER .EQ. 1 ) THEN
         CLOSE( IDAT, STATUS = 'DELETE' )
         CALL WLOG( 1, 'WRTSTL: Deleted old .dat file' )
      ELSE
         MSGTXT = ' ' 
         MSGTXT = 'WRTSTL: Above error messages normal if .dat '//
     1       'file doesn''t already exist.'
         CALL WLOG( 1, MSGTXT )
         CALL WLOG( 1, 'If it does, it would be deleted.' )
      END IF
      IER = VLBOPE( IDAT, DATFILE, 'TEXT', 'NEW', RESULT )
      IF( IER .EQ. 1 ) THEN
         CALL WLOG( 1, 'WRTSTL: Opened new .dat file' )
      ELSE
         CALL WLOG( 1, RESULT(1:LEN1(RESULT) ) )
         CALL ERRLOG( 'WRTSTL: Open of .dat file failed' )
      END IF
C
C     Now write the file contents.
C
C     Put column headers on the csv file.
C
      WRITE( ICSV, '( A )' )
     1  ' Name, latitude, longitude '
      WRITE( IDAT, '( A, A )' )
     1  '! SCHED station file corresponging to plot file ', 
     2  DATFILE(1:LEN1(DATFILE))
C
C     Now loop through the stations.
C     For ELEV, use the input value - don't have shifted version.
C     For LAT and LONG, use the shifted values, which have replaced
C     the input values at this point.
C
C     Google Earth uses east longitude, so I need to use the negative
C     of the west longitude in SCHED in the csv file.
C
      DO ISTA = 1, NSTA
         ISCAT = STANUM(ISTA)
         IF( PSTBCK(ISTA,1) .EQ. 1 ) THEN
C
C           csv file
C
            WRITE( ICSV, '( 2A, F12.7, A, F12.7 )' )
     1           STANAME(ISTA)(1:LEN1(STANAME(ISTA))), ', ', 
     1           LAT(ISCAT) / RADDEG, ', ', -1.0 * LONG(ISCAT) / RADDEG
C
C           dat file
C
            CHLAT = TFORM( LAT(STANUM(ISTA)), ' ', 1, 2, 2, ':: ' )
            CHLONG = TFORM( LONG(STANUM(ISTA)), ' ', 1, 3, 2, ':: ' )
            WRITE( IDAT, '( 2A, 3A, A, F8.1, 2A, 2A, A )' )
     1           'station=', STANAME(ISTA),
     2           ' stcode=''', STCODE(ISCAT), '''', 
     3           ' elev=', ELEV(ISCAT),  
     4           ' lat=', CHLAT,
     5           ' long=', CHLONG,
     6           ' ax2lim=10,90 /'
         END IF 
      END DO 
C
C     Close the files so we can use them without exiting SCHED.
C
      CLOSE( UNIT = ICSV )
      CLOSE( UNIT = IDAT )
C
      RETURN
      END
