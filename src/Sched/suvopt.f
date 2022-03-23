      SUBROUTINE SUVOPT( QUALITY, NLONG, NLAT, QLIMS, GHLAT, GHLONG )
C
C     Make a grid around a station and get the quality factor
C     on points on the grid.
C
C     When this is run, only one station should be color red.  That
C     is the one for which the grid will be made.
C
C     The same quality factor will be used as for UVOPT.
C
C     Dec. 14, 2010.  Changed calls to SCHSRC to calls to STAGEO
C     as SCHSRC will be part of STAGEO soon.
C
      INCLUDE    'sched.inc'
      INCLUDE    'plot.inc'
C
C     Set the grid parameters. Be sure to change the format
C     statement near the end if you change this.
C
      INTEGER    NLAT, NLONG
      REAL       QUALITY(NLONG,NLAT), QLIMS(4)
      DOUBLE PRECISION  STEP
C
C     Use LASTJSCN rather than LASTISCN to avoid confusion with
C     the LASTISCN in SCHOPT.
C
      INTEGER    ILONG, ILAT, NTEST, ISTA, KSTA, KS, ISCN, LEN1
      INTEGER    IOERR, VLBOPE, FLAG(MAXSTA), NCOMB, IC, IER
      INTEGER    LASTJSCN
      LOGICAL    EXISTS
      REAL       TOP5(5,MAXSRC)
      CHARACTER  OPTFILE*80, INFOLINE*128
      CHARACTER  OPSTAT*4, OPTEXT*256
      DOUBLE PRECISION   SLAT, SLONG, LASTTIME, T_AVAIL
      REAL       QLATMIN, QLATMAX, QLONMIN, QLONMAX
C
      CHARACTER  GHLAT(NLAT)*9, GHLONG(NLONG)*10, TFORM*15
      CHARACTER  CHLAT*9, CHLONG*10
C --------------------------------------------------------------------
C     Get the user specified step size.
C
      STEP = ( GRIDSTEP / 60.D0 ) * RADDEG
C
C     Make a .opt file much like that made by UVOPT.
C
C     Open the .opt print file.  Always write over 
C     whatever was there before.  Make a new one 
C     for each optimization effort.
C     
      WRITE( OPTFILE, '(A,A)' ) EXPCODE(1:LEN1(EXPCODE)), '.OPT'
      CALL DWCASE( OPTFILE )
      INQUIRE( FILE=OPTFILE, EXIST=EXISTS )
      IF( EXISTS ) THEN
         OPSTAT = 'OLD'
      ELSE
         OPSTAT = 'NEW'
      END IF
      CALL WLOG( 0, 'SUVOPT:  Writing new configuration file '//
     1   OPTFILE(1:LEN1(OPTFILE)) )
      IOERR = VLBOPE( IOPT, OPTFILE, 'TEXT', OPSTAT, OPTEXT )
      IF( IOERR .NE. 1 ) CALL ERRLOG( ' Open problem:'//OPTEXT )
C
C     Write some information about the stations.  First the ones
C     to be helf fixed.
C
      WRITE( IOPT,*) 'Station optimization.'
      WRITE( IOPT, '( A, I4, A, I4 )' ) '   GRIDNR = ', GRIDNR, 
     1     '   GRIDNT = ', GRIDNT
      WRITE( IOPT, '( A, F10.2, A, F10.2 )' ) '   GRIDMIN = ', GRIDMIN, 
     1     '   GRIDMAX = ', GRIDMAX
      WRITE( IOPT, '( A, F10.2 )' ) '   GRIDW0 = ', GRIDW0
C
      WRITE( IOPT,*) 'Stations held fixed: '
      DO ISTA = 1, NSTA
         IF( PSTBCK(ISTA,1) .EQ. 1 .AND. PSTBCK(ISTA,2) .EQ. 0 ) THEN
            CHLAT = TFORM( LAT(STANUM(ISTA)), ' ', 1, 2, 2, ':: ' )
            CHLONG = TFORM( LONG(STANUM(ISTA)), ' ', 1, 3, 2, ':: ' )
            WRITE( IOPT, '( 3X, A8, 4X, A, 4X, A )' )
     1          STANAME(ISTA), CHLAT, CHLONG
         END IF
      END DO
C
      NTEST = 0
      KSTA = 0
      WRITE( IOPT,*) ' '
      WRITE( IOPT,*) 'Station for grid: '
C
C     Get the station for the grid.  Set flags for UVQUAL while at it.
C
      DO ISTA = 1, NSTA
         FLAG(ISTA) = 0
         IF( PSTBCK(ISTA,1) .EQ. 1 .AND. PSTBCK(ISTA,2) .EQ. 0 ) 
     1      FLAG(ISTA) = 1
         IF( PSTBCK(ISTA,1) .EQ. 1 .AND. PSTBCK(ISTA,2) .EQ. 1 ) THEN
            NTEST = NTEST + 1
            IF( NTEST .GT. 1 ) THEN
               FLAG(ISTA) = 1
               CALL WLOG( 1, 'SUVOPT:  Too many red points.  Do first.')
            ELSE
C
C              Get station information for the tested station.  Use the
C              original elevation.  Keep LAT and LONG to restore later.
C
               FLAG(ISTA) = 2
               KSTA = ISTA
               KS = STANUM(ISTA)
               SLAT = LAT(KS)
               SLONG = LONG(KS)
               CHLAT = TFORM( SLAT, ' ', 1, 2, 2, ':: ' )
               CHLONG = TFORM( SLONG, ' ', 1, 3, 2, ':: ' )
               WRITE( IOPT, '( 3X, A8, 4X, A, 4X, A )' )
     1             STANAME(ISTA), CHLAT, CHLONG
            END IF
         END IF
      END DO
      IF( KSTA .EQ. 0 ) THEN
         CALL WLOG( 1, 'SUVOPT: No station to test.  Highlight one.' )
         CLOSE( UNIT = IOPT )
         RETURN
      END IF
C
C     Loop over location grid moving station KSTA to grid points.
C
      IC = 0
      NCOMB = NLAT * NLONG
      DO ILAT = 1, NLAT
         LAT(KS) = SLAT + ( NLAT / 2 - ILAT + 1 ) * STEP
         IF( ILAT .EQ. 1 ) QLATMAX = LAT(KS) / RADDEG
         IF( ILAT .EQ. NLAT) QLATMIN = LAT(KS) / RADDEG
         DO ILONG = 1, NLONG
            LONG(KS) = SLONG + ( NLONG / 2 - ILONG  + 1 ) * STEP
            IF( ILONG .EQ. 1 ) QLONMAX = LONG(KS) / RADDEG
            IF( ILONG .EQ. NLONG ) QLONMIN = LONG(KS) / RADDEG
            IC = IC + 1
C
C           Set the geometry and schedule parameters, much like
C           moving a station in PLOTSTA.
C        
            CALL GEOXYZ( 0, LONG(KS), LAT(KS), ELEV(KS),
     1            XPOS(KS), YPOS(KS), ZPOS(KS), IER )
            IF( IER .NE. 0 ) CALL WLOG( 1,
     1          'SUVOPT: Problem with coordinate '//
     2          'conversions for '// STANAME(STANUM(KSTA)) )
C
C           Now update the scan geometry parameters.
C            
            LASTJSCN = 0
            DO ISCN = SCAN1, SCANL
               IF( STASCN(ISCN,KSTA) ) THEN
C
C                 Call STAGEO.  Prior to Dec. 14, 2010, this was
C                 calls to SCHSRC and SLEW plus setting TONSRC.
C                 I want to absorb SCHSRC into STAGEO.
C
                  CALL STAGEO( ISCN, ISTA, STARTJ(ISCN), LASTJSCN,
     1                 LASTTIME, T_AVAIL, 'SUVOPT_1' )
                  LASTJSCN = ISCN
               END IF
            END DO
C
C           Get the quality measure for this array.
C
            CALL UVQUAL( IC, FLAG, QUALITY(ILONG,ILAT), TOP5, INFOLINE )
C
C           Write the result.
C
            GHLAT(ILAT) = TFORM( LAT(KS), ' ', 1, 2, 2, ':: ' )
            GHLONG(ILONG) = TFORM( LONG(KS), ' ', 1, 3, 2, ':: ' )
C            WRITE( IOPT, '( 3X, A8, 4X, A, 4X, A, 2I5, F8.4 )' )
C     1             STANAME(KSTA), GHLAT(ILAT), GHLONG(ILONG), 
C     2             ILAT, ILONG, QUALITY(ILONG,ILAT)
         END DO
C
         MSGTXT = ' ' 
         WRITE( MSGTXT, '( A, I7, A, I8 )' ) 
     1      'UVOPT:  Station grid. Latitude row: ', ILAT,
     2      '  Grid point: ', IC
         CALL WLOG( 0, MSGTXT(1:LEN1(MSGTXT)) )
         MSGTXT = ' ' 
      END DO
C
C     Put back the station coordinates.
C     Reset the geometry and schedule parameters.
C     
      LONG(KS) = SLONG
      LAT(KS) = SLAT
C
      CALL GEOXYZ( 0, LONG(KS), LAT(KS), ELEV(KS),
     1      XPOS(KS), YPOS(KS), ZPOS(KS), IER )
      IF( IER .NE. 0 ) CALL WLOG( 1,
     1    'SUVOPT: Problem with coordinate '//
     2    'conversions for '// STANAME(KSTA) )
C     
C     Now update the scan geometry parameters.
C      
      LASTJSCN = 0
      DO ISCN = SCAN1, SCANL
         IF( STASCN(ISCN,KSTA) ) THEN
            CALL STAGEO( ISCN, ISTA, STARTJ(ISCN), LASTJSCN,
     1                   LASTTIME, T_AVAIL, 'SUVOPT_2' )
            LASTJSCN = ISCN
         END IF
      END DO
C
C     Write the quality grid.
C
      WRITE( IOPT, '( 1X, /, A )' ) ' Latitude, Longitude Grid.'
      WRITE( IOPT, '( 8X, A10, 24( A7 ) )' ) 
     1      ( GHLONG(ILONG), ILONG = 1, NLONG )
      DO ILAT = 1, NLAT
         WRITE( IOPT, '( 1X, /, 2X, A9, 25F7.3 )' ) GHLAT(ILAT), 
     1      ( QUALITY(ILONG,ILAT), ILONG = 1, NLONG )
      END DO
C
C     Plot contours on the map.
C
      QLIMS(1) = QLONMIN
      QLIMS(2) = QLONMAX
      QLIMS(3) = QLATMIN
      QLIMS(4) = QLATMAX
C
C     Close the print file so that the buffers will flush.
C
      CLOSE( UNIT=IOPT )
C
      RETURN
      END



