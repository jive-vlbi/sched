      SUBROUTINE PLOTSTA( KSET, SCREEN, COLOR, INTERACT )
C
C     Routine for SCHED that plots the station positions
C     and allows the user to select stations for the uv
C     plots interactively.  It also triggers configuration
C     optimization based on user cursor input.
C
C     Split from PLOTUV 28 Dec 2001  RCW.
C     Fixed problem with flagged times after a source moved  3 May 2018 RCW.
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      INTEGER           KSET
      LOGICAL           SCREEN, COLOR
C
      INTEGER           ISTA, IMARK
      INTEGER           IS, ISCN, LASTJSCN, IER
      INTEGER           PGCURS, INTICKS
      LOGICAL           MSCREEN, MCOLOR
      REAL              CURD, CURDM, COL1, COL2, COL3
      REAL              XPLT(MAXSTA), YPLT(MAXSTA), XCH, YCH
      REAL              XCHNEW, YCHNEW
      LOGICAL           FIRST, PLOTCONT, INTERACT
      CHARACTER         CH*1, CHN*1
      SAVE              FIRST, PLOTCONT
      DOUBLE PRECISION  LASTTIME, T_AVAIL
C
C     For the station grid quality contour plot (when S typed)
C     GHLAT and GHLONG are declared here to take advantage of
C     the parameters.
C
      INTEGER    NLAT, NLONG, MDEV, LDEV, PGOPEN, ICOLIND
      PARAMETER  (NLAT=25)
      PARAMETER  (NLONG=25)
      REAL       QUALITY(NLONG,NLAT), QLIMS(4)
      REAL       ZOOM, ZXR, ZYR
      CHARACTER  GHLAT(NLAT)*9, GHLONG(NLONG)*10
      CHARACTER  MAPOUT*40
      SAVE       QUALITY, QLIMS
C                                        DATA needs to be last.
      DATA              FIRST / .TRUE. /
C -------------------------------------------------------------------
C     Set some parameters (now in common so don't use parameter
C     or data statements).
C
      IF( FIRST ) THEN
         DOTSIZE = 2.2
         RINGSIZE = DOTSIZE + 0.8
         DO ISTA = 1, MAXSTA
            MOVEDS(ISTA) = .FALSE.
         END DO
         FIRST = .FALSE.
         PLOTCONT = .FALSE.
C
C        Set the plot limits for the station plots.  Use user input 
C        MAPLIM if given.  Otherwise, use something appropriate for NMA.
C        This is inside the IF( FIRST ) so that it doesn't happen after
C        a zoom operation.
C
         IF( MAPLIM(1) .EQ. MAPLIM(2) .OR. MAPLIM(3) .EQ. MAPLIM(4) ) 
     1        THEN
            LONMIN = 103.0
            LONMAX = 109.0
            LATMIN = 32.0
            LATMAX = 37.0
         ELSE
            LONMIN = MAPLIM(1)
            LONMAX = MAPLIM(2)
            LATMIN = MAPLIM(3)
            LATMAX = MAPLIM(4)
         END IF
         LATMID = ( LATMIN + LATMAX ) / 2.0
      END IF
C
C     Set line widths etc. 
C
      IF( SCREEN ) THEN
         NTICKS = 9
         LINESZ = PLYLW(1)
         LABELSZ = PLYAW(1)
         COLIND    = 16
      ELSE
         NTICKS = 1
         LINESZ = PLYLW(2)
         LABELSZ = PLYAW(2)
         IF( PFLBCK .LE. 2 ) THEN
            COLIND    = 17
         ELSE
            COLIND    = 18
         END IF
      END IF
C
C     Plot array in latitude/longitude and allow interactive
C     setting of stations.
C
C     Only do this one if OBSTYP = CONFIG
C
      IF( CONFIG ) THEN
C
C        Plot the latitude and longitude distribution of antennas.
C        Try to size so that local X and Y scales are similar.
C        Basically, squeeze longitudes by COS(LATMID).  Think of
C        the scales being in length units like km modulo a constant.
C        
         CALL PGPAGE
C
         CALL PLMAP( SCREEN, COLOR )
C
         IF( PLOTCONT ) THEN
            CALL PLQUAL( QUALITY, NLONG, NLAT, QLIMS, SCREEN )
         END IF
C
C        Now allow user to change station selections if in screen
C        mode.
C
         IF( SCREEN .AND. INTERACT ) THEN
            CALL WRTMSG( 1, 'PLOTSTA', 'plstainstructions' )
            XCH = ( LONMAX + LONMIN ) / 2.0
            YCH = ( LATMAX + LATMIN ) / 2.0
  100       CONTINUE
            IF( PGCURS( XCH, YCH, CH ) .EQ. 1 ) THEN
C
C              First deal with a zoom request.
C
               IF( CH .EQ. 'z' .OR. CH .EQ. 'Z' ) THEN
                  IF( CH .EQ. 'z' ) THEN
                     ZOOM = 1.5
                  ELSE 
                     ZOOM = 1.0 / 1.5
                  END IF
                  ZXR = LONMAX - LONMIN
                  ZYR = LATMAX - LATMIN
                  LONMIN = XCH - ZXR * ZOOM / 2.0 
                  LONMAX = XCH + ZXR * ZOOM / 2.0 
                  LATMIN = YCH - ZYR * ZOOM / 2.0
                  LATMAX = YCH + ZYR * ZOOM / 2.0
                  LATMID = ( LATMIN + LATMAX ) / 2.0
                  CALL PGPAGE
                  CALL PLMAP( SCREEN, COLOR )
                  IF( PLOTCONT ) THEN
                     CALL PLQUAL( QUALITY, NLONG, NLAT, QLIMS, SCREEN )
                  END IF
C 
               END IF
C
C              Then deal with a postscript map request.
C
               IF( CH .EQ. 'P' .OR. CH .EQ. 'p' ) THEN
C
C                 Save the old device index and color index.
C
                  CALL PGQID( LDEV )
                  ICOLIND = COLIND
                  INTICKS = NTICKS
C
C                 Define the output file and color.
C
                  IF( CH .EQ. 'P' ) THEN
                     MAPOUT = 'station_map_c.ps/vcps'
                     MCOLOR = .TRUE.
                     COLIND = 18
                     NTICKS = 1
                  ELSE
                     MAPOUT = 'station_map_bw.ps/vps'
                     MCOLOR = .FALSE.
                     COLIND = 17
                     NTICKS = 1
                  END IF                  
C
C                 The color definitions don't carry to the new
C                 image, so get and save the one we need.
C
                  CALL PGQCR( COLIND, COL1, COL2, COL3 )
C
C                 Open the plot.
C
                  MDEV = PGOPEN( MAPOUT )
                  IF( MDEV .LE. 0 ) THEN
                     CALL WLOG( 1, 'PLOTSTA: Can not open map. '//
     1                   'Does it already exist?' )
                  ELSE
C
C                    Make the postscript plot.
C
                     CALL PGSLCT( MDEV )
                     CALL PGSCR( COLIND, COL1, COL2, COL3 )
                     MSCREEN = .FALSE.
                     CALL WLOG( 1, 'PLOTSTA: Writing map file ' //
     1                          MAPOUT )
                     CALL PLMAP( MSCREEN, MCOLOR )
                     IF( PLOTCONT ) THEN
                        CALL PLQUAL( QUALITY, NLONG, NLAT, 
     1                               QLIMS, MSCREEN )
                     END IF
                     CALL PGCLOS
                  END IF
C
C                 Put back some things.
C
                  CALL PGSLCT( LDEV )
                  COLIND = ICOLIND
                  NTICKS = INTICKS
               END IF
C
C              Make all other requests not case sensitive.
C
               CALL UPCASE( CH )
C
               MSGTXT = ' '
               WRITE( MSGTXT, '( 2A )' ) 'PLOTSTA: Got key: ', CH
               CALL WLOG( 1, MSGTXT )
C
C              An X return means proceed with the UV plots.
C              That actually will not be tested but will fall
C              through the key selections.
C
C              An O means optimize.
C 
C              Turn off contour plotting of quality measure if
C              this command is likely to change anything related
C              to the quality.
C
               IF( CH .NE. 'X' .AND. CH .NE. 'G' ) PLOTCONT = .FALSE.
C
C              For modes that need to know the station that was 
C              clicked, find it.
C
               IF( CH .EQ. 'M' .OR. CH .EQ. 'D' .OR. CH .EQ. 'A' ) THEN
C          
C                 Do X + Y rather than SQRT( X^2 + Y^2 ) for speed.
C                 Note that XPLT and YPLT are also obtained, which
C                 are used to erase the old position later.
C          
                  CURDM = 1.E15
                  IMARK = 0
                  DO ISTA = 1, NSTA
                     XPLT(ISTA) = LONG(STANUM(ISTA)) / RADDEG
                     YPLT(ISTA) = LAT(STANUM(ISTA)) / RADDEG
                     CURD = ABS( XPLT(ISTA) - XCH ) + 
     1                      ABS( YPLT(ISTA) - YCH )
                     IF(  CURD .LT. CURDM ) THEN
                        IMARK = ISTA
                        CURDM = CURD
                     END IF
                  END DO
C
C                 If I don't get a good station number, try again)
C
                  IF( IMARK .EQ. 0 ) THEN
                     MSGTXT = ' '
                     MSGTXT = 'PLOTSTA:  '//
     1                        'Marked station not identified. '//
     2                        '  Try again.'
                     CALL WLOG( 1, MSGTXT )
                     GO TO 100
                  END IF
C
C                 Record the station catalog number of the marked station.
C
                  IS = STANUM(IMARK)
C
C                 Deal with a request to move a station.  This happens
C                 if you type "M" or middle click (giving 'D')
C
                  IF( CH .EQ. 'M' .OR. CH .EQ. 'D' ) THEN
C
                     MOVEDS(IMARK) = .TRUE.
C
C                    Don't put this in the log.  Use PUTOUT. 
C
                     CALL PUTOUT( 'PLOTSTA: Click on new position.' )
                     XCHNEW = XCH
                     YCHNEW = YCH
                     IER = PGCURS( XCHNEW, YCHNEW, CHN )
                     IF( IER .EQ. 1 ) THEN
C
C                       Announce the new position.
C
                        WRITE( MSGTXT, '( 2A, 2F12.7, F8.1A, 2F12.7 )' )
     1                      STATION(IS), ' New lat. long. elev: ',
     2                      YCHNEW, XCHNEW, ELEV(IS), '    Old: ', 
     3                      LAT(IS) / RADDEG, LONG(IS) / RADDEG
                        CALL WLOG( 1, MSGTXT )
C
C                       Set the new coordinates.  This must come after 
C                       above lines.
C
                        LONG(IS) = XCHNEW * RADDEG
                        LAT(IS) = YCHNEW * RADDEG
C
C                       Erase the old position and plot the new one.
C
                        CALL PGSCH( RINGSIZE ) 
                        CALL PGSCI( COLIND )
                        CALL PGPT1( XPLT(IMARK), YPLT(IMARK), -9 )
C
C                       Now plot the new position, complete with ring.
C
                        CALL PLMARK( IMARK, MOVEDS, RINGSIZE, DOTSIZE )
C
C                       Make the changes needed to use the new
C                       position.  First get the XYZ coordinates.
C
                        CALL GEOXYZ( 0, LONG(IS), LAT(IS), ELEV(IS),
     1                        XPOS(IS), YPOS(IS), ZPOS(IS), IER )
                        IF( IER .NE. 0 ) CALL WLOG( 1,
     1                      'PLOTSTA: Problem with coordinate '//
     1                      'conversions for '// STANAME(IMARK) )
C
C                       Now update the scan geometry parameters.
C                       Use STAGEO, not SCHSRC which will be absorbed
C                       into STAGEO.
C                        
C                       Do not select only scans with STASCN set. 
C                       That gets into trouble when the new position
C                       allows observations when the previous position
C                       did not.  Simple example - moving station to
C                       much higher latitude for a high dec source.
C
                        LASTJSCN = 0
                        DO ISCN = SCAN1, SCANL
C Don't do this                            IF( STASCN(ISCN,IMARK) ) THEN
                              CALL STAGEO( ISCN, IMARK, STARTJ(ISCN),
     1                              LASTJSCN, LASTTIME, T_AVAIL )
                              LASTJSCN = ISCN
                              STASCN(ISCN,IMARK) = 
     1                           UP1(ISCN,IMARK) .EQ. ' ' .AND.
     2                           UP2(ISCN,IMARK) .EQ. ' ' .AND.
     3                           ( EL1(ISCN,IMARK) + EL2(ISCN,IMARK) ) /
     4                              2.0 .GT. OPMINEL(ISCN)               
C End of inappropriate selection          END IF
                        END DO
C
C                       Get the cursor to stay at the new position.
C 
                        XCH = XCHNEW
                        YCH = YCHNEW
C
                     END IF
                     GO TO 100
C
C
C                 Deal with toggling a station off and on.  This happens
C                 if you left click (giving an 'A')
C
                  ELSE IF( CH .EQ. 'A' ) THEN
C          
C                    Toggle flags.
C          
                     CALL PGSCH( DOTSIZE ) 
                     IF( PSTBCK(IMARK,1) .EQ. 0 .AND.
     1                   PSTBCK(IMARK,2) .EQ. 1 ) THEN
                        PSTBCK(IMARK,1) = 1
                        PSTBCK(IMARK,2) = 0
                        PSTNUM = PSTNUM + 1
                     ELSE IF( PSTBCK(IMARK,1) .EQ. 1 .AND. 
     1                        PSTBCK(IMARK,2) .EQ. 0 ) THEN
                        PSTBCK(IMARK,1) = 0
                        PSTBCK(IMARK,2) = 0
                        PSTNUM = PSTNUM - 1
                     ELSE IF( PSTBCK(IMARK,1) .EQ. 0 .AND.
     1                        PSTBCK(IMARK,2) .EQ. 0 ) THEN
                        PSTBCK(IMARK,1) = 1
                        PSTBCK(IMARK,2) = 1
                        PSTNUM = PSTNUM + 1
                     ELSE IF( PSTBCK(IMARK,1) .EQ. 1 .AND. 
     1                        PSTBCK(IMARK,2) .EQ. 1 ) THEN
                        PSTBCK(IMARK,1) = 0
                        PSTBCK(IMARK,2) = 1
                        PSTNUM = PSTNUM - 1
                     ELSE
                        CALL ERRLOG( 'PLOTSTA: Toggle error' )
                     END IF
C
C                    Replot the point.
C
                     CALL PLMARK( IMARK, MOVEDS, RINGSIZE, DOTSIZE )
                  END IF
                  GO TO 100
C
C              Now deal with other requests based on click values.
C
               ELSE IF( CH .EQ. 'O' ) THEN
C
C                 Optimize.
C
                  CALL WLOG( 1, 'PLOTSTA: Optimizing configuration.' )
                  GRIDUSED = .TRUE.
C
                  CALL UVOPT
C
C                 Replot the points to reflect any changes by UVOPT
C
                  DO ISTA = 1, NSTA
                     IF( PSTBCK(ISTA,1) .NE. 1 ) THEN
                        CALL PLMARK( ISTA, MOVEDS, RINGSIZE, DOTSIZE )
                     END IF
                  END DO
                  DO ISTA = 1, NSTA
                     IF( PSTBCK(ISTA,1) .EQ. 1 ) THEN
                        CALL PLMARK( ISTA, MOVEDS, RINGSIZE, DOTSIZE )
                     END IF
                  END DO
C
                  GO TO 100
C
               ELSE IF( CH .EQ. 'S' ) THEN
C
C                 Make a grid of points around a station.
C
                  CALL WLOG( 1, 'PLOTSTA: Get station grid.' )
C
                  CALL SUVOPT( QUALITY, NLONG, NLAT, QLIMS, 
     1                         GHLAT, GHLONG )
C
                  CALL PLQUAL( QUALITY, NLONG, NLAT, QLIMS, SCREEN )
                  GRIDUSED = .TRUE.
                  PLOTCONT = .TRUE.
C
                  GO TO 100
C

               ELSE IF( CH .EQ. 'G' ) THEN
C
C                 Toggle grid plot
C
                  GRIDUSED = .NOT. GRIDUSED
                  GO TO 100
C
               ELSE IF( CH .EQ. 'K' ) THEN
C
C                 Make special SKA plots.
C
                  CALL PLSKA
                  GO TO 100
C
               ELSE IF( CH .EQ. 'Z' .OR. CH .EQ. 'P' ) THEN
C
C                 For zoom, just go back for more input.
C
                  GO TO 100
C
               ELSE IF( CH .EQ. 'X' ) THEN
C
C                 Do nothing here.  Will end interaction with map.
C                 and, in the right call, cause the uv coverage
C                 to be plotted.  Was a right click.
C
               ELSE
C
C                 Detect an unrecognized command.
C
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, A )' ) 
     1                 'PLOTSTA: Unrecognized command: ', CH
                  CALL WLOG( 1, MSGTXT ) 
                  GO TO 100
               END IF
            END IF
         END IF
      END IF
C
      CALL PGEBUF
C
      RETURN
      END
