      SUBROUTINE PLMAP( SCREEN, COLOR )
C
C     Plot a map showing station positions and, if the vector
C     files are available, continental, national, and state
C     boundaries, and roads.
C
C     This routine was split from PLOTSTA so that it could be
C     used elsewhere.
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      INTEGER           ISTA
      INTEGER           MAPLWID
      LOGICAL           SCREEN, COLOR
      REAL              X1, X2, Y1, Y2, XWID, YWID, NSUB
      REAL              LONRANGE, MIDLAT, PGRND, KMSCALE
      REAL              KMX1, KMX2, KMY, KMC1, KMC2, KMWID
      REAL              XSCALE, YSCALE, SCALE, CLAT
      REAL              XL, XR, YB, YT, XSHADE(4), YSHADE(4)
      REAL              COL1, COL2, COL3 
      CHARACTER         STATES*80, WORLD*80, ROADS*80
      CHARACTER         KMC*20
C ----------------------------------------------------------------------
C
      IF( .NOT. SCREEN .AND. .NOT. PUBPLOT ) CALL PGIDEN
C     
C     Get the viewsurface in inches.
C     
      CALL PGQVSZ( 1, X1, X2, Y1, Y2 )
C     
      CLAT = COS( LATMID * RADDEG )
      XWID = ( X2 - X1 ) * 0.80
      YWID = ( Y2 - Y1 ) * 0.80
      XSCALE = ( LONMAX - LONMIN ) * CLAT / XWID
      YSCALE = ( LATMAX - LATMIN ) / YWID
      SCALE = MAX( XSCALE, YSCALE )
      YB = 0.11
      XL = 0.11
      XR = XL + ( LONMAX - LONMIN ) * CLAT / ( SCALE * ( X2 - X1 ) )
      YT = YB + ( LATMAX - LATMIN ) / ( SCALE * ( Y2 - Y1 ) ) 
      CALL PGSVP( XL, XR, YB, YT )
C     
C     Set the world coordinates.
C     
      IF( LATMIN .EQ. LATMAX) LATMAX = LATMIN + 0.1
      IF( LONMIN .EQ. LONMAX) LONMAX = LONMIN + 0.1
      CALL PGSWIN( LONMAX, LONMIN, LATMIN, LATMAX )
C     
C     Color the background.
C     
      IF( COLOR ) THEN
         XSHADE(1) = LONMIN 
         XSHADE(2) = XSHADE(1)
         XSHADE(3) = LONMAX 
         XSHADE(4) = XSHADE(3)
         YSHADE(1) = LATMIN 
         YSHADE(2) = LATMAX 
         YSHADE(3) = YSHADE(2)
         YSHADE(4) = YSHADE(1)
         CALL PGSCI( COLIND )
         CALL PGPOLY( 4, XSHADE, YSHADE )
      END IF
      
C     
C     Plot political stuff based on GIS files found by
C     Brian Butler.
C     
C     As of 3 Jan 02, the program is
C     /home/planetas/vla/evla/DEM/plote003
C     
C     Draw the vectors.  The second variable indicates whether
C     the file needs to be converted from Albers projection in
C     meters to lat/long.  The third variable gives the number
C     of lines to skip before reading the vectors.
C     I have copied the files to the same place as the ephemeris
C     data because that will be available for local SCHED use
C     and doesn't go into releases.
C     
C     Color 20 is white.  Usually white is obtained from 
C     color 1 but that color gets converted to black when the
C     background is converted from black to white for prints.
C     
C     On Jan 28, Brian gave me a new roads file "tra0003-2.dat"
C     that combines the vectors with road numbers in a more 
C     convenient manner.  It also has colors.  In the form that
C     he provided, they are all 1.  But I have edited the file
C     to something else.
C     
C     The following is needed for the New Mexico roads.  Try to
C     avoid blending with the background color.
C     
C *******  for the moment, make the NM roads go away when on a
C          white background.
C
      CALL PGQCR( COLIND, COL1, COL2, COL3 )
      IF( COL1 .GT. 0.75 ) THEN
         CALL PGSCR( 20, 1.0, 1.0, 1.0 )
      ELSE 
         CALL PGSCR( 20, 1.0, 1.0, 1.0 )
      END IF
C     
C     Draw the grid now so that other elements are on top.
C     
      IF( .NOT. PUBPLOT ) THEN
         IF( SCREEN ) THEN
            CALL PGSCI( 0 ) 
         ELSE
            CALL PGSCI( 1 ) 
         END IF
         CALL PGSCH( 1.2 )
         CALL PGSLW( 1 )
         CALL PGBOX( 'G', 0.0, 0, 'G', 0.0, 0 )
      END IF
C     
C     Use a line width that is not quite as high as is being used
C     for borders etc, but close.
C     
C
C     Plot the maps in inverse order of importance to get the 
C     right elements on top of each other.
C     
C     Use the new roads file, but leave the hooks for the old
C     one.  
C
C     The line width MAPLWID will be used for the interstates.
C     Other roads will use a scaled smaller value.
C     
      IF( LONMIN .GE. 90.0 .AND. LONMAX .LE. 120.0 .AND.
     1    LATMIN .GE. 25.0 .AND. LATMAX .LE. 45.0 ) THEN
C         ROADS = '$PLANET_DATA/roads2.dat'
         ROADS = '$PLANET_DATA/tra0003-2.dat'
         CALL ENVIR( ROADS )
         MAPLWID = LINESZ * 1.2
C         CALL PLOTMAP( ROADS, 1, 0, 20, IMAP,
C                       LONMAX, LONMIN, LATMIN, LATMAX, MAPLWID )
         CALL PLOTMAP( ROADS, 3, 0, 20, IMAP,
     1                 LONMAX, LONMIN, LATMIN, LATMAX, MAPLWID )
      END IF
C     
C     Plot world if map goes outside central US.
C     
      IF( .NOT. ( LONMIN .GT. 80.0 .AND. LONMAX .LT. 120.0 .AND.
     1    LATMIN .GT. 33.0 .AND. LATMAX .LT. 48.0 ) ) THEN
         WORLD = '$PLANET_DATA/wdbtemp4.e00'
         CALL ENVIR( WORLD )
         MAPLWID = MAX( 1.0, 0.8 * LINESZ )
         CALL PLOTMAP( WORLD, 1, 2, 20, IMAP,
     1                 LONMAX, LONMIN, LATMIN, LATMAX, MAPLWID )
      END IF
C     
C     Plot states if map is not confined to New Mexico.
C     Will be done for up to full world plots.  Put last to let
C     state lines dominate.
C     
      IF( .NOT. ( LONMIN .GE. 103.0 .AND. LONMAX .LE. 109.0 .AND.
     1    LATMIN .GE. 32.0 .AND. LATMAX .LE. 37.0 ) ) THEN
         STATES = '$PLANET_DATA/states.e00'
         CALL ENVIR( STATES )
         MAPLWID = LINESZ * 1.5
         CALL PLOTMAP( STATES, 2, 2, 2, IMAP,
     1                 LONMAX, LONMIN, LATMIN, LATMAX, MAPLWID )
      END IF
C     
C     Reraw the borders etc. to get the main ticks, but not
C     the grid, on top.
C     
      IF( SCREEN ) THEN
         CALL PGSCI( 0 ) 
      ELSE
         CALL PGSCI( 1 ) 
      END IF
      CALL PGSCH( 1.2 )
C      CALL PGSLW( 1 )
C      CALL PGBOX( 'AGTS', 0.0, 0, 'AGTS', 0.0, 0 )
      CALL PGSCI( NTICKS ) 
      CALL PGSLW( LABELSZ )
      CALL PGBOX( 'BCNTS', 0.0, 0, 'BCNTS', 0.0, 0 )
C     
C     Label the axes.
C     
      CALL PGSCI( 2 )
      CALL PGSCH( 1.2 )
      IF( PUBPLOT ) THEN
         CALL PGLAB( 'Longitude (deg)', 'Latitude (deg)', ' ' )
      ELSE 
         CALL PGLAB( 'Longitude (deg)', 'Latitude (deg)', 
     1         'Station locations for '// EXPCODE )
      END IF
C     
C     Loop over stations and plot them.  Do the unselected
C     ones first so that, if there is symbol overlap, the
C     selected ones end up on top.  For stations that
C     have been moved, draw a large green dot that 
C     turns into a ring around the symbol.
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
C     Plot a scale of km.  This is a bit approximate.
C
      LONRANGE = ABS( LONMAX - LONMIN )
      MIDLAT = ( LATMAX + LATMIN ) / 2.0
      KMWID = ABS( LONRANGE ) * 6378. * RADDEG * COS( MIDLAT * RADDEG )
      KMSCALE = PGRND( KMWID * 0.1, NSUB )
      KMY = LATMIN + ( LATMAX - LATMIN ) * 0.1
      KMX1 = LONMAX - 0.8 * LONRANGE 
      KMX2 = KMX1 - LONRANGE * KMSCALE / KMWID
      CALL PGSLW( 10 )
      CALL PGSCI( 1 )
      CALL PGMOVE( KMX1, KMY )
      CALL PGDRAW( KMX2, KMY )
      CALL PGSLW( LABELSZ )
      IF( KMSCALE .LT. 1.0 ) THEN
         WRITE( KMC, '( F4.0, A )' ) KMSCALE * 1000., ' m'
      ELSE IF( KMSCALE .LT. 100.0 ) THEN
         WRITE( KMC, '( F3.0, A )' ) KMSCALE, ' km'
      ELSE IF( KMSCALE .LT. 10000.0 ) THEN
         WRITE( KMC, '( F5.0, A )' ) KMSCALE, ' km'
      ELSE 
         WRITE( KMC, '( F7.0, A )' ) KMSCALE, ' km'
      END IF
      KMC1 = ( KMX1 + KMX2 ) / 2.0
      KMC2 = KMY - ( LATMAX - LATMIN ) * 0.05
      CALL PGSCH( 1.2 )
      CALL PGPTXT( KMC1, KMC2, 0.0, 0.5, KMC )
C
      RETURN
      END
