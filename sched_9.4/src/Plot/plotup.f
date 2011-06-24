      SUBROUTINE PLOTUP( KSET, XAXIS, YAXIS, SCREEN, COLOR,
     1                   RXMIN, RXMAX, RYMIN, RYMAX )
C
C     Routine for SCHED that plots sky coverage according to requests
C     from user gathered by plotter.f
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      INTEGER           ISCN, ISTA, KSET, IER
      INTEGER           NPTX, NPTY, ICOLOR, ITICKS
      INTEGER           ISRC, NPSRC, NSUN
      INTEGER           I, NPSTA, LINSIZ, LABSIZ, CIND, ISET
      LOGICAL           SCREEN, COLOR
      REAL              XPT(4), YPT(4), XSHADE(4), YSHADE(4)
      REAL              XMIN, XMAX, YMIN, YMAX, XM, YM, YOF
      REAL              HA, EL, AZ, PA
      CHARACTER         LABELX*60, LABELY*60, XAXIS*(*), YAXIS*(*)
      DOUBLE PRECISION  TMID, RAS, DECS, LSTTIM
      DOUBLE PRECISION  DAY0, RXMIN, RXMAX, RYMIN, RYMAX
C -------------------------------------------------------------------
C
C     Get and set sun coordinates and parameters if requested
C
      IF( PXYSUN ) THEN
         TMID = TFIRST + ( TEND - TFIRST ) / 2.D0
         CALL SUNPOS( TMID, RAS, DECS )
         IF( RAS .LT. 0.D0 ) RAS = TWOPI + RAS
         NSUN = 1
         RYMAX = RYMAX + 1.0
      ELSE
         NSUN = 0
      END IF
C
C     Set plot scale and axis labels depending on request.  Check the
C     request.
C
      DAY0 = DINT( TFIRST )
      CALL PLOTDEF( IER, 'X', XAXIS, RXMIN, RXMAX, DAY0, LABELX, 
     1              XMIN, XMAX )
      IF( IER .NE. 0 ) GO TO 990
      CALL PLOTDEF( IER, 'Y', YAXIS, RYMIN, RYMAX, DAY0, LABELY, 
     1              YMIN, YMAX )
      IF( IER .NE. 0 ) GO TO 990
C
C     Station Offset from source and sun on Y axis
C
      YOF =  1.0 / ( 1.5 - ( ( PSONUM + NSUN ) * 0.05 ) ) /
     1             ( PSTNUM + 1 ) 
C
C     Set the world coordinates and plot area.
C
      IF( POPBCK .NE. 5 ) CALL PGPAGE
      CALL PGSAVE
      IF( .NOT. SCREEN ) CALL PGIDEN
C
      CALL PGSVP( 0.15, 0.95, 0.22, 0.87 )
      IF( XMIN .EQ. XMAX) XMAX = XMIN + 0.1
      IF( YMIN .EQ. YMAX) YMAX = YMIN + 0.1
      CALL PGSWIN( XMIN, XMAX, YMIN, YMAX )
C
C     Color the background.
C
      IF( COLOR ) THEN
         IF( SCREEN ) THEN
            ITICKS = 9
            LINSIZ = PLYLW(1)
            LABSIZ = PLYAW(1)
            CIND    = 16
         ELSE
            ITICKS = 1
            LINSIZ = PLYLW(2)
            LABSIZ = PLYAW(2)
            IF( PFLBCK .LE. 2 ) THEN
               CIND    = 17
            ELSE
               CIND    = 18
            END IF
         END IF
         XSHADE(1) = XMIN !  + ( XMAX - XMIN ) * 0.002
         XSHADE(2) = XSHADE(1)
         XSHADE(3) = XMAX ! - ( XMAX - XMIN ) * 0.002
         XSHADE(4) = XSHADE(3)
         YSHADE(1) = YMIN ! + ( YMAX - YMIN ) * 0.004
         YSHADE(2) = YMAX ! - ( YMAX - YMIN ) * 0.004
         YSHADE(3) = YSHADE(2)
         YSHADE(4) = YSHADE(1)
         CALL PGSCI( CIND )
         CALL PGPOLY( 4, XSHADE, YSHADE )
      END IF
C
C     Select normal or time axes.
C
      IF( SCREEN ) THEN
         CALL PGSCI( 0 )
      ELSE
         CALL PGSCI( 1 )
      END IF
C
      CALL PGSCH( 0.85 )
      IF( XAXIS .EQ. 'UT' .OR. XAXIS .EQ. 'GST' .OR.
     1    XAXIS .EQ. 'LST' ) THEN
         CALL PGSLW( 1 )
         CALL PGTBOX( 'GYZXH', 0.0, 0, 'G', 1.0, ( PSONUM + NSUN ) )
         CALL PGSLW( LABSIZ )
         CALL PGSCI( ITICKS )
C
C        Check if a Time Offset was set and plot it on top axis 
C        before the plot of regular axis
C
         CALL PLOTOFS( DAY0, XAXIS, XMIN, XMAX, YMIN, YMAX )         
C
C        Plot regular axis
C
         CALL PGTBOX( 'BCNTSYZXH', 0.0, 0, 'BCTS', 0.0, 0 )
      ELSE
         CALL PGSLW( 1 )
         CALL PGBOX( 'G', 0.0, 0, 'G', 1.0, ( PSONUM + NSUN ) )
         CALL PGSLW( LABSIZ )
         CALL PGSCI( ITICKS )
         CALL PGBOX( 'BCNTS', 0.0, 0, 'BCTS', 0.0, 0 )
      END IF
C
C     Label axes
C
      CALL PGSCI( 2 )
      CALL PGSCH( 1.1 )
      CALL PGMTXT( 'B', 2.2, 0.5, 0.5, LABELX)
      CALL PGSCI( 10 )
      CALL PGSCH( 1.3 )
C
C     Give Experiment code and setup file if one was specified.
C
      IF( KSET .LE. 0 ) THEN
         CALL PGMTXT( 'T', 3.0, 0.5, 0.5,
     1                'Experiment code: '//EXPCODE )
      ELSE
         CALL PGMTXT( 'T', 3.0, 0.0, 0.0,
     1                'Experiment code: '//EXPCODE )
         CALL PGSCH( 0.8 )
         CALL PGSCI( 11 )
         CALL PGMTXT( 'T', 5.0, 1.0, 1.0,
     1                ' Setup: '//SETFILE(KSET) )
      END IF
C
C     Draw the vertical line mark of the init experiment
C     in green color
C
      IF( PXINI .NE. UNSET .AND. PXINI .NE. XMIN ) THEN
         CALL PGSCI( 3 )
         CALL PGSLW( LINSIZ )
         CALL PGMOVE( PXINI, YMIN )
         CALL PGDRAW( PXINI, YMAX )
      END IF
C
C     Draw the vertical line mark of the end experiment
C     in red color
C
      IF( PXEND .NE. UNSET .AND.  PXEND .NE. XMAX ) THEN
         CALL PGSCI( 2 )
         CALL PGSLW( LINSIZ )
         CALL PGMOVE( PXEND, YMIN )
         CALL PGDRAW( PXEND, YMAX )
      END IF
C
      CALL PGSCH( 1.0 )
      CALL PGBBUF
C
C     Draw Stations Name
C
      IF( PSTNUM .GT. 0 .AND.
     1    ( NSUN .EQ. 1 .OR. PSONUM .GT. 0 ) ) THEN
C
         NPSTA = 0
         DO ISTA = 1, NSTA
           IF( PSTBCK(ISTA,1) .EQ. 1 ) THEN 
            NPSTA = NPSTA + 1
C
C           Get color for this station.
C
            ICOLOR = MOD( NPSTA - 1, 13 ) + 2 
            IF( ICOLOR .GE. 10 ) ICOLOR = ICOLOR + 1
            CALL PGSCI( ICOLOR )
C
C           Write station name at bottom of the plot.
C
            YM = 4.2 + MOD( NPSTA - 1, 4 ) * 1.3
            XM = 0.05 + ( ( NPSTA - 1 ) / 4 ) * 0.15
            CALL PGMTXT( 'B', YM, XM, 0.0, STANAME(ISTA) )
           END IF
         END DO
      END IF
C
C     Draw Sun if requested
C
      IF( PXYSUN ) THEN
         NPSRC = 1
C
C        Write Sun label at left of the plot.
C
         CALL PGSLW( LABSIZ )
         CALL PGSCI( 2 )
         CALL PGMTXT( 'LV', 0.7, 0.03, 1.0, 'SUN' )
C
C        Loop over station.
C
         NPSTA = 0
         DO ISTA = 1, NSTA
           IF( PSTBCK(ISTA,1) .EQ. 1 ) THEN 
            NPSTA = NPSTA + 1
C
C           Get color for this station.
C
            ICOLOR = MOD( NPSTA - 1, 13 ) + 2 
            IF( ICOLOR .GE. 10 ) ICOLOR = ICOLOR + 1
            CALL PGSCI( ICOLOR )
            CALL PGSLW( LINSIZ )
C
C           Draw data.
C
            DO ISCN = SCAN1, SCANL
             DO ISET = 1, NSETF
              IF(PSFPOI(ISET) .EQ. 1 .AND. 
     1           SETNUM(ISCN) .EQ. ISET) THEN
C
C              Get Sun Elevation
C
               TMID = STARTJ(ISCN) + 
     1                      ( STOPJ(ISCN) - STARTJ(ISCN) ) / 2.D0
               CALL PLSGEO( ISTA, RAS, DECS, TMID,
     1                      HA, EL, AZ, LSTTIM, PA )
C
C              Draw Sun points if the EL is greater or equal than the
C              Sun Elevation selected 
C
               IF( EL .GE. PSUNEL ) THEN
C
C                 Get points to plot.
C
                  CALL PLOTPT( ISCN, ISTA, XAXIS, XPT, NPTX, DAY0 )
                  CALL PLOTPT( ISCN, NPSRC, YAXIS, YPT, NPTY, DAY0 )
C
C                 Add Y source offset
C
                  DO I = 1, NPTY
                     YPT(I) = YPT(I) + ( YOF * NPSTA )
                  END DO
C
C                 Plot them, including the ambiguity points if present.
C
                  CALL PGLINE( 2, XPT, YPT )
                  IF( NPTX .EQ. 4 ) CALL PGLINE( 2, XPT(3), YPT )
                  IF( NPTY .EQ. 4 ) CALL PGLINE( 2, XPT, YPT(3) )
                  IF( NPTX .EQ. 4 .AND. NPTY .EQ. 4 ) 
     1                CALL PGLINE( 2, XPT(3), YPT(3) )
               END IF
              END IF
             END DO
            END DO
C
           END IF
         END DO
      ELSE
         NPSRC = 0
      END IF
C
C     Loop over Source.
C
      DO ISRC = 1, NSRC
        IF( PSOBCK(ISRC) .EQ. 1 ) THEN
         NPSRC = NPSRC + 1
C
C        Write source name at left of the plot.
C
         YM = ( ( 1.0 / ( PSONUM + NSUN ) ) * ( NPSRC - 1 ) ) + 0.03
         CALL PGSLW( LABSIZ )
         CALL PGSCI( 10 )
         CALL PGMTXT( 'LV', 0.7, YM, 1.0, SRCNAME(ISRC) )
C
C     Loop over station.
C
      NPSTA = 0
      DO ISTA = 1, NSTA
        IF( PSTBCK(ISTA,1) .EQ. 1 ) THEN 
         NPSTA = NPSTA + 1
C
C        Get color for this station.
C
         ICOLOR = MOD( NPSTA - 1, 13 ) + 2 
         IF( ICOLOR .GE. 10 ) ICOLOR = ICOLOR + 1
         CALL PGSCI( ICOLOR )
         CALL PGSLW( LINSIZ )
C
C        Draw data.
C
         DO ISCN = SCAN1, SCANL
          DO ISET = 1, NSETF

            IF((PSFPOI(ISET) .EQ. 1 .AND. SETNUM(ISCN) .EQ. ISET)
     1          .AND. SRLSTN(SRCNUM(ISCN)) .EQ. ISRC ) THEN
               IF( STASCN(ISCN,ISTA) .AND. 
     1             UP1(ISCN,ISTA) .EQ. ' ' .AND.
     2             UP2(ISCN,ISTA) .EQ. ' ' ) THEN

C
C                 Get points to plot.
C
                  CALL PLOTPT( ISCN, ISTA, XAXIS, XPT, NPTX, DAY0 )
                  CALL PLOTPT( ISCN, NPSRC, YAXIS, YPT, NPTY, DAY0 )
C
C                 Add Y source offset
C
                  DO I = 1, NPTY
                     YPT(I) = YPT(I) + ( YOF * NPSTA )
                  END DO
C
C                 Plot them, including the ambiguity points if present.
C
                  CALL PGLINE( 2, XPT, YPT )
                  IF( NPTX .EQ. 4 ) CALL PGLINE( 2, XPT(3), YPT )
                  IF( NPTY .EQ. 4 ) CALL PGLINE( 2, XPT, YPT(3) )
                  IF( NPTX .EQ. 4 .AND. NPTY .EQ. 4 ) 
     1                CALL PGLINE( 2, XPT(3), YPT(3) )
C
C              END Scan Loop
C  
               END IF
            END IF
           END DO
         END DO
C
C     END Stations Loop
C  
        END IF
      END DO
C
C     END Sources Loop
C  
        END IF
      END DO
C
      CALL PGEBUF
C
  990 CONTINUE
      CALL PGUNSA
      RETURN
      END
