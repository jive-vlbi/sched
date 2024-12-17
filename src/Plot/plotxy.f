      SUBROUTINE PLOTXY( KSET, KSRC, KSTA, XAXIS, YAXIS,
     1                   SCREEN, COLOR, RXMIN, RXMAX, RYMIN, RYMAX )
C
C     Routine for SCHED that plots sky coverage according to requests
C     from user gathered by plotter.f
C
C     Changed meaning of KSRC to schedule source, not catalog source.
C     Removed JSRC.  28 Dec 2001. RCW.
C     PLOTXY is called by PLOTTY, which selects plot type from terminal
C     input.  There KSRC can be non-zero if a source is selected.
C     PLOTXY is also called from PLPLOT.  There KSRC seems to always
C     be zero.  That will be the usual call.
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      INTEGER           ISCN, ISTA, KSTA1, KSTAN, KSET, IER
      INTEGER           J, NPTX, NPTY, ICOLOR, ITICKS
      INTEGER           LINSIZ, LABSIZ, CIND, ISET
      INTEGER           ISRC, KSRC, LSRC, KSTA, CSTA, NPSRC
      LOGICAL           SCREEN, COLOR
      REAL              XPT(4), YPT(4), XSHADE(4), YSHADE(4)
      REAL              XMIN, XMAX, YMIN, YMAX, XM, YM
      CHARACTER         LABELX*60, LABELY*60, XAXIS*(*), YAXIS*(*)
      DOUBLE PRECISION  DAY0, RXMIN, RXMAX, RYMIN, RYMAX
C -------------------------------------------------------------------
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
C     Find which station this is in the station list.
C
      IF( KSTA .EQ. 0 ) THEN
         KSTA1 = 1
         KSTAN = NSTA         
      ELSE
         KSTA1 = KSTA
         KSTAN = KSTA
      END IF
C
C     Set the world coordinates and plot area.
C
      IF( POPBCK .NE. 5 ) CALL PGPAGE
      CALL PGSAVE
      IF( .NOT. SCREEN ) CALL PGIDEN
C
      CALL PGSVP( 0.1, 0.95, 0.27, 0.87 )
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
         CALL PGTBOX( 'GYZXH', 0.0, 0, 'G', 0.0, 0 )
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
         CALL PGTBOX( 'BCNTSYZXH', 0.0, 0, 'BCNTS', 0.0, 0 )
      ELSE
         CALL PGSLW( 1 )
         CALL PGBOX( 'G', 0.0, 0, 'G', 0.0, 0 )
         CALL PGSLW( LABSIZ )
         CALL PGSCI( ITICKS )
         CALL PGBOX( 'BCNTS', 0.0, 0, 'BCNTS', 0.0, 0 )
      END IF
C
C     Label axes
C
      CALL PGSCI( 2 )
      CALL PGSCH( 1.1 )
      CALL PGMTXT( 'B', 2.2, 0.5, 0.5, LABELX)
      CALL PGMTXT( 'L', 2.2, 0.5, 0.5, LABELY)
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
C
C     Loop over scans and stations.
C
      CALL PGBBUF
      J = 0
      DO ISTA = KSTA1, KSTAN
        IF( PSTBCK(ISTA,1) .EQ. 1 ) THEN 
           J = J + 1
C
C        Get color for this station.
C
         ICOLOR = MOD( J - 1, 13 ) + 2 
         IF( ICOLOR .GE. 10 ) ICOLOR = ICOLOR + 1
         CALL PGSCI( ICOLOR )
C
C        Write station name below plot.
C
         CALL PGSLW( LABSIZ )
         YM = 5.0 + MOD( J - 1, 5 ) * 1.3
         XM = 0.05 + ( ( J - 1 ) / 5 ) * 0.15 
         CALL PGMTXT( 'B', YM, XM, 0.0, STANAME(ISTA) )
C
C        Draw horizon in az/el plots.
C
         CALL PGSLW( LINSIZ )
         CSTA = STANUM(ISTA)
         IF( XAXIS .EQ. 'AZ' .AND. YAXIS .EQ. 'EL' ) THEN
            CALL PGLINE( NHORIZ(CSTA), HORAZ(1,CSTA), HOREL(1,CSTA) )
         END IF
C
C        Draw data.  (LSRC prevents problems on Sun's when KSRC=0)
C
         DO ISCN = SCAN1, SCANL
           IF( KSRC .EQ. 0 ) THEN
              LSRC = 0
           ELSE
              LSRC = SRCATN(KSRC)
           END IF
C          Set the setup files if multiple selected
           DO ISET = 1, NSETF
C
            IF(((PSFPOI(ISET) .EQ. 1 .AND. SETNUM(ISCN) .EQ. ISET)
     1         .AND. (SRCNUM(ISCN) .EQ. LSRC .OR. KSRC .EQ. 0))
     3         .AND.  PSOBCK(SRLSTN(SRCNUM(ISCN))) .EQ. 1 ) THEN
               IF( STASCN(ISCN,ISTA) .AND. 
     1             UP1(ISCN,ISTA) .EQ. ' ' .AND.
     2             UP2(ISCN,ISTA) .EQ. ' ' ) THEN

C
C                 Get points to plot.
C
                  CALL PLOTPT( ISCN, ISTA, XAXIS, XPT, NPTX, DAY0 )
                  CALL PLOTPT( ISCN, ISTA, YAXIS, YPT, NPTY, DAY0 )
C
C                 Plot them, including the ambiguity points if present.
C
                  CALL PGLINE( 2, XPT, YPT )
                  IF( NPTX .EQ. 4 ) CALL PGLINE( 2, XPT(3), YPT )
                  IF( NPTY .EQ. 4 ) CALL PGLINE( 2, XPT, YPT(3) )
                  IF( NPTX .EQ. 4 .AND. NPTY .EQ. 4 ) 
     1                CALL PGLINE( 2, XPT(3), YPT(3) )
C
               END IF
            END IF
           END DO
         END DO
       END IF
      END DO
C
C     Give sources.
C
      CALL PGSLW( LABSIZ )
      CALL PGSCI( 2 )
      IF( KSRC .EQ. 0 ) THEN
         NPSRC = 0
         DO ISRC = 1, NSRC
            IF( PSOBCK(ISRC) .EQ. 1 ) THEN
               NPSRC = NPSRC + 1
               XM = 0.85
               YM = 6.0 + MOD( NPSRC - 1, 5 ) * 1.3
               IF( NPSRC .GE. 4 ) THEN
                  CALL PGMTXT( 'B', YM, XM, 0.0, 'More' )
                  GO TO 300
               ELSE
                  CALL PGMTXT( 'B', YM, XM, 0.0, SRCNAME(ISRC) )
               END IF
            END IF
         END DO
      ELSE
         CALL PGMTXT( 'B', 6.0, 0.85, 0.0, SRCNAME(KSRC) )
      END IF
  300 CONTINUE
C
      CALL PGEBUF
C
  990 CONTINUE
      CALL PGUNSA
      RETURN
      END
