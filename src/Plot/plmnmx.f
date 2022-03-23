      SUBROUTINE PLMNMX( XYAXIS, XYMIN, XYMAX, MODE )
C
C     Routine for SCHED that plots sky coverage according to requests
C     from user gathered by plotter.f.  This routine is only called 
C     for axis type Sec (Sec(z)).  Called by PLAXST and PLOTDEF.
C
C     Significant debugging changes made March 2010.  Also added 
C     comments to help me understand.  Note use of LSRC, KSRC, and
C     KSET was removed.  RCW.
C
C     XYAXIS is the axis type.  XYMIN and XYMAX are the min and max.
C     MODE = 1 means PXYSEC true which means autoscale.
C     MODE = 0 means use provided scale. 
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      INTEGER           MODE, ISCN, ISTA
C      INTEGER           LSRC, KSRC, KSET, I
      REAL              PT1, PT2, XYMIN, XYMAX
      CHARACTER         XYAXIS*(*), TAXIS*5
C -------------------------------------------------------------------
      IF( XYAXIS .NE. 'Sec' ) THEN
C
C        Don't try to use this routine for other than SECZ operations.
C
C        Some compilers don't like concatenation with variable of 
C        indeterminent length in a call argument.
C
         TAXIS = XYAXIS
         CALL WLOG(1, 'PLMNMX:  Programming error - this routine '//
     1       'should only be called for XYAXIS=Sec' )
         CALL WLOG(1, 'PLMNMX:  It was called with XYAXIS = '//
     1       TAXIS )
         CALL WLOG(1, 'PLMNMX:  Returning without doing anything.' )
         return
      END IF
C
C     Find min/max if MODE 1 selected.  Note that, by MIN and MAX here, 
C     the values for the bottom and top of the plot are meant.  But
C     the scale is inverted, with the highest number on the bottom 
C     or the "MIN".  Starting with the default settings doesn't work
C     because they will likely be beyond the limit in the data and 
C     won't get changed.  For the self scale, use the default min as
C     the max and visa versa to start the process.
C
      IF( MODE .EQ. 1 ) THEN
         XYMIN = PXSLIM(8,1)
         XYMAX = PXSLIM(8,2)
C
C        Loop over stations.
C
         DO ISTA = 1, NSTA
            IF( PSTBCK(ISTA,1) .EQ. 1 ) THEN 
C
C              Loop over scans and get the minimum and maximum.
C              SRCNUM(ISCN) is the source number in the main catalog.
C              SRLSTN(SRCNUM(ISCN) is the schedule source number
C              (see sched.inc) for the scan's source.
C              PSOBCK is tells if a schedule source is selected.
C
C              The original had LSRC and KSRC, but they don't seem
C              to be set to anything but zero.  In other routines, 
C              KSRC is a call argument, and is transfered to LSRC.  
C              Drop all that here.
C
               DO ISCN = SCAN1, SCANL
C
C                  Look at data if the setup and source are selected,
C                  or set to take all.  PSFBCK = 1 means all setups 
C                  selected.  PSFPOI(I) means setup I is selected. 
C                  PSOBCK means source in schedule catalog is selected.
C
                  IF( ( PSFPOI(SETNUM(ISCN)) .EQ. 1 .OR. 
     1                      PSFBCK .EQ. 1 ) .AND.
     2                 PSOBCK(SRLSTN(SRCNUM(ISCN))) .EQ. 1 ) THEN
C
                     IF( STASCN(ISCN,ISTA) .AND. 
     1                   UP1(ISCN,ISTA) .EQ. ' ' .AND.
     2                   UP2(ISCN,ISTA) .EQ. ' ' ) THEN

C                       Get min and max value of the points to plot.
C
                        PT1 = 1.0/SIN( EL1(ISCN,ISTA) * RADDEG )
                        PT2 = 1.0/SIN( EL2(ISCN,ISTA) * RADDEG )
C
C                       RCW - inverted the .LT. and .GT. below since
C                       the MIN and MAX are really bottom and top and
C                       the axis is inverted.
C
                        IF( PT1 .LT. XYMAX ) XYMAX = PT1
                        IF( PT1 .GT. XYMIN ) XYMIN = PT1
                        IF( PT2 .LT. XYMAX ) XYMAX = PT2
                        IF( PT2 .GT. XYMIN ) XYMIN = PT2
C
                     END IF
                  END IF
               END DO
            END IF
         END DO
C
C        Add offset for clear plot.  Only do when self scaling as
C        this tends to change the fixed scale with each command to 
C        plot.  Recall again, that counter intuitively, XYMIN is the
C        large number and XYMAX is the small one.
C
         IF( XYMIN .LE. 2.0 ) THEN
            XYMIN = XYMIN + 0.1
            XYMAX = XYMAX - 0.1
         ELSE IF( XYMIN .LE. 6.0 ) THEN
            XYMIN = XYMIN + 0.2
            XYMAX = XYMAX - 0.2
         ELSE IF( XYMIN .LE. 15.0 ) THEN
            XYMIN = XYMIN + 0.5
            XYMAX = XYMAX - 0.5
         ELSE
            XYMIN = XYMIN + 1.0
            XYMAX = XYMAX - 1.0
         END IF
C
      END IF
C
      IF( MODE .EQ. 0 .AND. XYMIN .LT. 1.0 ) XYMIN = 1.0
C
      RETURN
      END
