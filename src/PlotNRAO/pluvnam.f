      SUBROUTINE PLUVNAM( KSET, SCREEN )
C
C     Subroutine to write the panel with the station names etc
C     when doing UV plots of more than one station.
C
C     Pulled out of PLOTSTA  3 apr 2002  RCW.
C
C     ========================================================
C     Second Panel - Station names and coordinates.  Put after
C     above so that interactive changes are reflected.
C
C
C     Parameters for position and size of station labels.
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      INTEGER           KSET
      LOGICAL           SCREEN
C
      INTEGER           ISTA, NPSTA
      INTEGER           LBCL, ICOL, NLBCOL
      REAL              X1, Y1
      REAL              LBYS, LBXS, LBYR, LBXR, LBCH
      CHARACTER         CHLAT*9, CHLONG*10, TFORM*15, POSLINE*40
C ---------------------------------------------------------------------
      IF( PSTNUM .LE. 20 ) THEN
C
C        1 Column 20 Labels Max
C
         LBCL = 1
         LBCH = 1.2
         LBYS = 0.9
         LBYR = 0.8 / 20
         LBXS = 0.15
         LBXR = 0.45
      ELSE IF( PSTNUM .LE. 30 ) THEN
C
C        1 Column 30 Labels Max
C
         LBCL = 1
         LBCH = 1.0
         LBYS = 0.9
         LBYR = 0.8 / 30
         LBXS = 0.2
         LBXR = 0.45
C
      ELSE IF( PSTNUM .LE. 94 ) THEN
C
C        2 Column 94 Labels Max
C
         LBCL = 2
         LBCH = 0.80
         LBYS = 1.0
         LBYR = 0.8 / 35
         LBXS = 0.1
         LBXR = 0.45
C
      ELSE
C
C        3 Column 180 Labels Max
C
         LBCL = 3
         LBCH = 0.5
         LBYS = 1.0
         LBYR = 0.8 / 60
         LBXS = 0.05
         LBXR = 0.3
C
      END IF
C
C     Set the viewport.
C
      CALL PGPAGE
      IF( .NOT. SCREEN ) CALL PGIDEN
      CALL PGSVP( 0.05, 0.95, 0.05, 0.95 )
      CALL PGSWIN( 0.0, 1.0, 0.0, 1.0 )
C
C     Write the list of antennas.
C
      CALL PGSCH( LBCH )
      CALL PGSLW( LABELSZ )
C
C     Set pointers to switch column and position label
C
      NPSTA = 0
      X1 = LBXS
      ICOL = 1
      NLBCOL = PSTNUM / LBCL
      IF( MOD( PSTNUM, LBCL) .GT. 0. ) NLBCOL = NLBCOL + 1
C
C     Loop over stations
C
      DO ISTA = 1, NSTA
         IF( PSTBCK(ISTA,1) .EQ. 1 ) THEN
            NPSTA = NPSTA + 1
C
C           Set the color
C
            IF( PSTBCK(ISTA,2) .EQ. 1 ) THEN
               CALL PGSCI( 2 )
            ELSE
               IF( SCREEN ) THEN
                  CALL PGSCI( 5 )
               ELSE
                  CALL PGSCI( 4 )
               END IF
            END IF
C
C           Check for column change and set new label position
C               
            IF( NPSTA .GT. NLBCOL ) THEN
               NPSTA = 1
               ICOL = ICOL + 1
               X1 = LBXS + ( LBXR * ( ICOL - 1 ) )
            END IF
C
C           Write label in the correct place
C
            Y1 = LBYS - NPSTA * LBYR
C
C           Give the station position along with the name.
C
            CHLAT = TFORM( LAT(STANUM(ISTA)), ' ', 1, 2, 2, ':: ' )
            CHLONG = TFORM( LONG(STANUM(ISTA)), ' ', 1, 3, 2, ':: ' )
            POSLINE = CHLAT // '  ' // CHLONG
            CALL PGTEXT( X1, Y1, STANAME(ISTA) )
            IF( MOVEDS(ISTA) ) CALL PGSCI( 3 )
            CALL PGTEXT( X1 + 0.35 * LBXR, Y1, POSLINE )
         END IF
      END DO
C
C     Sources not needed because this routine only used if each has
C     its own panel.
C
C     For CONFIG mode, write the time range and el limit.
C     Give the el limit for the first station only.
C
      IF( CONFIG ) THEN
         WRITE( POSLINE, '( F5.1, A, F4.0, A )' )
     1      ( TEND - TFIRST ) * 24.0, ' hours,', OPMINEL(1),
     2      ' deg min elevation.'
         CALL PGSCI(2)
         CALL PGMTXT( 'B', 1.3, 0.5, 0.5, POSLINE )
      END IF
C
C     Give setup.
C
      IF( KSET .GT. 0 ) THEN
         CALL PGSCI( 2 )
         CALL PGMTXT( 'T', 0.3, 0.5, 0.5, 'Setup: '//SETFILE(KSET) )
      END IF
C
      CALL PGEBUF
      RETURN
      END
