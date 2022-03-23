      SUBROUTINE PLSKA( )
C
C     Routine to plot the sequence of plots that I (RCW) want for the 
C     SKA to avoid a lot of cut and pasting.  This is very specific and
C     probably does not really belong in the heart of SCHED, but this
C     is the easiest way to do it.
C
      INCLUDE     'sched.inc'
      INCLUDE     'plot.inc'
C
      INTEGER     PGOPEN, IPDEV, IPODEV, OCOLIND, ONTICKS, OPFLBCK
      REAL        SAVELIMS(4), RGB, OLABELSZ
C
C     For histogram
C
      INTEGER           NBIN
      PARAMETER         (NBIN=11)
      INTEGER           I, J, IB, COUNT(NBIN), BCNT(NBIN)
      DOUBLE PRECISION  LIMITS(NBIN), RLIMS(NBIN)
      DOUBLE PRECISION  X, Y, Z, X2, Y2, Z2
      DOUBLE PRECISION  X0, Y0, Z0, RADIUS, BLENGTH
      LOGICAL           GOT0
      CHARACTER         NAME1*8
      DATA  LIMITS / 0.D0, 0.3D0, 1.D0, 3.5D0, 10.D0, 35.D0,
     1               100.D0, 350.D0, 1000.D0, 3500.D0, 1.D4 /

C ----------------------------------------------------------------------
C
C     Save the current PGPLOT environment.  Also save the current
C     latitude and longitude range for the plots.
C
      CALL PGSAVE
      SAVELIMS(1) = LONMIN
      SAVELIMS(2) = LONMAX
      SAVELIMS(3) = LATMIN
      SAVELIMS(4) = LATMAX
      CALL PGQID( IPODEV )
      OCOLIND = COLIND
      ONTICKS = NTICKS
      OLABELSZ = LABELSZ
      OPFLBCK = PFLBCK
C
C     Open the new graphics device.
C     It seems that the PGSCR needs to be rerun for the new device.
C
      IPDEV = PGOPEN( 'skamap.ps/cps' )
      COLIND = 18
      NTICKS = 1
      LABELSZ = 2
      PFLBCK = 3
      RGB    = 0.0625 * 12
      CALL PGSCR( 18, RGB, RGB, RGB )
C
C     Divide into 4 subpanels.
C
      CALL PGSUBP( 2, 2 )
C
C     Do the first panel.
C
      CALL PGPANL( 1, 1 )
      LONMIN = 38.0
      LONMAX = 176.0
      LATMIN = -10.0
      LATMAX = 78.0
      CALL PLMAP( .FALSE., .TRUE. )
C
C     Do the first panel.
C
      CALL PGPANL( 2, 1 )
      LONMIN = 92.
      LONMAX = 122.
      LATMIN = 23.0
      LATMAX = 43.0
      CALL PLMAP( .FALSE., .TRUE. )
C
C     Do the first panel.
C
      CALL PGPANL( 1, 2 )
      LONMIN = 104.6
      LONMAX = 110.3
      LATMIN = 32.5
      LATMAX = 36.1
      CALL PLMAP( .FALSE., .TRUE. )
C
C     Do the first panel.
C
      CALL PGPANL( 2, 2 )
      LONMIN = 106.9
      LONMAX = 108.4
      LATMIN = 33.55
      LATMAX = 34.55
      CALL PLMAP( .FALSE., .TRUE. )
C
C     Close the current plots.
C
      CALL PGCLOS
C
C     Now do the UV plots.
C
      IPDEV = PGOPEN( 'skauv.ps/cps' )
      NTICKS = 1
      RGB    = 0.0625 * 12
      CALL PGSCR( 18, RGB, RGB, RGB )
C
C     Divide into 4 subpanels.
C
      CALL PGSUBP( 2, 2 )
C
C     Do the first panel.
C
C      CALL PGPANL( 1, 1 )
      CALL PLOTUV( 0, 1, 0, .FALSE., .TRUE., 5000.D0, -5000.D0,
     1             -5000.D0, 5000.D0 )
C      CALL PGPANL( 2, 1 )
      CALL PLOTUV( 0, 1, 0, .FALSE., .TRUE., 500.D0, -500.D0,
     1             -500.D0, 500.D0 )
C      CALL PGPANL( 1, 2 )
      CALL PLOTUV( 0, 1, 0, .FALSE., .TRUE., 100.D0, -100.D0,
     1             -100.D0, 100.D0 )
C      CALL PGPANL( 2, 2 )
      CALL PLOTUV( 0, 1, 0, .FALSE., .TRUE., 50.D0, -50.D0,
     1             -50.D0, 50.D0 )
C
C     Close the current plots.
C
      CALL PGCLOS
      CALL PGSLCT( IPODEV )
C
C     Retrieve the PGPLOT environment from before this routine 
C     was called.
C     Also retrieve the plot limits.
C
      CALL PGUNSA
      LONMIN = SAVELIMS(1)
      LONMAX = SAVELIMS(2)
      LATMIN = SAVELIMS(3)
      LATMAX = SAVELIMS(4)
      COLIND = OCOLIND
      NTICKS = ONTICKS
      LABELSZ = OLABELSZ
      PFLBCK = OPFLBCK
C
C     Do a histogram.
C
      GOT0 = .FALSE.
      DO IB = 1, NBIN
         RLIMS(IB) = LIMITS(IB) / ( SQRT( 3.D0 ) )
         COUNT(IB) = 0
         BCNT(IB) = 0
      END DO
C
C     Get the radial histogram.
C
      DO I = 1, NSTA
         IF( PSTBCK(I,1) .EQ. 1 ) THEN
            X = XPOS(STANUM(I))
            Y = YPOS(STANUM(I))
            Z = ZPOS(STANUM(I))
            IF( .NOT. GOT0 ) THEN
               NAME1 = STATION(STANUM(I))
               X0 = X
               Y0 = Y
               Z0 = Z
               GOT0 = .TRUE.
            END IF
            RADIUS = SQRT( (X-X0)**2 + (Y-Y0)**2 + (Z-Z0)**2 )
            RADIUS = RADIUS / 1000.0D0
            MSGTXT = ' '
            WRITE( MSGTXT,'( A8, 4F14.2 )' ) STANAME(I), RADIUS, X, Y, Z
            CALL WLOG( 1, MSGTXT )
            DO IB = 1, NBIN - 1
               IF( RADIUS .GT. RLIMS(IB) .AND. RADIUS .LE. RLIMS(IB+1) )
     1          COUNT(IB) = COUNT(IB) + 1
            END DO
         END IF
      END DO
      CALL WLOG( 1, ' ' )
      CALL WLOG( 1, 'Histogram of station distances from ' // NAME1 )
      CALL WLOG( 1, '      Radius Range     Diameter  Number' )
      DO IB = 1, NBIN - 1
         MSGTXT = ' '
         WRITE( MSGTXT,'( 3F10.2, I7 )' )
     1     RLIMS(IB), RLIMS(IB+1), LIMITS(IB+1), COUNT(IB)
         CALL WLOG( 1, MSGTXT )
      END DO
C
C     Get baseline histogram
C
      DO I = 1, NSTA - 1
         IF( PSTBCK(I,1) .EQ. 1 ) THEN
            X = XPOS(STANUM(I))
            Y = YPOS(STANUM(I))
            Z = ZPOS(STANUM(I))
            DO J = I+1, NSTA
               IF( PSTBCK(J,1) .EQ. 1 ) THEN
                  X2 = XPOS(STANUM(J))
                  Y2 = YPOS(STANUM(J))
                  Z2 = ZPOS(STANUM(J))
                  BLENGTH = SQRT( (X-X2)**2 + (Y-Y2)**2 + 
     1                     (Z-Z2)**2 )
                  BLENGTH = BLENGTH / 1000.0D0
                  DO IB = 1, NBIN - 1
                     IF( BLENGTH .GT. RLIMS(IB) .AND. 
     1                   BLENGTH .LE. RLIMS(IB+1) )
     2                   BCNT(IB) = BCNT(IB) + 1
                  END DO
               END IF
            END DO
         END IF
      END DO
      CALL WLOG( 1, ' ' )
      CALL WLOG( 1, 'Histogram of Baseline Lengths' )
      CALL WLOG( 1, '       Range         Number' )
      DO IB = 1, NBIN - 1
         MSGTXT = ' '
         WRITE( MSGTXT, '( 2F10.2, I7 )' )
     1     LIMITS(IB), LIMITS(IB+1), BCNT(IB)
         CALL WLOG( 1, MSGTXT )
      END DO
C
      RETURN
      END
