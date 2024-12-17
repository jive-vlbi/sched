      SUBROUTINE PLOTUV( TYP, KSET, KSRC, KSTA, SCREEN, COLOR,
     1                   RXMIN, RXMAX, RYMIN, RYMAX )
C
C     Routine for SCHED that makes UV plots.
C
C     Small modification by Craig Walker on 21 Dec. 2001 as part
C     of plotting different sources on different panels.  Removed
C     JSRC as a call variable and made KSRC be the source in 
C     the schdule, not the catalog.  SRCATN(KSRC) is the source
C     in the catalog.  SRCNAME(KSRC) is the source name used.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'plot.inc'
C
      CHARACTER         ULAB*20, VLAB*20
      INTEGER           ISCN, ISTA, KSTA, ISRC, JSTA, LINSIZ, NPSTA
      INTEGER           KSET, KSRC, NPSRC, ITICKS, LABSIZ, LSRC
      INTEGER           LBMAX, LBCL, LBLC, ICOL, NLBCOL, CIND
      INTEGER           IB, TYP, ISET, LMSC
      LOGICAL           SCREEN, COLOR
      REAL              AU1, AU2, AV1, AV2
      REAL              BU1, BU2, BV1, BV2
      REAL              U12(2), V12(2)
      REAL              UMIN, UMAX, VMIN, VMAX
      REAL              X1, X2, Y1, Y2, XWID, YWID, PLTWID
      REAL              XL, XR, YB, YT, XSHADE(4), YSHADE(4)
      REAL              LBXY, LBYS, LBXS, LBYR, LBXR, LBCH
      REAL              MFSSC, MFS1, MFSINC, WVFRQ
      DOUBLE PRECISION  RXMIN, RXMAX, RYMIN, RYMAX
C -------------------------------------------------------------------
C
C     Set the plot limits.  U first.
C
      IF( RXMIN .NE. UNSET .AND. RXMAX .NE. UNSET ) THEN
         UMIN = RXMIN
         UMAX = RXMAX
      ELSE IF( RXMIN .EQ. UNSET .AND. RXMAX .EQ. UNSET ) THEN
         CALL MAXBAS( UMAX )
         UMIN = UMAX
         UMAX = -UMAX
      ELSE IF( RXMIN .NE. UNSET ) THEN
         UMIN = RXMIN
         UMAX = -RXMIN
      ELSE
         UMAX = RXMAX
         UMIN = -RXMAX
      END IF
C
C     Get V scale.
C
      IF( RYMIN .NE. UNSET .AND. RYMAX .NE. UNSET ) THEN
         VMIN = RYMIN
         VMAX = RYMAX
      ELSE IF( RYMIN .EQ. UNSET .AND. RYMAX .EQ. UNSET ) THEN
         VMIN = UMAX
         VMAX = -VMIN
      ELSE IF( RYMIN .NE. UNSET ) THEN
         VMIN = RYMIN
         VMAX = -RYMIN
      ELSE
         VMAX = RYMAX
         VMIN = -RYMAX
      END IF
C
C     Get new page.
C
      IF( POPBCK .NE. 5 ) CALL PGPAGE
      CALL PGSAVE
      IF( .NOT. SCREEN .AND. .NOT. PUBPLOT ) CALL PGIDEN
C
C     Label dimensions and spacing.
C
      LBMAX = PSONUM + PSTNUM
      IF( LBMAX .LE. 30 ) THEN
C
C        1 Column 30 Labels Max
C
         LBCL = 1
         LBXY = 0.325
         LBCH = 1.0
         LBYS = 1.0
         LBXS = 12.0
         LBLC = 30
         LBXR = 0
C
      ELSE IF( LBMAX .LE. 70 ) THEN
C
C        2 Column 70 Labels Max
C
         LBCL = 2
         LBXY = 0.375
         LBCH = 0.80
         LBYS = 1.0
         LBXS = 23.0
         LBLC = 35
         LBXR = 9
C
      ELSE
C
C        3 Column 180 Labels Max
C
         LBCL = 3
         LBXY = 0.375
         LBCH = 0.5
         LBYS = 1.0
         LBXS = 35.0
         LBLC = 60
         LBXR = 9
C
      END IF
      LBYR = LBYS / LBLC
C
C     Get the viewsurface.  For single uv plots, I want the 
C     plot in the right 70% with labels on the left.  
C     When there are going to be multiple panels (KSRC=0), don't
C     write labels and use the full view surface for the plot.
C     First get the viewsurface in inches.
C
      CALL PGQVSZ( 1, X1, X2, Y1, Y2 )
C
C     correct  misalignement in Portrait format
C
C      IF( .NOT. SCREEN .AND. 
C     1  ( PFLBCK .EQ. 2 .OR. PFLBCK .EQ. 4 .OR. PFLBCK .EQ. 6 ) ) THEN 
C         XWID = ( X2 - X1 ) * 0.60
C         YB   = 0.265
C      ELSE
C         XWID = ( X2 - X1 ) * 0.65
C         YB   = 0.11
C      END IF
C
      IF( KSRC .EQ. 0 ) THEN
         XWID = ( X2 - X1 ) * 0.65
      ELSE
         XWID = ( X2 - X1 ) * 0.80
         LBXY = 0.11
      END IF
      YWID = ( Y2 - Y1 ) * 0.80
      PLTWID = MIN( XWID, YWID )
      XL = LBXY
      XR = XL + PLTWID / ( X2 - X1 )
      YB = 0.11
      YT = YB + PLTWID / ( Y2 - Y1 )
      CALL PGSVP( XL, XR, YB, YT )
C
C     Set the world coordinates.
C
      IF( UMIN .EQ. UMAX) UMAX = UMIN + 0.1
      IF( VMIN .EQ. VMAX) VMAX = VMIN + 0.1
      CALL PGSWIN( UMAX, UMIN, VMIN, VMAX )
C
C     Set line widths etc. 
CRCW  This moved outside of the IF(COLOR) block because
C     the parameters are used later and are otherwise not
C     initialized.
C
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
C
C     Color the background.
C
      IF( COLOR ) THEN
         XSHADE(1) = UMIN 
         XSHADE(2) = XSHADE(1)
         XSHADE(3) = UMAX 
         XSHADE(4) = XSHADE(3)
         YSHADE(1) = VMIN 
        YSHADE(2) = VMAX 
         YSHADE(3) = YSHADE(2)
         YSHADE(4) = YSHADE(1)
         CALL PGSCI( CIND )
         CALL PGPOLY( 4, XSHADE, YSHADE )
      END IF
C
C     Draw the borders etc.
C
      IF( SCREEN ) THEN
         CALL PGSCI( 0 ) 
      ELSE
         CALL PGSCI( 1 ) 
      END IF
      CALL PGSLW( 1 )
      CALL PGBOX( 'AGTS', 0.0, 0, 'AGTS', 0.0, 0 )
      CALL PGSCI( ITICKS ) 
      CALL PGSLW( LABSIZ )
      CALL PGBOX( 'BCNTS', 0.0, 0, 'BCNTS', 0.0, 0 )
C
C     Plot the optimizing grid if it was used.
C
      IF( GRIDUSED ) CALL PLGRID( DECP(SRCATN(KSRC)) )
      CALL PGSLW( LABSIZ )
C
C     Label the axes.
C
      ULAB = 'U ('
      VLAB = 'V ('
      ICOL = 4
      IF( PXYTYP(TYP) .EQ. 'Wv' ) THEN
         LMSC = 10**PXYWLE
         IF( PXYWLE .EQ. 3 ) THEN
             ULAB(ICOL:) = 'Kilo '
             VLAB(ICOL:) = 'Kilo '
             ICOL = 9
         ELSE IF( PXYWLE .EQ. 6 ) THEN
                 ULAB(ICOL:) = 'Mega '
                 VLAB(ICOL:) = 'Mega '
                 ICOL = 9
         END IF
         ULAB(ICOL:) = 'Wavelength)'
         VLAB(ICOL:) = 'Wavelength)'
      ELSE
         ULAB(ICOL:) = 'Km)'
         VLAB(ICOL:) = 'Km)'
      END IF
C
      CALL PGSCI( 2 )
      CALL PGSCH( 1.2 )
      IF( PUBPLOT ) THEN
         CALL PGLAB( ULAB, VLAB, ' ' )
      ELSE IF( KSRC .EQ. 0 ) THEN
         CALL PGLAB( ULAB, VLAB, 'UV Coverage for '// EXPCODE )
      ELSE
         CALL PGLAB( ULAB, VLAB, 'UV Coverage for '// 
     1         SRCNAME(KSRC)//' in '//EXPCODE )
      END IF
      CALL PGSCH( LBCH )
C
      CALL PGSCH( LBCH )
      CALL PGSLW( LABSIZ )
C
C     Write the list of antennas but only if they aren't being
C     written in another panel.  KSTA will be non-zero when multiple
C     panels are being written.   Also don't do it if PUBPLOT (for
C     publication quality plots) is set.
C
      IF( KSRC .EQ. 0 .AND. .NOT. PUBPLOT ) THEN
C
C        Set pointers to switch column and position label
C
         NPSTA = 0
         X1 = LBXS
         ICOL = 1
         NLBCOL = PSTNUM / LBCL
         IF( MOD( PSTNUM, LBCL) .GT. 0. ) NLBCOL = NLBCOL + 1
C
C        Loop over stations
C
         DO ISTA = 1, NSTA
            IF( KSTA .EQ. ISTA .OR. PSTBCK(ISTA,2) .EQ. 1 ) THEN
               CALL PGSCI( 2 )
            ELSE
               CALL PGSCI( 9 )
            END IF
            IF( PSTBCK(ISTA,1) .EQ. 1 ) THEN
               NPSTA = NPSTA + 1
C
C              Check for column change and set new label position
C               
               IF( NPSTA .GT. NLBCOL ) THEN
                  NPSTA = 1
                  ICOL = ICOL + 1
                  X1 = LBXS - ( LBXR * ( ICOL - 1 ) )
               END IF
C
C              Write label in the correct place
C
               Y1 = LBYS - NPSTA * LBYR
               CALL PGMTXT( 'LV', X1, Y1, 0.0, STANAME(ISTA) )
            END IF
         END DO

C
C        Write the list of sources.
C
         CALL PGSCI( 9 )
C
C        Set pointers to switch column and position label
C
         NPSTA  = NLBCOL
         NPSRC  = 0
         X1     = LBXS
         ICOL   = 1
         NLBCOL = LBLC - NPSTA
C
C        Loop over stations
C
         CALL PGSCI( 11 )
         IF( KSRC .EQ. 0 ) THEN
            NPSRC = 0
            DO ISRC = 1, NSRC
               IF( PSOBCK(ISRC) .EQ. 1 ) THEN
                  NPSRC = NPSRC + 1
C
C                 Check for column change and set new label position
C
                  IF( NPSRC .GT. NLBCOL ) THEN
                     NPSRC = 1
                     ICOL = ICOL + 1
                     X1 = LBXS - ( LBXR * ( ICOL - 1 ) )
                  END IF
C
C                 Write label in the correct place
C
                  Y1 = LBYS - LBYR * NPSTA - LBYR - NPSRC * LBYR
C
C                 Check for max label allowed
C
                  IF( ICOL .GT. LBCL ) THEN
                     IF( NPSRC .EQ. 1 ) THEN
                        CALL PGMTXT( 'LV', X1, Y1, 0.0, 'More' )
                     END IF
                     GO TO 300
                  ELSE
                     CALL PGMTXT( 'LV', X1, Y1, 0.0, SRCNAME(ISRC) )
                  END IF
               END IF
            END DO
         END IF
  300    CONTINUE
C
C        Give setup.
C
         IF( KSET .GT. 0 ) THEN
            CALL PGMTXT( 'T', 0.3, 0.5, 0.5, 'Setup: '//SETFILE(KSET) )
         END IF
      END IF
C
C     Get parameters for plotting of multiple lines for MFS.
C     MFSRAT is the ratio of the upper to the lower frequency.
C     NMFS is the number of frequencies to use.
C
      IF( NMFS .GT. 1 ) THEN
         MFS1 = 2.0 / ( 1.0 + MFSRAT )
         MFSINC = MFS1 * ( MFSRAT - 1.0 ) / ( NMFS - 1 )
      ELSE
         MFS1 = 1.0
         MFSINC = 0.0
      END IF
C
C     Loop over scans. (LSRC prevents problems when KSRC=0
C
      CALL PGSLW( LINSIZ )
      IF( KSRC .EQ. 0 ) THEN
         LSRC = 0
      ELSE
         LSRC = SRCATN(KSRC)
      END IF
      DO ISCN = SCAN1, SCANL
       DO ISET = 1, NSETF
C
         IF((PSFPOI(ISET) .EQ. 1 .AND. SETNUM(ISCN) .EQ. ISET) .AND. 
     1      (SRCNUM(ISCN) .EQ. LSRC .OR. KSRC .EQ. 0) .AND. 
     2       PSOBCK(SRLSTN(SRCNUM(ISCN))) .EQ. 1 ) THEN
            CALL PGBBUF
C
C           Wavelength (Km)
C
            IF( PXYTYP(TYP) .EQ. 'Wv' ) THEN
               WVFRQ = (30000.0 / SFFREQ(1,SETNUM(ISCN))) / 10**5
            END IF
C
C           Loop over station A.
C
            DO ISTA = 1, NSTA - 1
               IF( STASCN(ISCN,ISTA) .AND. 
     1             UP1(ISCN,ISTA) .EQ. ' ' .AND.
     2             UP2(ISCN,ISTA) .EQ. ' ' ) THEN
C
C                 Get station UV
C
                  CALL STAUV( ISCN, ISTA, AU1, AU2, AV1, AV2 )
C
C                 Loop over station B.
C
                  DO JSTA = ISTA + 1, NSTA
                     IF( STASCN(ISCN,JSTA) .AND. 
     1                   UP1(ISCN,JSTA) .EQ. ' ' .AND.
     2                   UP2(ISCN,JSTA) .EQ. ' ' ) THEN
C
C                       Plot the segment only if it was selected
C
                        IF( (.NOT. PSTBAS .AND. (PSTBCK(ISTA,1) .EQ. 1
     1                       .OR.  PSTBCK(JSTA, 1) .EQ. 1)) .OR.
     2                       (PSTBAS .AND. (PSTBCK(ISTA,1) .EQ. 1 .AND.
     3                        PSTBCK(JSTA,1) .EQ. 1)) ) THEN  
C
C                          Set the segment color
C
                           IF( JSTA .EQ. KSTA .OR. ISTA .EQ. KSTA .OR.
     1                         PSTBCK(JSTA,2) .EQ. 1 .OR.
     2                         PSTBCK(ISTA,2) .EQ. 1 ) THEN
                              CALL PGSCI( 2 )
                           ELSE
CRCW                              CALL PGSCI( 12 )
                              CALL PGSCI( 4 )
                           END IF
C
C                          Get station UV
C
                           CALL STAUV( ISCN, JSTA, BU1, BU2, BV1, BV2 )
C
C                          Now plot the line segment.  Do multiple
C                          lines if multi-frequency synthesis plots are
C                          requested.
C
                           DO IB = 1, NMFS
                              MFSSC = MFS1 + ( IB - 1 ) * MFSINC
                              U12(1) = ( BU1 - AU1 ) * MFSSC
                              U12(2) = ( BU2 - AU2 ) * MFSSC
                              V12(1) = ( BV1 - AV1 ) * MFSSC
                              V12(2) = ( BV2 - AV2 ) * MFSSC
                              IF( PXYTYP(TYP) .EQ. 'Wv' ) THEN
                                U12(1) = (U12(1) / WVFRQ) / LMSC 
                                U12(2) = (U12(2) / WVFRQ) / LMSC
                                V12(1) = (V12(1) / WVFRQ) / LMSC
                                V12(2) = (V12(2) / WVFRQ) / LMSC
                              END IF
                              CALL PGLINE( 2, U12, V12 )
                              CALL PGLINE( 2, U12, V12 )
                              CALL PGLINE( 2, U12, V12 )
                              CALL PGLINE( 2, U12, V12 )
C
C                             And the conjugate
C
                              U12(1) = -U12(1)
                              U12(2) = -U12(2)
                              V12(1) = -V12(1)
                              V12(2) = -V12(2)
                              CALL PGLINE( 2, U12, V12 )
                           END DO
C
                        END IF
C
                     END IF
                  END DO
C
               END IF
            END DO
            CALL PGEBUF
         END IF
       END DO
      END DO
C
C  990 CONTINUE
      CALL PGUNSA
      RETURN
      END
