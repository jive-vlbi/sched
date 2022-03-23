      SUBROUTINE PLOTBM( KSET, KSTA, SCREEN, COLOR )
C
C     Routine for SCHED that makes BEAM plots.
C
C     This routine asks the user questions so SCHED must be run
C     in the mode where the user starts the program, and then
C     interactively asks the user what he/she wants.
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
      INCLUDE 'beam.inc'
C
      INTEGER           NBMPL
      PARAMETER         (NBMPL=8)
C
      CHARACTER         STRING*24, CW*2
      INTEGER           ISCN, ISTA, KSTA, JSTA, LINSIZ, NPSTA
      INTEGER           KSET, ITICKS, LABSIZ, ISRC, NPSRC
      INTEGER           LBMAX, LBCL, LBLC, LBMR, ICOL, NLBCOL, AUVI
      INTEGER           KSRC, I, J, N, K, CPP, CPN, CPH, LEN1
      INTEGER           IRI, IRF, JCL, JCR, NCOL, CIND
      LOGICAL           SCREEN, COLOR
      REAL              UMIN, UMAX, VMIN, VMAX, CF, CB
      REAL              X1, X2, Y1, Y2, XWID, YWID, PLTWID
      REAL              XL, XR, YB, YT, XSHADE(4), YSHADE(4)
      REAL              LBXY, LBYS, LBXS, LBYR, LBXR, LBCH
      REAL              AU1, AU2, AV1, AV2
      REAL              BU1, BU2, BV1, BV2
      REAL              BEAM(KXSIZE,KYSIZE), BTR(6), BEMIN, BEMAX
      REAL              BMPL(NBMPL), GEMIN
      DOUBLE PRECISION  U12(NPBAS,NBASE), V12(NPBAS,NBASE), UVMAX
      DOUBLE PRECISION  XYINT
      SAVE              BEAM, BMPL, XYINT, GEMIN
      DATA  BMPL        / 0.01, 0.03, 0.05, 0.08, 0.1, 0.3, 0.5, 0.8 /
C -------------------------------------------------------------------
C     Set the plot limits.
C
      UMAX = 1.0
      UMIN = -UMAX
      VMAX = 1.0
      VMIN = -VMAX
C
C     Get new page.
C
      IF( POPBCK .NE. 5 ) CALL PGPAGE
      CALL PGSAVE
      IF( .NOT. SCREEN ) CALL PGIDEN
C
C     Y axis Labels dimension and spacing
C
      LBMAX = PSTNUM
      LBMR  = 50
      IF( LBMAX .LE. 15 ) THEN
C
C        1 Column 20 Labels Max
C
         LBCL = 1
         LBXY = 0.325
         LBCH = 0.9
         LBYS = 1.0
         LBXS = 17.0
         LBLC = 25
         LBXR = 0
C
      ELSE 
C
C        2 Column 50 Labels Max
C
         LBCL = 2
         LBXY = 0.325
         LBCH = 0.70
         LBYS = 1.0
         LBXS = 23.0
         LBLC = 25
         LBXR = 9
C
      END IF
      LBYR = LBYS / LBLC
C
C     Get the viewsurface.  I want the plot in the right 70% with
C     labels on the left.  First get the viewsurface in inches.
C
      CALL PGQVSZ( 1, X1, X2, Y1, Y2 )
      XWID = ( X2 - X1 ) * 0.65
      YWID = ( Y2 - Y1 ) * 0.80
      PLTWID = MIN( XWID, YWID )
      XL = LBXY
      YB = 0.11
      XR = XL + PLTWID / ( X2 - X1 )
      YT = YB + PLTWID / ( Y2 - Y1 )
      CALL PGSVP( XL, XR, YB, YT )
C
C     Set the world coordinates.
C
      CALL PGSWIN( UMAX, UMIN, VMAX, VMIN )
C
C     Set the same colours for DISPLAY or GIF Image
C
      CALL PGQINF( 'TYPE', STRING, N )
      IF( STRING .EQ. 'GIF' .OR. STRING .EQ. 'VGIF') SCREEN = .TRUE.
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
C     Set only the first source selected
C
      NPSRC = 0
      DO ISRC = 1, NSRC
         IF( PSOBCK(ISRC) .EQ. 1 .AND. NPSRC .EQ. 0 ) THEN
            NPSRC = 1
            IF( PSOBM .NE. ISRC ) PBMEXE = .TRUE.
            PSOBM = ISRC
            KSRC  = ISRC
         END IF
      END DO
C
C     Set only the first setup file selected
C
      IF( KSET .LE. 0 ) THEN
         DO I = 1, NSETF
            IF( KSET .LE. 0 .AND. PSFPOI(I) .EQ. 1 ) KSET = I
         END DO
      END IF
C
C     Draw the borders etc.
C
      IF( SCREEN ) THEN
         CALL PGSCI( 0 ) 
      ELSE
         CALL PGSCI( 1 ) 
      END IF
C
C     Label the axes.
C
      CALL PGSLW( LABSIZ )
      CALL PGSCI( 2 )
      CALL PGSCH( 1.2 )
      J = PBMFRQ * 10
      CALL PGNUMB( J, -1, 1, STRING, N )
      CALL PGLAB( 'RA (mas)', 'DEC (mas)', 'Beam for '// 
     1      SRCNAME(PSOBM)(1:LEN1(SRCNAME(PSOBM)))//' in '//EXPCODE//
     2      ' ('//STRING(1:N)//'cm)' )
      CALL PGSCH( LBCH )
C
C     Write the list of antennas.
C
      CALL PGSCH( LBCH )
      CALL PGSLW( LABSIZ )
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
         IF( KSTA .EQ. ISTA .OR. PSTBCK(ISTA,2) .EQ. 1 ) THEN
            CALL PGSCI( 2 )
         ELSE
            CALL PGSCI( 9 )
         END IF
         IF( PSTBCK(ISTA,1) .EQ. 1 ) THEN
            NPSTA = NPSTA + 1
C
C           Check for column change and set new label position
C
            IF( NPSTA .GT. NLBCOL ) THEN
               NPSTA = 1
               ICOL = ICOL + 1
               X1 = LBXS - ( LBXR * ( ICOL - 1 ) )
            END IF
C
C           Write label in the correct place
C
            Y1 = LBYS - NPSTA * LBYR
            CALL PGMTXT( 'LV', X1, Y1, 0.0, STANAME(ISTA) )
         END IF
      END DO
C
C     Give setup.
C
      CALL PGSCH( 1.0 )
      IF( NSETF .GT. 1 ) THEN
         CALL PGMTXT( 'T', 0.3, 0.5, 0.5, 'Setup: '//SETFILE(KSET) )
      END IF
C
C     Loop over scans to get the array dimensions for U and V.
C
      AUVI  = 0
      I     = 0
      UVMAX = 0.0
C
      DO ISCN = SCAN1, SCANL
         IF( KSRC .EQ. SRLSTN(SRCNUM(ISCN)) .AND.
     1       ( SETNUM(ISCN) .EQ. KSET .OR. KSET .EQ. 0 ) ) THEN
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
                     IF( STASCN(ISCN,ISTA) .AND.
     1                   UP1(ISCN,JSTA) .EQ. ' ' .AND.
     2                   UP2(ISCN,JSTA) .EQ. ' ' ) THEN
C
C                       Get station UV
C
                        CALL STAUV( ISCN, JSTA, BU1, BU2, BV1, BV2 )
C
C                       Now fill the U and V array with MLambda,
C                       increment the array index and calculate the
C                       max of U and V.
C
                        IF( (.NOT. PSTBAS .AND. (PSTBCK(ISTA,1) .EQ. 1
     1                       .OR.  PSTBCK(JSTA, 1) .EQ. 1)) .OR.
     2                       (PSTBAS .AND. (PSTBCK(ISTA,1) .EQ. 1 .AND.
     3                        PSTBCK(JSTA,1) .EQ. 1)) ) THEN
                            I = I + 1
                            U12(I,1) = ( BU1 - AU1 ) / 
     1                                 ( PBMFRQ / 100 ) * 1000
                            V12(I,1) = ( BV1 - AV1 ) /
     1                                 ( PBMFRQ / 100 ) * 1000
                            UVMAX  = MAX( UVMAX, DSQRT( U12(I,1)**2 +
     1                                    V12(I,1)**2 ) )
                            U12(I,1) = U12(I,1) * 0.0001
                            V12(I,1) = V12(I,1) * 0.0001
C                            I = I + 1
C                            U12(I,1) = -U12(I-1,1)
C                            V12(I,1) = -V12(I-1,1)
C
                            I = I + 1
                            U12(I,1) = ( BU2 - AU2 ) /
     1                                 ( PBMFRQ / 100 ) * 1000
                            V12(I,1) = ( BV2 - AV2 ) /
     1                                 ( PBMFRQ / 100 ) * 1000
C
                            UVMAX  = MAX( UVMAX, DSQRT( U12(I,1)**2 +
     1                                    V12(I,1)**2 ) )
                            U12(I,1) = U12(I,1) * 0.0001
                            V12(I,1) = V12(I,1) * 0.0001
C                            I = I + 1
C                            U12(I,1) = -U12(I-1,1)
C                            V12(I,1) = -V12(I-1,1)
C
                            AUVI = AUVI + 2
                        END IF
C
                     END IF
                  END DO
C
               END IF
            END DO
         END IF
      END DO
C
C     Calculate Beam only if not already done or if important
C     parameter whas changed
C
      IF( PBMEXE ) THEN
         PBMEXE = .FALSE.
C
C        Rezero Beam Matrix.
C
         DO I = 1, KXSIZE
            DO J = 1, KYSIZE
               BEAM(I,J) = 0.0
            END DO
         END DO
C
C        Give Beam Matrix.
C
         XYINT  = 0.0
         CALL PLBEAM( BEAM, U12, V12, AUVI, UVMAX, PBMWGT,
     1                PBMCEL, XYINT )
         XYINT  = XYINT / 10
C
C        Rescale Beam Matrix.
C
         GEMIN = 1000.0
         DO I = 1, KXSIZE
            DO J = 1, KYSIZE
               BEAM(I,J) = BEAM(I,J) / 1000.0
               GEMIN = MIN( GEMIN, BEAM(I,J) )
            END DO
         END DO
C
      END IF
C
C     Check Beam Matrix
C
      IF( GEMIN .EQ. 0.0 ) THEN
         CALL PGSCI( 1 )
         CALL PGPOLY( 4, XSHADE, YSHADE )
         CALL PGSCH( 2.0 )
         CALL PGSCI( 2 )
         CALL PGSLW( 6 )
         CALL PGPTXT(0.25, -0.5, 0.0, 0.0, 'WARNING:')
         CALL PGSCI( 0 )
         CALL PGSLW( 5 )
         CALL PGPTXT(0.98, -0.25, 0.0, 0.0,
     1               'The setup file selected is wrong')
         CALL PGPTXT(0.88, 0.0, 0.0, 0.0,
     1               'or not used in the schedule')
         CALL PGUNSA
         RETURN
      END IF
C
C     Limits of Beam Size Selected
C
      NCOL = PBMPIX / 2
C
      IRI  = KXSIZE / 2 - NCOL + 1
      IRF  = KXSIZE / 2 + NCOL
      JCL  = KYSIZE / 2 - NCOL + 1
      JCR  = KYSIZE / 2 + NCOL
C
C     Rescale Beam Matrix.
C
      BEMIN = 1000.0
      BEMAX = -1000.0
C
      DO J = JCL, JCR
         DO I = IRI, IRF
            BEMIN = MIN( BEMIN, BEAM(I,J) )
            BEMAX = MAX( BEMAX, BEAM(I,J) )
         END DO
      END DO
C
C     Set Negative Contours Limits and Coniugate
C
      IF( BEMIN .LT. 0.0 ) THEN
         BMPL(1) = BEMIN / 3 * 2
         BMPL(2) = BEMIN / 3
         BMPL(3) = ABS( BMPL(2) )
         BMPL(4) = ABS( BMPL(1) )
      ELSE
         BMPL(1) = 0.01
         BMPL(2) = 0.03
         BMPL(3) = 0.05
         BMPL(4) = 0.08
      END IF
C
C     Plot Beam
C
      CALL PGWNAD(-0.5+IRI, 0.5+IRF, -0.5+JCL, 0.5+JCR)
C
      BTR(1) = 0.0
      BTR(2) = 1.0
      BTR(3) = 0.0
      BTR(4) = 0.0
      BTR(5) = 0.0
      BTR(6) = 1.0
C
      CALL PGSCIR ( 17, 97 )
      CALL PLPALE ( 17, 97, PBMPAL, BEMIN, GEMIN )
      CALL PGSITF ( PBMCTF )
C
C     Set colours for differents palettes and devices
C
      IF( PBMPAL .EQ. 0 ) THEN
         CPP = 11 
         CPN = 2
         CW = 'RG'
         IF( SCREEN ) THEN
            CPH = 7
            CF = BEMIN
            CB = BEMAX
         ELSE
            CPH = 8
            CF = BEMAX
            CB = BEMIN
         END IF
         CALL PGGRAY ( BEAM, KXSIZE, KYSIZE, IRI, IRF, JCL, JCR,
     1                 CF, CB, BTR )
      ELSE
         CW = 'RI'
         IF( PBMPAL .EQ. 1 ) THEN
            CPP = 9 
            CPN = 11 
            IF( SCREEN ) THEN
               CPH = 7
            ELSE
               CPH = 8
            END IF
            CF = BEMIN
            CB = BEMAX
         ELSE
            IF( SCREEN ) THEN
               CPP = 7
            ELSE
               CPP = 8
            END IF
            CPN = 2 
            CPH = 1
            CF = BEMAX
            CB = BEMIN
         END IF
         CALL PGIMAG ( BEAM, KXSIZE, KYSIZE, IRI, IRF, JCL, JCR,
     1                 CF, CB, BTR )
      END IF
C
      CALL PGSCI( ITICKS ) 
C
C     Wedge Scale
C
      IF( PBMCTF .EQ. 0 ) THEN
         STRING = 'Linear'
      ELSE IF( PBMCTF .EQ. 1 ) THEN
             STRING = 'Logarithmic'
      ELSE
                 STRING = 'Square-Root'
      ENDIF
      
      CALL PGWEDG( CW,  1.0, 3.0, CF, CB, STRING )
C
C     Contours Plot in different colours
C
      IF( PBMCON ) THEN
C
C        Negative and coniugate
C
         CALL PGSCI( CPN ) 
         CALL PGCONT( BEAM, KXSIZE, KYSIZE, IRI, IRF, JCL, JCR,
     1                BMPL, 4, BTR ) 
C
C        Other Positive
C
         CALL PGSCI( CPP ) 
         CALL PGCONT( BEAM, KXSIZE, KYSIZE, IRI, IRF, JCL, JCR,
     1                BMPL(5), 4, BTR ) 
C
C        HPBW
C
         CALL PGSCI( CPH ) 
         CALL PGCONT( BEAM, KXSIZE, KYSIZE, IRI, IRF, JCL, JCR,
     1                BMPL(7), 1, BTR ) 
      END IF
C
C     Replot the Axis
C
      UMAX = XYINT * PBMPIX / 2
      UMIN = -UMAX
      VMAX = UMAX
      VMIN = -VMAX
      CALL PGSWIN( UMAX, UMIN, VMIN, VMAX )
C
C     Draw the borders etc.
C
      IF( SCREEN ) THEN
         CALL PGSCI( 0 ) 
      ELSE
         CALL PGSCI( 1 ) 
      END IF
      CALL PGSCI( ITICKS ) 
      CALL PGSLW( LABSIZ )
      CALL PGBOX( 'BCNTS', 0.0, 0, 'BCNTS', 0.0, 0 )
C
C     Draw Beam Parameters ( Min, Max, Contours Limits )
C
      CALL PGSCI( 11 )
      CALL PGSCH( LBCH )
      X1 = LBXS
      Y1 = Y1 - 0.1
      I = BEMIN * 10**5
      CALL PGNUMB( I, -5, 1, STRING, N )
      CALL PGMTXT( 'LV', X1, Y1, 0.0, 'Min: '//STRING(1:N) )
C
      Y1 = Y1 - 0.05
      CALL PGMTXT( 'LV', X1, Y1, 0.0, 'Max: 1.0' )
C
      IF( PBMCON ) THEN
         Y1 = Y1 - 0.1
         CALL PGMTXT( 'LV', X1, Y1, 0.0, 'Contours Limits:')
C
         Y2 = Y1
         K  = NBMPL / 2
         CALL PGSCI( CPN ) 
         DO I = 1, K
            Y2 = Y2 - 0.05
            J = BMPL(I) * 10**3
            CALL PGNUMB( J, -3, 1, STRING, N ) 
            IF( STRING(1:1) .EQ. '-' ) THEN
               CALL PGMTXT( 'LV', X1, Y2, 0.0, STRING(1:N) )
            ELSE
               CALL PGMTXT( 'LV', X1, Y2, 0.0, '+'//STRING(1:N) )
            END IF
         END DO
C
         X1 = X1 - 6.0
         Y2 = Y1
         K  = K + 1
         CALL PGSCI( CPP ) 
         DO I = K, NBMPL
            Y2 = Y2 - 0.05
            J = BMPL(I) * 10**3
            CALL PGNUMB( J, -3, 1, STRING, N ) 
            IF( BMPL(I) .EQ. 0.5 ) CALL PGSCI( CPH )
            IF( STRING(1:1) .EQ. '-' ) THEN
               CALL PGMTXT( 'LV', X1, Y2, 0.0, STRING(1:N) )
            ELSE
               CALL PGMTXT( 'LV', X1, Y2, 0.0, '+'//STRING(1:N) )
            END IF
            IF( BMPL(I) .EQ. 0.5 ) CALL PGSCI( CPP )
         END DO
C
      END IF
C
      CALL PGUNSA
      RETURN
      END
