      SUBROUTINE PLAXBM
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
C     Display Axis BEAM area
C
C     To add or modify the options in the OPTIONS Panel, check also
C     the INCLUDE file 'plot.inc' and the soubroutine 'plinit.f'
C
      CHARACTER     MESSAG*80, STRING*4
      INTEGER       I, N
      LOGICAL       PBTR
      REAL          CSIZ, XT, YT
C ----------------------------------------------------------------------
C
C     Set Font, Radio Buttons Policy and Label offset
C
      CSIZ  = PPNDIM(3)
      PBTR  = .TRUE.
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     Titles
C
      CALL PGSLW( 5 )
      CALL PGSCF( 1 )
      CALL PGSCH( PPNDIM(3) )
C
C     Titles
C
      YT = PBMTXT(4,1) + 0.04
      XT = PPSDIM(1)
      MESSAG = 'Select Scales to Plot Beam'
      CALL PGSCI( PPNCOL(7) )
      CALL PGPTXT(XT, YT, 0.0, 0.0, MESSAG )
C
C     Frequency Scales
C
      CALL PGSCI( PPNCOL(2) )
      YT = PBMTXT(3,1) + 0.025
      CALL PGPTXT(XT, YT, 0.0, 0.0, 'Frequency (cm)' )
      I = PBMFRQ * 10
      CALL PGNUMB( I, -1, 1, STRING, N )
      CALL PLSTXT( PBMTXT(1,1), PBMTXT(2,1), PBMTXT(3,1),
     1             PBMTXT(4,1), STRING(1:N), 0, .TRUE. )
      CALL PLSBAR( PBMSBR(1,1), PBMSBR(2,1), PBMSBR(3,1),
     1             PBMSBR(4,1), 1, 1, PBMBXM(1,1) )
C
C     Pixel Scales
C
      YT = PBMTXT(3,2) + 0.025
      CALL PGPTXT(XT, YT, 0.0, 0.0, 'Size in Pixel of Beam' )
      CALL PGNUMB( PBMPIX, 0, 1, STRING, N )
      CALL PLSTXT( PBMTXT(1,2), PBMTXT(2,2), PBMTXT(3,2),
     1             PBMTXT(4,2), STRING(1:N), 0, .TRUE. )
      CALL PLSBAR( PBMSBR(1,2), PBMSBR(2,2), PBMSBR(3,2),
     1             PBMSBR(4,2), 1, 1, PBMBXM(1,3) )
C
C     Cell Scales
C
      YT = PBMTXT(3,3) + 0.025
      CALL PGPTXT(XT, YT, 0.0, 0.0, 'Oversampling Factor' )
      CALL PGNUMB( PBMCEL, 0, 1, STRING, N )
      CALL PLSTXT( PBMTXT(1,3), PBMTXT(2,3), PBMTXT(3,3),
     1             PBMTXT(4,3), STRING(1:N), 0, .TRUE. )
      CALL PLSBAR( PBMSBR(1,3), PBMSBR(2,3), PBMSBR(3,3),
     1             PBMSBR(4,3), 1, 1, PBMBXM(1,5) )
C
C     Weight Scales
C
      YT = PBMTXT(3,4) + 0.025
      CALL PGPTXT(XT, YT, 0.0, 0.0, 'Weight' )
      IF( PBMWGT .EQ. 0 ) THEN
         MESSAG = 'Natural'
      ELSE
         MESSAG = 'Uniform'
      END IF
      CALL PLSTXT( PBMTXT(1,4), PBMTXT(2,4), PBMTXT(3,4),
     1             PBMTXT(4,4), MESSAG, 0, .TRUE. )
      CALL PLSBAR( PBMSBR(1,4), PBMSBR(2,4), PBMSBR(3,4),
     1             PBMSBR(4,4), 1, 1, PBMBXM(1,7) )
C
C     Image Scales
C
      YT = PBMTXT(3,5) + 0.025
      CALL PGPTXT(XT, YT, 0.0, 0.0, 'Image Transfer Function' )
      CALL PGNUMB( PBMCTF, 0, 1, STRING, N )
      CALL PLSTXT( PBMTXT(1,5), PBMTXT(2,5), PBMTXT(3,5),
     1             PBMTXT(4,5), STRING(1:N), 0, .TRUE. )
      CALL PLSBAR( PBMSBR(1,5), PBMSBR(2,5), PBMSBR(3,5),
     1             PBMSBR(4,5), 1, 1, PBMBXM(1,9) )
C
C     Palette Scales
C
      YT = PBMTXT(3,6) + 0.025
      CALL PGPTXT(XT, YT, 0.0, 0.0, 'Color Palette' )
      IF( PBMPAL .EQ. 0 ) THEN
         MESSAG = 'Gray'
      ELSE IF( PBMPAL .EQ. 1 ) THEN
         MESSAG = 'Thermal'
      ELSE
         MESSAG = 'RGB'
      END IF
      CALL PLSTXT( PBMTXT(1,6), PBMTXT(2,6), PBMTXT(3,6),
     1             PBMTXT(4,6), MESSAG, 0, .TRUE. )
      CALL PLSBAR( PBMSBR(1,6), PBMSBR(2,6), PBMSBR(3,6),
     1             PBMSBR(4,6), 1, 1, PBMBXM(1,11) )
C
C     Contours Scales
C
      YT = PBMTXT(3,7) + 0.025
      CALL PGPTXT(XT, YT, 0.0, 0.0, 'Plot Contours' )
      IF( PBMCON ) THEN
         MESSAG = 'Yes'
      ELSE
         MESSAG = 'No'
      END IF
      CALL PLSTXT( PBMTXT(1,7), PBMTXT(2,6), PBMTXT(3,7),
     1             PBMTXT(4,7), MESSAG, 0, .TRUE. )
      CALL PLSBAR( PBMSBR(1,7), PBMSBR(2,7), PBMSBR(3,7),
     1             PBMSBR(4,7), 1, 1, PBMBXM(1,13) )
C
C
C     Set only the first source selected
C
      N = 0
      DO 10 I = 1, NSRC
         IF( PSOBCK(I) .EQ. 1 .AND. N .EQ. 0 ) THEN
            N = 1
            IF( PSOBM .NE. I) PBMEXE = .TRUE.
            PSOBM = I
         END IF
 10   CONTINUE
C
C     Antennas and Source
C
      YT = PPSDIM(3) - 0.02
      CALL PGNUMB( PSTNUM, 0, 1, STRING, N )
      MESSAG = STRING(1:N)//' Antennas and '//SRCNAME(PSOBM)
      CALL PGPTXT( XT, YT, 0.0, 0.0, MESSAG )
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
