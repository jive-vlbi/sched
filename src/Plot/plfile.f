      SUBROUTINE PLFILE
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
C     Display FILE area
C
C     To add or modify the options in the FILES Panel, check also
C     the INCLUDE file 'plot.inc' and the soubroutine 'plinit.f'
C
      INTEGER       I, J, K, IS, REV, INDEXR
      REAL          XL, CSIZ, XT, YT
      LOGICAL       PBTR
C ----------------------------------------------------------------------
C
C     Set Font, Radio Buttons Policy and Label offset
C
      CSIZ  = PPNDIM(3)
      PBTR  = .TRUE.
      XL    = 0.02
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     Clear Area if Needed
C
      IF( PRWARN ) THEN
         CALL PGSFS( 1 )
         CALL PGSCI( 0 )
         CALL PGRECT( PPSDIM(1), PPSDIM(2), PPSDIM(3), PPSDIM(4) )
      END IF
C
C     Titles
C
      CALL PGSLW( 5 )
      CALL PGSCF( 1 )
      CALL PGSCH( PPNDIM(3) )
      CALL PGSCI( PPNCOL(7) )
C
C     Save Image
C
      XT = PPSDIM(1)
      YT = PPSDIM(4)
      CALL PGPTXT(XT, YT, 0.0, 0.0,
     1            'Select the Image Format to Save' )
C
C     Save Parameters
C
      YT = PFLBUT(4,2) + 0.03
      CALL PGPTXT(XT, YT, 0.0, 0.0,
     1            'Saves/Load current inputs file' )
C
C     Radio Box
C
      DO 30 I=1,PFLBXM
         REV = 0
         IF ( I .EQ. PFLBCK) REV = 1
         CALL PLBUTS( PBTR, PFLBOX(1,I), PFLBOX(2,I), PFLBOX(3,I),
     1                PFLBOX(4,I), XL, CSIZ, PFLVAL(I), 0, REV)
 30   CONTINUE
C
C     Action Buttons and Text Area
C
      DO 40 I=1,PFLBTM
          CALL PLBUTA( PFLBUT(1,I), PFLBUT(2,I), PFLBUT(3,I),
     1                 PFLBUT(4,I), PFLLAB(I), PPNCOL(1) )
 40   CONTINUE
C
      DO 50 I=1,2
          CALL PLSTXT( PFLTXT(1,I), PFLTXT(2,I), PFLTXT(3,I),
     1                 PFLTXT(4,I), PFLFIL(I+1), 0, .TRUE. )
 50   CONTINUE
C
C     Setup File ALL Text Area and Select box
C
      XL = (PPSDIM(2) - PPSDIM(1) - (0.035 * 4)) * (-1)
C
      CALL PLSTXT( PSFTXT(1,1), PSFTXT(2,1), PSFTXT(3,1),
     1             PSFTXT(4,1), ' ', 0, .TRUE. )
C
      CALL PLBUTX( PSFBXP(1,6), PSFBXP(2,6), PSFBXP(3,6),
     1             PSFBXP(4,6), XL, CSIZ, 'Plot All Setup File',
     2             PPNCOL(7), PSFBCK )
C
C     Setup File Text Scroll Area and Select boxes
C
      CALL PLSTXT( PSFTXT(1,2), PSFTXT(2,2), PSFTXT(3,2),
     1             PSFTXT(4,2), ' ', 0, .TRUE. )
C
      CALL PLSBAR( PSFSBR(1), PSFSBR(2), PSFSBR(3),
     1             PSFSBR(4), 1, 1, PSFBXM )
C
      K = NSETF - ((PSFCNT - 1) * 5)
      IF( K .GT. 5 ) K = 5
      DO 60 I=1,K
         J   = ((PSFCNT - 1) * 5) + I
         IS  = INDEXR( SETFILE(J), '/' ) + 1
         IF( IS .EQ. 0 ) IS = 1
         CALL PLBUTX( PSFBXP(1,I), PSFBXP(2,I), PSFBXP(3,I),
     1                PSFBXP(4,I), XL, CSIZ, SETFILE(J)(IS:),
     2                0, PSFPOI(J) )
 60   CONTINUE
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
