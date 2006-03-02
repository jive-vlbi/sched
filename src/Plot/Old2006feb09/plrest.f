      SUBROUTINE PLREST
C
C     Routine for sched that reinitialize some default
C     parameters after a restart.
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      INTEGER          I
      DOUBLE PRECISION ANLONG
C ----------------------------------------------------------------------
C
C     Reset Menu pointer to rewrite current panel
C
      PMNBCK(2) = 0
C
C     ReSet Sources Selection
C
      PSONUM = 0
      DO 40 I=1,NSRC
         IF( PSOBCK(I) .EQ. 1 ) PSONUM = PSONUM + 1
 40   CONTINUE
C
C     Reset Sources Scroll Pointers
C
      IF( NSRC .GT. PSOPLT ) THEN
         PSOSCR = .TRUE.
      ELSE
         PSOSCR = .FALSE.
         PSOCNT = 0
      END IF
C
C     ReSet Station Selection
C
      PSTNUM = 0
      DO 60 I=1,NSTA
         IF( PSTBCK(I,1) .EQ. 1 ) PSTNUM = PSTNUM + 1
 60   CONTINUE
C
C     Reset Stations Scroll Pointers
C
      IF( NSTA .GT. PSTPLT ) THEN
         PSTSCR = .TRUE.
      ELSE
         PSTSCR = .FALSE.
         PSTCNT = 0
      END IF
C
C     ReSet Schedule Axis Value for UT Axis Type
C
      CALL PLTIME( 'UT', 0.D0, TFIRST, TEND, TWOPI, RADHR,
     1              PUTVAL, PANDAY, PADAYS )
C
C     ReSet Schedule Axis Value for GST Axis Type
C
      CALL PLTIME( 'GST', 0.D0, TFIRST, TEND, TWOPI, RADHR,
     1             PSGVAL, PANDAY, PADAYS )
C
C     ReSet Schedule Axis Value for LST Axis Type
C
      IF( POFVAL(2) .EQ. 0 ) THEN
          ANLONG = 0.D0
      ELSE
          ANLONG = LONG(STANUM(POFVAL(2)))
      END IF
      CALL PLTIME( 'LST', ANLONG, TFIRST, TEND, TWOPI, RADHR,
     1             PLSVAL, PANDAY, PADAYS )
C
C     ReSet all defaults value for axis type
C
C      CALL PLAXIN
C
C     ReSet Exec Beam Pointers 
C
      PBMEXE = .TRUE.
C
      RETURN
      END
