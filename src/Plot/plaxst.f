      SUBROUTINE PLAXST( AXIS )
C
C     Routine for sched that plot an axis value object
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      CHARACTER*(*)     AXIS
      INTEGER           AP, EP, K, I, J
      REAL              XYMIN, XYMAX
C ----------------------------------------------------------------------
C     
C     Set Pointers to Axis Type
C
      IF( AXIS .EQ. 'X' ) THEN
         AP = 1
         EP = 1
      ELSE
         AP = 2
         EP = 3
      END IF
C
C     Set type pointer of the value matrix
C
      K = PXYBCK(AP)
C
C     Set/unset Flag for Sec(z) axis selected and auto/manual
C
      IF( PXYTYP(K) .EQ. 'Sec' ) THEN
         PXYONE = .TRUE.
         J = EP + 1
         IF( PXYSEC ) THEN
            I = 1
         ELSE
            I = 0
            XYMIN = PXSVAL(K,EP,1)
            XYMAX = PXSVAL(K,J,1)
         END IF
         CALL PLMNMX( PXYTYP(K), XYMIN, XYMAX, I )
         PXSVAL(K,EP,1) = XYMIN
         PXSVAL(K,J,1) = XYMAX
      ELSE
         PXYONE = .FALSE.
      END IF
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     Set Axis Value
C
      IF( AXIS .EQ. 'X' ) THEN
         CALL PLAXSX()
         IF( POPTYP(POPBCK) .EQ. 'UV' ) THEN
C
C           Clone Y Axis
C
            PXYBCK(2) = K
            CALL PLSTXT( PXYTXT(1,2), PXYTXT(2,2), PXYTXT(3,2),
     1                   PXYTXT(4,2), PXYTYP(PXYBCK(2)), 0, .TRUE. )
            CALL PLAXSY()
         END IF
      ELSE
         CALL PLAXSY()
         IF( POPTYP(POPBCK) .EQ. 'UV' ) THEN
C
C           Clone X Axis
C
            PXYBCK(1) = K
            CALL PLSTXT( PXYTXT(1,1), PXYTXT(2,1), PXYTXT(3,1),
     1                   PXYTXT(4,1), PXYTYP(K), 0, .TRUE. )
            CALL PLAXSX()
         END IF
      END IF
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
