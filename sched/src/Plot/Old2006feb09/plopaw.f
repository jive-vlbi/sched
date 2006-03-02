      SUBROUTINE PLOPAW( MODE )
C
C     Routine for sched that check and set Axis and Label width 
C     MODE: 0 = Initial set for display and printer
C           1 = check and set for display
C           2 = check and set for printer
C
      INCLUDE 'plot.inc'
C
      CHARACTER        ANUM*2
      INTEGER          MODE, I, J
C ----------------------------------------------------------------------
C     
C
C     Set the initial line width
C
      IF( MODE .EQ. 0 ) THEN
         DO 10 I=1,2
            CALL PGNUMB( PLYAW(I), 0, 1, ANUM, J )
            CALL PLSTXT( PAWTXT(1,I), PAWTXT(2,I), PAWTXT(3,I),
     1                   PAWTXT(4,I), ANUM, 1, .FALSE. )
 10      CONTINUE
C
C     Check and set selected line width
C
      ELSE
         I = MODE
         IF( PLYAW(I) .GT. 10 ) PLYAW(I) = 1
         IF( PLYAW(I) .LT. 1  ) PLYAW(I) = 10
         CALL PGNUMB( PLYAW(I), 0, 1, ANUM, J )
         CALL PLSTXT( PAWTXT(1,I), PAWTXT(2,I), PAWTXT(3,I),
     1                   PAWTXT(4,I), ANUM, 1, .FALSE. )
      END IF
C
      RETURN
      END
