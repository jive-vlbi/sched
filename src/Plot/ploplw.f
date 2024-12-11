      SUBROUTINE PLOPLW( MODE )
C
C     Routine for sched that check and set line width 
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
            CALL PGNUMB( PLYLW(I), 0, 1, ANUM, J )
            CALL PLSTXT( PLWTXT(1,I), PLWTXT(2,I), PLWTXT(3,I),
     1                   PLWTXT(4,I), ANUM, 1, .FALSE. )
 10      CONTINUE
C
C     Check and set selected line width
C
      ELSE
         I = MODE
         IF( PLYLW(I) .GT. 10 ) PLYLW(I) = 1
         IF( PLYLW(I) .LT. 1  ) PLYLW(I) = 10
         CALL PGNUMB( PLYLW(I), 0, 1, ANUM, J )
         CALL PLSTXT( PLWTXT(1,I), PLWTXT(2,I), PLWTXT(3,I),
     1                   PLWTXT(4,I), ANUM, 1, .FALSE. )
      END IF
C
      RETURN
      END
