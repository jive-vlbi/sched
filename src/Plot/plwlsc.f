      SUBROUTINE PLWLSC( CH, MODE, AP )
C
C     Routine for sched that plot a Text Area
C     with value for time offset
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      CHARACTER        CH, CH1
      INTEGER          MODE, AP, OSCALE, I
      DOUBLE PRECISION DPVAL
C ----------------------------------------------------------------------
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     Clear Text Area or Update scale value 
C
      IF( MODE .EQ. 0 ) THEN
C
C        Clear the big tex area to avoid overlap
C
         CALL PGSCI( PPNCOL(1) )
         CALL PGRECT( PXYTXT(1,3), PXYTXT(2,3),
     1                PXYTXT(3,3), PXYTXT(4,3) )
      ELSE
C
C        save the Old scale value
C
         OSCALE = PXYWLE        
C
C        Update scale value
C
         IF( CH .EQ. 'A' ) THEN
            PXYWLE = PXYWLE + 3
         ELSE IF( CH .EQ. 'D' .OR. CH .EQ. 'X' ) THEN
            PXYWLE = PXYWLE - 3
         END IF
C
C        Check scale value
C
         IF( PXYWLE .LT. 0 ) PXYWLE = 6 
         IF( PXYWLE .GT. 6 ) PXYWLE = 0
C
C        Set new X/Y Axis Value
C
         OSCALE = OSCALE - PXYWLE
         DO 10 I=1,4
           DPVAL = PXSVAL(13, I, 1) * 10.**OSCALE
           PXSVAL(13,I,1) = DPVAL
 10      CONTINUE
C
         CALL PLAXSX()
         CALL PLAXSY()
C
      END IF
C
C     Plot Text Area and value
C
      IF( PXYTYP(PXYBCK(AP)) .EQ. 'Wv' ) THEN
C
C        ASCII part of the label
C
         IF( PXYWLE .EQ. 3 ) THEN
            CH1 = 'K'
            PXYEXP = 7
         ELSE IF( PXYWLE .EQ. 6 ) THEN
            CH1 = 'M'
            PXYEXP = 4
         ELSE
            CH1 = ' '
            PXYEXP = 9
         END IF
C
C        Plot the label with the lamda symbol = 637
C
         CALL PLSTXS( PXYTXT(1,5), PXYTXT(2,5), PXYTXT(3,5),
     1                PXYTXT(4,5), CH1, 0, .FALSE., 637 )
C
      ELSE
C
C        Set Default Axis Value Exponent
C
         PXYEXP = 4
      END IF
C
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
