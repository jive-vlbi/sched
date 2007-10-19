      SUBROUTINE PLAXSS( AP, EP )
C
C     Routine for sched that plot the XY axis sign
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      CHARACTER         CH
      INTEGER           AP, EP, K, I
C ----------------------------------------------------------------------
C     
C     Set Pointers to Axis Type
C
      K = PXYBCK(AP)
C
C     XY Axis Sign
C
      IF( PXSTYP(K)(1:1) .EQ. 's' ) THEN
         PASIGN(AP) = .TRUE.
         DO 10 I=EP, EP+1
            CH = '+'
            IF( PXSSGN(K,I) .EQ. -1 ) CH = '-'
            CALL PLSTXT( PSGTXT(1,I), PSGTXT(2,I), PSGTXT(3,I),
     1                   PSGTXT(4,I), CH, 1, PASIGN(AP) )
 10      CONTINUE
      ELSE
         PASIGN(AP) = .FALSE.
      ENDIF
C
      RETURN
      END
