      SUBROUTINE PLEND
C
C     Routine for sched that that set all default plotting
C     parameters to use in conjunction with the terminal input.
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      INTEGER          I
C ----------------------------------------------------------------------
C
C     Set all Station Select Button (PSTBCK)
C     To select   the plot      buttons ( 1 )
C     To unselect the highlight buttons ( 0 )
C
      DO 10 I=1,PSTMAX
         PSTBCK(I, 1) = 1
         PSTBCK(I, 2) = 0
 10   CONTINUE
C
C     Set Single Baseline to false
C
      PSTBAS = .FALSE.
C
C     Set Plot Sun to false
C
      PXYSUN = .FALSE.
C
C     Set zoom to false
C
      PRZOOM = .FALSE.
      PRDCAL = .FALSE.
      PRDWIN = .FALSE.
C
C     Set all Sources Selected
C
      DO 20 I=1,PSOMAX
         PSOBCK(I) = 1
 20   CONTINUE
C
C     Set time offset to 0
C
      POFVAL(1) = 0
      POFVAL(2) = 0
C
C     Set the type of plot to 0 to permit the set of new page
C    
      POPBCK = 0
C
C     Reset the start/stop experiment to avoid the plot of
C     vertical line mark
C
      PXINI = UNSET
      PXEND = UNSET
C
C     Set the operational mode to terminal. The RESTART
C     operation is processed by terminal interface.
C
      PMTTYT = .TRUE.
C
C     Reset the Line Width, Background Colors and type of device
C
      DO 30 I=1,2
         PLYLW(I) = 7
         PLYAW(I) = 5
 30   CONTINUE
C
      DO 40 I=1,3
         PLYBG(I) = 11
 40   CONTINUE
C
      PFLBCK = 3
C      
      RETURN
      END
