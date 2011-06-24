      SUBROUTINE PLOPIO( MODE, IERR )
C
C     Routine for sched that set some layout parameters 
C     MODE: 0 set the parameters to default
C           1 read the parameters from file
C           2 write parameters to file
C
      INCLUDE 'plot.inc'
C
      CHARACTER        OPTFIL*80, TMPFIL*80
      INTEGER          MODE, IERR, LUNOPT
C ----------------------------------------------------------------------
C
      IERR   = 0
C
C     MODE = 0 ; Set the default options
C
      IF( MODE .EQ. 0 ) THEN
         PLYLW(1) = 5
         PLYLW(2) = 3
         PLYAW(1) = 5
         PLYAW(2) = 3
         PLYBG(1) = 12
         PLYBG(2) = 16
         PLYBG(3) = 12
         PLYCT(1,1) = 2
         PLYCT(2,1) = 4
         PLYCT(3,1) = 6
         PLYCT(4,1) = 8
         PLYCT(5,1) = 7
         PLYCT(6,1) = 5
         PLYCT(1,2) = 4
         PLYCT(2,2) = 4
         PLYCT(3,2) = 6
         PLYCT(4,2) = 7
         PLYCT(5,2) = 11
         PLYCT(6,2) = 12
         RETURN
      END IF
C
      LUNOPT = 92
C
C     Try to set the preferences file name relative to user
C     root directory. If fail set the file name relative to
C     the working directory.
C
      TMPFIL = '$HOME/.schedrc'
      OPTFIL = TMPFIL
C
      CALL ENVIR( TMPFIL )
      IF( TMPFIL .EQ. OPTFIL) THEN
C                          The expansion of $HOME failed.
         OPTFIL = '.schedrc'
      ELSE
C                          The expansion succedes.
         OPTFIL = TMPFIL
      END IF
C
C     Try to open the preferences file to Read/Write to avoiding
C     the creation error if the file already exist
C
 100  FORMAT(17I4)
C
      OPEN( UNIT=LUNOPT, FILE=OPTFIL, FORM='FORMATTED',
     1      STATUS='UNKNOWN', ERR=500 )
      REWIND( UNIT=LUNOPT )
C
C     MODE = 1 ; Read the default options from configuration
C                file.
C
      IF( MODE .EQ. 1 ) THEN
         READ( LUNOPT, 100, ERR=500, END=500 ) PLYLW(1), PLYLW(2),
     1           PLYAW(1), PLYAW(2), PLYBG(1), PLYBG(2), PLYBG(3),
     2           PLYCT(1,1), PLYCT(2,1), PLYCT(3,1), PLYCT(4,1),
     3           PLYCT(5,1), PLYCT(6,1), PLYCT(1,2), PLYCT(2,2),
     4           PLYCT(3,2), PLYCT(4,2), PLYCT(5,2), PLYCT(6,2)
C
C        Check the values
C
         IF( PLYLW(1) .EQ. 0 ) PLYLW(1) = 5
         IF( PLYLW(2) .EQ. 0 ) PLYLW(2) = 3
         IF( PLYAW(1) .EQ. 0 ) PLYAW(1) = 5
         IF( PLYAW(2) .EQ. 0 ) PLYAW(2) = 3
         IF( PLYBG(1) .EQ. 0 ) PLYBG(1) = 12
         IF( PLYBG(2) .EQ. 0 ) PLYBG(2) = 16
         IF( PLYBG(3) .EQ. 0 ) PLYBG(3) = 12
         IF( PLYCT(1,1) .EQ. 0 ) PLYCT(1,1) = 2
         IF( PLYCT(2,1) .EQ. 0 ) PLYCT(2,1) = 4
         IF( PLYCT(3,1) .EQ. 0 ) PLYCT(3,1) = 6
         IF( PLYCT(4,1) .EQ. 0 ) PLYCT(4,1) = 8
         IF( PLYCT(5,1) .EQ. 0 ) PLYCT(5,1) = 7
         IF( PLYCT(6,1) .EQ. 0 ) PLYCT(6,1) = 5
         IF( PLYCT(1,2) .EQ. 0 ) PLYCT(1,2) = 4
         IF( PLYCT(2,2) .EQ. 0 ) PLYCT(2,2) = 4
         IF( PLYCT(3,2) .EQ. 0 ) PLYCT(3,2) = 6
         IF( PLYCT(4,2) .EQ. 0 ) PLYCT(4,2) = 7
         IF( PLYCT(5,2) .EQ. 0 ) PLYCT(5,2) = 11
         IF( PLYCT(6,2) .EQ. 0 ) PLYCT(6,2) = 12
C     
C
C     MODE = 2 ; Write the default options to configuration
C                file.
C
      ELSE
         WRITE( LUNOPT, 100, ERR=500 ) PLYLW(1), PLYLW(2),
     1           PLYAW(1), PLYAW(2), PLYBG(1), PLYBG(2), PLYBG(3),     
     2           PLYCT(1,1), PLYCT(2,1), PLYCT(3,1), PLYCT(4,1),
     3           PLYCT(5,1), PLYCT(6,1), PLYCT(1,2), PLYCT(2,2),
     4           PLYCT(3,2), PLYCT(4,2), PLYCT(5,2), PLYCT(6,2)
C
      END IF
      GOTO 600
C
 500  IERR = 1
 600  CLOSE( UNIT=LUNOPT )
      RETURN
      END
