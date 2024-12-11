      SUBROUTINE VXRL8(HEAD)
      IMPLICIT NONE
C
C     Routine specific for the VEX extension of SCHED. 
C     Writes the Roll table for 8 track roll
C     By Huib Jan van Langevelde, JIVE 011200
C     Uses the revised, reverse roll
C
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
      
      INTEGER HEAD

      INTEGER NGROUP, IGROUP, ITRACK, I, J
      INTEGER ROLLTO, MINTRK
      INTEGER LEN1
      CHARACTER LINE*132, DEL*1
      
      LINE = ' '
      NGROUP = 32/8
C
C     loop groups, 4 for 8 roll
C
      DO IGROUP = 1, NGROUP
C
C     maximal track number in a group, cf home track below
C
         IF( IGROUP .LE. NGROUP/2 ) THEN
C
C        do even numbers
C
            MINTRK = (IGROUP-1)*2*32/NGROUP + 2
         ELSE
            MINTRK = (IGROUP-1-NGROUP/2)*2*32/NGROUP+2
         ENDIF
C
C     write the columns
C
         DO I = 1, 32/NGROUP
C
C        home track is still simple
C
            IF( IGROUP .LE. NGROUP/2 ) THEN  
C
C           do even numbers
C
               ITRACK = (IGROUP-1)*2*32/NGROUP + I*2
            ELSE
               ITRACK = (IGROUP-1-NGROUP/2)*2*32/NGROUP + I*2 + 1
            ENDIF
            WRITE( LINE(1:24), '( 5X, A, I1, 1X, A1, 1X, I2, 1X, A1 )' ) 
     1          'roll_def = ',HEAD, COL, ITRACK, COL
C        
C        let's roll, but be carfull out there!
C
            DO J = 1, 8
               DEL = COL
               IF( J .EQ. 8 ) DEL = SEP
C           
C           simple formula to determine out track:
C           
               ROLLTO = ITRACK-2*(J-1)
               IF( ROLLTO .LT. MINTRK)  ROLLTO=ROLLTO+2*32/NGROUP
               WRITE( LINE(25+(J-1)*3:24+J*3), '( I2, A1 )' )
     1             ROLLTO, DEL
            END DO
            WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
         END DO
         WRITE( IVEX, '( A1 )' ) COM
      END DO
      
      RETURN
      END
