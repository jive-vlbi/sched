      SUBROUTINE VXRL16(HEAD)
      IMPLICIT NONE
C
C     Routine specific for the VEX extension of SCHED. 
C     Writes the Roll table for 16 track roll
C     By Huib Jan van Langevelde, JIVE 011200
C     Uses the revised, reverse roll
C
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 

      INTEGER HEAD

      INTEGER NGROUP, IGROUP, ITRACK, ALTGRP, I, J, JALT
      INTEGER ROLLTO, MINTRK
      INTEGER LEN1
      CHARACTER LINE*132, DEL*1
      
      LINE = ' '
      NGROUP = 4          
C
C     loop groups, still 4 for 16 roll, but after 8 group changes
C
      DO IGROUP = 1, NGROUP
C
C        write columns, maxtrack depends on group we're in
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
C           let's roll, but be carfull out there!
C
            DO J = 1, 16
               DEL = COL
               IF( J .EQ. 16 ) DEL = SEP
C
C                    out track switches to other group after 8
C
               IF( J .LE. 8 ) THEN
                  ALTGRP = IGROUP
                  JALT = J
               ELSE       
                  IF( MOD (IGROUP, 2) .EQ. 0 ) THEN
                     ALTGRP = IGROUP - 1
                     JALT = J - 8
                  ELSE
                     ALTGRP = IGROUP + 1
                     JALT = J - 8
                  END IF
               ENDIF
C
C              now it should be the track
C
               IF( ALTGRP .LE. NGROUP/2 ) THEN 
C
C              this is simply itrack+2*(JALT-1) for even 
C
                  ROLLTO = (ALTGRP-1)*2*32/NGROUP+I*2-(JALT-1)*2
                  MINTRK = (ALTGRP-1)*2*32/NGROUP + 2
               ELSE
C
C              and for odd it is:
C
                  ROLLTO = (ALTGRP-1-NGROUP/2)
     1                *2*32/NGROUP+I*2-(JALT-1)*2+1
                  MINTRK = 
     1                (ALTGRP-1-NGROUP/2)
     2                *2*32/NGROUP+2+1
               ENDIF
               IF( ROLLTO .LT. MINTRK)  ROLLTO=ROLLTO+2*32/NGROUP
C
C              so let's write it out...
C
               WRITE( LINE(25+(J-1)*3:24+J*3), '( I2, A1 )' )
     1             ROLLTO, DEL
            END DO
            WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
         END DO
         WRITE( IVEX, '( A1 )' ) COM

      END DO
      
      RETURN
      END
