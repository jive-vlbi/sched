      SUBROUTINE VXUNQL( NXX, XXLINK )
C
C     Checks whether the Name has no illegal chars
C     and whether it is unique, else makes it unique
C     we reserve the #I2 at the end of the string to make things
C     unique
C     By H.J. van Langevelde, JIVE, 300496 
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C      
      CHARACTER XXLINK(MAXMOD)*32, NAMORI*32
      INTEGER NXX, LEN1, LPOS, I, SEQPOS, SEQNO
      LOGICAL CHRFND, CHANGED
C ----------------------------------------------------------------------
      IF( DEBUG ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I5, 2X, A )' ) 'VXUNQL: Starting ', 
     1        NXX, XXLINK(NXX)
         CALL WLOG( 1, MSGTXT )
      END IF
C
      CHANGED = .FALSE.
      NAMORI = XXLINK(NXX)
      CALL VXSTNM( XXLINK(NXX), .TRUE. )
C
C     Check if name is unique in this def block
C
      IF( NXX .GT. 1 ) THEN
         DO I = 1, NXX-1
            IF( XXLINK(NXX) .EQ. XXLINK(I) ) THEN
C
C           it's not unique, find out if it has a #seq number
C               
               CHRFND = .FALSE.
               DO LPOS = 1, 32
                  IF( XXLINK(NXX)(LPOS:LPOS) .EQ. '#' ) THEN
                     CHRFND = .TRUE.
                     SEQPOS = LPOS
                  END IF
               END DO
C
C              if it does, increment
C
               IF( CHRFND ) THEN
                  READ( XXLINK(NXX)(SEQPOS+1:SEQPOS+2), 
     1                 '( I2 )' ) SEQNO
C
C                 by adding 1
C
                  IF( SEQNO .LT. 99 ) THEN
                     WRITE( XXLINK(NXX)(SEQPOS+1:SEQPOS+2), 
     1                   '( I2.2 )' ) SEQNO+1
                     CHANGED = .TRUE.
                  ELSE
                     CALL ERRLOG( 'VXUNQL: Passed 99 sequence numbers' )
                  END IF
               ELSE
C
C                 plain name so simply add #02
C                  
                  SEQPOS = LEN1( XXLINK(NXX))+1
                  IF( SEQPOS .LE. 29 ) THEN
                     WRITE( XXLINK(NXX)(SEQPOS:SEQPOS+2), 
     1                    '( A1, I2.2 )' ) '#', 2
                     CHANGED = .TRUE.                  
                  END IF
               END IF
            END IF
C
C        continue loop, name just invented may actually exist already
C
         END DO
      END IF
C
C     now write out a message if we did anything, in Debug mode only 
C
      IF( CHANGED .AND. DEBUG ) THEN
         WRITE( MSGTXT , '( A, A )' )
     1        'VXUNQL: Changed VEX def: ', NAMORI(1:LEN1(NAMORI))
         CALL WLOG( 1, MSGTXT )
         WRITE( MSGTXT , '( A, A )' )
     1        'VXUNQL:              to: ', 
     2        XXLINK(NXX)(1:LEN1(XXLINK(NXX)))
         CALL WLOG( 1, MSGTXT )
      ENDIF
C         
      RETURN
      END
