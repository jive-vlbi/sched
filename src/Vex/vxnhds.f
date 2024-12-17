      INTEGER FUNCTION VXNHDS( KS )
      IMPLICIT NONE
C 
C     Routine specific for the VEX extension of SCHED. 
C     Returns the number of heads used by  
C     Setup KS
C     By H.J. van Langevelde, JIVE, 011200 
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 

      INTEGER KS
      INTEGER ICH, IP

      VXNHDS = 1
      IF (TWOHEAD) THEN
C
C     Possibly more than 1, say 2
C
         DO ICH = 1, NCHAN(KS)
            DO IP = 1, TAPEMODE(KS)
               IF ( TRACK(ICH,IP,KS) .GE. 64 ) 
     1             VXNHDS = 2
            END DO
         END DO            
C     
      END IF
      RETURN
      END
