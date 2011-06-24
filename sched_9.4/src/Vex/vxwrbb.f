      SUBROUTINE VXWRBB
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the BB = $BBC section 
C     By H.J. van Langevelde, JIVE, 300496 
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C 
C     Huib's local variables 
C      
      INTEGER   IBB, KS, ICH, JCH
      INTEGER   LEN1
      LOGICAL NEWFND
C ----------------------------------------------------------------------
C
C     the BBC section is fairly trivial in the SCHED context
C     it connects BBC's to IF's, find out how many
C     Physical number is set to logical number for SCHED
C
      WRITE( IVEX, '( A, A1 )' ) '$BBC', SEP    
      DO IBB = 1, NBBVEX
         KS = BBISSET(IBB)
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1        BBLINK(IBB)(1:LEN1(BBLINK(IBB))), SEP
         CALL VXSTLI( IBB, NSTABB, ISTABB )
C
C        only write for new BBC's
C
         DO ICH = 1, NCHAN(KS)
            NEWFND = .TRUE.
            IF( ICH .GT. 1 ) THEN
               DO JCH = 1, ICH-1
                  IF( BBC(ICH,KS) .EQ. BBC(JCH,KS)) NEWFND = .FALSE.
               END DO
            END IF
            IF( NEWFND ) THEN 
               WRITE( IVEX, '( 5X, A, 1X, A1, A, I2.2, 1X, A1, I3, 
     1             1X, A1, 1X, A1, A, A, A1  )' ) 
     2             'BBC_assign =', 
     3             LNK, 'BBC', BBC(ICH,KS), COL, BBC(ICH,KS), COL, LNK, 
     4             'IF_', IFCHAN(ICH,KS)(1:LEN1(IFCHAN(ICH,KS))), SEP
            END IF
         END DO
C
         WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
      END DO
      WRITE( IVEX, '( A )' ) COMLIN
C
      RETURN
      END
