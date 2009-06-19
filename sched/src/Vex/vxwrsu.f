      SUBROUTINE VXWRSU
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the SU = $SOURCE section 
C     By H.J. van Langevelde, JIVE, 300496 
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C 
      INTEGER   ISRC, I, NVXSRC
      INTEGER   LEN1
      CHARACTER TMPSRC*32
      LOGICAL   FOUND
C ----------------------------------------------------------------------
C
C     Source section
C
      NVXSRC = 0
      WRITE( IVEX, '( A, A1 )' ) '$SOURCE', SEP
C  
      DO ISRC = 1, MSRC      
         IF( SUSED(ISRC) ) THEN

            FOUND = .FALSE.

            DO I = 1, 5
C
C           denoted by '*'
C
               IF( CSUSED(I,ISRC) .EQ. '*' ) THEN
C
C              Count and warn if too many for PCFS
C
                  NVXSRC = NVXSRC + 1
                  IF ( NVXSRC .GT. 300 ) THEN
                     WRITE( MSGTXT, '( A )' ) 
     1                   'VXWRSU: WARNING: More than 300 sources'// 
     2                   ' in this schedule. This VEX will NOT run!'
                     CALL WLOG( 1,MSGTXT)
                  END IF

                  TMPSRC = SOURCE(I,ISRC)
                  CALL VXSTNM(TMPSRC,.FALSE.)
                  WRITE( IVEX, '( A1 )' ) COM
                  WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1                 TMPSRC(1:LEN1(TMPSRC)), SEP 
                  FOUND = .TRUE.

                  CALL VXSUDT( ISRC, I )

                  WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP  
               END IF
            END DO
C
C           Check if found, then write other coordinates in comments
C
            IF( .NOT. FOUND ) CALL ERRLOG('VXWRSU: Schedule source'//
     1          ' name cannot be reproduced')
         END IF
      END DO
      WRITE( IVEX, '( A )' ) COMLIN
      RETURN
      END

