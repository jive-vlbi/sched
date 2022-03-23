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

            DO I = 1, MALIAS
C
C           denoted by '*'
C
               IF( CSUSED(I,ISRC) .EQ. '*' ) THEN
C
C              Count and warn if too many for PCFS
C              See email exchange of June 26, 2013 for change to 1000.
C
                  NVXSRC = NVXSRC + 1
                  IF ( NVXSRC .GT. 1000 ) THEN
                     WRITE( MSGTXT, '( A )' ) 
     1                   'VXWRSU: WARNING: More than 1000 sources'// 
     2                   ' in this schedule. This VEX will NOT run'//
     3                   ' on the Field System!'
                     CALL WLOG( 1,MSGTXT)
                  END IF
C
                  TMPSRC = SOURCE(I,ISRC)
                  CALL VXSTNM(TMPSRC,.FALSE.)
                  WRITE( IVEX, '( A1 )' ) COM
                  WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1                 TMPSRC(1:LEN1(TMPSRC)), SEP 
                  FOUND = .TRUE.
                  CALL VXSUDT( ISRC, I )
C
                  WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP  
               END IF
            END DO
C
C           Check if found.  
C           (This comment was also here, but doesn't seem to 
C           apply - RCW Mar 2012):, then write other coordinates in comments
C
C           Output text changed Mar 2012  RCW.
C
            IF( .NOT. FOUND ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A )' )
     1            'VXWRSU: A catalog source is marked as used but ',
     2                  'the alias used is not identified.'
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, I5, 2A )' )
     1            'VXWRSU: Source number is: ', ISRC, 
     2            '  The first alias is: ', 
     3            SOURCE(1,ISRC)
               CALL WLOG( 1, MSGTXT )
C                
               CALL ERRLOG('VXWRSU: Probable program bug.'//
     1             '  Please report.' )
            END IF
         END IF
      END DO
      WRITE( IVEX, '( A )' ) COMLIN
      RETURN
      END

