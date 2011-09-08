      SUBROUTINE VXWRHP
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the HP = $HEAD_POSITION section 
C     By H.J. van Langevelde, JIVE, 300496 
C
C     Tape stuff removed July 22, 2010  RCW
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C 
      INTEGER   IHP, KS, ASTAT, IMODE
      INTEGER   LEN1
C ----------------------------------------------------------------------
C
C     Write the HEADPOS sections, this is fixed
C
      WRITE( IVEX, '( A, A1 )' ) '$HEAD_POS', SEP   
      DO IHP = 1, NHPVEX
         KS = HPISSET(IHP)
C
C        Find a station in this VEX group
C
         DO IMODE = 1, NMDVEX
            IF ( NSTAHP(IHP,IMODE) .GT. 0 ) THEN 
               ASTAT = ISTAHP(1,IHP,IMODE)
            ENDIF
         END DO
C
C        Removed 2 head test - Tape only.
C
C        Not much to do any more - head positions are history.
C
         IF( USEDISK (ASTAT) ) THEN 
            WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1          HPLINK(IHP)(1:LEN1(HPLINK(IHP))), SEP  
            CALL VXSTLI( IHP, NSTAHP, ISTAHP )
            WRITE( IVEX, '( A1, 4X, A )' ) COM,
     1          ' Head positions irrelevant for Disk: empty def'
            WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
         END IF
      END DO
C
      WRITE( IVEX, '( A )' ) COMLIN
      RETURN
      END
