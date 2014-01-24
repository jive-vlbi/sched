      SUBROUTINE VXWRST
C 
C     Routine specific for the VEX extension of SCHED. 
C     Writes a specific section of the VEX file 
C     In this case the ST = $STATION section 
C     By H.J. van Langevelde, JIVE, 300496 
C     Added pointing_sector.  RCW.  Jan. 7, 2014.
C     Removed pointing_sector - belongs in ANTENNA section.  Jan 23, 2014 RCW
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
      INCLUDE 'vxlink.inc' 
C      
      INTEGER   ISTA, ISCAT
      INTEGER   LEN1
C ----------------------------------------------------------------------
C
C     loop through antennas for STATION section
C
      WRITE( IVEX, '( A, A1 )' ) '$STATION', SEP
      DO ISTA = 1, NSTA
         ISCAT = STANUM(ISTA)
         WRITE( IVEX, '( A1 )' ) COM
         WRITE( IVEX, '( A, A, A1 )' ) 'def ',
     1           STCODE(ISCAT)(1:LEN1(STCODE(ISCAT))), SEP
C
C        there is a difference between site and antenna
C        both are not mandatory, but we have things for both
C        do not write CLOCK section or station ID for now
C
         WRITE( IVEX, '( 5X, A, A, A1 )' ) 'ref $SITE = ',
     1       SILINK(ISTASI(ISTA))(1:LEN1(SILINK(ISTASI(ISTA)))), SEP
         WRITE( IVEX, '( 5X, A, A, A1 )' ) 'ref $ANTENNA = ',
     1       ANLINK(ISTAAN(ISTA))(1:LEN1(ANLINK(ISTAAN(ISTA)))), SEP
         WRITE( IVEX, '( 5X, A, A, A1 )' ) 'ref $DAS = ',
     1       DALINK(ISTADA(ISTA))(1:LEN1(DALINK(ISTADA(ISTA)))), SEP
C
         WRITE( IVEX, '( A, A1 )' ) 'enddef',SEP
C
      END DO
      WRITE( IVEX, '( A )' ) COMLIN
      RETURN
      END
