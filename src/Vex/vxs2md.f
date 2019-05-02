      SUBROUTINE VXS2MD( ISET, S2MODE, VALID, NS2USD, IS2USD, DOWARN )
Cf2py intent(in) ISET, DOWARN
Cf2py intent(out) S2MODE, VALID, NS2USD, IS2USD
C 
C     Routine specific for the VEX extension of SCHED. 
C     Figures out S2 mode name and checks validity
C     By H.J. van Langevelde, JIVE, 230996
C     This routine figures out the S2 mode for ISET
C     and returns the cable identification IS2USD
C     Checks wether the mode is a valid S2 mode
C 
      INCLUDE 'sched.inc' 
      INCLUDE 'schset.inc' 
C
      INTEGER ISET, NS2USD
      CHARACTER IS2USD(16)*4, S2MODE*7
      LOGICAL VALID, DOWARN
      CHARACTER THEDAR*5
C
      INTEGER ISTA,I
C ----------------------------------------------------------------------
C     set up defaults
C
      DO I = 1, 16
         IS2USD(I) = ' '
      END DO
C
C     first find out what DAR
C
      DO ISTA = 1, NSTA
         IF( STATION(STANUM(ISTA)) .EQ. SETSTA(1,ISET) )
     1       THEDAR = DAR(STANUM(ISTA))
      END DO
C
C     user channels must be 1, 2, 4, 8, 16
C
      IF( THEDAR .EQ. 'VLBA' .OR. 
     1    THEDAR .EQ. 'VLBAG' ) THEN
         IF( DEBUG ) CALL WLOG( 1,'       Using VXS2VL ')
         CALL VXS2VL( NCHAN(ISET), BBC(1, ISET), 
     1       NETSIDE(1, ISET), SAMPRATE(ISET), BITS(1,ISET), 
     2       TAPEMODE(ISET), S2MODE, VALID, NS2USD, IS2USD )
      ELSE
         WRITE( MSGTXT, '( A, A, A )' )
     1       'VXS2MD: S2 Modes with DAR = ', THEDAR,
     2       ' wiring not implemented, using naive S2 mode '
         IF( DOWARN )
     1       CALL WLOG( 1, MSGTXT ) 
         IF( DEBUG ) CALL WLOG( 1,'       Using VXS2DF ')
         CALL VXS2DF( NCHAN(ISET), SAMPRATE(ISET), BITS(1,ISET), 
     1       TAPEMODE(ISET), S2MODE, VALID, NS2USD, IS2USD )
      END IF 
C
      RETURN
      END
