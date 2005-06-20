      SUBROUTINE SCHSRC( LSCN, ISCN, ISTA, STARTT, STOPT )
C
C     Subroutine for SCHED to get geometry of observation. 
C     Called by SCNGEO (from SCHOPT) and OPTGEO.
C
      INCLUDE   'sched.inc'
C
      INTEGER       ISTA, ISCN, LSCN
      DOUBLE PRECISION   STARTT, STOPT
      CHARACTER     HORCHK*1
C ------------------------------------------------------------------
C     Get geometry at start and end of scan.  
C
      CALL SCHGEO( ISCN, ISTA, STARTT, HA1(ISCN,ISTA), 
     1             EL1(ISCN,ISTA), AZ1(ISCN,ISTA),
     2             LST1(ISCN,ISTA), PA1(ISCN,ISTA) )
C
      CALL SCHGEO( ISCN, ISTA, STOPT, HA2(ISCN,ISTA),
     1             EL2(ISCN,ISTA), AZ2(ISCN,ISTA),
     2             LST2(ISCN,ISTA), PA2(ISCN,ISTA) )
C
C     Adjust azimuth for cable wrap for ALTAZ antennas.
C
      CALL WRAP( ISCN, LSCN, ISTA )
C
C     Check if rising, setting, below horizon, etc.
C
      UP1(ISCN,ISTA) = HORCHK( STANUM(ISTA), HA1(ISCN,ISTA), 
     1           AZ1(ISCN,ISTA), EL1(ISCN,ISTA), SRCNUM(ISCN) )
      UP2(ISCN,ISTA) = HORCHK( STANUM(ISTA), HA2(ISCN,ISTA), 
     1           AZ2(ISCN,ISTA), EL2(ISCN,ISTA), SRCNUM(ISCN) )
C
      RETURN
      END
