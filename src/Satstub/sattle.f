      SUBROUTINE SATTLE( ISAT, ISCN, ISTA, INSTRU, RA, DEC, 
     1                  DRA, DDEC, T0, DISTAU )
C
C     This is the stub for the satellite tracking routine that reads
C     TLE files.  It will complain and die if it is called.
C
      INTEGER            ISAT, ISCN, ISTA
      CHARACTER          INSTRU*(*)
      DOUBLE PRECISION   RA, DEC, DRA, DDEC, T0, DISTAU
C --------------------------------------------------------------
      CALL ERROR( 'SATTLE stub: TLE based satellite tracking routine '//
     1   'called but not supported in this installation of SCHED.' )
      RETURN
      END
