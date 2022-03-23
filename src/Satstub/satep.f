      SUBROUTINE SATEP( ISAT, ISCN, ISTA, INSTRU, RA, DEC, 
     1                  DRA, DDEC, T0, DISTAU )
C
C     This is the stub for the satellite tracking routine.  It
C     will complain and die if it is called.
C
      INTEGER            ISAT, ISCN, ISTA
      CHARACTER          INSTRU*(*)
      DOUBLE PRECISION   RA, DEC, DRA, DDEC, T0, DISTAU
C --------------------------------------------------------------
      CALL ERROR( 'SATEP stub: Satellite tracking routine '//
     1   'called but not supported in this installation of SCHED.' )
      RETURN
      END
