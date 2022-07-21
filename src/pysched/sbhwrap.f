      SUBROUTINE SBHWRAP( ISRC, TSCAN, TBASE, ESCAN, EBASE, 
     1     ISET, KFSETS)
Cf2py intent(in) ISRC, ISET
Cf2py intent(out) TSCAN, TBASE, ESCAN, EBASE, KFSETS
C     Wrapper around SBHOURS to make sure f2py knows how much
C     memory to allocate for KFSETS.
      INTEGER ISRC, ISET
      DOUBLE PRECISION   TBASE, TSCAN, TBSTRT, ESCAN, EBASE
      CHARACTER   KFSETS*(78)
      CALL SBHOURS( ISRC, TSCAN, TBASE, ESCAN, EBASE, 
     1     ISET, KFSETS)
      RETURN
      END
