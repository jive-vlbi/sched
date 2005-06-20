      SUBROUTINE GETSUN
C
C     Calculate the distance from the sun for all sources.  Just
C     getting one number per source.
C
C     Use the low accuracy SLALIB routines.
C
C     Use the times for the extremes of the experiment calculated
C     in SCHTIM.
C
      INCLUDE 'sched.inc'
C
      INTEGER           ISRC
      DOUBLE PRECISION  TMID, RAS, DECS, SLA_DSEP
C --------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'GETSUN starting' )
C
C     Get the position of the sun.
C
      TMID = TFIRST + ( TEND - TFIRST ) / 2.D0
      CALL SUNPOS( TMID, RAS, DECS )
C
C     Get the angular offset from each source.
C
      DO ISRC = 1, MSRC
         SUNDIS(ISRC) = SLA_DSEP( RAP(ISRC), DECP(ISRC),
     1         RAS, DECS ) / RADDEG
      END DO
C
      RETURN
      END







