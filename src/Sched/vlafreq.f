      SUBROUTINE VLAFREQ( KS, VLO, VFLO, VBP, ERRS )
C
C     Routine for SCHED, called by CHKVLA and PRTSET, that determines
C     the LO sum, VLBI firstlo, and observing band for each VLA IF
C     for VLA observations.  The returned values are arrays of 4, 
C     one for each IF.
C
C     It assumes that all parameters are filled, either by the user
C     or by VLASETF for the defaults.
C
C     See the manual for gory details of the equations for the LO's.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      LOGICAL           ERRS
      INTEGER           KS, VSIDE, I, BWCODE
      DOUBLE PRECISION  VLO(4), VFLO(4), VBP(2,4), FIFA, FIFB
      DOUBLE PRECISION  FFILTL, FFILTH, FLK(4), BBBW
C  -------------------------------------------------------------------
      IF( SDEBUG ) CALL WLOG( 0, 'VLAFREQ starting:' )
C
C     Get the contribution of LO's beyond the first.  These
C     are shared by all systems.
C
      FIFA = VLASYNA(KS) + 900.0D0 + FLUKEA(KS)
      FIFB = VLASYNB(KS) + 800.0D0 + FLUKEB(KS)
C
C     Get the VLALO sums and corresponding VLBI FIRSTLO's based 
C     on the VLA frequencies.
C
      IF( VLABAND(KS)(2:2) .EQ. 'X' .OR. 
     1    VLABAND(KS)(2:2) .EQ. 'U' ) THEN
         VLO(1) = 1000.D0 * VLAFEAB(KS) - FIFA
         VLO(2) = 1000.D0 * VLAFEAB(KS) - FIFB
         VLO(3) = 1000.D0 * VLAFECD(KS) - FIFA
         VLO(4) = 1000.D0 * VLAFECD(KS) - FIFB
         DO I = 1, 4
            VFLO(I) = VLO(I) + 600.D0
         END DO
         VSIDE = -1
      ELSE IF( VLABAND(KS)(2:2) .EQ. 'Q' ) THEN
         VLO(1) = 1000.D0 * ( VLAFEAB(KS) - VLAFECD(KS) ) + FIFA
         VLO(2) = 1000.D0 * ( VLAFEAB(KS) - VLAFECD(KS) ) + FIFB
         VLO(3) = 1000.D0 * ( VLAFEAB(KS) - VLAFECD(KS) ) + FIFA
         VLO(4) = 1000.D0 * ( VLAFEAB(KS) - VLAFECD(KS) ) + FIFB
         DO I = 1, 4
            VFLO(I) = VLO(I) - 600.D0
         END DO
         VSIDE = 1
      ELSE
         VLO(1) = 1000.D0 * VLAFEAB(KS) + FIFA
         VLO(2) = 1000.D0 * VLAFEAB(KS) + FIFB
         VLO(3) = 1000.D0 * VLAFECD(KS) + FIFA
         VLO(4) = 1000.D0 * VLAFECD(KS) + FIFB
         DO I = 1, 4
            VFLO(I) = VLO(I) - 600.D0
         END DO
         VSIDE = 1
      END IF
C
C     To get the bandpass, we need to take into account both the
C     front end and backend filters.
C
C     Get the offsets of the baseband edge from the edge of the
C     50 MHz bands.  This is needed to tell where the front end
C     filters are.
C
      FLK(1) = FLUKEA(KS) - 100.D0
      FLK(2) = FLUKEB(KS) - 200.D0
      FLK(3) = FLUKEA(KS) - 100.D0
      FLK(4) = FLUKEB(KS) - 200.D0
C
C     Loop over the IF's.
C
      DO I = 1, 4
C
C        Get the "lower" (closest to LO) and "upper" edges of band.
C        Note that the 12.5 MHz filter is centered at 27 MHz in band.
C
         IF( FEFILTER(KS)(I:I) .EQ. ' ' .OR. 
     1       FEFILTER(KS)(I:I) .EQ. '0' ) THEN
            FFILTL = VLO(I) + VSIDE * (  0.00D0 - FLK(I) )
            FFILTH = VLO(I) + VSIDE * ( 50.00D0 - FLK(I) )
         ELSE IF( FEFILTER(KS)(I:I) .EQ. '1' ) THEN
            FFILTL = VLO(I) + VSIDE * ( 12.50D0 - FLK(I) )
            FFILTH = VLO(I) + VSIDE * ( 37.50D0 - FLK(I) )
         ELSE IF( FEFILTER(KS)(I:I) .EQ. '2' ) THEN
            FFILTL = VLO(I) + VSIDE * ( 20.75D0 - FLK(I) )
            FFILTH = VLO(I) + VSIDE * ( 33.25D0 - FLK(I) )
         ELSE
            CALL WLOG( 1, 'VLAFREQ: FEFILTER check failed.' )
            ERRS = .TRUE.
C
C           Avoid trouble with uninitialized variables later.
C
            FFILTL = VLO(I) + VSIDE * (  0.00D0 - FLK(I) )
            FFILTH = VLO(I) + VSIDE * ( 50.00D0 - FLK(I) )
         END IF
C
C        Get the baseband bandwidth.
C
         READ( VLABW(KS)(I:I), '( I1 )', ERR=200 ) BWCODE
         GO TO 300
  200       CONTINUE
            CALL WLOG( 1, 'VLAFREQ: Invalid VLABW: ' // VLABW(KS) )
            ERRS = .TRUE.
  300    CONTINUE
         IF( BWCODE .LT. 0 .OR. BWCODE .GT. 9 ) THEN
            CALL WLOG( 1, 'VLAFREQ: VLABW check failed.' )
            ERRS = .TRUE.
         END IF
         BBBW = 50.0D0 / ( 2.D0**BWCODE )
C
C        Now get the edges of the band.
C
         IF( VSIDE .GT. 0 ) THEN
            VBP(1,I) = MAX( FFILTL, VLO(I) )
            VBP(2,I) = MIN( FFILTH, VLO(I) + BBBW )
            IF( VBP(2,I) .LT. VBP(1,I) ) THEN
               CALL WLOG( 1, 'VLAFREQ:  VLA IF blocked by filters!' )
               ERRS = .TRUE.
            END IF
         ELSE
            VBP(1,I) = MIN( FFILTL, VLO(I) )
            VBP(2,I) = MAX( FFILTH, VLO(I) - BBBW )
            IF( VBP(2,I) .GT. VBP(1,I) ) THEN
               CALL WLOG( 1, 'VLAFREQ:  VLA IF blocked by filters!' )
               ERRS = .TRUE.
            END IF
         END IF
C
      END DO
C
      RETURN
      END
