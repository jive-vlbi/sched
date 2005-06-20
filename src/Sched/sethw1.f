      SUBROUTINE SETHW1( KS )
C
C     Routine for SCHED that sets some hardware items that can 
C     be set at the time this routine is called by SETDEFS.
C
C     Removed from SETCHAN as part of effort to isolate hardware
C     dependencies.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
C
      INTEGER           ICH, KS
C ---------------------------------------------------------------------
C     If the polarization is now known and RCHAN and LCHAN were set,
C     the IFCHAN can now be set.
C
      DO ICH = 1, NCHAN(KS)
         IF( POL(ICH,KS)(1:3) .EQ. 'RCP' .AND. RCHAN(KS) .NE. ' ' .AND.
     1       IFCHAN(ICH,KS) .EQ. ' ' ) THEN
            IFCHAN(ICH,KS) = RCHAN(KS)
         END IF
         IF( POL(ICH,KS)(1:3) .EQ. 'LCP' .AND. LCHAN(KS) .NE. ' ' .AND.
     1       IFCHAN(ICH,KS) .EQ. ' ') THEN
            IFCHAN(ICH,KS) = LCHAN(KS)
         END IF
      END DO
C
C     Not much here yet.
C
      RETURN
      END
