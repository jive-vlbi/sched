      SUBROUTINE FRFADJ( KS, IIF, KF, FRAD1, FRAD2 )
C
C     Routine for SCHED, called by at least FCOMPARE, FMATCH,
C     and FSFREQ that makes small adjustments to FRF1 and
C     FRF2 from the frequency catalog to account for the 
C     shift from 500-1000 MHz for the VLBA DAR to 512-1024 for
C     the RDBE.  KS is the setup file under consideration.  It
C     is used to sense the use of the RDBE.  KF and IIF are
C     the frequency catalog entry and IF number.  On output,
C     FRAD1 and FRAD are either FRF1 and FRF2 or slightly
C     adjusted values.
C
C     To make any adjustments, DBE(KS) must be one of the 
C     the RDBE options.  Also to adjust FRAD1, the difference
C     between FRF1 and FLO1 must be 500 or 1000 MHz from FLO1.  
C     Likewise, to adjust FRAD2, FRF2 must be 500 or 1000 MHz 
C     from FLO1.  Note that any numbers not at the 500 and 1000
C     MHz limits will be kept as is, preserving accounting for
C     filters etc.  Also if new entries are made for the RDBE
C     with the 512-1024 bounds, they will be unmodified.
C
C     This routine is a bit ugly, but it prevents having to 
C     redo the freq.dat table to match the RDBE ranges.
C
C     Note that, if the freq.dat file (or freq_RDBE.dat) has
C     been adjusted for the RDBE, then the tests for 500 or
C     1000 will fail and adjustments will not be made.  This
C     will be the case if using freq_RDBE.dat.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
      INCLUDE   'schfreq.inc'
C
      INTEGER     KS, KF, IIF
      DOUBLE PRECISION FRAD1, FRAD2
      LOGICAL     ADJWARN, CHG
      DATA        ADJWARN / .TRUE. /
      SAVE        ADJWARN
C -------------------------------------------------------------
      CHG = .FALSE.
      IF( DBE(KS)(1:4) .EQ. 'RDBE' ) THEN
C
C        Low frequency edge - first USB, then LSB.
C
         IF( FRF1(IIF,KF) - FLO1(IIF,KF) .EQ. 500.0 ) THEN
            FRAD1 = FRF1(IIF,KF) + 12.0
            CHG = .TRUE.
         ELSE IF( FLO1(IIF,KF) - FRF1(IIF,KF) .EQ. 1000.0 ) THEN
            FRAD1 = FRF1(IIF,KF) - 24.0
            CHG = .TRUE.
         ELSE
            FRAD1 = FRF1(IIF,KF)
         END IF
C
C        High frequency edge - first USB, then LSB.
C
         IF( FRF2(IIF,KF) - FLO1(IIF,KF) .EQ. 1000.0 ) THEN
            FRAD2 = FRF2(IIF,KF) + 24.0
            CHG = .TRUE.
         ELSE IF( FLO1(IIF,KF) - FRF2(IIF,KF) .EQ. 500.0 ) THEN
            FRAD2 = FRF2(IIF,KF) - 12.0
            CHG = .TRUE.
         ELSE
            FRAD2 = FRF2(IIF,KF)
         END IF
C
      ELSE
         FRAD1 = FRF1(IIF,KF)
         FRAD2 = FRF2(IIF,KF)
      END IF
C      write(*,*) 'frfadj: ', ks, iif, kf, frf1(iif,kf), frf2(iif,kf),
C     1      frad1, frad2, ' ', dbe(ks)
C
C     Issue a warning if adjustments are being made.  But only
C     do it once.
C
      IF( CHG ) THEN
         IF( ADJWARN ) THEN
            CALL WRTMSG( 0, 'FRFADJ', 'FRADJWARN' )
            ADJWARN = .FALSE.
         END IF
      END IF      
      RETURN
      END
