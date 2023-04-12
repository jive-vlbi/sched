      SUBROUTINE SORTRFX(TMP_FREQTOTAL, FREQTOTAL, SRT, SINDEX)
C
C     Routine for SCHED called by *BANDFREQ classes for 
C     sorting settings by RF values (usually RF1).
C     This class sorts an rf1 values from the least number and also 
C     stores new order of indexes for consistency.
C     This is not really needed since sched does not care about the 
C     order of frequency settings, but it is useful for debugging and 
C     comparing with frequency catalogs.
C
      INCLUDE 'schfreq.inc'
C
      INTEGER           FREQTOTAL
      INTEGER           TMP_FREQTOTAL
      INTEGER           ISET, JSET, JF, KF
      INTEGER           SINDEX(MFREQ), SRT(MFREQ)
C
      DO ISET = TMP_FREQTOTAL, FREQTOTAL
          DO JSET = ISET+1, FREQTOTAL
              IF( SRT(ISET) .GT. SRT(JSET) ) THEN
                  KF = SRT(ISET)
                  SRT(ISET) = SRT(JSET)
                  SRT(JSET) = KF
                  JF = SINDEX(ISET)
                  SINDEX(ISET) = SINDEX(JSET)
                  SINDEX(JSET) = JF
              END IF
          END DO
      END DO
      END