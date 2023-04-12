      SUBROUTINE GETRFX (TLO1, ST, LOSIZE, RF1L, RF2L, IRF1, IRF2)
C
C     Routine called by test_freq.f to calculate rf1 and rf2 list values 
C
      INCLUDE 'schfreq.inc'
C
      INTEGER           I, IRF1, IRF2, LOSIZE
      DOUBLE PRECISION  TLO1(MFREQ)
      INTEGER           RF1L(MFREQ), RF2L(MFREQ)
      INTEGER           ST
C
C     Parameters:
C       TLO1 - Pre-calculated list of lo settings
C       ST - Starting index
C       LOSIZE - Size of LO setting list (Last index)
C       RF1L - List to store rf1 values
C       RF2L - List to store rf2 values
C       IRF1 - Value of sideband to calculate rf1 from lo
C       IRF2 - Value of sideband to calculate rf2 from lo
C
      DO I = ST, LOSIZE
          RF1L(I) = TLO1(I) + IRF1
          RF2L(I) = TLO1(I) + IRF2
      END DO
C      
C
      END