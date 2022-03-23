C     Include file for BEAM and subroutines.
C
C
C     The multiple commons used in some cases were the result of 
C     restrictions on common size in some compiler (Microsoft?).
C
C     Note that the non-character variables are ordered in the COMMONs
C     with the larger ones (in terms of bytes) first.  This prevents
C     word alignment problems.
C
C     NPBAS must be >= MAXSCN * MAXSTA * 2
C
C     Array dimensions parameters:    
C
      INTEGER           NBASE, NPBAS, KXSIZE, KYSIZE
      INTEGER           MINPIX, MAXPIX
      PARAMETER         (NBASE =1)
      PARAMETER         (NPBAS = 3000 * 100 * 2 )
      PARAMETER         (KXSIZE=256)
      PARAMETER         (KYSIZE=256)
      PARAMETER         (MINPIX=32)
      PARAMETER         (MAXPIX=256)
C      
C     End of BEAM include file.
C
