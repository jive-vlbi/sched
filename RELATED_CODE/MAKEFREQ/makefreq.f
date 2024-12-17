      PROGRAM MAKEFREQ
C
C     Make freq.dat entries based on possible LO settings.
C     This version is for 3mm.
C
C     Oct. 11, 2011.  Modify to allow 512-1024 final 
C     IF by a parameter setting.
C
C     For 3mm:
C
C     Keep Synthesizer 3 in range 10.9 to 13.6 (Hayward, Mar. 1, 
C     2004).  It is multiplied by 6.  Hayward suggests picking
C     a first IF close to 14.2 GHz.
C
C     The 2 cm IF converter is used.  All 4 filters are allowed
C     (Barry, Mar. 1. 2004). The filter is chosen on the basis 
C     of the first baseband channel RF (or first IF for 3mm) 
C     frequency.
C
C     By leaving out the second LO settings that have IF's that
C     cross the filter boundaries, we avoid confusion and insure
C     full 500 MHz bands all the time.  This means leaving out
C     the N.1 (where N is 12, 13, or 14) MHz LO settings.  We
C     are left with a good set of allowed settings that allow
C     any frequency to be reached.  There are still many duplicates.
C     
C     Restrict the RF ranges to fall in: 79.7 to 96.2 GHz
C
C     Bob Hayward (Mar 1, 2004) suggests be
C
C
C Receiver   Nominal      Channel 1      Max range     Good range
C filter  filter range      range
C    1:  11820 - 12980    0   - 12900  11800 - 13000  12100 - 12950
C    2:  12820 - 13980  12900 - 13900  12800 - 14000  12850 - 13950
C    3:  13820 - 14980  13900 - 14900  13800 - 15000  13850 - 14950
C    4:  14770 - 15630  14900 -  inf   14700 - 15700  14800 - 15500
C
      INTEGER  NWLO1, NWLO2, ILO1, ILO2 
      INTEGER  NCOMBO, ISET, JSET, NSET
      PARAMETER (NWLO1=12)
      PARAMETER (NWLO2=11)
      PARAMETER (NCOMBO=NWLO1*NWLO2)
      REAL  WLO1(NWLO1), WLO2(NWLO2)
      REAL  SUMLO(NCOMBO)
      REAL  SYN3(NCOMBO), SYN1(NCOMBO)
      REAL  IF1(NCOMBO), IF2(NCOMBO), SRF(NCOMBO)
      REAL  RF1(NCOMBO), RF2(NCOMBO)
      REAL     FIF1, FIF2
      INTEGER FLAG(NCOMBO), SINDEX(NCOMBO)
      CHARACTER NAME(NCOMBO)*7
      DATA  WLO1 / 10.9, 11.1, 11.4, 11.6, 11.9, 12.1, 
     1             12.4, 12.6, 12.9, 13.1, 13.4, 13.6 /
      DATA  WLO2 / 11.4, 11.6, 11.9,   12.4, 12.6, 12.9,  
     1             13.4, 13.6, 13.9,   14.4, 14.6  /
      DATA  FIF1, FIF2 / 512.0, 1024.0 /
C      DATA  FIF1, FIF2 / 500.0, 1000.0 /
C -------------------------------------------------------------
C     Make all possible combinations.
C
      NSET = 0
      DO ILO1 = 1, NWLO1
         DO ILO2 = 1, NWLO2
            NSET = NSET + 1
            FLAG(NSET) = 0
            SUMLO(NSET) = 6 * WLO1(ILO1)*1000. + WLO2(ILO2)*1000.
            SYN1(NSET) = WLO2(ILO2)
            SYN3(NSET) = WLO1(ILO1)
            IF1(NSET) = SUMLO(NSET) + FIF1 - 6 * WLO1(ILO1)*1000.
            IF2(NSET) = SUMLO(NSET) + FIF2 - 6 * WLO1(ILO1)*1000.
            RF1(NSET) = SUMLO(NSET) + FIF1
            RF2(NSET) = SUMLO(NSET) + FIF2
            NAME(NSET) = 'v3mm_'//CHAR(96+ILO1)//CHAR(96+ILO2)
         END DO
      END DO
C
C     Check for out of bounds.
C
      DO ISET = 1, NSET
         IF( RF1(ISET) .LT. 79700. ) FLAG(ISET) = 1
         IF( RF2(ISET) .GT. 96200. ) FLAG(ISET) = 1
         DO JSET = 1, NSET
            IF( ISET .NE. JSET .AND. RF1(ISET) .EQ. RF1(JSET) ) THEN
               IF( ABS( IF1(ISET) - 14200.0 ) .GT.
     1             ABS( IF1(JSET) - 14200.0 ) ) 
     2            FLAG(ISET) = FLAG(ISET) + 2
            END IF
         END DO
      END DO
C
C     Sort by RF1
C
      DO ISET = 1, NSET
         SINDEX(ISET) = ISET
         SRF(ISET) = RF1(ISET)
      END DO
      CALL SORTCW( NSET, SRF, SINDEX )
C
C     Write the results:
C
      DO ISET = 1, NSET
         JSET = SINDEX(ISET)
         IF( FLAG(JSET) .EQ. 0 ) THEN
C            WRITE( *, '( 2I5, 7F10.2, I4 )' ) 
C     1           ILO1, ILO2, SYN3(JSET), SYN1(JSET), 
C     2           SUMLO(JSET), IF1(JSET), IF2(JSET), 
C     4           RF1(JSET), RF2(JSET), FLAG(JSET)
             WRITE( *, '( A, A, A )' )
     1          'Name = ', NAME(JSET), '  Station = VLBA'//
     2          '   Priority = 0'
             WRITE( *, '( A, F6.0, A, F6.0, A )' )
     1          '  rf1    = ', RF1(JSET), ', ', RF1(JSET),
     2          '   ifname = B,    D'
             WRITE( *, '( A, F6.0, A, F6.0, A, F4.1 )' )
     1          '  rf2    = ', RF2(JSET), ', ', RF2(JSET),
     2          '   fe    = ''3mm'', ''3mm''    syn(1) = ', 
     3          SYN1(JSET)
             WRITE( *, '( A, F6.0, A, F6.0, A, F4.1 )' )
     1          '  pol    =   RCP,   LCP    lo1    = ',
     2          SUMLO(JSET), ', ', SUMLO(JSET), '  syn(3) = ', 
     3          SYN3(JSET)
             WRITE( *, '( A, F6.0, A, F6.0 )' )
     1          '  !  First IF = ', IF1(JSET), ' to ', 
     2          IF2(JSET)
             WRITE( *, '( A )' ) ' /' 
         END IF
      END DO
      STOP
      END
