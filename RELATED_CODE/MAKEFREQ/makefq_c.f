      PROGRAM MAKEFREQ
C
C     Make freq.dat entries based on possible LO settings.
C     This version is for the new 4-8 GHz receiver.
C
C     This can make entries for one or the other of the freq.dat
C     for the old system or freq_RDBE.dat for the new system.
C     The difference is the IF range 500-1000 MHz (old) or 
C     512-1024 MHz (new).  The choice is hard-wired in the code.
C     Typically this program will only get run once for each case
C     to build the catalog so a recompile is not hard.
C
C     There are two input filters on the IF converter:
C        3900-5900 MHz
C        5600-7900 MHz
C
C     The mixes can be upper or lower sideband.  The LO should
C     be no more than 1000 MHz into the filter band or there
C     will be aliasing.  Preferably it will be less.
C
C     The IF converter can put out two pairs (4 total) of IF
C     signals with 2 independent tunings.  This makes for a lot
C     of combinations.  Some day, SCHED might be able to tune
C     separately, but for now, just be gross.
C
C     The settings are based on my spreadsheet on my laptop "lodore"
C     in /Users/cwalker/VLBA/Upgrade_technologies/C-band-frequencies.ods
C     Some account is taken of aliased signals.
C
C     Downweight the priority of the 4.6-5.1 window with 5.6 LO vs the
C     one with 4.1 LO so legacy projects don't change.
C
C
      INTEGER   NF, IF, JF, KF, IFAC, IFBD
      PARAMETER (NF=23)
      INTEGER   PRIO(NF), SINDEX(NF)
      REAL      LO(NF), SB(NF)
      REAL      RF1(NF), RF2(NF), SRF(NF)
      REAL      RFN1(NF), RFN2(NF)
      REAL      RFO1(NF), RFO2(NF)
      CHARACTER NAME(NF)*7
      DATA  LO   /   3.4,    3.6,    3.9,    4.1,    4.4, 
     1               5.4,    5.6,    5.9,    6.1,    6.4,    6.6,  
     2               4.9,    5.1,    5.4,    5.6,    5.9,    6.1,
     3               7.4,    7.6,    7.9,    8.1,    8.4,    8.6 /
      DATA  SB   /   1.0,    1.0,    1.0,    1.0,    1.0, 
     1               -1.,    -1.,    -1.,    -1.,    -1.,     -1.,
     2               1.0,    1.0,    1.0,    1.0,    1.0,     1.0,
     3               -1.,    -1.,    -1.,    -1.,    -1.,     -1. /
      DATA  PRIO /     1,      0,      0,      0,      2,
     1                 2,      1,      0,      0,      0,      2,
     2                 2,      0,      0,      0,      0,      2,
     3                 2,      0,      0,      0,      0,      1 /
C
C     New system IF upper and lower ends.
C
      DATA  RFN1 / 3912.0, 4112.0, 4412.0, 4612.0, 5050.0,
     1             4376.0, 4576.0, 4876.0, 5076.0, 5376.0, 5576.0,
     2             5600.0, 5612.0, 5912.0, 6112.0, 6412.0, 6800.0,
     3             6376.0, 6576.0, 6876.0, 7076.0, 7376.0, 7576.0 /
      DATA  RFN2 / 4424.0, 4624.0, 4924.0, 5124.0, 5424.0,
     1             4750.0, 5088.0, 5388.0, 5588.0, 5888.0, 5900.0,
     2             5924.0, 6124.0, 6424.0, 6624.0, 6924.0, 7124.0,
     3             6700.0, 7088.0, 7388.0, 7588.0, 7888.0, 7900.0 /
C
C     Old system IF upper and lower ends.
C
      DATA  RFO1 / 3900.0, 4100.0, 4400.0, 4600.0, 5050.0,
     1             4400.0, 4600.0, 4900.0, 5100.0, 5400.0, 5600.0,
     2             5600.0, 5600.0, 5900.0, 6100.0, 6400.0, 6800.0,
     3             6400.0, 6600.0, 6900.0, 7100.0, 7400.0, 7600.0 /
      DATA  RFO2 / 4400.0, 4600.0, 4900.0, 5100.0, 5400.0,
     1             4750.0, 5100.0, 5400.0, 5600.0, 5900.0, 5900.0,
     2             5900.0, 6100.0, 6400.0, 6600.0, 6900.0, 7100.0,
     3             6700.0, 7100.0, 7400.0, 7600.0, 7900.0, 7900.0 /

C -------------------------------------------------------------
C     Pick new or old by commenting the unwanted lines.
C
      DO IF = 1, NF
C         RF1(IF) = RFO1(IF)
C         RF2(IF) = RFO2(IF)
         RF1(IF) = RFN1(IF)
         RF2(IF) = RFN2(IF)
      END DO
C
C     Sort by RF1
C
      DO IF = 1, NF
         SINDEX(IF) = IF
         SRF(IF) = RF1(IF)
      END DO
      CALL SORTCW( NF, SRF, SINDEX )
C
C     Write the results for AC only.
C
      DO IF = 1, NF
         JF = SINDEX(IF)
         NAME(JF) = 'vc_'//CHAR(96+JF)
         WRITE( *, '( A, A, A, I1 )' )
     1      'Name = ', NAME(JF), '  Station = VLBA'//
     2      '   Priority = ', PRIO(JF)
         WRITE( *, '( A, F6.0, A, F6.0, A )' )
     1      '  rf1    = ', RF1(JF), ', ', RF1(JF),
     2      '   ifname = A,    C'
         WRITE( *, '( A, F6.0, A, F6.0, A, F4.1 )' )
     1      '  rf2    = ', RF2(JF), ', ', RF2(JF),
     2      '   fe    = ''6cm'', ''6cm''    syn(2) = ', 
     3      LO(JF)
         WRITE( *, '( A, F6.0, A, F6.0, A, F4.1 )' )
     1      '  pol    =   RCP,   LCP    lo1    = ',
     2      LO(JF) * 1000.0, ', ', LO(JF) * 1000.0
         WRITE( *, '( A )' ) ' /' 
      END DO
C
C     Now write the 4 IF options using AC and BD.  To cut the
C     options down, write only those with RF1(AC)<RF1(BD) and
C     don't do the ones where the two are the same.  I'm not
C     eliminating the strongly overlapped ones because, with
C     opposite sidebands, avoiding the crossover points might be
C     possible in one and not the other.
C
C     For the priority, add 1 to discourage use of the dual 
C     sets if they are not needed.
C
      DO IFAC = 1, NF
         DO IFBD = 1, NF
            JF = SINDEX(IFAC)
            KF = SINDEX(IFBD)
            IF( RF1(KF) .GT. RF1(JF) ) THEN
               NAME(JF) = 'vc_'//CHAR(96+JF)//CHAR(96+KF)
               WRITE( *, '( A, A, A, I1 )' )
     1            'Name = ', NAME(JF), '  Station = VLBA'//
     2            '   Priority = ', PRIO(JF) + PRIO(KF) +1
               WRITE( *, '( A, F6.0, A, F6.0, A, F6.0, A, F6.0, A )' )
     1            '  rf1    = ', RF1(JF), ', ', RF1(JF), ', ', 
     2            RF1(KF), ', ', RF1(KF), 
     3            '   ifname = A, C, B, D'
               WRITE( *, '( A, F6.0, A, F6.0, A, F6.0, A, F6.0, A )' )
     1            '  rf2    = ', RF2(JF), ', ', RF2(JF), ', ',
     2            RF2(KF), ', ', RF2(KF), 
     3            '   fe  = ''6cm'', ''6cm'', ''6cm'', ''6cm'''
               WRITE( *, '( A, F4.1, A, F4.1 )' )
     1            '  syn(2) = ', LO(JF), '   syn(1) = ', LO(KF) 
               WRITE( *, '( A, F6.0, A, F6.0, A, F6.0, A, F6.0 )' )
     1            '  pol    =   RCP,   LCP,   RCP,  LCP      lo1 = ',
     2            LO(JF) * 1000.0, ', ', LO(JF) * 1000.0, ', ',
     3            LO(KF) * 1000.0, ', ', LO(KF) * 1000.0
               WRITE( *, '( A )' ) ' /' 
            END IF
         END DO
      END DO

      STOP
      END
