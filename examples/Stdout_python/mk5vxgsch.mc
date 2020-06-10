  COVER INFORMATION 

 Station:    MEDICINA  (Code Mc ) 
 Experiment: Network Monitoring Expt - Mk5 test
 Exp. Code:  MK5VXG  


Schedule Version:       2.10
Processed by SCHED version:  11.60  Release 11.6; Feburary 2020

PI:       Cormac

Address:  JIVE
 
 
 

Phone:    +31-521-596512
EMAIL:    reynolds@jive.nl
Fax:
Phone during observation: +31-521-596512

Observing mode:

Notes:
 
          This is a Mk5 experiment
 

Schedule for MEDICINA  (Code Mc )                                   Page   2
               Network Monitoring Expt - Mk5 test
  UP:  D => Below limits;  H => Below horizon mask;  W => still slewing at end;  blank => Up.
  Early: Seconds between end of slew and start.   Dwell: On source seconds. 
  Disk: GBytes recorded to this point.
  TPStart:  Recording start time.  Frequencies are LO sum (band edge).
  SYNC: Time correlator is expected to sync up.
----------------------------------------------------------------------------------------
Start UT  Source               Start / Stop                 Early    Disk   TPStart
Stop UT                  LST      EL    AZ   HA  UP   ParA  Dwell   GBytes    SYNC
----------------------------------------------------------------------------------------

 --- Tue  25 Feb 2003   Day  56 ---

 Next scan frequencies:  1642.25  1642.25  1650.25  1650.25
 Next BBC frequencies:    347.25   347.25   355.25   355.25
 Next scan bandwidths:      8.00     8.00     8.00     8.00

13 30 00  DA193        00 36 36  33.1 424.5 -5.3     -56.9     0        0   13 30 00
13 52 00  ---          00 58 39  36.7 427.2 -5.0     -58.9  1320       21   13 30 01

13 55 00  DA193        01 01 40  37.2 427.6 -4.9     -59.1   177       21   13 55 00
14 06 00  ---          01 12 42  39.0 429.0 -4.7     -60.0   660       32   13 55 01

14 11 00  DA193        01 17 43  39.8 429.6 -4.6     -60.4   297       32   14 11 00
14 22 00  ---          01 28 44  41.7 430.9 -4.4     -61.3   660       42   14 11 01

14 25 00  3C84         01 31 45  70.1 449.1 -1.8     -72.2   119       42   14 25 00
14 47 00  ---          01 53 48  74.0 453.2 -1.4     -72.0  1320       63   14 25 01

14 50 00  DA193        01 56 49  46.5 434.4 -4.0     -63.4   120       63   14 50 00
15 12 00  ---          02 18 53  50.3 437.2 -3.6     -64.8  1320       85   14 50 01

15 15 00  3C84         02 21 53  79.0 460.7 -1.0     -69.4   119       85   15 15 00
15 37 00  ---          02 43 57  82.8 471.3 -0.6     -62.5  1320      106   15 15 01

15 40 00  DA193        02 46 57  55.2 440.9 -3.1     -66.4   120      106   15 40 00
15 51 00  ---          02 57 59  57.1 442.4 -3.0     -66.9   660      116   15 40 01

15 56 00  DA193        03 03 00  58.0 443.1 -2.9     -67.1   297      116   15 56 00
16 07 00  ---          03 14 02  60.0 444.7 -2.7     -67.6   660      127   15 56 01

16 11 00  3C84         03 18 02  87.0 533.1  0.0      -6.6   128      127   16 11 00
16 33 00  ---          03 40 06  85.3 592.6  0.3      49.1  1320      148   16 11 01

16 37 00  DA193        03 44 07  65.3 449.4 -2.2     -68.2    45      148   16 37 00
16 59 00  ---          04 06 10  69.3 453.5 -1.8     -67.9  1320      169   16 37 01

17 03 00  3C84         04 10 11  80.4 616.3  0.8      67.7    19      169   17 03 00
17 25 00  ---          04 32 14  76.5 623.5  1.2      71.1  1320      190   17 03 01

17 29 00  DA193        04 36 15  74.6 460.8 -1.3     -65.7    19      190   17 29 00
17 40 00  ---          04 47 17  76.5 464.4 -1.1     -64.0   660      201   17 29 01

17 45 00  DA193        04 52 18  77.4 466.3 -1.1     -63.0   297      201   17 45 00
17 56 00  ---          05 03 20  79.2 471.3 -0.9     -59.8   660      212   17 45 01

18 00 00  3C84         05 07 20  70.2 630.7  1.8      72.2    24      212   18 00 00
18 22 00  ---          05 29 24  66.3 634.2  2.2      71.8  1320      233   18 00 01


SETUP FILE INFORMATION:
   NOTE: If DOPPLER, FREQ, or BW were used, see the individual scans for the final BBC settings.


 ======== Setup file: NME-MK5.18CM
   Matching groups in /home/eldering/sched/catalogs/freq_RDBE.dat:
     mc18cm_g        Generated from sess119L5nme.setini, sess119LG.setini, sess219L5nme.setini, sess*

   Setup group:    1         Station: MEDICINA          Total bit rate:   128
   Format: MARK5B            Bits per sample: 2         Sample rate: 16.000
   Number of channels:  4    DBE type: DBBC_DDC         Speedup factor:   1.00

   Disk used to record data.

   1st LO=   1295.00   1295.00   1295.00   1295.00
   Net SB=         U         U         U         U
   IF SB =         U         U         U         U
   Pol.  =      RCP       LCP       RCP       LCP 
   BBC   =         1         9         2        10
   BBC SB=         U         U         U         U
   IF    =        A1        C1        A1        C1

  The following frequency sets based on these setups were used.

   Frequency Set:   1  Setup file default.  Used with PCAL = 1MHz
   LO sum=   1642.25  1642.25  1650.25  1650.25
   BBC fr=    347.25   347.25   355.25   355.25
   Bandwd=      8.00     8.00     8.00     8.00
    Matching frequency sets:   1

   Track assignments are: 
    track1=   2,  6,  4,  8
    barrel=roll_off 

 POSITIONS OF SOURCES USED IN RECORDING SCANS

   Source                         Source position (RA/Dec)                        Error
                        (B1950)             (J2000)             (Date)            (mas)

 * 3C84              03 16 29.567265   * 03 19 48.160096     03 19 59.000217       1.50
   J0319+4130        41 19 51.91692    * 41 30 42.10404      41 31 29.21931        2.86
   0316+413          /home/eldering/sched/catalogs/sources.gsfc
   J0319+41          GSFC 2016a X/S astro solution,  10291 observations.

   0552+398          05 52 01.407168   * 05 55 30.805611     05 55 43.523746       0.00
   J0555+3948        39 48 21.94581    * 39 48 49.16496      39 49 00.05192        0.00
 * DA193             /home/eldering/sched/catalogs/sources.gsfc
   J0555+39          GSFC 2016a X/S astro solution, 415688 observations.



SOURCE SCAN SUMMARY FOR SOURCES LISTED ABOVE

     Scan hours are for recording scans only. 
     Baseline hours are only counted for scans above horizon at both ends.
  Source       Setup file             Frequency sets                  Observing hours
                                   (duplicates not shown)              Scan  Baseline
  3C84       NME-MK5.18CM          1 2 3 4 5 6                       1.833    23.319
  DA193      NME-MK5.18CM          1 2 3 4 5                         2.200    21.814


EFFECT OF SOLAR CORONA

  The solar corona can cause unstable phases for sources too close to the Sun.
  SCHED provides warnings at individual scans for distances less than 10 degrees.
  The distance from the Sun to each source in this schedule is:
    Source         Sun distance (deg) 
   3C84                82.6
   DA193              111.5

  Barry Clark estimates from predictions by Ketan Desai of IPM scattering sizes 
  that the Sun will cause amplitude reductions on the  longest VLBA baselines 
  at a solar distance of 60deg F^(-0.6) where F is in GHz. 
  For common VLBI bands, this is: 
       327 MHz        117. deg 
       610 MHz         81. deg 
       1.6 GHz         45. deg 
       2.3 GHz         36. deg 
       5.0 GHz         23. deg 
       8.4 GHz         17. deg 
      15.0 GHz         12. deg 
      22.0 GHz          9. deg 
      43.0 GHz          6. deg 

