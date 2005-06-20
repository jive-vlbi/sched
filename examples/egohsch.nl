  COVER INFORMATION 

 Station:    VLBA_NL   (Code Nl ) 
 Experiment: Experiment title (include project code)
 Exp. Code:  egOH    


Schedule Version:       1.00                                              
Processed by SCHED version:   6.0  Release: March 2005                    

PI:       PI Name                                                         

Address:  Address (line one)                                              
          Address (line two)                                              
          Address (line three)                                            
                                                                          

Phone:    Telephone number                                                
EMAIL:    e-mail address                                                  
Fax:      Fax number                                                      
Phone during observation: Tel. during observations                        

Observing mode: 18cm spectral line observations                           

Notes:    Special instructions                                            
                                                                          
                                                                          
                                                                          

Schedule for VLBA_NL   (Code Nl )                                   Page   2
               Experiment title (include project code)
  UP:  D => Below limits;  H => Below horizon mask.  blank => Up.
  Early: Seconds between end of slew and start.   Dwell: On source seconds. 
  Tapes: Drive, Index, Start footage / Direction, Head Group, End footage.
  TPStart:  Tape motion start time.  Frequencies are LO sum (band edge).
     Automatic tape allocation specified.  Tape positions are just estimates.
----------------------------------------------------------------------------------------
Start UT  Source               Start / Stop                 Early    Tapes      TPStart
Stop UT                  LST      EL    AZ   HA  UP   ParA  Dwell  (see above)
----------------------------------------------------------------------------------------

 --- Sat   3 Jun 1995   Day 154 ---

 Next scan frequencies:  1612.22  1612.22  1665.34  1665.34  1667.24  1667.24
                         1720.36  1720.36
 Next BBC frequencies:    787.78   787.78   734.66   734.66   732.76   732.76
                          679.64   679.64
 Next scan bandwidths:      0.25     0.25     0.25     0.25     0.25     0.25
                            0.25     0.25

03 08 00  SRC1         13 46 08  10.7  98.0 -5.1     -47.6     0   1  1      0  03 08 00
04 00 00  ---          14 38 17  20.2 107.3 -4.2     -45.4  3120   F  1  10400

04 08 00  SRC1         14 46 18  21.6 108.8 -4.1     -44.9   471   1  2  10400  04 08 00
05 00 00  ---          15 38 27  30.5 119.4 -3.2     -40.5  3120   R  1  14400

05 02 00  CAL1         15 40 27  38.2 132.4 -2.4     -33.4    98   1  2  14400  05 02 00
05 30 00  ---          16 08 32  41.9 140.2 -1.9     -28.5  1680   R  1   8800

05 30 00  SRC1         16 08 32  35.2 126.4 -2.7     -36.9   -19   1  2   8800  05 30 00
05 54 00  ---          16 32 36  38.6 132.6 -2.3     -33.3  1421   R  1   4000

06 02 00  SRC1         16 40 37  39.7 134.8 -2.2     -32.0   472   1  1   4000  06 02 00
06 54 00  ---          17 32 45  45.6 150.6 -1.3     -21.5  3120   F  2   6400


SETUP FILE INFORMATION:
   NOTE: If DOPPLER, FREQ, or BW were used, see the individual scans for the final BBC settings.


 Setup file: egOH.set
   Matches group v20cm_2 in /jop21_1/reynolds/sched_devel/S6.0v84/catalogs/freq.dat
    FD filtered above 1760.  SC filtered above 1740.

   Setup group:    3         Station: VLBA_NL           Total bit rate:   32
   Format: VLBA1:1           Bits per sample: 2         Sample rate:  2.000
   Number of channels:  8    Passes/head pos:   2       Speedup factor:   4.00

   Tape speeds: Low density -  66.665 ips,  High density -  40.00 ips
   Time per pass for  17600 ft, High density tapes is: 01:28:00

   1st LO=   2400.00   2400.00   2400.00   2400.00   2400.00   2400.00   2400.00   2400.00
   Net SB=         U         U         U         U         U         U         U         U
   Pol.  =      RCP       LCP       RCP       LCP       RCP       LCP       RCP       LCP 
   BBC   =         1         2         3         4         5         6         7         8
   BBC SB=         L         L         L         L         L         L         L         L
   IF    =        A         C         A         C         A         C         A         C 

   VLBA FE=     20cm     omit     20cm     omit
   VLBA Synth=  15.4      2.4     15.4

  The following frequency sets based on this setup were used.

  Frequency Set:   3  Based on FREQ, BW, and/or DOPPLER in schedule.  Used pcal sets:   1
   LO sum=    1612.22   1612.22   1665.34   1665.34   1667.24   1667.24   1720.36   1720.36
   BBC fr=     787.78    787.78    734.66    734.66    732.76    732.76    679.64    679.64
   Bandwd=      0.250     0.250     0.250     0.250     0.250     0.250     0.250     0.250

  The following pulse cal sets were used with this setup:

   Pulse cal detection set:   1  PCAL = OFF 
    PCALXB1=   S1    S2    S3    S4    S5    S6    S7    S8 
    PCALXB2=   M1    M2    M3    M4    M5    M6    M7    M8 
    PCALFR1=     0     0     0     0     0     0     0     0
    PCALFR2=     0     0     0     0     0     0     0     0

   Track assignments are: 
    track1=   2,  6, 10, 14, 18, 22, 26, 30
    track2=   3,  7, 11, 15, 19, 23, 27, 31
    barrel=roll_auto
 SOURCE LIST -- Experiment title (include project code)
     Catalog positions marked with *. 
     Precession of date coordinates is based on stop time of first scan.
     Names used in schedule marked with *. 
     Short names used in VLA and SNAP files marked with +. 
     Observation date used in B1950/J2000 coordinate conversion (PRECDATE):  1979.900
     No adjustments are made for rates (DRA, DDEC).
     Scan hours are for recording scans only. 
     Baseline hours are only counted for scans above horizon at both ends.

   Source                         Source position (RA/Dec)                        Error
                        (B1950)             (J2000)             (Date)            (mas)

 * SRC1            * 18 50 46.247000     18 53 18.650919     18 53 06.404918       0.00
                   * 01 11 12.45000      01 14 58.08319      01 14 42.99893        0.00
                     From catalog imbedded in main SCHED input file.
                     Doppler based on LSR frame and radio definition.  Velocities:
                         10.00    10.00    20.00    20.00    30.00    30.00    40.00    40.00
                        3.000 scan hours,     66.139 baseline hours above horizon.

 * CAL1            * 18 00 00.000000     18 02 32.620579     18 02 20.469562       0.00
                   * 01 00 00.00000      01 00 05.59106      01 00 10.18979        0.00
                     From catalog imbedded in main SCHED input file.
                     Doppler based on other sources.
                        0.467 scan hours,     16.800 baseline hours above horizon.



  The solar corona can cause unstable phases for sources too close to the Sun.
  SCHED provides warnings at individual scans for distances less than 10 degrees.
  The distance from the Sun to each source in this schedule is:
    Source         Sun distance (deg) 
   SRC1               140.5
   CAL1               149.7

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

