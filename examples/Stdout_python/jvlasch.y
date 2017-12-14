  COVER INFORMATION 

 Station:    VLA27     (Code Y  ) 
 Experiment: jvla vlbi test.
 Exp. Code:  jvla    


Schedule Version:       1.00
Processed by SCHED version:  11.50

PI:       Amy Mioduszewski

Address:  NRAO
          PO Box 0
          Socorro, NM 87801
 

Phone:    575 835 7263
EMAIL:    amiodusz@nrao.edu
Fax:
Phone during observation: 575 835 7263

Observing mode:

Notes:
 
 
 

Schedule for VLA27     (Code Y  )                                   Page   2
               jvla vlbi test.
  UP:  D => Below limits;  H => Below horizon mask;  W => still slewing at end;  blank => Up.
  Early: Seconds between end of slew and start.   Dwell: On source seconds. 
  Disk: GBytes recorded to this point.
  TPStart:  Recording start time.  Frequencies are LO sum (band edge).
  SYNC: Time correlator is expected to sync up.
----------------------------------------------------------------------------------------
Start UT  Source               Start / Stop                 Early    Disk   TPStart
Stop UT                  LST      EL    AZ   HA  UP   ParA  Dwell   GBytes    SYNC
----------------------------------------------------------------------------------------

 --- Wed  25 Apr 2012   Day 116 ---

 Next scan frequencies:  4988.00  4988.00  5020.00  5020.00
 Next BBC frequencies:    912.00   912.00   880.00   880.00
 Next scan bandwidths:     32.00    32.00    32.00    32.00

17 00 00  J0217+7349   00 05 57  47.0  12.9 -2.2    -138.4     0        0   17 00 00
17 05 00  =0212+735    00 10 58  47.2  12.5 -2.1    -139.9   300       19   17 00 00

17 05 20  J0217+7349   00 11 18  47.2  12.4 -2.1    -140.0    14       19   17 05 20
17 10 20  =0212+735    00 16 19  47.4  12.0 -2.0    -141.5   300       39   17 05 20

17 10 40  J0217+7349   00 16 39  47.4  12.0 -2.0    -141.6    14       39   17 10 40
17 15 40  =0212+735    00 21 40  47.6  11.6 -1.9    -143.1   300       58   17 10 40

17 16 00  J0217+7349   00 22 00  47.7  11.6 -1.9    -143.2    14       58   17 16 00
17 21 00  =0212+735    00 27 01  47.9  11.2 -1.9    -144.7   300       77   17 16 00

17 21 20  J0217+7349   00 27 21  47.9  11.1 -1.9    -144.8    14       77   17 21 20
17 26 20  =0212+735    00 32 22  48.1  10.7 -1.8    -146.3   300       97   17 21 20

17 26 40  J0217+7349   00 32 42  48.1  10.7 -1.8    -146.4    14       97   17 26 40
17 31 40  =0212+735    00 37 43  48.3  10.2 -1.7    -148.0   300      116   17 26 40


SETUP FILE INFORMATION:
   NOTE: If DOPPLER, FREQ, or BW were used, see the individual scans for the final BBC settings.


 ======== Setup file: trdbea.6cm
 --- WARNING ---  This group does not match an entry in the frequency catalog.
                  This might be ok because the catalog is not complete.
                  But be very careful to be sure that the setup is correct.

   Setup group:   12         Station: VLA27             Total bit rate:   512
   Format: VDIF              Bits per sample: 2         Sample rate: 64.000
   Number of channels:  4    DBE type: WIDAR            Speedup factor:   1.00

   Disk used to record data.

   1st LO=   5900.00   5900.00   5900.00   5900.00
   Net SB=         U         U         U         U
   IF SB =         L         L         L         L
   Pol.  =      RCP       LCP       RCP       LCP 
   BBC   =         1         2         3         4
   BBC SB=         L         L         L         L
   IF    =        A         C         B         D 

  The following frequency sets based on these setups were used.

   Frequency Set:  12  Setup file default.  Used with PCAL = off
   LO sum=   4988.00  4988.00  5020.00  5020.00
   BBC fr=    912.00   912.00   880.00   880.00
   Bandwd=     32.00    32.00    32.00    32.00
    Matching frequency sets:  12

   Track assignments are: 
    track1=   1,  2,  3,  4
    barrel=roll_off 

 POSITIONS OF SOURCES USED IN RECORDING SCANS

   Source                         Source position (RA/Dec)                        Error
                        (B1950)             (J2000)             (Date)            (mas)

   0212+735          02 12 49.921889   * 02 17 30.813369     02 18 38.603239       0.36
 * J0217+7349        73 35 40.08546    * 73 49 32.62179      73 52 56.12551        0.10
                     /home/eldering/sched/catalogs/sources.rfc
                     rfc_2015a Petrov, 2015, unpublished.  56612 observations



SOURCE SCAN SUMMARY FOR SOURCES LISTED ABOVE

     Scan hours are for recording scans only. 
     Baseline hours are only counted for scans above horizon at both ends.
  Source       Setup file             Frequency sets                  Observing hours
                                   (duplicates not shown)              Scan  Baseline
  J0217+7349 trdbea.6cm            1 11 12                           0.500    32.992


EFFECT OF SOLAR CORONA

  The solar corona can cause unstable phases for sources too close to the Sun.
  SCHED provides warnings at individual scans for distances less than 10 degrees.
  The distance from the Sun to each source in this schedule is:
    Source         Sun distance (deg) 
   J0217+7349          60.4

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

