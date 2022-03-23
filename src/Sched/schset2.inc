C     Include file setup information.
C
C     PROGRAMMER NOTES:  If any data are added here, add it to SETCOP
C                        and to SAMESET.
C                        Must invoke sched.inc before this file.
C                   
C
C     For each scan in SCHED, a setup file is specified.  Each setup
C     file consists of a number of setup groups, one for each distinct
C     station (the VLBA counts as one usually).  The use of one 
C     group per station  avoids the need for a station dimension on 
C     all the arrays.  This is a bit wasteful of space for such things 
C     as VLA information, since it will only occur once per
C     setup file, but the logic and bookkeeping will be easier.
C
C     When the setup file is read, it can include anywhere from zero
C     to MANT stations per group.  If zero, SCHED will assume that 
C     all stations that use that setup should be included.  When 
C     there is more than one station (including after dealing with 
C     the zero default), SCHED breaks the setup into one group per 
C     station before dealing with any other defaults.  This allows 
C     the defaults (eg FIRSTLO) to vary between stations.  
C
C     The array NSETUP will contain the setup group index (usually 
C     set to KS) required for any combination of station and scan. 
C
C     Note that SETSTA is the setup file station name and ISETSTA 
C     is the station index in the station catalog of the first SETSTA 
C     after expansion to one station per group.
C
C     There also is the concept of "frequency sets".  These are 
C     distinct setup group, frequency, and bandwidth combinations
C     after taking into account any FREQ, BW, or DOPPLER specified
C     in the schedule.  For each frequency set, the setup group 
C     (FSETKS) and the scan number where it first appeared (FSETSCN)
C     are recorded.  The frequency set is recorded for each scan
C     (FSETI - in sched.inc)
C
C     Some parameters   MSET   - Max number of setup groups after
C                                expanding to one per station.
C                       MCHAN  - Max number of channels.
C                       MAXPC  - Max number of pcal detector devices.
C                       MAXTON - Max number of pcal tones+statecounts.
C                       MANT   - Max number of stations in each setup.
C                                Note that after SETEXPND, there will
C                                only be one station in the setup.
C                       MTPMOD - Maximum passes per head position.
C                       NOTSET - Flag for unset parameters.  Must match
C                                UNSET in sched.inc.
C                       MFSET  - Max number of frequency sets.
C                       MPSET  - Max number of pulse cal sets.
C                                   Pcal sets being removed Sep. 2013. RCW.
C
C     Set up to get a number of these from parameters in sched.inc.
C     2002may01  RCW.  This means sched.inc must be invoked before 
C     schset.inc
C     Change MSET=200+MAXSTA to MSET=200+5*MAXSTA to help with some test
C     observations.  2007jun7.
C
C     Move setting of MSET to sched.inc to facilitate making large 
C     configuration testing version.
C      INTEGER           MSET
C      PARAMETER         (MSET=400+5*MAXSTA)
C
      INTEGER           MCHAN, MAXPC, MAXTON, MANT, MTPMOD, MFSET
      INTEGER           MPSET
      DOUBLE PRECISION  NOTSET
      PARAMETER         (MCHAN=MAXCHN)
      PARAMETER         (MAXPC=8)
      PARAMETER         (MAXTON=2*MAXPC)
      PARAMETER         (MANT=MAXSTA)
      PARAMETER         (MTPMOD=8)
      PARAMETER         (NOTSET=UNSET)
      PARAMETER         (MFSET=MSET*2)
      PARAMETER         (MPSET=MSET*2)
C
C     Bookkeeping, etc. NSET  - The number of setup groups read.
C                       SETMSG - Just useful to have a character string
C                           handy and avoid new ones in every routine.
C                       LISTKS - points to another setup group that is
C                           deemed to be the same and is the only one
C                           listed in the summary.
C
      INTEGER           NSET, LISTKS(MSET)
      CHARACTER         SETMSG*120
C
C     Relate the setup to file names and other catalogs.
C     ISETNUM and SETNAME are the number and name of the input setup
C     file.  Such a file may contain several setup groups.
C     IFREQNUM is the frequency catalog entry that applies to the group.
C     It has been made channel dependent so that multiple groups can
C     be used.
C     IFREQIF is the IF from the freq. catalog to use for the channel.
C     SDEBUG is set equal to DEBUG, which is in sched.inc, by RDSET.
C     SFCHAN is a pointer from the setup group channel to the setup
C      file logical channel.
C     SGCHAN is the corresponding pointer from the setup file logical
C      channel to the setup group channel (one per logical channel -
C      0 implies no correspondence).
C
      INTEGER           ISETNUM(MSET)
      INTEGER           IFREQNUM(MCHAN,MSET)
      INTEGER           IFREQIF(MCHAN,MSET)
      CHARACTER         SETNAME(MSET)*80
      LOGICAL           SDEBUG
      INTEGER           SFCHAN(MCHAN,MSET), SGCHAN(MCHAN,MSET)
C
C     The COMMONs for bookkeeping parameters.
C
      COMMON  /SETN1/   NSET, LISTKS, ISETNUM, IFREQNUM, IFREQIF, 
     1                  SDEBUG, SFCHAN, SGCHAN
      COMMON  /SETC1/   SETMSG, SETNAME
C
C     The setup information:
C
C     For VLBA and similar antennas
C
C     Note on sidebands, which can be a bit confusing:
C     Note that only 2 of NETSIDE, SIDEBD and SIDE1 are independent.
C       NETSIDE - Sideband of the baseband channel relative to RF.
C       SIDEBD  - Sideband of baseband relative to final IF.
C       SIDE1   - Sideband of final IF relative to RF.
C       CRDSIDE - Sideband of VLBA legacy BBC channel relative to IF.
C                 Comparable to, but may differ from SIDEBD.
C       
C
      DOUBLE PRECISION  FREQREF(MCHAN,MSET)
      DOUBLE PRECISION  FIRSTLO(MCHAN,MSET)
      DOUBLE PRECISION  FIFMIN(MCHAN,MSET)
      DOUBLE PRECISION  FIFMAX(MCHAN,MSET)
      DOUBLE PRECISION  BBSYN(MCHAN,MSET)
      DOUBLE PRECISION  BBSYN2(MCHAN,MSET)
      DOUBLE PRECISION  BBFILT(MCHAN,MSET)
      DOUBLE PRECISION  CORINV(MCHAN,MSET)
      DOUBLE PRECISION  SYNTH(3,MSET)
      REAL              AZCOLIM(MSET), ELCOLIM(MSET)
      REAL              PTINCR(MSET)
      REAL              PTOFF(MSET)
      REAL              SAMPRATE(MSET)
      REAL              SPEEDH(MSET)
      REAL              SPEEDL(MSET)
      REAL              FANOUT(MSET)
      REAL              SPEEDUP(MSET)
      REAL              BESTOVER(MSET)
      REAL              TOTBW(MSET)
      INTEGER           ISETSTA(MSET)
      INTEGER           NCHAN(MSET)
      INTEGER           TRACK(MCHAN,MTPMOD,MSET)
      INTEGER           PERIOD(MSET)
      INTEGER           BITS(MCHAN,MSET)
      INTEGER           LEVEL(MSET)
      INTEGER           BBC(MCHAN,MSET)
      INTEGER           TAPEMODE(MSET)      
      INTEGER           TPMODE(MSET)      
      INTEGER           SWTCHDUR(MSET)
      INTEGER           PCALFR1(MAXPC,MSET)
      INTEGER           PCALFR2(MAXPC,MSET)
      INTEGER           VFESYN(MCHAN,MSET)
      LOGICAL           DUALPOL(MSET)
      LOGICAL           FRSWITCH(MSET)
      LOGICAL           DUALX(MSET)
      LOGICAL           MODETEST(MSET)
      LOGICAL           USED(MSET)
      CHARACTER         SETSTA(MANT,MSET)*8
      CHARACTER         BAND(MSET)*5
      CHARACTER         IFDIST(4,MSET)*3
      CHARACTER         SIDEBD(MCHAN,MSET)*1
      CHARACTER         CRDSIDE(MCHAN,MSET)*1
      CHARACTER         POL(MCHAN,MSET)*4
      CHARACTER         NOISE(4,MSET)*6
      CHARACTER         NOISEFRQ(MSET)*4
      CHARACTER         FE(4,MSET)*6
      CHARACTER         IFCHAN(MCHAN,MSET)*2
      CHARACTER         ALTIFC(MCHAN,MSET)*2
      CHARACTER         LOGGING(MSET)*8
      CHARACTER         STRING(4,MSET)*80
      CHARACTER         FORMAT(MSET)*8
      CHARACTER         DBE(MSET)*8
      CHARACTER         FIRMFILE(MSET)*80
      CHARACTER         SPCAL(MSET)*4
      CHARACTER         PCALX1(MAXPC,MSET)*3
      CHARACTER         PCALX2(MAXPC,MSET)*3
      CHARACTER         NETSIDE(MCHAN,MSET)*1
      CHARACTER         SIDE1(MCHAN,MSET)*1
      CHARACTER         RCHAN(MSET)*2
      CHARACTER         LCHAN(MSET)*2
      CHARACTER         LCP50CM(MSET)*6
      CHARACTER         RCP50CM(MSET)*6
      CHARACTER         BARREL(MSET)*9
      CHARACTER         M4PATCH(MSET)*8
C
C     Some derived quantities.
C
      INTEGER           MINTRAK(MSET)
      INTEGER           MAXTRAK(MSET)
      REAL              MINTBPS(MSET)
      REAL              MAXTBPS(MSET)
      REAL              TBPS(MSET)
      REAL              TOTBPS(MSET)
      REAL              WRTBPS(MSET)
      LOGICAL           VLBAMKIV(MSET)
      LOGICAL           RECUSED(MSET)
C
C     Keep track of last values written for some parameters.
C
      INTEGER           MLEVEL, LLEVEL(MCHAN)
C
      COMMON  /SETN2A/  FREQREF, FIRSTLO, FIFMIN, FIFMAX,
     1                  BBSYN, BBSYN2, BBFILT, CORINV, SYNTH, 
     2                  AZCOLIM, ELCOLIM, PTINCR,  PTOFF, 
     3                  SAMPRATE, SPEEDH, SPEEDL, FANOUT,
     4                  SPEEDUP, BESTOVER, TOTBW
      COMMON  /SETN2B/  NCHAN, ISETSTA, TRACK, PERIOD, BITS, MLEVEL,
     1                  LEVEL, LLEVEL, BBC, TAPEMODE, TPMODE, SWTCHDUR,
     2                  PCALFR1, PCALFR2, VFESYN, DUALPOL, FRSWITCH, 
     3                  DUALX, MODETEST, USED, MINTRAK, MAXTRAK, 
     4                  MINTBPS, MAXTBPS, TBPS, TOTBPS, WRTBPS, 
     5                  VLBAMKIV, RECUSED
      COMMON  /SETC2/   SETSTA, BAND, SIDEBD, CRDSIDE, POL, NOISE, 
     1                  NOISEFRQ, FE, IFCHAN, ALTIFC, LOGGING, 
     2                  STRING, FORMAT, DBE, FIRMFILE, IFDIST, SPCAL, 
     3                  PCALX1, PCALX2, NETSIDE, SIDE1, RCHAN, LCHAN, 
     4                  LCP50CM, RCP50CM, BARREL, M4PATCH
C
C     VLA information.
C     Included are the fluke set, the fluke frequencies, the F.E. LO
C     frequencies and the 2-4 GHz synthesizer frequencies (MHz)
C     The 2-4 GHz synthesizer frequencies are the true frequency.  The
C     value sent to the LO card is rounded to the nearest MHz.
C
      INTEGER           FLUKESET(MSET)   
      DOUBLE PRECISION  FLUKEA(MSET)     
      DOUBLE PRECISION  FLUKEB(MSET)     
      DOUBLE PRECISION  VLAFEAB(MSET)    
      DOUBLE PRECISION  VLAFECD(MSET)    
      DOUBLE PRECISION  VLASYNA(MSET)    
      DOUBLE PRECISION  VLASYNB(MSET)    
      CHARACTER         FEFILTER(MSET)*4
      CHARACTER         VLAIF(MSET)*10
      CHARACTER         VLAROT(MSET)*10
      CHARACTER         VLABAND(MSET)*2
      CHARACTER         VLABW(MSET)*4
      LOGICAL           VLALOFI(MSET)
      LOGICAL           VLAVA(MSET)
      LOGICAL           VLAVB(MSET)
      LOGICAL           VLAVR(MSET)
      LOGICAL           VLAVL(MSET)
C
      COMMON  /SETN3/   FLUKEA, FLUKEB, VLAFEAB, VLAFECD,
     1                  VLASYNA, VLASYNB, FLUKESET, VLALOFI,
     2                  VLAVA, VLAVB, VLAVR, VLAVL
      COMMON  /SETC3/   FEFILTER, VLAIF, VLAROT, VLABAND, VLABW
C
C     Frequency sets.  Pcal sets are being retired.
C     FSSAME is the lowest numbered set that has the same 
C     frequencies, bandwidths, BBC frequencies, PCAL, and input 
C     setup file.
C
      INTEGER           NFSET
      INTEGER           FSETKS(MFSET)
      INTEGER           FSETSCN(MFSET)
      INTEGER           FSETPS(3,MFSET)
      INTEGER           FSSAME(MFSET)  
      CHARACTER         FSPCAL(MFSET)*4
C
C     Trying to purge the pcal sets and have done so for the
C     code in src/Sched.  But they are also used in the Vex code.
C     That still needs to be dealt with.  Meanwhile keep the
C     variables.  Have a one-to-one correspondence between 
C     pcal sets and frequency sets and get the PS variables for
C     each.
C
      INTEGER           NPSET
      CHARACTER         PSPCAL(MPSET)*4
      CHARACTER         PSX1(MAXPC,MPSET)*3
      CHARACTER         PSX2(MAXPC,MPSET)*3
      INTEGER           PSFR1(MAXPC,MPSET)
      INTEGER           PSFR2(MAXPC,MPSET)
      
C
      COMMON  /FSETN/   NFSET, FSETKS, FSETSCN, FSETPS, FSSAME,
     1                  NPSET, PSFR1, PSFR2
      COMMON  /FSETC/   FSPCAL, PSPCAL, PSX1, PSX2
C
C     End of setup include file.





