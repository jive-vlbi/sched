C     Include file for SCHED and subroutines.
C
C
C     The multiple commons used in some cases were the result of 
C     restrictions on common size in some compiler (Microsoft?).
C
C     Note that the non-character variables are ordered in the COMMONs
C     with the larger ones (in terms of bytes) first.  This prevents
C     word alignment problems.
C
C     Program Version (set in sched.f):
C
      REAL         VERNUM
      CHARACTER    VERSION*40
      COMMON    / VERN /  VERNUM
      COMMON    / VERC /  VERSION
C
C
C     Constants.
C
      DOUBLE PRECISION PI, TWOPI, RADDEG, RADHR, SIDR, UNSET, ONESEC
      PARAMETER        (PI=3.141592653589793238D0)
      PARAMETER        (TWOPI=PI*2.0D0)
      PARAMETER        (RADDEG=PI/180.0D0)
      PARAMETER        (RADHR=PI/12.0D0)
      PARAMETER        (SIDR=1.00273791D0)
      PARAMETER        (UNSET=-9999.D0)
      PARAMETER        (ONESEC=1.D0/86400.D0)
C
C
C     Array sizes:  MAXCAT - Station catalog.    
C                   MAXSRC - Source catalog.
C                   MAXSCN - Number of scans.
C                   MAXSTA - Stations used in schedule.
C                   MAXLGP - Groups of rest frequencies.
C                   MAXSET - Setup files. - see also SCHSET.INC
C                   MAXCHN - Number of channels - see also SCHSET.INC
C                   MAXLCH - Dopset channels.   
C                   MAXSAT - Number of satellites.
C                   MAXCRD - Maximum number of VLBA legacy system channels
C                            when observing with the RDBE.
C
C                   MSET   - Number of setup groups - used in schset.inc
C
C     If you change a parameter here, you may need to make related
C     changes elsewhere.  Generally if the other parameters are higher
C     than you want to set the local one, that is ok.
C     Here are a few I have chased down (incomplete for now).
C     The biggest arrays are dimensioned (MAXSCN,MAXSTA) and there are
C     a considerable number of them.  Therefore those are the two
C     parameters to be most careful about to keep the size down.
C     There are also some (MAXCHN,MAXSCN) arrays, so be careful of them.
C     Actually the largest arrays are in the Vex section where there are 
C     (MAXMOD,MAXMOD) and even a number of (MAXSTA,MAXMOD,MAXMOD) 
C     arrays where is now MAXMOD=5*MAXSTA (It used to be set equal to
C     MSET and those arrays got too big for 32 bit machines when setting 
C     up for SKA configuration testing).
C
C      Parameter    Related parameters
C       MAXCHN       rdcat.inc:MVEL
C                    vlbabws.f:MCHAN (set at 128 - may not need change)
C       MAXSTA       plot.inc:PSTMAX (set at 1000 - may not need change)
C       MAXSRC       plot.inc:PSOMAX (set at 2000 - may not need change)
C       MAXCAT       rdcat.inc:MDB (set at 1000 - may not need change)

C
      INTEGER       MAXCAT, MAXSRC, MALIAS, MAXSCN, MAXSTA, MAXLCH
      INTEGER       MAXLGP, MAXSET, MAXCHN, MAXSAT, MSET
      INTEGER       MINTENT, MSCINT, MAXCRD
C
C     For normal usage.  Phase referencing and pointing observations
C     need lots of scans, but the number of stations is limited.
C     MSET needs to be large enough for one setup group per frequency
C     group per station.  The number is driven by wideband VLBA 
C     single dish testing.
C
      PARAMETER     (MAXSCN=4000)
      PARAMETER     (MAXSTA=40)
      PARAMETER     (MSET  =400+5*MAXSTA)
C
C     Big-array (eg SKA) configuration test version
C     Note that it will be safe to set MAXMOD (in vxlink.inc) small 
C     if the executable is only for use with OBSTYPE='CONFIG' because 
C     the Vex code will not be called.
C
C      PARAMETER     (MAXSCN=400)
C      PARAMETER     (MAXSTA=400)
C      PARAMETER     (MSET  =MAXSTA)
C
C     Other parameters
C
      PARAMETER     (MAXSRC=5000)
      PARAMETER     (MALIAS=10)
      PARAMETER     (MAXCAT=1000) 
      PARAMETER     (MAXLGP=10)
      PARAMETER     (MAXSET=50)
      PARAMETER     (MAXCHN=32)
      PARAMETER     (MAXLCH=MAXCHN)
      PARAMETER     (MAXSAT=5)
      PARAMETER     (MAXCRD=4)
C
C     For intents.
C
      PARAMETER     (MINTENT=100)
      PARAMETER     (MSCINT=25)
C
C
C     Unit Numbers.    Main keyin input read on unit 5.
C                      IUSCH - Main schedule input if not 5.
C                      IUSRC - Source catalog. 
C                      IUSTA - Station catalog.
C                      IUTAP - Input tape information.
C                      IUSET - Setup file.     
C                      IPRT  - Telescope operator file.  
C                      IULOC - Non VLBA telescope control file.
C                      IUVBA - VLBA telescope control file.
C                      ISUM  - Summary file.   
C                      IUOPT - Opti sched output.
C                      IVEX  - VEX output file.
C                      IFRQ  - Frequency catalog
C                      IFLAG - Flag file.
C                      IUPK  - Reference pointing inputs.
C                      IOMS  - Output file for OMS
C                      IMAP  - Map files for uv plots.
C                      IOPT  - Configuration optimization output
C                      ILOG  - Run log file.
C                      IMSG  - File with text messages.
C                      IORB  - Spacecraft table for VLBA correlator
C                      IV2D  - Output v2d file.
C                      IPRE  - Periods of allow preemption for EOP.
C                      ICSV  - .csv file in WRTSEL in plotting code.
C                      IDAT  - .dat file in WRTSEL in plotting code.
C                      
C                      
      INTEGER          IUSCH, IUSRC, IUSTA, IUTAP, IUSET
      INTEGER          IPRT, IULOC, IUVBA, ISUM, IUOPT, IVEX, IFRQ
      INTEGER          IFRQLST, IFLAG, ICOV, IUPK, IOMS, IV2D, IMAP
      INTEGER          IOPT, ILOG, IMSG, IORB, IPRE, ITLE, ICSV, IDAT
C
      PARAMETER        (IUSCH=8)
      PARAMETER        (IUSRC=9)
      PARAMETER        (IUSTA=10)
      PARAMETER        (IUTAP=11)
C     Unit 12 is used by the JPL ephemeris routines.
      PARAMETER        (IUSET=13)
      PARAMETER        (IPRT =14)
      PARAMETER        (IULOC=15)
      PARAMETER        (IUVBA=16)
      PARAMETER        (ISUM =17)
      PARAMETER        (IUOPT=18)
      PARAMETER        (IVEX =19)
      PARAMETER        (IFRQ =20)
      PARAMETER        (IFRQLST=21)
      PARAMETER        (IFLAG=22)
      PARAMETER        (ICOV=23)
C     Unit 24 is used by RDLOC.
      PARAMETER        (IUPK=25)
      PARAMETER        (IOMS=26)
      PARAMETER        (IMAP=27)
      PARAMETER        (IOPT=28)
      PARAMETER        (ILOG=29)
      PARAMETER        (IMSG=30)
      PARAMETER        (IORB=31)
      PARAMETER        (IV2D=32)
      PARAMETER        (IPRE=33)
      PARAMETER        (ITLE=34)
      PARAMETER        (ICSV=35)
      PARAMETER        (IDAT=36)
C
C
C     Station catalog.   Contains MSTA stations.
C
C                        There are two station lists:
C                        1.  The schedule stations. NSTA entries.
C                            A lot of schedule information is indexed
C                            according to this list.  The tape 
C                            initialization information is also indexed
C                            to this list.  There can be MAXSTA entries.
C                            ISTA generally refers to this list.
C                        2.  The station catalog.  MSTA entries with
C                            a maximum of MAXCAT (which is rather 
C                            larger than MAXSTA).  
C
C                        There can be many catalog stations that are 
C                        not used in the schedule.
C
C                        Pointers:
C                          STANUM(ISTA) is the station catalog entry for 
C                            schedule station ISTA.
C                          ISETSTA(KS) is the station catalog entry 
C                            for setup group KS (see schset.inc).
C                          ISCHSTA(KSTA) is the schedule station 
C                            corresponding to station catalog entry 
C                            KSTA.  It can be 0.
C
C                        Note that, if you want the schedule station
C                        corresponding to a setup, you need
C                        ISCHSTA(ISETSTA(KS)).  Since only setup groups
C                        that are used are retained, that should always
C                        be non-zero.
C
C                        VLBADAR true indicates that both VLBA and
C                        a local type control file are needed.  This
C                        is for Green Bank and the VLA.
C
C                        STATION and CONTROL are upper case.
C
C                        Note that STANAME(ISTA) = STATION(STANUM(ISTA))
C                        and STCODEU is the upper case version of STCODE
C
      INTEGER          MSTA
      INTEGER          ISCHSTA(MAXCAT), MJDRATE(MAXCAT)
      CHARACTER        STAFILE*1024
      CHARACTER        STVER*20
      CHARACTER        LOCAFILE*1024
      CHARACTER        LOCAVER*20
      CHARACTER        STATION(MAXCAT)*8
      CHARACTER        STCODE(MAXCAT)*3
      CHARACTER        STCODEU(MAXCAT)*3
      DOUBLE PRECISION XPOS(MAXCAT), YPOS(MAXCAT), ZPOS(MAXCAT)
      DOUBLE PRECISION DXPOS(MAXCAT), DYPOS(MAXCAT), DZPOS(MAXCAT)
      DOUBLE PRECISION ELEV(MAXCAT), LAT(MAXCAT), LONG(MAXCAT)
      CHARACTER        POSREF(MAXCAT)*80
C
      CHARACTER        CONTROL(MAXCAT)*4, MOUNT(MAXCAT)*5
      CHARACTER        DAR(MAXCAT)*5, RECORDER(MAXCAT)*6
      CHARACTER        DISK(MAXCAT)*6, MEDIADEF(MAXCAT)*6
      CHARACTER        TSCAL(MAXCAT)*4, DBBCVER(MAXCAT)*8
      INTEGER          NBBC(MAXCAT), STNDRIV(MAXCAT), NHEADS(MAXCAT)
      LOGICAL          VLBADAR(MAXCAT), USEONSRC(MAXCAT)
C
      INTEGER          NHORIZ(MAXCAT)
      REAL             HORAZ(200,MAXCAT), HOREL(200,MAXCAT)
C
      INTEGER          NAXLIM(MAXCAT)
      REAL             AX1LIM(6,MAXCAT), AX2LIM(6,MAXCAT)
      REAL             AX1RATE(MAXCAT), AX2RATE(MAXCAT)
      REAL             AX1ACC(2,MAXCAT), AX2ACC(2,MAXCAT)
      REAL             TSETTLE(MAXCAT), MINSETUP(MAXCAT)
      REAL             MAXSRCHR(MAXCAT), TLEVSET(MAXCAT)
      REAL             ZALIM(MAXCAT), AXOFF(MAXCAT)
C
      COMMON  /SCHSTA/ XPOS, YPOS, ZPOS, DXPOS, DYPOS, DZPOS, 
     1                 ELEV, LAT, LONG, 
     2                 MSTA, ISCHSTA, MJDRATE, 
     3                 NBBC, STNDRIV, NHEADS, VLBADAR, USEONSRC,
     4                 NHORIZ, HORAZ, HOREL,
     5                 NAXLIM, AX1LIM, AX2LIM, AX1RATE, AX2RATE, 
     6                 AX1ACC, AX2ACC, TSETTLE, MINSETUP, MAXSRCHR,
     7                 TLEVSET, ZALIM, AXOFF
      COMMON  /SCHCST/ STAFILE, STVER, LOCAFILE, LOCAVER, STATION, 
     1                 STCODE, STCODEU, POSREF, CONTROL, MOUNT, DAR, 
     2                 RECORDER, DISK, MEDIADEF, TSCAL, DBBCVER
C
C
C     Source catalog.  There are 3 source lists!  
C                      1.  Main source catalog.  MSRC entries.
C                            All sources from imbedded catalogs and
C                            used sources from the external catalog.
C                            ISRC from a scan refers to this list.
C                            Nearly all source pointers should aim
C                            at this list.
C                      2.  List of all sources read with minimal
C                            low precision information.  Used for
C                            plotting.  The variables are kept in
C                            srlist.inc.
C                      3.  Schedule sources.  NSRC entries.
C                            Includes name SRCNAME(KSRC) and 
C                            pointer SRCATN(KSRC) that points to 
C                            the corresponding catalog source.
C                            This list used mainly for plotting.
C                            The variables are near the bottom
C                            of this file (seek SRCNAME).
C                            This list gets built more than once,
C                            so do not create and use pointers
C                            to it except in isolated parts of the
C                            code.
C
C                      For each scan, SRCNUM(ISCN), IDOPSRC(ISCN), and
C                      IVLAPHS(ISCN) refer to entries in the main 
C                      catalog.
C
C                      Pointers:
C                        SRCATN(KSRC) is main catalog entry of schedule
C                          source KSRC.
C                        SRLSTN(ISRC) is the schedule source entry for
C                          main catalog source ISRC.
C                          Note KSRC = SRLSTN(SRCATN(KSRC))
C                          SRLSTN gets rebult whenever the schedule
C                          sources list gets rebuilt.
C
C                      C1950, C2000, and CDATE  record which 
C                      epoch was in the catalog.  
C
C                      There is room for MALIAS (now 10) names for source 
C                      aliases.  Plus there is an 8 or less character version 
C                      for schedules that need it.
C
C                      WARNING, there is some complexity in the source
C                      bookkeeping:
C
C                      The source name used for each scan is 
C                      SCNSRC(ISCN).
C
C                      WHICHCAT indicates which source catalog type
C                      the source was read from  (in-line, main file, 
C                      pointing file, planet, satellite).
C
C     The main source catalog:
C     Some day, add another name, which is the alias actually used,
C     to simplify usage.  Meanwhile, consider adding more aliases
C     and more characters per name as we have hit both limits.
C         
      INTEGER          MSRC
      CHARACTER        SRCFILE*1024, SRVER*20, SRCFILE2*1024, SRVER2*20
      CHARACTER        SOURCE(MALIAS,MAXSRC)*12, SOUR8(MAXSRC)*8
      CHARACTER        CALCODE(MAXSRC)*1, REMARK(MAXSRC)*80
      DOUBLE PRECISION RACAT(MAXSRC),  DECCAT(MAXSRC)
      DOUBLE PRECISION RA1950(MAXSRC), D1950(MAXSRC)
      DOUBLE PRECISION RA2000(MAXSRC), D2000(MAXSRC)
      DOUBLE PRECISION RAP(MAXSRC), DECP(MAXSRC), PMTIME(MAXSRC)
      DOUBLE PRECISION EPOCHT(MAXSRC)
      DOUBLE PRECISION PMRA(MAXSRC), PMDEC(MAXSRC), PARALAX(MAXSRC)
      CHARACTER*1      C1950(MAXSRC), C2000(MAXSRC), CDATE(MAXSRC)
      REAL             RAERR(MAXSRC), DECERR(MAXSRC)
      REAL             DRA(MAXSRC), DDEC(MAXSRC), VLSR(MAXLCH,MAXSRC)
      CHARACTER        WHICHCAT(MAXSRC)*1 
      CHARACTER        VELREF(MAXSRC)*1, VELDEF(MAXSRC)*1
C
C                      The following are calculated by SCHED.
C                       SUSED indicates the source was used.
C                       USEDREC indicates data were recording on src.
C                       USEDPHS indicates source used as a phase
C                              center in a multiple center scan(s).
C                       USEDCENT indicates the source was a pointing
C                              center for multiple phase center
C                              observations.
C                       SRLSTN pointer to SRCNAME (see above).
C                       SUNDIS is the distance from the sun.
C                       PLANET means the source is a planet and the
C                              position will be from the JPL ephemeris.
C                       SATEL  means that the source is a satellite
C                              with orbital elements in SATFILE or
C                              TLEFILE.
C                       SATN is the satellite index for the source.
C                       CSUSED are flags for lists that show which 
C                              names were used.
C                       DIDNDOP Number of channels for which doppler
C                              calculations were done for source.
C                       DOPPED Indicates a doppler calc was done
C                              for a scan with this source.  It might
C                              not have been the dopsrc.
C
      LOGICAL          SUSED(MAXSRC), DOPPED(MAXSRC)
      LOGICAL          USEDREC(MAXSRC), USEDPHS(MAXSRC)
      LOGICAL          USEDCENT(MAXSRC)
      INTEGER          SRLSTN(MAXSRC), DIDNDOP(MAXSRC), SATN(MAXSRC)
      REAL             SUNDIS(MAXSRC)
      LOGICAL          PLANET(MAXSRC), SATEL(MAXSRC)
      CHARACTER        CSUSED(MALIAS,MAXSRC)*1
C
C     Items for the multiple phase center capability.
C     MCENT is the maximum number of groups of phase centers.
C     CTRSRCN is the list of source names in each center.  
C     CTRSRCI is pointers into the main source catalog.
C     Both are needed because the catalogs are not loaded until
C     after the lists of phase centers are read.
C     ICENT is the index of the centers group for the scan in the 
C     centers list
C     USEDPHS above flags sources used in centers.
C
      INTEGER          MCENT, MCSRC
      PARAMETER        ( MCENT = 100 )
      PARAMETER        ( MCSRC = 1000 )
      CHARACTER        CTRNAME(MCENT)*12
      CHARACTER        CTRSRCN(MCSRC,MCENT)*12
      CHARACTER        CENTERS(MAXSCN)*12
      INTEGER          CTRSRCI(MCSRC,MCENT), ICENT(MAXSCN)
      INTEGER          NCENT, NCSRC(MCENT)
C
C     Keep a list of pairings of pointing centers with groups
C     of phase centers.  This avoids lots of hoop jumping when
C     making various lists and displays.
C
      INTEGER          NPAIR
      INTEGER          PAIRSRC(MCENT*2), PAIRCENT(MCENT*2)
C
C     Items for use by the routines that can automatically generate
C     a geodetic segment for atmospheric calibration (DELZN segment
C     or geosegment).  
C     GEOSRC is the list of sources to use
C     GEOSRCI is the main source catalog source number
C     GEOLEN gives the length of the geo segment (0 => no segment)
C     GEOISCN records the original scan number to serve as a pointer
C       to the original scan when scans insertions are happening
C       into cells beyond NSCANS.
C     ANYGEO says whether there is any geo insertion in this schedule.
C     MSEG is not used here, but is in some subroutines.  It is the
C       maximum number of scans in an inserted geodetic segment.
C     GEOLOWEL and GEOHIEL are the top of the "low" elevation region,
C       and the bottom of the "high" elevation region.  They are set
C       in GEOCHK
C
      INTEGER          MGEO, NGEO, MSEG
      PARAMETER        ( MGEO = 400 )
      PARAMETER        ( MSEG = 100 )
      CHARACTER        GEOSRC(MGEO)*12
      INTEGER          GEOSRCI(MGEO), GEOISCN(MAXSCN), GEOPRT
      INTEGER          GEOTRIES, GEOBACK, GEOSREP
      REAL             GEOSLEW, GEOLOWEL, GEOHIEL
      LOGICAL          ANYGEO
      DOUBLE PRECISION GEOLEN(MAXSCN), GEOSLOW
C
      COMMON  /SCHSOU/ RACAT, DECCAT, RA1950, D1950, RA2000, D2000, 
     1                 RAP, DECP, PMTIME, EPOCHT, PMRA, PMDEC, 
     2                 PARALAX, GEOLEN, GEOSLOW, MSRC, RAERR, 
     3                 DECERR, DRA, DDEC, 
     4                 VLSR, SUSED, DOPPED, USEDREC, USEDPHS, USEDCENT,
     5                 SRLSTN, DIDNDOP, SATN, SUNDIS, PLANET, SATEL,
     6                 CTRSRCI, ICENT, NCENT, NCSRC, 
     7                 NPAIR, PAIRSRC, PAIRCENT, NGEO, GEOSRCI,
     8                 GEOISCN, GEOPRT, GEOTRIES, GEOBACK, GEOSREP, 
     9                 GEOSLEW, GEOLOWEL, GEOHIEL, ANYGEO 
      COMMON  /SCHCSC/ SRCFILE, SRVER, SRCFILE2, SRVER2, 
     1                 SOURCE, SOUR8, CALCODE, REMARK, 
     2                 C1950, C2000, CDATE, WHICHCAT, CSUSED,
     3                 VELREF, VELDEF, CTRNAME, CTRSRCN, CENTERS,
     4                 GEOSRC
C
C     Satellite information from SATINI section.
C     SATFILE is the satellite spice file.
C
      INTEGER          NSAT
      INTEGER          SATNUM(MAXSAT)
      CHARACTER        SATNAME(MAXSAT)*12
      CHARACTER        SATFILE(MAXSAT)*128
      CHARACTER        TLEFILE(MAXSAT)*128
      CHARACTER        KERFILE(MAXSAT)*128
C
      COMMON  /SCHSAT/ NSAT, SATNUM
      COMMON  /SCHCAT/ SATNAME, SATFILE, TLEFILE, KERFILE
C 
C
C     Spectral line rest frequencies.
C
      INTEGER          NLGP, NRESTFQ(MAXLGP)
      DOUBLE PRECISION RESTFREQ(MAXLCH,MAXLGP)
      CHARACTER        LINENAME(MAXLGP)*8
C
      COMMON  /SCHLIN/ RESTFREQ, NLGP, NRESTFQ
      COMMON  /SCHCLI/ LINENAME
C
C
C     Program control: Parameters that guide what SCHED does.
C                      Also an often-needed string.
C                      MISSING implies can plot, but not make files.
C
      INTEGER          LINEPG
      INTEGER          ORIGEN(MAXSCN)
      LOGICAL          PLOT, PUBPLOT, DEBUG, OVERWRIT 
      LOGICAL          DOVEX, VEXTEST, DOMKA, DOVSOP, MISSING, MOVING
      LOGICAL          NOSET, COVERLET, AUTOPEAK, PKWATCH
      LOGICAL          OVERRIDE, READLOG, DODOWN(MAXSCN)
      CHARACTER        MSGTXT*256, SUMITEM(10)*8, DOSTA*8
      CHARACTER        EPHFILE*1024, FREQFILE*1024, FREQVER*20
      CHARACTER        PEAKFILE*1024, LOGFILE*80, MSGFILE*1024
      DOUBLE PRECISION FREQLIST(2), PRECDATE
C
C                      Schedule optimization and plotting:
C                      Also configuration optimization.
C
      DOUBLE PRECISION OPDUR
      CHARACTER        OPHASTA*8
      REAL             OPMINEL(MAXSCN), OPTSLEW, OPTLOWT
      REAL             OPSLEWWT(MAXSCN), OPSLEWTI(MAXSCN)
      REAL             OPHLIMWT(MAXSCN), OPHLIMTI(MAXSCN)
      REAL             OPHMAXDT(MAXSCN)
      REAL             OPMINSEP(MAXSCN), OPHA(MAXSCN)
      REAL             OPHAWID(MAXSCN), OPHAWT(MAXSCN)
      INTEGER          OPMIAN(MAXSCN), OPSKIP
      INTEGER          OPELPRIO(4), OPMISS(MAXSCN)
      INTEGER          OPPRTLEV
      LOGICAL          OPNOSUB, TAPESYNC
      CHARACTER        OPTMODE*8
      REAL             MAPLIM(4)
      INTEGER          GRIDNR, GRIDNT
      REAL             GRIDMIN, GRIDMAX, GRIDW0
      REAL             GRIDSTEP
      LOGICAL          GRIDUSED, GRIDVLA
      CHARACTER        GRIDMEAS*8
      INTEGER          NMFS
      REAL             MFSRAT
      REAL             DOTSIZE, RINGSIZE, LONMIN, LONMAX
      REAL             LATMIN, LATMAX, LATMID
      INTEGER          NTICKS, LINESZ, LABELSZ, COLIND
      LOGICAL          MOVEDS(MAXSTA)
C
      COMMON  /SCHCON/ FREQLIST, PRECDATE, OPDUR, LINEPG, ORIGEN, PLOT, 
     1                 PUBPLOT, DEBUG, OVERWRIT, DOVEX, VEXTEST, DOMKA, 
     2                 DOVSOP, MISSING, MOVING, NOSET, COVERLET,
     3                 AUTOPEAK, PKWATCH, OVERRIDE, READLOG, DODOWN,
     4                 OPMINEL, OPTSLEW, OPTLOWT, OPSLEWWT, OPSLEWTI,
     5                 OPHLIMWT, OPHLIMTI, OPHMAXDT, OPMINSEP, OPHA, 
     6                 OPHAWID, OPHAWT, OPMIAN, OPSKIP, OPELPRIO, 
     7                 OPMISS, OPPRTLEV, OPNOSUB, TAPESYNC, MAPLIM,
     8                 GRIDNR, GRIDNT, GRIDMIN, GRIDMAX, GRIDW0, 
     9                 GRIDSTEP, GRIDUSED, GRIDVLA, NMFS, MFSRAT,
     A                 DOTSIZE, RINGSIZE, LONMIN, LONMAX, 
     B                 LATMIN, LATMAX, LATMID,
     C                 NTICKS, LINESZ, LABELSZ, COLIND, MOVEDS
      COMMON  /SCHSCO/ MSGTXT, SUMITEM, DOSTA, EPHFILE, 
     1                 FREQFILE, FREQVER, PEAKFILE, LOGFILE, MSGFILE,
     2                 OPHASTA, OPTMODE, GRIDMEAS
C
C
C     Setup information:  Setup information is provided in setup files
C                      and from SCHED provided defaults.  A setup file
C                      (once the defaults are set) gives a complete
C                      description the hardware configuration at all
C                      antennas participating in a scan.  Each setup 
C                      file consists (perhaps after some expansion by
C                      SCHED) of a number of setup groups.  A setup
C                      group describes the hardware setup at a single
C                      station (or at all VLBA stations). 
C
C                      The variables declared here keep track of
C                      information that applies to a whole setup FILE.
C                      The actual setup group variables are 
C                      in the include file schset.inc. 
C
C                      The channel numbers used for the setup file
C                      are not necessarily the same as those used in
C                      a setup group thanks to special cases where
C                      some stations can only record a subset of the
C                      desired channels (eg a single polarization 
C                      station observing in a dual polarization
C                      experiment).  Index SFCHAN in the setup group
C                      information points to the setup file channel.
C
C                      SETFILE is the setup file name. 
C                      SETNUM (with scan info) tells which setup file 
C                       is used in a scan.  The setup group to use
C                       for a particular station for a scan is provided
C                       by the array NSETUP.
C                      MSCHN is the maximum number of channels per 
C                       logical channel for the setup file.
C                      FSPEED is the speed up factor for the
C                       setup file (same as SPEEDUP for all groups 
C                       in file).  
C                      SAMEBBC helps flag channels that use the same 
C                       bbc.
C                      MINBBC is the minimum number of BBC's available
C                       for scans using this setup file.  It depends on
C                       what is available at the stations.
C
C                      SFSIDE is the net sideband used for the
C                       logical channel throughout the setup file.
C                      SFFREQ is the frequency used for the logical
C                       channel (there can be small station offsets)
C                      SFFILT is the bandwidth for the logical chan.
C                      SFPOL is the polarization for the logical 
C                       channel.
C                      OKXC means that all setup groups contain a 
C                       subset of the channels in the most complete 
C                       group (max NCHAN, dual pol if any have it).
C                       If OKXC is false, don't allow DOPPLER, FREQ, 
C                       BW etc in schedule and don't do checks of 
C                       cross correlatability.  Also warn user.
C
      DOUBLE PRECISION SFFREQ(MAXCHN,MAXSET), SFFILT(MAXCHN,MAXSET)
      INTEGER          MSCHN(MAXSET), NSETF
      INTEGER          SAMEBBC(MAXCHN,MAXSET), MINBBC(MAXSET)
      REAL             FSPEED(MAXSET)
      LOGICAL          OKXC(MAXSET)
      CHARACTER        SETFILE(MAXSET)*80, SFSIDE(MAXCHN,MAXSET)*1
      CHARACTER        SFPOL(MAXCHN,MAXSET)*3
C
      COMMON  /SCHSF/  SFFREQ, SFFILT, MSCHN, NSETF, SAMEBBC, MINBBC, 
     1                 FSPEED, OKXC
      COMMON  /SCHSSF/ SETFILE, SFSIDE, SFPOL
C
C
C     Cover and correlator information:
C
      INTEGER          MCOVER
      PARAMETER        (MCOVER=16)
      CHARACTER        COVER(MCOVER)*150
      CHARACTER        PINAME*64, PHONE*64, OBSPHONE*64, EMAIL*64
      CHARACTER        ADDRESS(4)*64, FAX*64, OBSMODE*64, NOTE(4)*128
      CHARACTER        OBSTYP*8
      REAL             SCHVER
C
      INTEGER          MCOR
      PARAMETER        (MCOR=23)
      CHARACTER        CORSTUFF(MCOR)*150
      INTEGER          CORCHAN, CORFFT, CORNANT
      REAL             CORAVG
      REAL             CORAV2
      LOGICAL          CORPOL
      LOGICAL          CAEXACT
      LOGICAL          CAEXACT2
      CHARACTER        CORREL*64, CORTAPE*16, CORDFMT*8, CORWTFN*16
      CHARACTER        CORSRCS*64, CORSHIP(4)*64, CORNOTE(4)*(128)
C
      COMMON  /SCHCO/  SCHVER, CORCHAN, CORFFT, CORNANT, CORAVG, CORAV2, 
     1                 CORPOL, CAEXACT, CAEXACT2
      COMMON  /SCHSCO/ COVER, PINAME, PHONE, OBSPHONE, EMAIL, ADDRESS,
     1                 FAX, OBSMODE, NOTE, OBSTYP,
     2                 CORSTUFF, CORREL, CORTAPE, CORDFMT,
     3                 CORWTFN, CORSRCS, CORSHIP, CORNOTE
C
C
C     The schedule:    Scan independent items.
C                      NSCANS is the number of scans read in.
C                      SCAN1 and SCANL are the first and last scans 
C                         to process after the SCHOPT call.
C                      The optimization routines might cause these 
C                      to be very different from 1 and NSCANS.
C                      FUZZY means that there are some PREEMPT=EXTRA scans.
C                      GOTVLBA indicates that there are some VLBA stations.
C
C                      Input items:
C
      REAL             IATUTC
      INTEGER          LSTSTA, DOSCANS(2)
      LOGICAL          LST, MARK2, VLBITP, VLAONLY, NOTAPE, CONFIG
      LOGICAL          TANTS1(MAXSTA), TANTS2(MAXSTA), FUZZY, GOTVLBA
      LOGICAL          GOTPREEM, WRAP24
      CHARACTER        EXPT*72, EXPCODE*8
C
C                      Calculated items:
C
      DOUBLE PRECISION TFIRST, TEND, COREBEG, COREEND
      INTEGER          SCAN1, SCANL, NSCANS
      LOGICAL          DWELLS
C
C                      The list of stations used - the "schedule
C                      stations".  See the notes for the station 
C                      catalog for more information.
C
      INTEGER          NSTA, STANUM(MAXSTA)
      CHARACTER        STANAME(MAXSTA)*8
C
C                      The list of sources used - the "schedule 
C                      sources".  See the notes for the source 
C                      catalog for more information.  Caution, 
C                      these can change.  When they do, SRLSTN
C                      should too.
C
      INTEGER          NSRC, SRCATN(MAXSRC)
      CHARACTER        SRCNAME(MAXSRC)*12
C
      COMMON /SCHN1/   TFIRST, TEND, COREBEG, COREEND, IATUTC, 
     1                 LSTSTA, DOSCANS, LST, MARK2, VLBITP, VLAONLY, 
     2                 NOTAPE, CONFIG, TANTS1, TANTS2, FUZZY, 
     3                 GOTVLBA, GOTPREEM, WRAP24, SCAN1, SCANL,
     4                 NSCANS, DWELLS,  NSTA, STANUM, 
     5                 NSRC, SRCATN
      COMMON /SCHC1/   EXPT, EXPCODE, STANAME, SRCNAME
C
C
C                      Scan specific items.
C                      Note, STASCN(ISCN,ISTA) is a flag that indicates
C                         that station ISTA is in scan ISCN.  
C                      STARTJ and STOPJ are modified Julian days.
C                      PRESCAN and DUR are in fractional days.
C
C                      NOTE:  Any scan specific items should be copied
C                      in SCNDUP in case of loops or some optimizations.
C
C                      Pointers:
C                        NSETUP(ISCN,ISTA) is the setup group for 
C                          station ISTA in scan ISCN.
C                        FSETI(ISCN,ISTA) is the frequency set for
C                          station ISTA in scan ISCN.
C                        SRCNUM(ISCN) is the source catalog entry
C                          for the source if scan ISCN.
C                        IDOPSRC(ISCN) is the Doppler source for scan
C                          ISCN.
C                        SETNUM(ISCN) is the setup file number (not 
C                          setup group number) of scan ISCN.
C
C                      Intents.
C                        INTENT is list of distinct intent text strings.
C                        NINTENT is the number of such strings.
C                        ISCINT is an array giving intents used per scan.
C                        NSCINT is the number of intents in a scan.
C                        
C                      The variables starting the CRD (except CRDLINE)
C                      are for the settings to use for the VLBA legacy 
C                      system baseband converters while using the RDBE.
C                      They are separate to allow reference pointing
C                      on masers when the RDBE_PFB is in use, which
C                      does not allow fine tuning.
C
      DOUBLE PRECISION PRESCAN(MAXSCN), DUR(MAXSCN), GAP(MAXSCN)
      DOUBLE PRECISION MINDW(MAXSCN), STARTJ(MAXSCN), STOPJ(MAXSCN)
      DOUBLE PRECISION FREQ(MAXCHN,MAXSCN), DOPINCR(MAXSCN,2)
      DOUBLE PRECISION BW(MAXCHN,MAXSCN)
      DOUBLE PRECISION CRDFREQ(MAXCHN,MAXSCN), CRDBW(MAXCHN,MAXSCN)
      INTEGER          SRCNUM(MAXSCN), IDOPSRC(MAXSCN), QUAL(MAXSCN)
      INTEGER          SETNUM(MAXSCN), NSETUP(MAXSCN,MAXSTA)
      INTEGER          FSETI(MAXSCN,MAXSTA), DOPEAK(MAXSCN)
      INTEGER          HIGROUP(MAXSCN)
      INTEGER          POINT(MAXSCN), DURONLY(MAXSCN), NOWAIT(MAXSCN)
      INTEGER          ISCINT(MSCINT,MAXSCN), NSCINT(MAXSCN), NINTENT
      INTEGER          CRDNCH(MAXSCN), CRDCH1(MAXSCN)
      INTEGER          CRDSETCH(MAXCRD,MAXSCN)
      LOGICAL          STASCN(MAXSCN,MAXSTA), GOTCRD(MAXSCN)
      LOGICAL          NOTSYS(MAXSCN), DOPCAL(MAXSCN), CRDDOP(MAXSCN)
      LOGICAL          DWELL(MAXSCN), TANT1(MAXSCN), TANT2(MAXSCN)
      CHARACTER        ANNOT(MAXSCN)*128, SCNSRC(MAXSCN)*12
      CHARACTER        DOPSRC(MAXSCN)*12, LINES(MAXSCN)*8
      CHARACTER        PCAL(MAXSCN)*4, SCANTAG(MAXSCN)*4
      CHARACTER        PREEMPT(MAXSCN)*5, INTENT(MINTENT)*80
C
      COMMON  /SCHN2A/ SRCNUM, IDOPSRC, QUAL, SETNUM, NSETUP, FSETI,
     1                 DOPEAK, HIGROUP, POINT, DURONLY, NOWAIT, 
     2                 ISCINT, NSCINT, NINTENT, CRDNCH, CRDCH1, 
     3                 CRDSETCH, STASCN,
     4                 GOTCRD,NOTSYS, DOPCAL, CRDDOP, DWELL, TANT1,
     5                 TANT2
      COMMON  /SCHN2B/ PRESCAN, DUR, GAP, MINDW, FREQ, DOPINCR
      COMMON  /SCHN2C/ STARTJ, STOPJ, BW
      COMMON  /SCHN2D/ CRDFREQ, CRDBW
      COMMON  /SCHC2A/ ANNOT
      COMMON  /SCHC2B/ SCNSRC, DOPSRC, LINES, PCAL, SCANTAG, PREEMPT
      COMMON  /SCHC2C/ INTENT
C
C
C     Specific items for VLA.
C     VLASTA is the station number of the VLA.
C
      INTEGER          VLAUSERN, IVLAPHS(MAXSCN), VLAINTEG(MAXSCN)
      INTEGER          VLAPTIME(MAXSCN), VLARFANT, VLASTA
      LOGICAL          PHASING(MAXSCN)
      CHARACTER        VLATYPE*9, VLATSYS(MAXSCN)*1, VLAPEAK(MAXSCN)*9
      CHARACTER        VLAMODE(MAXSCN)*2
      CHARACTER        VLAPHS(MAXSCN)*12
C
      COMMON  /SCHN3/  VLAUSERN, IVLAPHS, VLAINTEG, VLAPTIME, VLARFANT,
     1                 VLASTA, PHASING
      COMMON  /SCHC3/  VLAMODE, VLATSYS, VLAPEAK, VLATYPE, 
     1                 VLAPHS
C
C
C     Specific items for VLBA
C     FOC and ROT are offsets for ROTPAT.
C
      INTEGER          PTDUR, PTSLEW(MAXSCN), CALTIME(MAXSCN), ROTPAT
      LOGICAL          PNTVLBA(MAXSCN), TANVLBA(MAXSCN), DOPN3DB(MAXSCN)
      REAL             FOCUS(MAXSCN), ROTATION(MAXSCN)
      REAL             FOC(20), ROT(20)
      REAL             SAZCOL(MAXSCN), SELCOL(MAXSCN)
      CHARACTER        CRDLINE(MAXSCN)*80
C
      COMMON  /SCHN4/  PTDUR, PTSLEW, CALTIME, ROTPAT, PNTVLBA, 
     1                 TANVLBA, DOPN3DB, FOCUS, ROTATION, FOC, ROT, 
     2                 SAZCOL, SELCOL
      COMMON  /SCHC4/  CRDLINE
C
C
C     Specific items for tape management.  
C
C                      Items from the schedule input.
C
      DOUBLE PRECISION TPREF, TPLEN
      DOUBLE PRECISION MINPAUSE(MAXSCN), PRESTART(MAXSCN)
      LOGICAL          AUTOTAPE, AUTOALOC(MAXSTA), AUTOREV(MAXSTA)
      LOGICAL          NOREC(MAXSCN)
      REAL             GRABTIME(2,MAXSCN), GRABGAP(MAXSCN)
      CHARACTER        DATAPATH(MAXSCN)*8, GRABTO(MAXSCN)*4
C
C                      Items from or related to the TAPEFILE.  Indexed
C                      by the normal station number.
C
      INTEGER          NTPSTA
      DOUBLE PRECISION TPTIME(MAXSTA)
      INTEGER          TPSDRIV(MAXSTA), TPSINDX(MAXSTA), TPLENG(MAXSTA)
      INTEGER          NDRIVE(MAXSTA), NHDPOS(MAXSTA)
      CHARACTER        TPOBS*8, DENSITY(MAXSTA)*1, HEADMODE(MAXSTA)*8
      LOGICAL          USETAPE(MAXSTA), USEDISK(MAXSTA), ANYTAPE
      LOGICAL          ALLDISK
      CHARACTER        MEDIA(MAXSTA)*8
C
C                      Calculated items. Some for summaries.
C                      Keep track of core and all, including extras, for 
C                      many of these.  
C
      INTEGER          NRDBCK(MAXSTA), NSTOP(MAXSTA), NRECONF(2,MAXSTA)
      INTEGER          NSTSC(MAXSTA), ENSTSC(MAXSTA), DNSTSC(MAXSTA)
      INTEGER          TPSCN(MAXSTA), ETPSCN(MAXSTA), DTPSCN(MAXSTA)
      DOUBLE PRECISION TPSTART(MAXSCN,MAXSTA)
      DOUBLE PRECISION DATASIZE, EDATASIZ, DDATASIZ
      DOUBLE PRECISION TCORR(MAXSCN,MAXSTA)
      REAL             TPHR(MAXSTA), ETPHR(MAXSTA), DTPHR(MAXSTA)
      REAL             SCNHR(MAXSTA), ESCNHR(MAXSTA), DSCNHR(MAXSTA)
      REAL             MAXDR, EMAXDR, DMAXDR
      REAL             GBYTES(MAXSCN,MAXSTA)
      REAL             TGBYTES(MAXSTA), EGBYTES(MAXSTA), DGBYTES(MAXSTA)
      REAL             TTSYNC(MAXSTA)
      LOGICAL          TWOHEAD, FASTTRK, ALLVLBA
C
      COMMON  /SCHN5/  TPREF, TPLEN, MINPAUSE, PRESTART, TPTIME, 
     1                 TPSTART, DATASIZE, EDATASIZ, DDATASIZ, TCORR, 
     2                 AUTOTAPE, AUTOALOC, AUTOREV, 
     3                 NTPSTA, TPSDRIV, TPSINDX, TPLENG, 
     4                 NDRIVE, NHDPOS, USETAPE, USEDISK, ANYTAPE,
     5                 ALLDISK, NRDBCK, NSTOP, NRECONF, 
     6                 NSTSC, ENSTSC, DNSTSC, 
     7                 TPSCN, ETPSCN, DTPSCN, TPHR, ETPHR, 
     8                 DTPHR, SCNHR, ESCNHR, DSCNHR, 
     9                 MAXDR, EMAXDR, DMAXDR, GBYTES, 
     A                 TGBYTES, EGBYTES, DGBYTES,
     B                 TTSYNC, TWOHEAD, FASTTRK, ALLVLBA, NOREC,
     C                 GRABTIME, GRABGAP
      COMMON  /SCHC5/  DATAPATH, GRABTO, TPOBS, DENSITY, HEADMODE, 
     1                 MEDIA
C
C
C     Calculated observation geometry information:
C                      TSLEW is the slew time from the previous source.
C                      TONSRC is the time the antenna reaches source.
C                      TCORR is the time SCHED predicts the correlator 
C                            will sync up.
C                      Beware that this and the tape info is where 
C                      most of the memory goes.
C
      DOUBLE PRECISION LST1(MAXSCN,MAXSTA), LST2(MAXSCN,MAXSTA)
      DOUBLE PRECISION TONSRC(MAXSCN,MAXSTA), TSLEW(MAXSCN,MAXSTA)
      REAL             EL1(MAXSCN,MAXSTA), AZ1(MAXSCN,MAXSTA)
      REAL             HA1(MAXSCN,MAXSTA), PA1(MAXSCN,MAXSTA)
      REAL             EL2(MAXSCN,MAXSTA), AZ2(MAXSCN,MAXSTA)
      REAL             HA2(MAXSCN,MAXSTA), PA2(MAXSCN,MAXSTA)
      CHARACTER        UP1(MAXSCN,MAXSTA)*1, UP2(MAXSCN,MAXSTA)*1
C
      COMMON  /SCHN6/  LST1, LST2, TONSRC, TSLEW,
     1                 EL1, AZ1, HA1, PA1, 
     2                 EL2, AZ2, HA2, PA2
      COMMON  /SCHC6/  UP1, UP2
C
C     End of SCHED include file.

