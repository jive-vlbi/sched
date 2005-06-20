C
C     Set VEX version and version of VX routines
C      
      CHARACTER VEXVER*3, VXSOFT*3
      PARAMETER (VEXVER = '1.5', VXSOFT = '.82')
C
C     Store number of different def in sections
C     this sturcture defines the referncing of different defs
C     in different blocks.
C
C     For each block XX there is a similar structure, where
C     XX = MD modes (not the same structure)
C     XX = FQ Frequency
C     XX = TR tracks
C     XX = IF 
C     XX = BB BBC
C     XX = PO pass_order
C     XX = HP head_position
C     XX = PH phase_cal
C     XX = RL barrel_roll
C
C     NXXVEX              total number of defs in block XX
C     NMODXX(IMODE)       number of XX defs referenced in mode IMODE
C     IMODXX(I,IMODE)     list (I-NMODXX) of defs XX referenced in mode IMODE
C     XXISET(IXX)         the original SCHED set (KS) that has the parameters
C                         block XX def number IXX 
C     NSTAXX(IXX,IMODE)   number of stations that use block XX def #IXX, 
C                         used for mode imode
C     ISTAXX(I,IXX,IMODE) list (1-NSTAXX) of stations that use block 
C                         IXX in IMODE
C     XXLINK(IXX)         name of IXX block def
C
C     the DA, SI, AN section is STATION based
C     XX = DA data_acquisition
C     SI = Site
C     An = Antenna
C
C     NXXVEX          total number of defs in block XX
C     STAXX(ISTA)     which defs XX referenced in station ISTA
C     XXLINK(IXX)     name of IXX block def
C
C     Finally the PR is global
C     XX = PR Proc_Timing
C
C     NPRVEX
C     PRLINK
C
      INTEGER MAXMOD
      PARAMETER ( MAXMOD = MAXSET )
C
      INTEGER NMDVEX, NFQVEX, NTRVEX, NIFVEX, NBBVEX, NHPVEX, NPOVEX
      INTEGER NRLVEX, NPHVEX, NDAVEX, NSIVEX, NANVEX, NPRVEX
C
C     which antenna used which set for all modes
C
      INTEGER MODSET(MAXSTA,MAXMOD)
C
C     store how many XX blocks are in each mode and which they are
C
      INTEGER NMODFQ(MAXMOD), IMODFQ(MAXMOD,MAXMOD)
      INTEGER NMODIF(MAXMOD), IMODIF(MAXMOD,MAXMOD) 
      INTEGER NMODBB(MAXMOD), IMODBB(MAXMOD,MAXMOD) 
      INTEGER NMODTR(MAXMOD), IMODTR(MAXMOD,MAXMOD) 
      INTEGER NMODHP(MAXMOD), IMODHP(MAXMOD,MAXMOD)
      INTEGER NMODPO(MAXMOD), IMODPO(MAXMOD,MAXMOD) 
      INTEGER NMODRL(MAXMOD), IMODRL(MAXMOD,MAXMOD) 
      INTEGER NMODPH(MAXMOD), IMODPH(MAXMOD,MAXMOD) 
      INTEGER ISTADA(MAXSTA), ISTASI(MAXSTA)
      INTEGER ISTAAN(MAXSTA)
C
C     for every MD we need to keep which file it came from
C     enables us to have a MODE for every scan
C
      INTEGER MDISFIL(MAXMOD)
C     A VLBA station may have its own setup, which is different
C     from the generic 'VLBA' setup which the other VLBA stations use.
C     Need to keep track of these 'special' VLBA stations and modes.
      LOGICAL VLBASSTA(MAXSTA, MSET), VLBASSET(MSET)
C
C     and the set that has the definitions for all these blocks
C     for mode work the other way (up rather than down)
C
      INTEGER FQISSET(MAXMOD), IFISSET(MAXMOD)
      INTEGER BBISSET(MAXMOD), TRISSET(MAXMOD)
      INTEGER HPISSET(MAXMOD), POISSET(MAXMOD)
      INTEGER RLISSET(MAXMOD), PHISSET(MAXMOD)
C
C     and which stations use which $BLOCK
C
      INTEGER NSTAFQ(MAXMOD,MAXMOD), ISTAFQ(MAXSTA,MAXMOD,MAXMOD)
      INTEGER NSTABB(MAXMOD,MAXMOD), ISTABB(MAXSTA,MAXMOD,MAXMOD)
      INTEGER NSTATR(MAXMOD,MAXMOD), ISTATR(MAXSTA,MAXMOD,MAXMOD)
      INTEGER NSTAIF(MAXMOD,MAXMOD), ISTAIF(MAXSTA,MAXMOD,MAXMOD)
      INTEGER NSTAHP(MAXMOD,MAXMOD), ISTAHP(MAXSTA,MAXMOD,MAXMOD)
      INTEGER NSTARL(MAXMOD,MAXMOD), ISTARL(MAXSTA,MAXMOD,MAXMOD)
      INTEGER NSTAPH(MAXMOD,MAXMOD), ISTAPH(MAXSTA,MAXMOD,MAXMOD)
      INTEGER NSTAPO(MAXMOD,MAXMOD), ISTAPO(MAXSTA,MAXMOD,MAXMOD)
C
C     names of all the links
C
      CHARACTER FQLINK(MAXMOD)*32, TRLINK(MAXMOD)*32, IFLINK(MAXMOD)*32
      CHARACTER BBLINK(MAXMOD)*32, HPLINK(MAXMOD)*32, POLINK(MAXMOD)*32
      CHARACTER RLLINK(MAXMOD)*32, PHLINK(MAXMOD)*32, MDLINK(MAXMOD)*32
      CHARACTER DALINK(MAXSTA)*32, SILINK(MAXSTA)*32, ANLINK(MAXSTA)*32 
      CHARACTER PRLINK(MAXMOD)*32
C
      COMMON /VEX1/ NMDVEX, NFQVEX, NTRVEX, NIFVEX, NBBVEX, 
     1     NHPVEX, NPOVEX, NRLVEX, NPHVEX, 
     2     NDAVEX, NSIVEX, NANVEX, NPRVEX,
     1     MDISFIL, MODSET, VLBASSTA, VLBASSET,
     1     NMODFQ, IMODFQ, FQISSET, NSTAFQ, ISTAFQ,
     1     NMODIF, IMODIF, IFISSET, NSTAIF, ISTAIF,
     1     NMODBB, IMODBB, BBISSET, NSTABB, ISTABB,
     1     NMODTR, IMODTR, TRISSET, NSTATR, ISTATR,
     1     NMODHP, IMODHP, HPISSET, NSTAHP, ISTAHP,
     1     NMODPO, IMODPO, POISSET, NSTAPO, ISTAPO,
     1     NMODRL, IMODRL, RLISSET, NSTARL, ISTARL,
     1     NMODPH, IMODPH, PHISSET, NSTAPH, ISTAPH,
     1     ISTADA, ISTASI, ISTAAN,
     2     FQLINK, TRLINK, IFLINK, BBLINK, HPLINK, POLINK,
     3     RLLINK, PHLINK, MDLINK, DALINK, SILINK, ANLINK,
     4     PRLINK
C
C     the tones are the first to be stored not constructed on the fly
C     and so are a few frequency settings
C
      INTEGER NTONES(MAXMOD), NVXCHN(MAXMOD), FQTOPH(MAXMOD)
      INTEGER NTONDET(MAXCHN,MAXMOD), ITONDET(2*MAXPC,MAXCHN,MAXMOD)
      INTEGER FQTOIF(MAXMOD), TONCHN(MAXCHN, MAXMOD)
      DOUBLE PRECISION VXLOSUM(MAXCHN, MAXMOD)
      LOGICAL DODETECT(MAXMOD)
      REAL TONEINT(MAXMOD)
      REAL VXBBFILT(MAXCHN,MAXMOD)
      CHARACTER TONLNK(MAXCHN,MAXMOD)*5, VXNETSID(MAXCHN, MAXMOD)*1
      COMMON /VEX2/ NTONES, FQTOPH, FQTOIF, TONCHN, TONEINT, DODETECT,
     1    NTONDET, ITONDET,
     1    TONLNK, NVXCHN, VXNETSID, VXLOSUM, VXBBFILT
C
C     Here is scan related info, as mode can change in scans
C
      INTEGER MODSCN(MAXSCN)
      COMMON /VEX3/ MODSCN
C
C     Commonly used character expressions
C
      CHARACTER COM*1, SEP*1, COL*1, LNK*1, BLN*1, QOT*1
      CHARACTER COMLIN*79
C
      PARAMETER (COM = '*', SEP = ';', COL = ':', LNK = '&', 
     1     QOT = '"', BLN = ' ',
     2     COMLIN = '*---------------------------------------'//
     3     '---------------------------------------' )
C
