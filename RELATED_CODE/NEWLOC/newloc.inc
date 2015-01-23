C     NEWLOC.INC   Include file for NEWLOC
C
C
      INTEGER           MS, MP, I, MWD
      PARAMETER         (MS=500)
      PARAMETER         (MP=80)
      PARAMETER         (MWD=80)
      CHARACTER         GOTPOS(MS)*4, GOTRAT(MS)*3, GOTOFF(MS)*3
C
C     VLBI position information.
C
      INTEGER           NS, NR, NO, IW
      CHARACTER         STA(MS)*8, PSTA(2)*8, BUFSTA*8
      DOUBLE PRECISION  VLBIX(MS), VLBIY(MS), VLBIZ(MS)
      DOUBLE PRECISION  VLBIEX(MS), VLBIEY(MS), VLBIEZ(MS)
      DOUBLE PRECISION  VLBIRX(MS), VLBIRY(MS), VLBIRZ(MS)
      DOUBLE PRECISION  VLBIOF(MS), VLBIBEG(MS), VLBIEND(MS)
      INTEGER           VLBIJDAY(MS)
      CHARACTER         FRAME(MS)*80
C
C     VLA position information.
C
      INTEGER           NV, IX, IY, IZ
      CHARACTER         VNAME(MP)*8
      DOUBLE PRECISION  VLAX(MP), VLAY(MP), VLAZ(MP)
      DOUBLE PRECISION  IRVLAX(MP), IRVLAY(MP), IRVLAZ(MP)
      DOUBLE PRECISION  VLAAXOF(MS)     
      LOGICAL           READV
      DOUBLE PRECISION  VLALEN, VLAMER, FACTOR
      DOUBLE PRECISION  IRVLAEX(MP), IRVLAEY(MP), IRVLAEZ(MP)
C
C     Input information etc.
C
      INTEGER           NWORDS, WLEN(MWD), YEAR, MONTH, DAY
      INTEGER           NPRT, IPRT
      INTEGER           CYEAR, CMONTH, CDAY
      INTEGER           JDAY, CJDAY
      DOUBLE PRECISION  JDATE, CJDATE, GMST, CL, SL
      CHARACTER         POSFILE*80, VELFILE*80, VLAFILE*80
      CHARACTER         DBFILE*80, OFFFILE*80, NEWFRM*80, VLAFRM*80
      CHARACTER         NEWLOCF*80
      CHARACTER         WORD(MWD)*80, INLINE*256, TEXT*80
C
      INTEGER           IP, VLALD, VLALM
      INTEGER           IPN8, IPPT, ISPT, ISLA, ISN8, IPCEN
      DOUBLE PRECISION  VLALONG, PI, RADSEC, VLALS, MNS
      DOUBLE PRECISION  VLAITS
      DOUBLE PRECISION  VLBN8PT, VLAN8PT, BLONN8PT, ALONN8PT
      DOUBLE PRECISION  BXY8PT, AXY8PT, BLAT8PT, ALAT8PT              
C
C     VLA longitude D,M,S   
C     Now working with VLA "ITRF" coordinates so this is just a small
C     offset from zero to account for any rotation of the VLA and 
C     ITRF frames.  
C
      PARAMETER         (VLALD=0.D0)
      PARAMETER         (VLALM=0.D0)
C
C     Choose which to use.  Without PT, I don't know how we can get
C     this so I am selecting zero for 2014.
C
C      PARAMETER         (VLAITS=0.76D0)
      PARAMETER         (VLAITS=0.D0)
C
      PARAMETER         ( VLALS = VLAITS )
C
C     VLA scaling factor.  Once converted from ns to meters.
C
      PARAMETER         (MNS=1.D0)
C
C     Put things in COMMON.
C
      COMMON  /NEWLCH/ GOTPOS, GOTRAT, GOTOFF,
     1                 STA, PSTA, BUFSTA,
     2                 FRAME, VNAME, POSFILE, VELFILE, VLAFILE,
     3                 DBFILE, OFFFILE, NEWFRM, VLAFRM,
     4                 NEWLOCF, WORD, INLINE, TEXT
C
      COMMON  /NEWLN8/ VLBIX, VLBIY, VLBIZ,
     1                 VLBIEX, VLBIEY, VLBIEZ,
     2                 VLBIRX, VLBIRY, VLBIRZ, VLBIOF, VLBIBEG,
     3                 VLBIEND, VLAX, VLAY, VLAZ, 
     4                 IRVLAX, IRVLAY, IRVLAZ, VLAAXOF,
     5                 VLALEN, VLAMER, FACTOR,
     6                 IRVLAEX, IRVLAEY, IRVLAEZ,
     7                 JDATE, CJDATE, GMST, CL, SL,
     8                 VLALONG, PI, RADSEC,
     9                 VLBN8PT, VLAN8PT, BLONN8PT, ALONN8PT,
     a                 BXY8PT, AXY8PT, BLAT8PT, ALAT8PT
C
      COMMON  /NEWLN4/ I, NS, NR, NO, IW,
     1                 NV, IX, IY, IZ, READV, 
     2                 NWORDS, WLEN, YEAR, MONTH, DAY,
     3                 NPRT, IPRT, CYEAR, CMONTH, CDAY,
     4                 JDAY, CJDAY, VLBIJDAY,
     5                 IP, IPN8, IPPT, ISPT, ISLA, ISN8, IPCEN

C ------------------------------------------------------------------------
