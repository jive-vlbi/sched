C     Include file for PLOT and subroutines.
C
C
C     The multiple commons used in some cases were the result of 
C     restrictions on common size in some compiler (Microsoft?).
C
C     Note that the non-character variables are ordered in the COMMONs
C     with the larger ones (in terms of bytes) first.  This prevents
C     word alignment problems.
C
C
C
C     Menu Panel parameters:    
C
      INTEGER          PPNCOL(10), PPOPEN, PPNACT
      REAL             PPNDIM(3), PPSDIM(4)
      COMMON  /PPANE/  PPNCOL, PPSDIM, PPNDIM, PPOPEN, PPNACT
C
C     Plot Window parameters:    
C
      CHARACTER        PPTYPE*2
      INTEGER          PPNWIN   
      COMMON  /PPWINN/ PPNWIN
      COMMON  /PPWINC/ PPTYPE
C
C     Menu parameters
C
      INTEGER          PMNBTM
      PARAMETER        (PMNBTM=10)
      CHARACTER*12     PMNVAL(PMNBTM)
      INTEGER          PMNBCK(2), PMNACT(PMNBTM)
      LOGICAL          PMTTYT
      REAL             PMNBUT(4,PMNBTM)
      COMMON /PAMENN/  PMNBUT, PMNACT, PMNBCK, PMTTYT
      COMMON /PAMENC/  PMNVAL
C
C     Menu Stations Area parameters:    
C     The constant PSTMAX is >= MAXSTA
C     The constant PSOMAX is >= MAXSRC
C     The constant PSFMAX is >= MAXSET
C     Both of these are set rather high to avoid problems when
C     MAXSTA and MAXSRC are fiddled.  If the limit is violated,
C     STMSG will catch it.
C
      INTEGER          PSTMAX, PSTPLT, PSOMAX, PSOPLT, PSFMAX 
      PARAMETER        (PSTMAX=1000)
      PARAMETER        (PSTPLT=20)
      PARAMETER        (PSOMAX=5000)
      PARAMETER        (PSOPLT=10)
      PARAMETER        (PSFMAX=100)
      CHARACTER*24     PSTLAB(4)
      LOGICAL          PSTBAS, PSOSCR, PSTSCR
      INTEGER          PSTBCK(PSTMAX, 2), PSOBCK(PSOMAX)
      INTEGER          PSTNUM, PSONUM, PSTCNT, PSOCNT, PSOBM
      REAL             PSTBXP(4, PSTPLT), PSTBXH(4, PSTPLT)
      REAL             PSTBXB(4,2), PSTSUN(4,4), PSTSBR(4) 
      REAL             PSTBXM(4,2), PSOBXP(4, PSOPLT)
      REAL             PSOSUN(4,2), PSOSBR(4), PSOBXM(4,2)
      COMMON  /PASTAC/ PSTLAB
      COMMON  /PASTAN/ PSOBCK, PSTBCK, PSTBXP, PSTBXH, PSOBXP, PSTSUN,
     1                 PSOSUN, PSOBXM, PSTBXM, PSTBXB, PSOSBR, PSTSBR,
     2                 PSOCNT, PSTNUM, PSONUM, PSTCNT, PSOBM,  PSTBAS,
     3                 PSOSCR, PSTSCR
C
C     Menu Files Area parameters
C
      INTEGER          PFLBTM, PFLBXM
      PARAMETER        (PFLBTM=3)
      PARAMETER        (PFLBXM=6)
      CHARACTER*12     PFLLAB(PFLBTM), PFLEXT(PFLBXM)
      CHARACTER*20     PFLVAL(PFLBXM)
      CHARACTER*40     PFLFIL(3)
      INTEGER          PFLBCK, PSFBCK, PSFPOI(PSFMAX), PSFCNT
      REAL             PFLBUT(4,PFLBTM), PFLBOX(4,PFLBXM)
      REAL             PFLTXT(4,2)
      REAL             PSFBXP(4, 6), PSFTXT(4, 2)
      REAL             PSFBXM(4,2), PSFSBR(4)
      COMMON /PAFILC/  PFLVAL, PFLLAB, PFLEXT, PFLFIL
      COMMON /PAFILN/  PSFPOI, PFLBOX, PSFBXP, PFLBUT, PFLTXT,
     1                 PSFTXT, PSFBXM, PSFSBR, PSFBCK, PFLBCK,
     1                 PSFCNT
C
C     Menu Axis Area parameters    
C
      INTEGER          POPBXM, PXYMAX
      PARAMETER        (POPBXM=6)
      PARAMETER        (PXYMAX=13)
      CHARACTER*2      POPTYP(POPBXM)
      CHARACTER*3      PXYTYP(PXYMAX)
      CHARACTER*12     POPVAL(POPBXM), PXSTYP(PXYMAX)
      INTEGER          POPBCK, PXYBCK(2), PXSEXP(4), POPLIM(POPBXM,4)
      INTEGER          PXSLIM(PXYMAX,2), PXSVAL(PXYMAX,4,3)
      INTEGER          PXSSGN(PXYMAX,4), PWLVAL(4)
      INTEGER          POFPOI(PXYMAX), POFVAL(2), PANDAY(3), PKMVAL(4)
      INTEGER          PUTVAL(2,3), PLSVAL(2,3), PSGVAL(2,3), PBMCEL
      INTEGER          PADAYS(3,3), PBMPIX, PBMCTF, PBMPAL, PBMWGT
      INTEGER          PSUEXP, PXYWLE, PXYEXP
      LOGICAL          PASIGN(2), PXYBIG, PYANTN, PBMEXE, PBMCON
      LOGICAL          PXYSUN, PLOSGN, PLOVAL, PXYONE, PXYSEC
      REAL             POPBOX(4, POPBXM), PBMFRQ
      REAL             PXYTXT(4, 5), PXYBXM(4,4), PXYSBR(4,2)
      REAL             PBMTXT(4, 7), PBMBXM(4,14), PBMSBR(4,7)
      REAL             PXLTXT(4, 3), PYBTXT(4,3), PXSDEF(4,4)
      REAL             PXRTXT(4, 3), PYTTXT(4,3), PSUBOX(4), PSUNEL
      REAL             PSUTXT(4), PSUSGN(4), PSUBXM(4), PSUDEF(4)
      REAL             PXSBXM(4, 4), PSGTXT(4,4), PXINI, PXEND
      REAL             PLOBOX(4, 2)
      COMMON  /PAAXIC/ POPTYP, POPVAL, PXYTYP, PXSTYP
      COMMON  /PAAXIN/ PXSVAL, PBMBXM, PXSSGN, PXSLIM, POPBOX, POPLIM,
     1                 PXSBXM, PSGTXT, PXYBXM, PXYTXT, PBMTXT, PBMSBR,
     2                 PXSDEF, PXLTXT, PXRTXT, PYBTXT, PYTTXT, POFPOI,
     3                 PXYSBR, PLOBOX, PADAYS, PUTVAL, PLSVAL, PSGVAL,
     4                 PXSEXP, PKMVAL, PWLVAL, PSUBOX, PSUTXT, PSUSGN, 
     5                 PSUBXM, PSUDEF, PANDAY, PXYBCK, PASIGN, POFVAL,
     6                 PSUNEL, PSUEXP, PXINI,  PXEND,  POPBCK, PBMPIX,
     7                 PBMCTF, PBMPAL, PBMFRQ, PBMWGT, PBMCEL, PXYBIG,
     8                 PYANTN, PBMEXE, PBMCON, PXYSUN, PLOSGN, PLOVAL,
     9                 PXYONE, PXYSEC, PXYWLE, PXYEXP
C
C     Menu Options Area parameters    
C
      INTEGER          PCTMAX
      PARAMETER        (PCTMAX=6)
      CHARACTER*12     PLYLAB(3)
      CHARACTER*14     PBGLAB(3), PCTLAB(PCTMAX)
      INTEGER          PLYLW(2), PLYAW(2), PLYBG(3), PLYCT(PCTMAX,2)
      INTEGER          PBGCK, PCTCK
      REAL             PLYBUT(4,3)
      REAL             PLWBXM(4,4), PLWTXT(4,2), PLWSBR(4,2)
      REAL             PAWBXM(4,4), PAWTXT(4,2), PAWSBR(4,2)
      REAL             PBGBXM(4,4), PBGTXT(4,2), PBGSBR(4,2)
      REAL             PCTBXM(4,6), PCTTXT(4,2), PCTSBR(4,3)
      COMMON  /PAOPTC/ PLYLAB, PBGLAB, PCTLAB
      COMMON  /PAOPTN/ PCTBXM, PLYCT,  PBGBXM, PLWBXM, PAWBXM, PLYBUT,
     1                 PBGTXT, PBGSBR, PLWTXT, PLWSBR, PAWTXT, PAWSBR,
     2                 PCTTXT, PCTSBR, PLYBG,  PLYLW,  PLYAW,  PBGCK,
     3                 PCTCK
C
C    RDPlot Window Control Button
C
      INTEGER          PRDBTN
      PARAMETER        (PRDBTN=8)
      CHARACTER*12     PRDVAL(4,PRDBTN)
      INTEGER          PRDACT(PRDBTN), PRDLAB(PRDBTN), PRDBCK
      INTEGER          PRDDIS(PRDBTN), PZOOCK, PCALCK
      LOGICAL          PRZOOM, PRDCAL, PRDWIN, PRDCAT, PRDNAM, PRWARN
      LOGICAL          PRZOFX, PRDAXI
      REAL             PRDBUT(4,PRDBTN), PZMWIN(4), PCALXY(2)
      DOUBLE PRECISION PZMVAL(4)
      COMMON /PARDPC/  PRDVAL
      COMMON /PARDPN/  PRDBUT, PZMVAL, PZMWIN, PRDACT, PRDDIS, PRDLAB,
     1                 PCALXY, PRDBCK, PZOOCK, PCALCK, PRZOOM, PRDCAL,
     2                 PRDWIN, PRDCAT, PRDNAM, PRWARN, PRZOFX, PRDAXI
C      
C     End of PLOT include file.
C
