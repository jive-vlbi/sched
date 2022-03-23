C     NEWSRC.INC   Include file for NEWSRC
C
C     Note that MFLX should agree with the array size in rdcat.inc
C
      INTEGER           MSRC
      PARAMETER         (MSRC=12000)
      INTEGER           MFLX
      PARAMETER         (MFLX=30)
C
C     The old catalog, which becomes the new catalog as sources are
C     adjusted or added.
C
C     Parameter MAL, then maximum number of aliases, is from rdcat.inc.  
C     Set to 10 last known.
C
      CHARACTER         VER*20, NAME(MAL,MSRC)*12, CALCODE(MSRC)*1
      DOUBLE PRECISION  RA(MSRC), DEC(MSRC)
      DOUBLE PRECISION  RAERR(MSRC), DECERR(MSRC)
      REAL              FLUX(MFLX,MSRC)
      CHARACTER         REMARK(MSRC)*80, EQUINOX(MSRC)*5
      CHARACTER         FLUXREF(MSRC)*15
      INTEGER           ISNEW(MSRC)
C
C     The input catalog.  Originally, each source was processed and
C     added or used to adjust the old catalog.  But it was found that
C     there are issues like sources with the same name so it is useful
C     to have the catalog.
C
      CHARACTER         NEWNAME(MAL,MSRC)*12
      DOUBLE PRECISION  NEWRA(MSRC), NEWDEC(MSRC)
      DOUBLE PRECISION  NEWRAE(MSRC), NEWDECE(MSRC)
      REAL              IFLX(MFLX,MSRC)
      LOGICAL           INOLD(MSRC)
      CHARACTER         INREMARK(MSRC)*80, INEQUIN(MSRC)*5
      CHARACTER         IFLXREF(MSRC)*15
C      
C
      COMMON   / NSNUM /  RA, DEC, RAERR, DECERR, FLUX, ISNEW, 
     1                    NEWRA, NEWDEC, NEWRAE, NEWDECE, IFLX,
     2                    INOLD
      COMMON   / NSCHR /  VER, NAME, CALCODE, REMARK, EQUINOX,
     1                    FLUXREF, NEWNAME, INREMARK, INEQUIN,
     2                    IFLXREF
C
C     Program variables.
C     NSRC is the number of sources in the old input catalog, and then
C     for the full catalog as the old catalog is enhanced by sources 
C     from the new catalog.
C     NCAT is the number of sources in the new input catalog.
C     USRREMRK is shorter than REMARK to allow room to add 
C     number of observations for some formats.
C
      INTEGER             NSRC, NCAT, NREPLACE, NADD
      LOGICAL             KEEPOLD, KEEPFLUX
      DOUBLE PRECISION    RADSEC
      CHARACTER           SRCFILE*80, GEOFILE*80, NEWFILE*80, CHOOSE*4
      CHARACTER           SOUFMT*8
      CHARACTER           USRREMRK*70, USRFLXRF*15
C
      COMMON   / NS2NUM /  NSRC, NCAT, NREPLACE, NADD, 
     1                     KEEPOLD, KEEPFLUX, RADSEC
      COMMON   / SN2CHR /  SRCFILE, GEOFILE, NEWFILE, CHOOSE, SOUFMT,
     1                     USRREMRK, USRFLXRF
C---------------------------------------------------------------------
