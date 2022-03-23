C     NEWSRC.INC   Include file for NEWSRC
C
C     Note that MFLX should agree with the array size in rdcat.inc
C
      INTEGER           MSRC
      PARAMETER         (MSRC=10000)
      INTEGER           MFLX
      PARAMETER         (MFLX=30)
C
C     The catalog.
C     Parameter MAL is from rdcat.inc.  Set to 10 last known.
C
      CHARACTER         VER*20, NAME(MAL,MSRC)*12, CALCODE(MSRC)*1
      DOUBLE PRECISION  RA(MSRC), DEC(MSRC), SEP(MSRC)
      REAL              RAERR(MSRC), DECERR(MSRC)
      REAL              FLUX(MFLX,MSRC), PEAKFLX(MSRC), RESOLV(MSRC)
      CHARACTER         REMARK(MSRC)*80, EQUINOX(MSRC)*5
      CHARACTER         FLUXREF(MSRC)*15
      LOGICAL           KEEP(MSRC)
C
      COMMON   / NSNUM /  RA, DEC, SEP, RAERR, DECERR, FLUX, PEAKFLX,
     1                    KEEP
      COMMON   / NSCHR /  VER, NAME, CALCODE, REMARK, EQUINOX,
     1                    FLUXREF
C
C     Program variables.
C
      INTEGER           NSRC
      CHARACTER         SRCFILE*80, GEOFILE*80, NEWFILE*80
C
      COMMON   / NS2NUM /  NSRC
      COMMON   / SN2CHR /  SRCFILE, GEOFILE, NEWFILE
C---------------------------------------------------------------------
