C     Include file for PROJECTIONS in RD Plot
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
C     Array dimensions parameters:    
C
      REAL             PRJPI, PRJPI2, PRJPI4, PRJRAD
      PARAMETER        (PRJPI=3.1415927)
      PARAMETER        (PRJPI2=PRJPI/2.)
      PARAMETER        (PRJPI4=PRJPI/4.)
      PARAMETER        (PRJRAD=PRJPI/648000.0)
C
      REAL             PJXDIM, PJYDIM, PJXOFF, PJYOFF
      PARAMETER        (PJXDIM=20.0)
      PARAMETER        (PJYDIM=13.0)
      PARAMETER        (PJXOFF=10.0)
      PARAMETER        (PJYOFF=6.5)
C
      INTEGER          PJXNOT, PJYNOT
      PARAMETER        (PJXNOT=4)
      PARAMETER        (PJYNOT=5)
C
      LOGICAL          PRJDEF
      INTEGER          PRJTYP, PZOCNT
      REAL             PZXDIM, PZYDIM, PZXOFF, PZYOFF
      REAL             PRJR, PRJN, PRJF, PRJRO0, PRJALF, PRJDEL
      REAL             PRJRX, PRJRY, PJYOFT
C
      COMMON /PRDPRJ/  PRJR,   PRJN,   PRJF,   PRJRO0, PRJALF,
     1                 PZXDIM, PZYDIM, PZXOFF, PZYOFF, PRJDEL,
     2                 PRJRX,  PRJRY,  PJYOFT, PRJTYP, PZOCNT, PRJDEF
C      
C     End of BEAM include file.
C
