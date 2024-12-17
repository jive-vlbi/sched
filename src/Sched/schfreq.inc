C    schfreq.inc
C
C    Include file for SCHED to hold station frequency setups as read 
C    from the frequency catalog.  The default frequency catalog is in
C    the file $SCHED/catalogs/freq.dat
C
C    Be a bit careful with terminology.  Frequency sets are not what is
C    here, but rather are related to setup groups.  This file relates
C    to the frequency data in the frequency catalog.  I might want to
C    do someting to make this difference a bit less potentially confusing
C    some day.
C
C     Parameters:
C           MFREQ    - Max number of frequency setups.
C           MFSTA    - Max number of stations described by a freq setup.
C           MFIF     - Max number of IF's

C           
      INTEGER       MFREQ, MFSTA, MFIF
      PARAMETER     (MFREQ=1200)
      PARAMETER     (MFSTA=10)
      PARAMETER     (MFIF=8)
C
C     Items common to all stations:
C        NFREQ     Number of frequency groups.
C        PRIO      Priority (low is better).
C        FSTNAM    Station names.
C        FIFNAM    IF name for each IF.  Items below are per IF.
C        FRF1      Lower bound of frequencies that can be observed.
C        FRF2      Upper bound of frequencies that can be observed.
C        FLO1      "FIRSTLO".
C        FPOL      Polarization.
C        FFE       Band name.
C        FRNAME    A name for the group.
C        FRNOTE    Informational note.
C
      INTEGER           NFREQ
      INTEGER           PRIO(MFREQ)
      INTEGER           FNIF(MFREQ)
      CHARACTER         FSTNAM(MFSTA,MFREQ)*8
      CHARACTER         FIFNAM(MFIF,MFREQ)*2
      CHARACTER         FALTIF(MFIF,MFREQ)*2
      DOUBLE PRECISION  FRF1(MFIF,MFREQ), FRF2(MFIF,MFREQ)
      DOUBLE PRECISION  FLO1(MFIF,MFREQ)
      CHARACTER         FRNAME(MFREQ)*12
      CHARACTER         FRNOTE(MFREQ)*80
      CHARACTER         FPOL(MFIF,MFREQ)*3
      CHARACTER         FFE(MFIF,MFREQ)*4
C
C     Some stuff rather specific to the VLBA.
C        FSYN      Synthesizer setting for 3 VLBA synthesizers.
C        FCH1RF1   Lower bound for Channel 1.  To ensure correct filter
C                  is used at 2 cm.
C        FCH1RF2   Upper bound for Channel 1.
C        FRCP50CM  Command for 50 cm filter in RCP.
C        FLCP50CM  Command for 50 cm filter in LCP.
C        FDUALX    Xband wide band setup.
C
      DOUBLE PRECISION  FSYN(3,MFREQ)
      DOUBLE PRECISION  FCH1RF1(MFIF,MFREQ), FCH1RF2(MFIF,MFREQ)
      CHARACTER         FRCP50CM(MFREQ)*6, FLCP50CM(MFREQ)*6
      LOGICAL           FDUALX(MFREQ)
C
C     Additional items for the VLA.
C
C     CHNSTA restricts the station name that can use that IF.
C     In both cases, they are actually assumed to be
C     hooked to VLA IFs A and D.  If anyone wants something with
C     IF's set to different frequencies, they are going to have
C     to make their own setup files.
C
C        FVBAND    "vlaband"
C        FVBW      "vlabw"
C        FVFEAB    "vlafeab"
C        FVFECD    "vlafecd"
C        FVSYNA    "vlasyna"
C        FVSYNB    "vlasynb"
C        FVFLKA    "flukea"
C        FVFLKB    "flukeb"
C        FVFILT    "fefilter"
C
      CHARACTER         FVCHNSTA(MFIF,MFREQ)*8
      CHARACTER         FVBAND(MFREQ)*2
      CHARACTER         FVBW(MFREQ)*4
      CHARACTER         FVFILT(MFREQ)*4
      DOUBLE PRECISION  FVFEAB(MFREQ), FVFECD(MFREQ)
      DOUBLE PRECISION  FVSYNA(MFREQ), FVSYNB(MFREQ)
      DOUBLE PRECISION  FVFLKA(MFREQ), FVFLKB(MFREQ)
C
C     The commons.
C
      COMMON / FRQN /  FRF1, FRF2, FLO1, FVFEAB, FVFECD,
     1                 FSYN, FCH1RF1, FCH1RF2,
     2                 FVSYNA, FVSYNB, FVFLKA, FVFLKB, 
     3                 NFREQ, PRIO, FNIF, FDUALX
      COMMON / FRQC /  FSTNAM, FIFNAM, FALTIF, FRNAME, FRNOTE, FPOL, 
     1                 FFE, FRCP50CM, FLCP50CM, FVCHNSTA, 
     2                 FVBAND, FVBW, FVFILT
C  ------------------------------------------------------------------



