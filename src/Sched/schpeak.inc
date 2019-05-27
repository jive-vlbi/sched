C     Include file for parameters related to the use of reference
C     pointing scans.
C     This include must follow sched.inc because of MAXSTA.
C
      INTEGER      MPKGRP, MPKSRC, MPKSTA
      PARAMETER    (MPKGRP=5)
      PARAMETER    (MPKSRC=100)
      PARAMETER    (MPKSTA=MAXSTA)
C
      LOGICAL      DOPOINT
      INTEGER      NPKGRP, NPKSTA(MPKGRP), NPKSRC(MPKGRP)
      INTEGER      PKLSET(MPKGRP), PKLSETL(MPKGRP)
      CHARACTER    PKSRC(MPKSRC,MPKGRP)*12
      INTEGER      PKSRNUM(MPKSRC,MPKGRP)
      INTEGER      PKGROUP(MPKSTA)
      REAL         PKMINEL(MPKSTA)
      CHARACTER    PSRCFILE*1024
      CHARACTER    PKLINES(MPKGRP)*8
      CHARACTER    PSETFILE(MPKGRP)*1024
      CHARACTER    PLSETFIL(MPKGRP)*1024
      CHARACTER    PVLAMODE(MPKGRP)*2
      CHARACTER    PKSTA(MPKSTA,MPKGRP)*8
      DOUBLE PRECISION  PKDWELL(MPKGRP), PKMINFQ(MPKGRP)
C
      COMMON  /SCHPEAKN/ PKDWELL, PKMINFQ, DOPOINT, NPKGRP, 
     1                   NPKSTA, NPKSRC, PKLSET, PKLSETL, PKSRNUM, 
     2                   PKGROUP, PKMINEL
      COMMON  /SCHPEAKC/ PKSRC, PSRCFILE, PKLINES, PSETFILE,
     1                   PLSETFIL, PVLAMODE, PKSTA
C ---------------------------------------------------------------------

