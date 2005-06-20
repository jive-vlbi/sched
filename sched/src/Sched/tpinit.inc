C     Include file to hold the tape initialization parameters
C     as read from input until they can be transfered to the
C     station based arrays stored in sched.inc.  This will
C     only be used by TPINI and TPTPNS.  It uses a parameter
C     set in sched.inc so it should follow inclusion of that
C     file.
C
C     In this file, the parameters are stored according to
C     the groups in the tape initialization input.  The
C     similar parameters in sched.inc will be stored using
C     the usual ISTA index to the station list.  TPTPNS will
C     make the transfer.  This was done to allow station 
C     dependent changes to some parameters when, for example
C     using the two head MarkIV mode when NHDPOS must be reduced.
C
C     Some day think about getting rid of some of this, especially
C     the option of specifying many stations in the tapefile.  I
C     don't think it has ever been used.  It was meant to deal with
C     the multiple projects per tape case, but that ended up being
C     done by another scheme (automatic tape allocation).
C
      INTEGER          RNTPSTA
      INTEGER          RTPNS(MAXSTA)
      DOUBLE PRECISION RTPTIME(MAXSTA)
      INTEGER          RTPSDRIV(MAXSTA), RTPSINDX(MAXSTA)
      INTEGER          RTPLENG(MAXSTA)
      INTEGER          RNDRIVE(MAXSTA), RNHDPOS(MAXSTA)
      CHARACTER        RTPSTA(MAXSTA)*8, RTPOBS*8, RDENSITY(MAXSTA)*1
      CHARACTER        RHEADMOD(MAXSTA)*8, RMEDIA(MAXSTA)*8

      COMMON  /RSCHN5/ RTPTIME, RNTPSTA, RTPNS, RTPSDRIV, 
     1                 RTPSINDX, RTPLENG, RNDRIVE, RNHDPOS
      COMMON  /RSCHC5/ RTPSTA, RTPOBS, RDENSITY, RHEADMOD, RMEDIA
C  ---------------------------------------------------------------------
