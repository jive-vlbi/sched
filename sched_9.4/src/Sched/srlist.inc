C     Include file SRLIST for SCHED.
C
C     This file defines variables that hold minimal, low precision 
C     information about all sources in any catalogs read.  It is not 
C     intended for use by the main schedule creation parts of SCHED, 
C     for which the information is both inadequate in content and 
C     precision.  Rather it is intended as a source of information 
C     for planning functions, mainly RD plots to help the user find 
C     calibrators.
C
C     SRLEPO is a shorthand epoch indicator with "J" = J2000, 
C     "B" = B1950, and "D" = DATE.  All coordinates will be converted
C     to J2000 before the plot section is reached.  Done by SCHPRE.
C
C     SRLRAE and SRLDECE are the errors on the RA and Dec.
C
      INTEGER      MAXSRL
      PARAMETER    (MAXSRL=10000)
C
      INTEGER      SRLN
      CHARACTER    SRLNAME(MAXSRL)*12, SRLCALC(MAXSRL)*1
      CHARACTER    SRLEPO(MAXSRL)*1
      REAL         SRLRA(MAXSRL), SRLDEC(MAXSRL)
      REAL         SRLRAE(MAXSRL), SRLDECE(MAXSRL)
      LOGICAL      SRLUSED(MAXSRL)
C
      COMMON   / SRLIS / SRLN, SRLRA, SRLRAE, SRLDECE, 
     1                   SRLDEC, SRLUSED
      COMMON   / SRCLI / SRLNAME, SRLCALC, SRLEPO
C ----------------------------------------------------------------------

