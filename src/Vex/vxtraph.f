      SUBROUTINE VXTRAPH(ISCN, NMODE, OMODE, INTERVAL, NTONE,
     1    NTONTMP, ITONTMP )
C
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 250796 
C
C     Adds a couple of extra PH definitions, becasue
C     a change of Phase cal insertion is detected in VXSCNS
C     NMODE = MODE # new mode in which these PH come
C     OMODE = MODE # of mode to which changes are made
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      REAL INTERVAL
      INTEGER NMODE, OMODE, ISCN, NTONE, NTONTMP(MAXCHN)
      INTEGER ITONTMP(2*MAXPC, MAXCHN)
C
      LOGICAL VXCFPH, FOUND
      CHARACTER VXNMPH*32
C
      INTEGER IXTRA, IP, ICH, ISTA, JSCN, INSCAN
C ----------------------------------------------------------------
C
      NMODPH(NMODE) = NMODPH(OMODE)
C
C     start adding a number of modes
C
      DO IXTRA = 1, NMODPH(NMODE)
         NPHVEX = NPHVEX + 1
         IF( NPHVEX .GT. MAXMOD ) CALL ERRLOG(
     1       'VXTRAPH: Number of $PH defs exceeding MAXMOD '//
     2       'array size...')
C
C        Need the SETUPS that are valid for this scan
C
         IMODPH(IXTRA,NMODE) = NPHVEX
         PHISSET(NPHVEX)=PHISSET(IMODPH(IXTRA,OMODE))
C     
C        and the corresponding links
C
         IF( INTERVAL .GT. 1E-15 ) DODETECT(NPHVEX) = .TRUE.
         NTONES(NPHVEX) = NTONE
         DO ICH = 1, NTONES(NPHVEX)
            NTONDET(ICH,NPHVEX) = NTONTMP(ICH)
            DO IP = 1, NTONTMP(ICH)
               ITONDET(IP,ICH,NPHVEX) = ITONTMP(IP,ICH)
            END DO
         END DO
C
C        Tone names and linking to FQ blocks sorted out
C        in VXTONE, but need a list of stations used.
C
         DO ISTA = 1, NSTA
            FOUND = .FALSE.
            DO JSCN = ISCN, SCANL
C
C              Look whether used in any scans which use this Group
C
               IF( SETNUM(ISCN) .EQ. SETNUM(JSCN) .AND. 
     1             STASCN(JSCN,ISTA) ) THEN 
                  FOUND = .TRUE.
                  INSCAN = JSCN
                  GOTO 100
               END IF
            END DO
C
  100       CONTINUE
            IF( FOUND ) THEN
C     
C           No danger to go over MAXSTA here
C
               IF( VXCFPH(NSETUP(INSCAN,ISTA), PHISSET(NPHVEX))) THEN 
                  NSTAPH(NPHVEX,NMODE) = NSTAPH(NPHVEX,NMODE) + 1
                  ISTAPH(NSTAPH(NPHVEX,NMODE),NPHVEX,NMODE) = ISTA
               END IF
            END IF
         END DO
C
C        IF names can be obtained from VXNMPH
C        Again 2nd par set .TRUE. to use VXLINK frequencies
C
         PHLINK(NPHVEX) = VXNMPH( NPHVEX, .TRUE. )
         CALL VXUNQL( NPHVEX, PHLINK )
C     
C        done inventing new PH
C
      END DO
C
      RETURN
      END

