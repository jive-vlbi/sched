      SUBROUTINE VXTRAIF(ISCN, NMODE, OMODE, INTERVAL)
C
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 250796 
C
C     Adds a couple of extra IF definitions, becasue
C     a change of Phase cal insertion is detected in VXSCNS
C     NMODE = MODE # new mode in which these IF come
C     OMODE = MODE # of mode to which changes are made
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      LOGICAL VXCFIF, FOUND
      REAL INTERVAL
      INTEGER NMODE, OMODE, ISCN, JSCN
C
      CHARACTER VXNMIF*32
C
      INTEGER IXTRA, ISTA, NTMPST, TMPSTA(MAXSTA), INSCAN
C ----------------------------------------------------------------------
C
C     start adding a number of modes
C
      DO IXTRA = 1, NMODIF(OMODE)
         NTMPST = 0
         DO ISTA = 1, NSTA
            FOUND = .FALSE.
C
C           check if any of the next scans use this telescope
C           in the same group. 
C
            DO JSCN = ISCN, SCANL
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
C              a proper comparison needs to use vxcfif, 
C              because many IF sections are not the straight order number
C
               IF( VXCFIF(NSETUP(INSCAN,ISTA), 
     1             IFISSET(IMODIF(IXTRA,OMODE))) ) THEN
                  NTMPST = NTMPST +1
                  TMPSTA(NTMPST) = ISTA
               END IF
            END IF
         END DO
C
C        Update if anything was found
C
         IF( NTMPST .GT. 0 ) THEN
            NMODIF(NMODE) = NMODIF(NMODE) + 1
            NIFVEX = NIFVEX + 1
            IF( NIFVEX .GT. MAXMOD ) CALL ERRLOG(
     1          'VXTRAIF: Number of $IF defs exceeding MAXMOD,'//
     2          '  array size...')
C
C           Need the SETUPS that are valid for this scan
C
            IMODIF(NMODIF(NMODE),NMODE) = NIFVEX
            IFISSET(NIFVEX)=IFISSET(IMODIF(IXTRA,OMODE))
C     
C           and the corresponding links
C
            TONEINT(NIFVEX) = INTERVAL
C
C           Tone names and linking to FQ blocks sorted out
C           in VXTONE, but need a list of stations used.
C
            NSTAIF(NIFVEX,NMODE) = NTMPST
            DO ISTA = 1, NTMPST
               ISTAIF(ISTA,NIFVEX,NMODE) = TMPSTA(ISTA)
            END DO
C
C           IF names can be obtained from VXNMIF
C           Again 2nd par set .TRUE. to use VXLINK frequencies
C
            IFLINK(NIFVEX) = VXNMIF( NIFVEX, .TRUE. )
            CALL VXUNQL( NIFVEX, IFLINK )
         END IF
C     
C        done inventing new IF
C
      END DO
C
      RETURN
      END

