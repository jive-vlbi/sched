      SUBROUTINE VXTRAFQ(ISCN, NMODE, OMODE, IFQ)
C
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 250796 
C
C     Adds a couple of extra FQ definitions, because
C     a new mode is detected in VXSCNS and created in VXTRAMD
C     NMODE = MODE # new mode in which these IF come
C     OMODE = MODE # of mode to which changes are made
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      LOGICAL VXCFFQ, FOUND
      INTEGER NMODE, OMODE, ISCN, JSCN, IFQ
C
      CHARACTER VXNMFQ*32
C
      INTEGER IXTRA, ISTA, NTMPST, TMPSTA(MAXSTA), INSCAN, ICH
C ----------------------------------------------------------------------
C
C     start adding a number of modes
C
      DO IXTRA = 1, NMODFQ(OMODE)
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
C              a proper comparison needs to use vxcffq, 
C              because many IF sections are not the straight order number
C
               IF( VXCFFQ(NSETUP(INSCAN,ISTA), 
     1             FQISSET(IMODFQ(IXTRA,OMODE))) ) THEN
                  NTMPST = NTMPST +1
                  TMPSTA(NTMPST) = ISTA
               END IF
            END IF
         END DO
C
C        Update if anything was found
C
         IF( NTMPST .GT. 0 ) THEN
            NMODFQ(NMODE) = NMODFQ(NMODE) + 1
            NFQVEX = NFQVEX + 1
            IF( NFQVEX .GT. MAXMOD ) CALL ERRLOG(
     1          'VXTRAFQ: Number of $FQ defs exceeding MAXMOD,'//
     2          '  array size...')
C
C           Need the SETUPS that are valid for this scan
C
            IMODFQ(NMODFQ(NMODE),NMODE) = NFQVEX
            FQISSET(NFQVEX)=FQISSET(IMODFQ(IXTRA,OMODE))
C     
C           and the corresponding values
C
            NVXCHN(NFQVEX) = NVXCHN(IFQ)
C
C           For a frequency change put it in
C
            DO ICH = 1, NVXCHN(NFQVEX)
               IF( FREQ(ICH,ISCN).GT.1E-6 ) THEN
                  VXLOSUM(ICH,NFQVEX) = FREQ(ICH,ISCN)
               ELSE
                  VXLOSUM(ICH,NFQVEX) = VXLOSUM(ICH,IFQ)
               END IF
C
C            BB can change too
C     
               IF( BW(ICH,ISCN) .GT.1E-6 ) THEN
                  VXBBFILT(ICH,NFQVEX) = BW(ICH,ISCN)
               ELSE
                  VXBBFILT(ICH,NFQVEX) = VXBBFILT(ICH,IFQ)
               END IF
               VXNETSID(ICH,NFQVEX) = VXNETSID(ICH,IFQ)
            END DO
C
C           Tone names and linking to FQ blocks sorted out
C           in VXTONE, but need a list of stations used.
C
            NSTAFQ(NFQVEX,NMODE) = NTMPST
            DO ISTA = 1, NTMPST
               ISTAFQ(ISTA,NFQVEX,NMODE) = TMPSTA(ISTA)
            END DO
C
C           IF names can be obtained from VXNMIF
C           Again 2nd par set .TRUE. to use VXLINK frequencies
C
            FQLINK(NFQVEX) = VXNMFQ( NFQVEX, .TRUE. )
            CALL VXUNQL( NFQVEX, FQLINK )
         END IF
C     
C        done inventing new FQ
C
      END DO
C
      RETURN
      END

