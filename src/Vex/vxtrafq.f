      SUBROUTINE VXTRAFQ(ISCN, NMODE, OMODE, KFQ)
C
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 250796 
C
C     Incorporating the effect of allowing sideband inversion
C     at the correlator - adds CORINV to the LO sum.  Also
C     adding more comments to help me understand the routine
C     because setup info like sideband are not transfering 
C     correctly.  This is because the IFQ in the call is
C     really only for some of the stations.  RCW  Oct. 13, 2011.  
C     Reset IFQ in the call to KFQ to get rid of it.  Set IFQ
C     below.
C
C     Adds a couple of extra FQ definitions, because
C     a new mode is detected in VXSCNS and created in VXTRAMD.
C     This routine is called from VXTRAMD
C     NMODE = MODE # new mode in which these IF come
C     OMODE = MODE # of mode to which changes are made
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      LOGICAL VXCFFQ, FOUND
      INTEGER NMODE, OMODE, ISCN, JSCN, IFQ, KFQ
C
      CHARACTER VXNMFQ*32
C
      INTEGER IXTRA, ISTA, NTMPST, TMPSTA(MAXSTA), INSCAN, ICH
C ----------------------------------------------------------------------
C
C     Start adding a number of modes
C
      DO IXTRA = 1, NMODFQ(OMODE)
         NTMPST = 0
         DO ISTA = 1, NSTA
            FOUND = .FALSE.
C
C           Check if any of the next scans, including the current
C           one, use this telescope in the same setup file.  
C           The original comment said setup group, but that is not 
C           literally true when looking at different stations.
C           Except when an antenna comes in late, this will likely
C           end up with INSCAN = ISCN.
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
C              See if this is the same frequency setup as the FQ mode
C              within OMODE that is being tested.
C              A proper comparison needs to use function VXCFFQ, 
C              because many IF sections are not the straight order number
C              VXCFFQ is true if the frequency blocks are identical.
C              It compares pol, side1, losum, bbfilt, bbc for each channel.
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
C           RCW Oct 16, 2011.  IFQ (now KFQ) as defined in the 
C           calling routine (IMODFQ(1,IMODE) is not for all 
C           stations.  Get the right one here.
C
            IFQ = IMODFQ(IXTRA,OMODE)
C
C
            NVXCHN(NFQVEX) = NVXCHN(IFQ)
C
C           For a frequency change put it in.  Take into account
C           a sideband inversion if one was needed, mainly for
C           the RDBE (Oct 13, 2011 RCW)
C
            DO ICH = 1, NVXCHN(NFQVEX)
               IF( FREQ(ICH,ISCN).GT.1E-6 ) THEN
                  ISTA = TMPSTA(NTMPST)
                       VXLOSUM(ICH,NFQVEX) = FREQ(ICH,ISCN) + 
     1                     CORINV(ICH,NSETUP(ISCN,ISTA))
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

