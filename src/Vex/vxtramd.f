      SUBROUTINE VXTRAMD(IMODE,IFS)
      IMPLICIT NONE
C
C     Make a new mode, NMDVEX.  Called by VXSCNS when it encounters
C     a need for a mode because of FREQ, BW, PCAL.
C     IMODE is the current VEX mode template, IFS (and IPS before pcal
C     sets were removed) is the augmenting freq set for one of the 
C     stations needing the new mode.
C     RCW Oct. 15, 2011.  Adding some comments while debugging 
C     transfer of incorrect setup data (sideband this time) from setup file.
C     RCW Sep. 10, 2013.  Removing pulse cal sets.  They have been merged
C     with the frequency sets.  Make case of PulseCal name portion from
C     the frequency set upper case for consistency with old files.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'       
C
      INTEGER ISCN, ICH, ISET, ISETFL, IMODE, I, IP, IFS
      INTEGER IFQ, IPH, IIF
      INTEGER ISTA, NTMP, TTMPCHN(MAXCHN), IXX
      INTEGER NTONTMP(MAXCHN), ITONTMP(MAXTON,MAXCHN)
      INTEGER NPER, NFC1, NFCN, LEN1, LPOS
      REAL    PHTONE
      LOGICAL MATCHPH
      CHARACTER NAME*32, UPCAL*4, FRTXT*16
C
C     Setup the necessary pointers
C *********************************************************************
C  This gets into trouble because these really need to be station
C  dependent when they are used to pass parameters - certainly parameters
C  that might vary by station.  I have fixed the use of IFQ in VXTRAFQ,
C  but these other parameters should also be dealt with in this routine.
C  I don't have time to deal with them now.  That means that the tone
C  specifications, in particular, may be wrong at least with sideband
C  inversion.  RCW  Oct. 16, 2011.
C *********************************************************************
C
      IFQ = IMODFQ(1,IMODE)
      IPH = IMODPH(1,IMODE)
      IIF = IMODIF(1,IMODE)
C
C     New MD increase numbers
C
      NMDVEX = NMDVEX + 1
      IF( NMDVEX .GT. MAXMOD ) CALL ERRLOG(
     1    'VXSCNS: Number of $MODE defs '//
     2    'exceeding MAXMOD aray size...')
C
C     The reference to the original set will be 
C     used for samprate, number of bits etc.
C
      ISCN = FSETSCN(IFS)
      ISETFL = SETNUM(ISCN)
      MDISFIL(NMDVEX) = ISETFL
C
C     Need first of the ISET SETUPS that is valid for this scan
C
      ISET = -1
      DO ISTA = 1, NSTA
         IF( STASCN(ISCN,ISTA) ) THEN
            ISET = NSETUP(ISCN,ISTA)
            GOTO 100
         END IF
      END DO
      IF( ISET .LT. 0 ) CALL ERRLOG('VXTRAMD: Scan not observed')
  100 CONTINUE
C     
C     And name the mode, currently nothing special
C
      NAME = ' '
      IF( FREQ(1,ISCN) .GT. 1E-6 ) THEN
         IF( .NOT. DOPCAL(ISCN) ) THEN
            NAME(1:10) = 'SwitchFreq'
            NPER=7
            CALL FRCHAR( FREQ(1,ISCN), NPER, NFC1, NFCN, FRTXT )
            WRITE( NAME(11:25), '(A)' ) FRTXT(NFC1:NFCN)
            LPOS = LEN1( NAME ) + 1
            WRITE( NAME(LPOS:LPOS+2), '(A)' ) 'MHz'
         ELSE
            NAME(1:8) = 'Doppler@'
            WRITE( NAME(9:20), '( A12 )' ) DOPSRC(ISCN)
         END IF
      ELSE
         IF( BW(1,ISCN) .GT. 1E-6 ) THEN
            NAME(1:8) = 'SwitchBW'
            IF( BW(1,ISCN) .LT. 1.0 ) THEN
               WRITE( NAME(9:14), '( I3, A3 )' ) 
     1             NINT( 1E3 * BW(1,ISCN)), 'kHz'
            ELSE IF (BW(1,ISCN) .GT. 10.0 ) THEN
               WRITE( NAME(9:16), '( F5.2, A3 )' ) BW(1,ISCN), 'MHz'
            ELSE
               WRITE( NAME(9:15), '( F4.2, A3 )' ) BW(1,ISCN), 'MHz'
            END IF
         ELSE
            NAME(1:8) = 'PulseCal'
            UPCAL = FSPCAL(IFS)
            CALL UPCASE( UPCAL )
            WRITE( NAME(9:12), '( A4 )' ) UPCAL
         END IF
      END IF
      MDLINK(NMDVEX) = NAME
      CALL VXUNQL( NMDVEX, MDLINK )
C
      IF( DEBUG ) THEN 
         WRITE( MSGTXT, '( A, A, A )' )
     1       'VXSCNS (VXTRAMD): Mode change in schedule',
     2       ', inserted $MODE:', MDLINK(NMDVEX)
         CALL WLOG( 1, MSGTXT )
      END IF
C
C     Always call new FQ (can be multiple now) 
C
C     IFQ is set above to be  IFQ = IMODFQ(1,IMODE). IMODFQ is the list of 
C     FQ blocks in IMODE.  VXTRAFQ takes a number of parameters from the 
C     IFQ entry.  But IMODFQ is station dependent.  Only using the first 
C     one causes wrong parameters to be passed.  In particular, the wrong 
C     netsideband is passed when correlator sideband inversion is used.  
C     This has to be fixed in VXTRAFQ inside the loop through FQ blocks.  
C     This is a problem was found in testing with egrdbe2.key in the TEST 
C     area, which includes GBT at K band with a sideband inversion, plus 
C     a FREQ spec part way through (would do the same if using Doppler).
C     So I modified VXTRAFQ to not use the IFQ from the call.  RCW
C     Oct 16, 2011.
C 
      CALL VXTRAFQ(ISCN, NMDVEX, IMODE, IFQ)
C     
C     Copy other links to this new MODE
C
      NMODBB(NMDVEX) = NMODBB(IMODE)
      DO I = 1, NMODBB(NMDVEX)
         IMODBB(I,NMDVEX) = IMODBB(I,IMODE)
         IXX = IMODBB(I,NMDVEX)
         NSTABB(IXX,NMDVEX) = NSTABB(IXX,IMODE)
         DO ISTA = 1, NSTABB(IXX,NMDVEX)
            ISTABB(ISTA,IXX,NMDVEX) = ISTABB(ISTA,IXX,IMODE)
         ENDDO
      END DO 
C     
      NMODTR(NMDVEX) = NMODTR(IMODE)
      DO I = 1, NMODTR(NMDVEX)
         IMODTR(I,NMDVEX) = IMODTR(I,IMODE)
         IXX = IMODTR(I,NMDVEX)
         NSTATR(IXX,NMDVEX) = NSTATR(IXX,IMODE)
         DO ISTA = 1, NSTATR(IXX,NMDVEX)
            ISTATR(ISTA,IXX,NMDVEX) = ISTATR(ISTA,IXX,IMODE)
         ENDDO
      END DO
C
      NMODHP(NMDVEX) = NMODHP(IMODE)
      DO I = 1, NMODHP(NMDVEX)
         IMODHP(I,NMDVEX) = IMODHP(I,IMODE)
         IXX = IMODHP(I,NMDVEX)
         NSTAHP(IXX,NMDVEX) = NSTAHP(IXX,IMODE)
         DO ISTA = 1, NSTAHP(IXX,NMDVEX)
            ISTAHP(ISTA,IXX,NMDVEX) = ISTAHP(ISTA,IXX,IMODE)
         ENDDO
      END DO
C     
      NMODPO(NMDVEX) = NMODPO(IMODE)
      DO I = 1, NMODPO(NMDVEX)
         IMODPO(I,NMDVEX) = IMODPO(I,IMODE)
         IXX = IMODPO(I,NMDVEX)
         NSTAPO(IXX,NMDVEX) = NSTAPO(IXX,IMODE)
         DO ISTA = 1, NSTAPO(IXX,NMDVEX)
            ISTAPO(ISTA,IXX,NMDVEX) = ISTAPO(ISTA,IXX,IMODE)
         ENDDO
      END DO
C     
      NMODRL(NMDVEX) = NMODRL(IMODE)
      DO I = 1, NMODRL(NMDVEX)
         IMODRL(I,NMDVEX) = IMODRL(I,IMODE)
         IXX = IMODRL(I,NMDVEX)
         NSTARL(IXX,NMDVEX) = NSTARL(IXX,IMODE)
         DO ISTA = 1, NSTARL(IXX,NMDVEX)
            ISTARL(ISTA,IXX,NMDVEX) = ISTARL(ISTA,IXX,IMODE)
         ENDDO
      END DO
C
C     Phase cal is complex, tones may change because
C     PHASECAL because of a FREQ change.
C     Calculate where the tones come in.
C     Update UPCAL - might not have been set before.
C
      UPCAL = FSPCAL(IFS)
      CALL UPCASE(UPCAL)
      IF( UPCAL .EQ. 'OFF' ) THEN
         PHTONE = 0.
      ELSE IF( UPCAL .EQ. ' ' ) THEN
         PHTONE = 1.
      ELSE IF( UPCAL .EQ. '1MHZ' ) THEN 
         PHTONE = 1.
      ELSE IF( UPCAL .EQ. '5MHZ' ) THEN
         PHTONE = 5.0
      ELSE 
         CALL ERRLOG( 'VXSCNS: Invalid PCAL: '
     1       //FSPCAL(IFS)//
     1       ' - Must be 1MHz, 5MHz, or off.' )
      END IF   
      CALL VXTON2( -1, ISET, NVXCHN(NFQVEX), 
     1    VXLOSUM(1,IFQ), 
     1    VXNETSID(1,NFQVEX), VXBBFILT(1,NFQVEX), 
     2    PHTONE, NTMP, TTMPCHN,
     3    NTONTMP, ITONTMP )
C     
C     check for change
C
      MATCHPH = PHTONE .EQ. TONEINT(IIF)
C     
C     with tones off, it doesn't matter where they go
C     
      IF( PHTONE .GT. 1E-3 ) THEN
         IF( NTMP .NE. NTONES(IPH) ) THEN 
            MATCHPH = .FALSE.
         ELSE
            DO ICH = 1, NTMP
               IF( NTONDET(ICH, IPH).NE.NTONTMP(ICH)) 
     1             THEN 
                  MATCHPH = .FALSE.
               ELSE
                  DO IP = 1, NTONDET(ICH,IPH)
                     IF( ITONDET(IP, ICH, IPH) .NE.
     1                   ITONTMP(IP, ICH) ) 
     2                   MATCHPH = .FALSE.
                  END DO
               END IF
            END DO
         END IF
      END IF
C     
C     So if this has remained the same it is easy
C     
      IF( MATCHPH ) THEN
C
C     Copy PH and IF def into the new MODE
C     CR 7 Oct 2004: fixed a horrible bug here in the indexing for
C       NSTAIF and NSTAPH (cf difference between IXX and I below)
C
         NMODIF(NMDVEX) = NMODIF(IMODE)
         DO I = 1, NMODIF(NMDVEX)
            IXX = IMODIF (I, IMODE)
            IMODIF(I,NMDVEX) = IMODIF(I,IMODE)
            NSTAIF(IXX,NMDVEX) = NSTAIF(IXX,IMODE)
            DO ISTA = 1, NSTAIF(IXX,IMODE)
               ISTAIF(ISTA,IXX,NMDVEX) = 
     1             ISTAIF(ISTA,IXX,IMODE)
            END DO
         END DO
C     
         NMODPH(NMDVEX) = NMODPH(IMODE)
         DO I = 1, NMODPH(NMDVEX)
            IXX = IMODPH (I, IMODE)
            IMODPH(I,NMDVEX) = IMODPH(I,IMODE)
            NSTAPH(IXX,NMDVEX) = NSTAPH(IXX,IMODE)
            DO ISTA = 1, NSTAPH(IXX,IMODE)
               ISTAPH(ISTA,IXX,NMDVEX) = 
     1             ISTAPH(ISTA,IXX,IMODE)
            END DO
         END DO
      ELSE
C     
C        carefully setup new modes
C
         CALL VXTRAPH( ISCN, NMDVEX, IMODE, PHTONE,
     1       NTMP, NTONTMP, ITONTMP )
C     
C        done inventing new PH 
C        do same for IF section
C     
         CALL VXTRAIF( ISCN, NMDVEX, IMODE, PHTONE )

C         
      END IF
C
      RETURN
      END

