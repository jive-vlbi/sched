      SUBROUTINE GETPSET
C
C     Routine for SCHED that finds the "pcal sets".  There can
C     be a separate pcal set for each frequency set for each pcal
C     state (off, 1MHz, 5MHz), but there will usually be less even
C     than there are frequency sets, since multiple frequency sets
C     can, and often will, share pcal setups.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER       KF, KP, KS, KSCN, ISCN, IPC, IDET, ISTA
      CHARACTER     CPCAL*4
      INTEGER       PCFR1(MAXPC), PCFR2(MAXPC)
      CHARACTER     PCX1(MAXPC)*3, PCX2(MAXPC)*3
      LOGICAL       NEWPS, MATCH
C
C     FSPCAL will be a flag indicating whether each of the 3 pcal
C     states got used for that frequency set.
C       FSPCAL(1:1) = ' ' for PCAL=off not used.  ='U' for used.
C       FSPCAL(2:2) = ' ' for PCAL=1MHz not used. ='U' for used.
C       FSPCAL(3:3) = ' ' for PCAL=5MHz not used. ='U' for used.
C
      CHARACTER     FSPCAL(MFSET)*3
      CHARACTER     PCOPT(3)*4
      DATA   PCOPT / 'OFF', '1MHZ', '5MHZ' /
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'GETPSET: Starting.' )
C
C     Initialize
C
      DO KF = 1, NFSET
         FSPCAL(KF) = '   '
      END DO
C
C     Loop through scans and stations to see what was used.
C
      DO ISCN = SCAN1, SCANL
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN
               KS = NSETUP(ISCN,ISTA)
C
C              Get upper case version of PCAL to simplify comparisons.
C              While at it, get setup value if it was not specified
C              for the scan.
C
               IF( PCAL(ISCN) .EQ. ' ' ) THEN 
                  CPCAL = SPCAL(KS)
               ELSE 
                  CPCAL = PCAL(ISCN)
               END IF
               CALL UPCASE( CPCAL )
C
C              Check for valid PCAL specification.
C
               IF( CPCAL .NE. 'OFF' .AND. CPCAL .NE. '1MHZ' .AND.
     1             CPCAL .NE. '5MHZ' ) THEN
                  CALL ERRLOG ( 'GETPSET: Invalid PCAL specification: '
     1                // CPCAL )
               END IF
C
C              Now sense what was used.  Do in such a way that we
C              don't spoil the information from a previous scan/station
C              that this PCAL was used in this frequency set.
C
               KF = FSETI(ISCN,ISTA)
               DO IPC = 1, 3
                  IF( CPCAL .EQ. PCOPT(IPC) ) FSPCAL(KF)(IPC:IPC) = 'U'
               END DO
            END IF
         END DO
      END DO
C
C     Loop through frequency sets to identify the pcal sets.
C     Within each frequency set, it is only possible to have 3
C     different pcal sets, one for each state of the detector.
C
      DO KF = 1, NFSET
         KS = FSETKS(KF)
         KSCN = FSETSCN(KF)
C
C        Get the pcal settings for each fset/pcal combination.
C
         DO IPC = 1, 3
C
C           Initialize the pointer to the pcal set.
C
            FSETPS(IPC,KF) = 0
C
            IF( FSPCAL(KF)(IPC:IPC) .EQ. 'U' ) THEN
C
C              Get the pulse cal settings.
C
               CALL PCALFQ( PCOPT(IPC), KF, PCX1, PCX2, PCFR1, PCFR2 )
C
C              Compare with previous pcal sets.
C
               NEWPS = .TRUE.
               IF( NPSET .NE. 0 ) THEN
                  KP = 1
                  DO WHILE( KP .LE. NPSET .AND. NEWPS )
                     MATCH = PCOPT(IPC) .EQ. PSPCAL(KP)
                     IDET = 1
                     DO WHILE( MATCH .AND. IDET .LE. MAXPC )
                        MATCH = MATCH .AND. 
     1                          PCX1(IDET) .EQ. PSX1(IDET,KP)
                        MATCH = MATCH .AND. 
     1                          PCX2(IDET) .EQ. PSX2(IDET,KP)
                        MATCH = MATCH .AND. 
     1                          PCFR1(IDET) .EQ. PSFR1(IDET,KP)
                        MATCH = MATCH .AND. 
     1                          PCFR2(IDET) .EQ. PSFR2(IDET,KP)
                        IDET = IDET + 1
                     END DO
                     IF( MATCH ) THEN                     
                        NEWPS = .FALSE.
                        FSETPS(IPC,KF) = KP
                     END IF
                     KP = KP + 1
                  END DO
               END IF
C
C              Set up a new pcal group.  Set PSX to upper case to
C              avoid confusion later.
C
               IF( NEWPS ) THEN
                  NPSET = NPSET + 1
                  IF( NPSET .GT. MPSET ) THEN
                     CALL ERRLOG( 'GETPSET:  Too many pcal sets.' )
                  END IF
                  FSETPS(IPC,KF) = NPSET
                  KP = NPSET
                  PSPCAL(KP) = PCOPT(IPC)
                  DO IDET = 1, MAXPC
                     PSX1(IDET,KP) = PCX1(IDET)
                     PSX2(IDET,KP) = PCX2(IDET)
                     PSFR1(IDET,KP) = PCFR1(IDET)
                     PSFR2(IDET,KP) = PCFR2(IDET)
                     CALL UPCASE( PSX1(IDET,KP) )
                     CALL UPCASE( PSX2(IDET,KP) )
                  END DO
               END IF
C
C              Done with this pcal/fset combination.
C
            END IF
C
C           Done with the 3 pcal options.
C
         END DO
C
C        Done with frequency sets.
C
      END DO
C
C     Just plain done.
C
      RETURN
      END
