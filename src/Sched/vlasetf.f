      SUBROUTINE VLASETF( VLABAND, FLUKEA, FLUKEB, VLAFEAB, VLAFECD,
     1                    VLASYNA, VLASYNB, FEFILTER, VLAIF, VLAROT,
     2                    VLABW, LOFI, ERRS )
C
C     Routine for SCHED to set the VLA parameters to the defaults
C     for the band they were not given.  Called by CHKVLA to set
C     any unspecified values and by GETFREQ to fill in defaults for
C     the frequency setups.  The two calls fill different variables
C     which is why this has a long call argument rather than using
C     the include files.
C
C     LOFI is a logical that tells whether or not LO and FI cards will
C     be needed.  It is .FALSE. if all values match the defaults
C     once this routine is done setting values.  It is only really used
C     by the call from CHKVLA.
C
      CHARACTER         VLABAND*(*)
      DOUBLE PRECISION  FLUKEA     
      DOUBLE PRECISION  FLUKEB     
      DOUBLE PRECISION  VLAFEAB    
      DOUBLE PRECISION  VLAFECD    
      DOUBLE PRECISION  VLASYNA    
      DOUBLE PRECISION  VLASYNB    
      CHARACTER         FEFILTER*(*)
      CHARACTER         VLAIF*(*)
      CHARACTER         VLAROT*(*)
      CHARACTER         VLABW*(*)
      LOGICAL           LOFI
      LOGICAL           ERRS
C
      INTEGER           MSTD, I
      PARAMETER         (MSTD=19)
C
      LOGICAL           GOT1, GOT2
      DOUBLE PRECISION  FEAB(MSTD), FECD(MSTD)
      DOUBLE PRECISION  SYNA(MSTD), SYNB(MSTD)
      DOUBLE PRECISION  FLKA(MSTD), FLKB(MSTD)
      CHARACTER         BD1(MSTD)*2, BD2(MSTD)*2
      CHARACTER         IF(MSTD)*10, RO(MSTD)*10, CH2*2
      CHARACTER         VBW(MSTD)*4, VFE(MSTD)*4, TEXT*128
      SAVE              BD1, FEAB, FECD, SYNA, SYNB, FLKA, FLKB
      SAVE              BD2, VBW, VFE, IF, RO
C
C     Record standard VLA bands and their FIRSTLO's.
C     See program VLABANDS that reads the VLA file obtained from
C     Ken Sowiniski and produces this data statement.
C     These numbers were current as of 29apr96.
C     Updated to message from Ken of 9oct97.
C
       DATA (BD1(I), FEAB(I), FECD(I), SYNA(I), SYNB(I), 
     A         FLKA(I), FLKB(I), I=1,10) /
     1 'CC', 0.0D0, 0.0D0,3860.1D0,3810.1D0,100.00000D0,200.00000D0,
     2 'UU',19.6D0,19.6D0,3610.1D0,3660.1D0,100.00000D0,200.00000D0,
     3 'KK',17.6D0,17.6D0,3860.1D0,3810.1D0,100.00000D0,200.00000D0,
     4 'LL',-3.2D0,-3.2D0,3639.9D0,3560.1D0,100.00000D0,200.00000D0,
     5 'XX',13.4D0,13.4D0,3939.9D0,3889.9D0,100.00000D0,200.00000D0,
     6 'QQ',51.6D0,13.0D0,3689.9D0,3739.9D0,100.00000D0,200.00000D0,
     7 'PP', 0.0D0, 0.0D0,-689.9D0,-710.1D0,115.83750D0,230.10000D0,
     8 '44', 0.0D0, 0.0D0,-939.9D0,-939.9D0,112.91875D0,212.91875D0,
     9 '4P', 0.0D0, 0.0D0,-939.9D0,-689.9D0,112.91875D0,215.83750D0,
     A 'LP',-3.2D0, 0.0D0,3639.9D0,-689.9D0,100.00000D0,215.83750D0 /
       DATA (BD1(I), FEAB(I), FECD(I), SYNA(I), SYNB(I), 
     A         FLKA(I), FLKB(I), I=11,19) /
     1 '18',-3.2D0,-3.2D0,3839.9D0,3810.1D0,100.00000D0,200.00000D0,
     2 'HH',-3.2D0,-3.2D0,3589.9D0,3639.9D0,115.10000D0,215.10000D0,
     3 'VP', 0.0D0, 0.0D0,-689.9D0,-689.9D0,112.50000D0,212.50000D0,
     4 'VL',-3.2D0,-3.2D0,3839.9D0,3839.9D0,100.00000D0,200.00000D0,
     5 'VC', 0.0D0, 0.0D0,3960.1D0,3960.1D0,100.00000D0,200.00000D0,
     6 'VX',13.0D0,13.0D0,3560.1D0,3560.1D0,100.00000D0,200.00000D0,
     7 'VU',19.9D0,19.9D0,3510.1D0,3510.1D0,100.00000D0,200.00000D0,
     8 'VK',17.5D0,17.5D0,3710.1D0,3710.1D0,100.00000D0,200.00000D0,
     9 'VQ',51.6D0,13.0D0,3510.1D0,3510.1D0,100.00000D0,200.00000D0 /
C 
C  VLAROT and VLAIF are no longer used by the VLA system and should be
C  left blank.  Leave this here for historical interest (which is to
C  say, in case it comes back somehow).
C
       DATA (BD2(I), VBW(I), VFE(I), IF(I), RO(I), I=1,10) /
     1 'CC', '0000', '0000', 'SYSCIF', 'SYSCROT',
     2 'UU', '0000', '0000', 'SYSUIF', 'SYSUROT',
     3 'KK', '0000', '0000', 'SYSKIF', 'SYSKROT',
     4 'LL', '0000', '0000', 'SYSLIF', 'SYSLROT',
     5 'XX', '0000', '0000', 'SYSXIF', 'SYSXROT',
     6 'QQ', '0000', '0000', 'SYSQIF', 'SYSQROT',
     7 'PP', '1111', '1111', 'SYSPIF', 'SYSPROT',
     8 '44', '0000', '0000', 'SYS4IF', 'SYS4ROT',
     9 '4P', '1111', '1111', 'SYS4PIF', 'SYS4PROT',
     A 'LP', '0101', '0101', 'SYSLPIF', 'SYSLPROT' /
C
       DATA (BD2(I), VBW(I), VFE(I), IF(I), RO(I), I=11,19) /
     1 '18', '0000', '0000', 'SYSLIF', 'SYSLROT',
     2 'HH', '0000', '0000', 'SYSLIF', 'SYSLROT',
     3 'VP', '1111', '1111', 'SYSPIF', 'VLBPROT',
     4 'VL', '0000', '0000', 'SYSLIF', 'VLBLROT',
     5 'VC', '0000', '0000', 'SYSCIF', 'VLBCROT',
     6 'VX', '0000', '0000', 'SYSXIF', 'VLBXROT',
     7 'VU', '0000', '0000', 'SYSUIF', 'VLBUROT',
     8 'VK', '0000', '0000', 'SYSKIF', 'VLBKROT',
     9 'VQ', '0000', '0000', 'SYSQIF', 'VLBQROT' /
C --------------------------------------------------------------------- 
      GOT1 = .FALSE.
      GOT2 = .FALSE.
C
C     Get stuff from first data statement.
C
      DO I = 1, MSTD
         IF( VLABAND .EQ. BD1(I) ) THEN
            IF( FLUKEA  .EQ. 0.D0 ) FLUKEA  = FLKA(I)
            IF( FLUKEB  .EQ. 0.D0 ) FLUKEB  = FLKB(I)
            IF( VLAFEAB .EQ. 0.D0 ) VLAFEAB = FEAB(I)
            IF( VLAFECD .EQ. 0.D0 ) VLAFECD = FECD(I)
            IF( VLASYNA .EQ. 0.D0 ) VLASYNA = SYNA(I)
            IF( VLASYNB .EQ. 0.D0 ) VLASYNB = SYNB(I)
            GOT1 = .TRUE.
C
C           Check VLAFEAB and VLAFECD in cases where it must be
C           either 0.0 or -3.2.  This is much easier to do here
C           than in CHKVLA because we have FEAB and FECD.
C
            IF( ( FEAB(I) .EQ. 0.D0 .AND. VLAFEAB .NE. 0.D0 ) .OR.
     1          ( FECD(I) .EQ. 0.D0 .AND. VLAFECD .NE. 0.D0 ) .OR.
     2          ( FEAB(I).EQ.-3.2D0 .AND. VLAFEAB .NE.-3.2D0 ) .OR.
     3          ( FECD(I).EQ.-3.2D0 .AND. VLAFECD .NE.-3.2D0 ) ) THEN
               TEXT = ' '
               WRITE( TEXT, '( 3A, F10.2, A, F10.2, A, F10.2 )' ) 
     1           'Band ', VLABAND, ' must have VLAFEAB =',
     2           FEAB(I), ' and VLAFECD =', FECD(I)
               CALL WLOG( 1, 'CHKVLA: ' // TEXT )
               WRITE( TEXT, '( A, F10.2, A, F10.2 )' ) 
     1           'Setup or frequency catalog specified VLAFEAB =', 
     2           VLAFEAB, ' and VLAFECD =', VLAFECD
               CALL WLOG( 1, '        ' // TEXT )
               ERRS = .TRUE.
            END IF
         END IF
      END DO
C
C     From second data statement.
C     Worry about whether VLABW and FEFILTER really should be
C     reset - user might have set them.
C
      DO I = 1, MSTD
         IF( VLABAND .EQ. BD2(I) ) THEN
            IF( FEFILTER .EQ. 'ZZZZ' ) FEFILTER = VFE(I)
            IF( VLABW .EQ. 'ZZZZ' ) VLABW = VBW(I)
            IF (VLAIF .NE. ' ' .OR. VLAROT .NE. ' ' ) THEN
               TEXT = ' '
               WRITE( TEXT, '( 2A )' ) 'VLASETF: Do not use VLAROT',
     1           ' or VLAIF.  They are obsolete and not used.'
               CALL WLOG( 1, TEXT )
               TEXT = ' '
               VLAROT = ' '
               VLAIF = ' '
            END IF
C            IF( VLAIF .EQ. ' ' ) VLAIF = IF(I)
C            IF( VLAROT .EQ. ' ' ) VLAROT = RO(I)
            GOT2 = .TRUE.
         END IF
      END DO
C
C     Warn of bad band.  Note cannot concatenate with character*(*)
C     call argument on some machines.
C
      IF( .NOT. ( GOT1 .AND. GOT2 ) ) THEN
         CH2 = VLABAND
         CALL WLOG( 1, 'VLASETF: Attempt to set VLA defaults for ' //
     1       'invalid band: '// CH2 )
         CALL ERRLOG( 'VLASETF: Problem in frequency catalog or ' //
     1       'setup file.' )
      END IF
C
C     Now determine whether LO and FI cards will be needed.
C     Want LOFI=T if not a standard band.
C     Wand LOFI=T if any values differ from the standard band at all.
C
      LOFI = .TRUE.
      DO I = 1, MSTD
         IF( VLABAND .EQ. BD1(I) ) THEN
            LOFI = .FALSE.
            IF( FLUKEA .NE. FLKA(I) .OR. FLUKEB .NE. FLKB(I) .OR.
     1          VLAFEAB .NE. FEAB(I) .OR. VLAFECD .NE. FECD(I) .OR.
     2          VLASYNA .NE. SYNA(I) .OR. VLASYNB .NE. SYNB(I) )
     3         LOFI = .TRUE.
         END IF
         IF( VLABAND .EQ. BD2(I) ) THEN
C            IF( FEFILTER .NE. VFE(I) .OR. VLAIF .NE. IF(I) .OR.
C     1          VLAROT .NE. RO(I) ) LOFI = .TRUE.
            IF( FEFILTER .NE. VFE(I) ) LOFI = .TRUE.
         END IF      
      END DO
C
      RETURN
      END

