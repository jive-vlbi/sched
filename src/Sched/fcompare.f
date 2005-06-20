      SUBROUTINE FCOMPARE( KS, KF, MATCH, OKIF, NBAD, PRTMISS )
C
C     Routine for SCHED called by setfcat that compares a setup file KS
C     with a frequency catalog entry to KF see if they are compatible.
C     The purpose is to narrow the range of those that will be
C     used to search for a source of defaults or error checks.
C
C     Require that every channel in the setup file match, in all ways
C     that have already been set in the setup file, an IF in the
C     frequency catalog.  However allow the FREQREF of some, but
C     not all, channels to fall outside the RF ranges.
C
C     Keep track of the best matches.  NBAD is the minimum number
C     of items that did not match for the worst channel.
C
C     SETFCAT will call this routine a second time if a match is
C     not found.  By then NBAD will be set.  On that call, print
C     some details of how the match failed for any with NBAD or
C     NBAD + 1 mismatches.  On those calls, PRTMISS will be .TRUE. 
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
      INCLUDE   'schfreq.inc'
C
      INTEGER          KS, KSTA, KF, IFINDX, ICH, IIF, I
      INTEGER          NBAD, CHBAD, IIFBAD, KFBAD
      LOGICAL          MATCH, CHMATCH, IFMATCH, OKIF(MCHAN,MFIF)
      LOGICAL          CFRQOK, FREQOK, GOTAFR, PRTMISS, DEBUGPRT
      LOGICAL          RFCLOSE
      LOGICAL          FCSTA, FCIFCH, FCFLO1, FCPOL, FCCH1, FCVLAS
      LOGICAL          FCFE, FC50, FCVLAIF
      LOGICAL          MFLKA, MFLKB, MFEAB, MFECD, MSYNA, MSYNB
      LOGICAL          MFEF, MVBND, MVBW
      CHARACTER        CHFLAGS*12
C ---------------------------------------------------------------------
C
C     Check frequency match.  Only do debug printout if some 
C     channel matches.
C
      GOTAFR = .FALSE.
      DO ICH = 1, NCHAN(KS)
         DO IIF = 1, FNIF(KF)
            IF(  FREQREF(ICH,KS) .GE. FRF1(IIF,KF) .AND.
     1           FREQREF(ICH,KS) .LE. FRF2(IIF,KF) ) THEN
               GOTAFR = .TRUE.
            END IF
         END DO
      END DO
      DEBUGPRT = SDEBUG .AND. GOTAFR
C
C     Initialize OKIF.
C
      DO ICH = 1, NCHAN(KS)
         DO IIF = 1, FNIF(KF)
            OKIF(ICH,IIF) = .FALSE.
         END DO
      END DO
C
C     FREQOK will sense if any channels are in the right frequency
C     range.
C
      FREQOK = .FALSE.
C
C     Check that the first setup file station uses this frequency
C     catalog entry.  Allow VLBA defaults.
C
      FCSTA = .FALSE.
      DO KSTA = 1, MFSTA
         IF( SETSTA(1,KS) .EQ. FSTNAM(KSTA,KF) .OR. 
     1       ( SETSTA(1,KS)(1:4) .EQ. 'VLBA' .AND.
     2         FSTNAM(KSTA,KF)(1:4) .EQ. 'VLBA' ) ) THEN
            FCSTA = .TRUE.
         END IF
      END DO
      IF( DEBUGPRT .AND. FCSTA ) THEN
         SETMSG = ' '
         WRITE( SETMSG, '( A, I4, A, I4 )' )
     1       'FCOMPARE: Got station and frequency match. KS:', KS,
     2       '  KF:', KF
         CALL WLOG( 0, SETMSG )
      ELSE
         DEBUGPRT = .FALSE.         
      END IF
C
C     Check for IFs that match the input channel parameters.
C
      MATCH = FCSTA
      IF( MATCH ) THEN
         KFBAD = 0
         DO ICH = 1, NCHAN(KS)         
            CHMATCH = .FALSE.
            CHBAD = 99
            DO IIF = 1, FNIF(KF)
C
C              Get the index of the IF that would be used in setup parms
C              This is for FE only at the moment and that applies
C              only to certain stations (VLBA).
C
               IFINDX = 0
               IF( FIFNAM(IIF,KF) .EQ. 'A' ) IFINDX = 1
               IF( FIFNAM(IIF,KF) .EQ. 'B' ) IFINDX = 2
               IF( FIFNAM(IIF,KF) .EQ. 'C' ) IFINDX = 3
               IF( FIFNAM(IIF,KF) .EQ. 'D' ) IFINDX = 4
               IF( IFINDX .EQ. 0 ) THEN
                  IF( INDEX( SETSTA(1,KS), 'VLBA' ) .NE. 0 ) THEN
C
C                    Die for stations that need FE.
C
                     CALL ERRLOG( 'FCOMPARE: Unrecognized IF name for '
     1                  // SETSTA(1,KS) // ' in frequency catalog.' )
                  ELSE
C
C                    Take a (hopefully) benign default.
C
                     IFINDX = 1
                  END IF
               END IF
C
               FCIFCH = IFCHAN(ICH,KS) .EQ. ' ' .OR.
     1                  IFCHAN(ICH,KS) .EQ. FIFNAM(IIF,KF) .OR.
     2                  IFCHAN(ICH,KS) .EQ. FALTIF(IIF,KF)

C
               FCFLO1 = FIRSTLO(ICH,KS) .EQ. NOTSET .OR. 
     1                  FIRSTLO(ICH,KS) .EQ. FLO1(IIF,KF)
C
C              RF - require be in same general part of RF.  Will
C              check actual RF later.
C
               RFCLOSE = FREQREF(ICH,KS) .GE. 0.8D0 * FRF1(IIF,KF) .AND.
     1                  FREQREF(ICH,KS) .LE. 1.2D0 * FRF2(IIF,KF)
C
C              Polarization.
C
               FCPOL = POL(ICH,KS) .EQ. ' ' .OR.
     1                 POL(ICH,KS)(1:1) .EQ. FPOL(IIF,KF)(1:1)
C
C              For 2 cm, be sure filter will be right.  Depends on
C              frequency of channel 1.  This will work for other bands 
C              to if that should become an issue.
C
               FCCH1 = ( FCH1RF1(IIF,KF) .EQ. 0.D0 .OR.
     1                   FCH1RF1(IIF,KF) .LT. FREQREF(1,KS) ) .AND.
     2                 ( FCH1RF2(IIF,KF) .EQ. 0.D0 .OR.
     3                   FCH1RF2(IIF,KF) .GT. FREQREF(1,KS) )
C
C              Make sure this is an allowed IF for this VLA station.
C              NOTE: the VLAVA, VLAVR, VLAVL test superceeds this one
C                    and is more general.
C
C               FCVLAS =  SETSTA(1,KS)(1:3) .NE. 'VLA' .OR.
C     1                 ( FVCHNSTA(IIF,KF) .EQ. ' ' .OR.
C     2                   FVCHNSTA(IIF,KF) .EQ. 'BOTH' .OR.
C     3                   FVCHNSTA(IIF,KF) .EQ. SETSTA(1,KS) )
               FCVLAS = .TRUE.
C
C              For the VLA phased array, try to limit choice of IF.
C              Allow for possible different phasing modes using the
C              same setup (ok for RR only, for example - could use
C              VR or VA)
C
               IF( SETSTA(1,KS) .EQ. 'VLA27' ) THEN
                  FCVLAIF = .TRUE.
                  IF( VLAVA(KS) .AND. ( FIFNAM(IIF,KF) .EQ. 'B'
     1              .OR. FIFNAM(IIF,KF) .EQ. 'C' ) ) FCVLAIF = .FALSE.
                  IF( VLAVB(KS) .AND. ( FIFNAM(IIF,KF) .EQ. 'A'
     1              .OR. FIFNAM(IIF,KF) .EQ. 'D' ) ) FCVLAIF = .FALSE.
                  IF( VLAVR(KS) .AND. ( FIFNAM(IIF,KF) .EQ. 'C'
     1              .OR. FIFNAM(IIF,KF) .EQ. 'D' ) ) FCVLAIF = .FALSE.
                  IF( VLAVL(KS) .AND. ( FIFNAM(IIF,KF) .EQ. 'A'
     1              .OR. FIFNAM(IIF,KF) .EQ. 'B' ) ) FCVLAIF = .FALSE.
               ELSE
                  FCVLAIF = .TRUE.
               END IF
C
C              If it is the VLBA, be sure of the front end spec.
C
               IF( INDEX( SETSTA(1,KS), 'VLBA' ) .NE. 0 ) THEN 
                  FCFE =  ( FE(IFINDX,KS) .EQ. 'omit' .OR.
     2                FE(IFINDX,KS) .EQ. FFE(IIF,KF) )
               ELSE
                  FCFE = .TRUE.
               END IF
C
C              Add up all the tests.
C
               IFMATCH = FCIFCH .AND. FCFLO1 .AND. FCPOL .AND. FCCH1
     1              .AND. FCVLAS .AND. FCVLAIF .AND. FCFE .AND. RFCLOSE
C
C              If the channel matches, record that fact.
C
               IF( IFMATCH ) THEN
                  CHMATCH = .TRUE.
                  OKIF(ICH,IIF) = .TRUE.
               END IF
C
C              Check for frequency in range.  Only require one channel
C              to be ok - allows for overflow at places like the VLA.
C
               CFRQOK = FREQREF(ICH,KS) .GE. FRF1(IIF,KF) .AND.
     1                  FREQREF(ICH,KS) .LE. FRF2(IIF,KF)
               IF( CFRQOK ) FREQOK = .TRUE.
C
C              Determine just how bad the match was.
C
               IIFBAD = 0
               IF( .NOT. FCSTA )   IIFBAD = IIFBAD + 1
               IF( .NOT. RFCLOSE ) IIFBAD = IIFBAD + 1
               IF( .NOT. FCIFCH )  IIFBAD = IIFBAD + 1
               IF( .NOT. FCFLO1 )  IIFBAD = IIFBAD + 1
               IF( .NOT. FCPOL )   IIFBAD = IIFBAD + 1
               IF( .NOT. FCCH1 )   IIFBAD = IIFBAD + 1
               IF( .NOT. FCVLAS )  IIFBAD = IIFBAD + 1
               IF( .NOT. FCVLAIF ) IIFBAD = IIFBAD + 1
               IF( .NOT. FCFE )    IIFBAD = IIFBAD + 1
               IF( .NOT. FREQOK )  IIFBAD = IIFBAD + 1
C
C              See if this was the best IF for this channel, save
C              number of mismatches and flags for printout.
C
               IF( IIFBAD .LT. CHBAD ) THEN
                  CHBAD = IIFBAD
                  WRITE( CHFLAGS, '( I2, 8L1 )' ) IIF, 
     1               FCIFCH, FCFLO1, FCPOL, FCCH1, FCVLAIF, FCFE, 
     2               CFRQOK, RFCLOSE
               END IF
C
            END DO
C
C           Require that some IF matches the channel.
C
            IF( .NOT. CHMATCH ) MATCH = .FALSE.
            IF( DEBUGPRT ) THEN
               SETMSG = ' '
               WRITE( SETMSG, '( A, 3I4, 2X, 2L1 )' ) 
     1              'FCOMPARE: CHMATCH: ', KS, KF, ICH, CHMATCH, MATCH
               CALL WLOG( 0, SETMSG )
            END IF
C
C           Get the degree by which the worst channel missed.
C
            KFBAD = MAX( KFBAD, CHBAD )
C
C           Some output if debugging or trying to explain why no
C           match was found (call with PRTMISS set true).
C
            IF( DEBUGPRT .OR. ( GOTAFR .AND. PRTMISS .AND.
     1             CHBAD .LE. NBAD + 1) ) THEN
               SETMSG = ' '
               WRITE( SETMSG, '( 3X, A12, I8, 6X, A2, 1X, ' //
     1            ' 8( 5X, A1 ) )' )
     2            FRNAME(KF), ICH, CHFLAGS(1:2),(CHFLAGS(I:I),I=3,10)
               CALL WLOG( 0, SETMSG )
            END IF
C
         END DO
C
C        Separate frequency sets in the table.
C
         IF( DEBUGPRT .OR. ( GOTAFR .AND. PRTMISS ) ) THEN
            CALL WLOG( 0, ' ' )
         END IF
C
C        Some more debug printout.
C
         IF( DEBUGPRT ) THEN
            SETMSG = ' '
            WRITE( SETMSG, '( A, L1 )' ) 'FCOMPARE: Out of loop: ', 
     1           MATCH
            CALL WLOG( 0, SETMSG )
         END IF
C
C        Check FREQOK.
C
         MATCH = MATCH .AND. FREQOK
         IF( DEBUGPRT ) write(*,*) ' fmatch', match, freqok
C
C        Check 50 cm filter.  Maybe use FC50 some day related NBAD.
C
         FC50 =  SETSTA(1,KS)(1:4) .NE. 'VLBA' .OR. (
     1           ( RCP50CM(KS) .EQ. 'DEF' .OR.
     2           RCP50CM(KS) .EQ. FRCP50CM(KF) )  .AND.
     3           ( LCP50CM(KS) .EQ. 'DEF' .OR.
     4           LCP50CM(KS) .EQ. FLCP50CM(KF) ) )
C
         IF( ( MATCH .AND. .NOT. FC50 ) .AND. ( DEBUGPRT .OR. 
     1       ( GOTAFR .AND. PRTMISS .AND. KFBAD .LE. NBAD + 1) ) ) THEN
            CALL WLOG( 0, '          50cm filter doesn''t match.' )
         END IF
C
         MATCH = MATCH .AND. FC50
C
C        Record how bad the miss was.
C
         NBAD = MIN( NBAD, KFBAD )
C
      END IF
C
C     Check the synthesizer settings for VLBA sites.  Allow the user
C     to set a synthesizer the frequency catalog doesn't care about
C     to anything.  Note that MATCH in the IF statement insures that 
C     the MATCH = statement is equivalent to MATCH = MATCH .AND. ...
C
      IF( SETSTA(1,KS)(1:4) .EQ. 'VLBA' .AND. MATCH ) THEN
         MATCH = ( SYNTH(1,KS) .EQ. 0.0 .OR. FSYN(1,KF) .EQ. 0.0 .OR.
     1             SYNTH(1,KS) .EQ. FSYN(1,KF) ) .AND.
     2           ( SYNTH(2,KS) .EQ. 0.0 .OR. FSYN(2,KF) .EQ. 0.0 .OR.
     3             SYNTH(2,KS) .EQ. FSYN(2,KF) ) .AND.
     4           ( SYNTH(3,KS) .EQ. 0.0 .OR. FSYN(3,KF) .EQ. 0.0 .OR.
     5             SYNTH(3,KS) .EQ. FSYN(3,KF) )
C
         IF( DEBUGPRT ) THEN
            IF( MATCH ) THEN
               CALL WLOG( 0, 'FCOMPARE: Synth matches.' )
            ELSE
               CALL WLOG( 0, 'FCOMPARE: Problem with synth matches.' )
            END IF
         END IF
C
      END IF
C
C     Check the VLA parameters.  Note that many unset parameters
C     will be set in VLASETF.  Note that MATCH in the IF statement  
C     insures that the MATCH = statement is equivalent to 
C     MATCH = MATCH .AND. ...
C
      IF( SETSTA(1,KS)(1:3) .EQ. 'VLA' .AND. MATCH ) THEN
         MFLKA = FLUKEA(KS) .EQ. 0.D0 .OR. FLUKEA(KS) .EQ. FVFLKA(KF)
         MFLKB = FLUKEB(KS) .EQ. 0.D0 .OR. FLUKEB(KS) .EQ. FVFLKB(KF)
         MFEAB = VLAFEAB(KS) .EQ. 0.D0 .OR. VLAFEAB(KS) .EQ. FVFEAB(KF)
         MFECD = VLAFECD(KS) .EQ. 0.D0 .OR. VLAFECD(KS) .EQ. FVFECD(KF)
         MSYNA = VLASYNA(KS) .EQ. 0.D0 .OR. VLASYNA(KS) .EQ. FVSYNA(KF)
         MSYNB = VLASYNB(KS) .EQ. 0.D0 .OR. VLASYNB(KS) .EQ. FVSYNB(KF)
         MFEF  = FEFILTER(KS).EQ.'ZZZZ' .OR. FEFILTER(KS).EQ.FVFILT(KF)
         MVBND = VLABAND(KS) .EQ. 'ZZ' .OR. VLABAND(KS) .EQ. FVBAND(KF)
         MVBW  = VLABW(KS) .EQ. 'ZZZZ' .OR. VLABW(KS) .EQ. FVBW(KF)
C
         MATCH =  MFLKA .AND. MFLKB .AND. MFEAB .AND. MFECD .AND. 
     1            MSYNA .AND. MSYNB .AND. MFEF .AND. MVBND .AND. MVBW
C
         IF( PRTMISS .OR. DEBUGPRT ) THEN
            IF( MATCH ) THEN
               CALL WLOG( 0, 'FCOMPARE: VLA stuff matches.' )
            ELSE
               CALL WLOG( 0,'FCOMPARE: The problem is with the match '
     1             // 'of VLA parameters (F indicates mismatch):' )
               CALL WLOG( 0, '     FLUKEA   FLUKEB  VLAFEAB  VLAFECD  '
     1            // 'VLASYNA  VLASYNB FEFILTER  VLABAND    VLABW' )
               SETMSG = ' '
               WRITE( SETMSG, '( 9( 8X, L1 ) )' ) 
     1             MFLKA, MFLKB, MFEAB, MFECD, MSYNA, MSYNB,
     2             MFEF, MVBND, MVBW
               CALL WLOG( 0, SETMSG )
            END IF
         END IF
      END IF
      IF( DEBUGPRT ) THEN
         IF( MATCH ) THEN
            CALL WLOG( 0, 'FCOMPARE: Got overall match.' )
         ELSE
            CALL WLOG( 0, 'FCOMPARE: Problem with overall  match.' )
         END IF
      END IF
C
C     Now we are done and MATCH = .TRUE. means that this frequency
C     group is compatible with the setup file in every way except
C     possibly the RF frequency of some channels.  
C
      RETURN
      END

