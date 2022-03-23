      SUBROUTINE SETUSYN
C
C     Routine for SCHED, called by SETDEFS (in turn called from 
C     DEFSET) to set any VLBA synthesizers that have not yet
C     been set, presumably because they are not going to be used.
C     The job is complicated by the need to avoid harmonics of 
C     the in-use synthesizers from beating against harmonics (including
C     the fundamental) of the un-used synthesizers to creat unwanted
C     tones in the IFs.  This was a serious issue in RFI testing
C     of the wideband C band receivers using the old default for 
C     unused synthesizers, 15.4 GHz.  It was most apparent with 
C     tones at 6200MHz (3*5.4 vs 15.4) and 7300 (2*8.1 vs 15.4).
C     The peak amplitudes of the tones could be several hundred in the
C     autocorrelation (average 1) and, without smoothing, they caused
C     ripples across the baseband of up to about 0.5.  Hanning smoothing
C     knocks that way down.  The tones related to synthesizers 1 and
C     2 are much worse than those associated with synthesizer 3, at
C     least for 6cm where both 1 and 2 go to the same IF converter.
C
C     This effect was identified on Oct 17, 2012.  Other higher order
C     cases were also found once the mechanism was identified.
C
C     Previously, the unset synthesizers were put at 15.4 GHz except
C     at 2cm where 10.9 GHz was used.  I vaguely recall that 15.9 was not 
C     used because that would beat against a common 4cm LO (7.6 GHz)
C     second harmonic to produce a birdy.  But even the 15.4 setting has
C     problems with other harmonics at 4 cm.  Here we calculate 
C     frequencies that should be ok on a case by case basis.  For 
C     an overview of possible interactions between LOs, see the 
C     program syn3_c.f in the $SCHED/RELATED_CODE/MAKEFREQ area.
C
C     With the new 6cm receiver, LO's between 3.4 and 8.6 GHz can be 
C     used.  Since there are two IFs, two synthesizers can be used with
C     settings based on the scientific needs of the observation.  
C     Most combinations will be subject to harmonic trouble.  This 
C     routine will try to warn about them.  It will also try to find a
C     benign setting for the third synthesizer.  But that is not always
C     possible so a best case will be picked and  warnings will be 
C     issued about possible trouble.  Note that the third synthesizer 
C     does not cause as much trouble as the other two, presumably 
C     because it does not feed the IF converter.
C
C     This routine will procede with two steps.  First, a setting 
C     that is benign for the synthesizers that have not  been set 
C     (maximum of 2) is selected.  This is done by starting down 
C     a list of possible options until a viable one is found.  
C     The list allows weird preference orders to be selected.  
C     The list is in TLO.  A preference will be given to a setting
C     that has no in-band birdies when harmonics are mixed with
C     harmonics of the pre-set LOs.  If no ideal case can be found,
C     a setting where the trouble is between the highest possible
C     harmonics will be selected.
C
C     Once all three synthesizer settings are known, the user will 
C     be warned if there are problems with mixing of harmonics 
C     creating tones in the IFs.  We don't claim to understand 
C     all the criteria that went into the user selected LOs so we 
C     ask the user to pay attention.  An enhancement would be to
C     try to avoid the actual baseband channels selected, but I'll
C     put that off for now.
C
C     Dec. 19, 2012.  The synthesizers create tones at harmonics of 
C     half the output frequency when working at or above 8.1 GHz.  Try
C     to protect against those.  They exist because a doubler is used
C     for the higher frequencies.  This will probably not be a problem
C     with the new synthesizers under development in 2013 for use in
C     slots 1 and 2.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
      INCLUDE   'schfreq.inc'
C
      INTEGER    KS, I, J, K, L, NLO, NTLO, NHI, NHJ, NHT
      INTEGER    BESTI, MLHAR, IHAR, LSETUP, DLO, SY(3), TSY
      LOGICAL    TONHD, USEIT, GOTGOOD
      DOUBLE PRECISION   FMAX
      PARAMETER  ( NTLO = 56 )
      PARAMETER  ( FMAX = 60.0D0 )
      DOUBLE PRECISION   LO(3), TLO(NTLO), IFF, LOI, LOJ, LOT
      DATA  TLO  / 
     1 15.4D0, 15.1D0, 14.9D0, 14.6D0, 14.4D0, 14.1D0, 15.6D0, 15.9D0,
     2 13.9D0, 13.6D0, 13.4D0, 13.1D0, 12.9D0, 12.6D0, 12.4D0, 12.1D0,
     3 11.9D0, 11.6D0, 11.4D0, 11.1D0, 10.9D0, 10.6D0, 10.4D0, 10.1D0,
     4  9.9D0,  9.6D0,  9.4D0,  9.1D0,  8.9D0,  8.6D0,  8.4D0,  8.1D0,
     5  7.9D0,  7.6D0,  7.4D0,  7.1D0,  6.9D0,  6.6D0,  6.4D0,  6.1D0,
     6  5.9D0,  5.6D0,  5.4D0,  5.1D0,  4.9D0,  4.6D0,  4.4D0,  4.1D0,
     7  3.9D0,  3.6D0,  3.4D0,  3.1D0,  2.9D0,  2.6D0,  2.4D0,  2.1D0 /
C ------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SETUSYN starting' )
C
C     First detect use of 2 LO's.  If so, check for issues 
C     with harmonics and report them.  Also flag if one needs 
C     to be set because only one is used.
C
C     There are deep nested loops so the indenting is less than
C     usual to keep the lines reasonably long.
C
C     Only issue the warnings for the first VLBA station in a setup
C     file to avoid lots of repetitions.  But do repeat the calculations
C     of where to set the LOs.  There are odd corner cases where 
C     different VLBA stations might have different settings so the user
C     will miss some warnings, but the settings will be done properly.
C
C
C     Loop over all setups and select only those for VLBA stations
C     (Other stations are unlikly to have the same first LO system)
C
      LSETUP = 0
      DO KS = 1, NSET
       TONHD = .TRUE.
       IF( SETSTA(1,KS)(1:4) .EQ. 'VLBA' ) THEN
C
C        Make a short list of the synthesizer settings already
C        established.
C
         NLO = 0
         DO I = 1, 3
            IF( SYNTH(I,KS) .NE. 0.D0 ) THEN
               NLO = NLO + 1
               LO(NLO) = SYNTH(I,KS)
               SY(NLO) = I
            END IF
         END DO
C
C        +++++
C        Find the possible birdies from pre-set tones.
C        +++++
C
C        Only do when 2 or 3 synthesizers have been set.
C
C        Test out to the 5th harmonic of the primary, or 10th when
C        checking harmonics of the tone at half the primary
C        for old type LOs above 8.1 GHz.  Don't worry about signals
C        above 60 GHz.  Give special warning when one of the
C        offending "harmonics" is the primary synthesizer output
C        as I expect those will be strong.
C
C        This segment is only for warnings so skip if just doing
C        a new station for the last setup file.
C
         IF( NLO .GE. 2 .AND. ISETNUM(KS) .NE. LSETUP ) THEN
C
C         Checking pairs of tones.  Loop over the first LO of a
C         pair.  It can only go to NLO - 1.
C
          DO I = 1, NLO - 1
C
C          Set the base tone for this LO.  MODETEST allows the
C          test value to be the output frequency when using the
C          new synthesizer.
C
           IF( LO(I) .LE. 8.0 .OR. 
     1         ( MODETEST(KS) .AND. SY(I) .LT. 3 ) ) THEN
              LOI = LO(I)
              NHI = MIN( 5, INT( FMAX / LOI ) )
           ELSE
              LOI = LO(I) / 2.0
              NHI = MIN( 10, INT( FMAX / LOI ) )
           END IF
C
C          Loop over the second frequency of the pair.
C
           DO J = I + 1, NLO
            IF( LO(J) .LE. 8.0 .OR. 
     1         ( MODETEST(KS) .AND. SY(J) .LT. 3 )) THEN
               LOJ = LO(J)
               NHJ = MIN( 5, INT( FMAX / LOJ ) )
            ELSE
               LOJ = LO(J) / 2.0
               NHJ = MIN( 10, INT( FMAX / LOJ ) )
            END IF
C
C           Now loop over the harmonics of each frequency
C           and look at the difference.  Note that the LOs
C           are in GHz so the IF is 0.512-1.024.
C           Allow a birdy at 500 (out of band), but not 1000 (in band)
C           Since all birdies will be at N*0.1 GHz, using 0.52 is
C           equivalent to 0.512.
C
            DO K = 1, NHI
             DO L = 1, NHJ
              IFF = ABS( LOI * K - LOJ * L )
              IF( IFF .GT. 0.52 .AND. IFF .LT. 1.03 ) THEN
               CALL HARMWARN( KS, SY(I), LO(I), LOI, 
     1                        SY(J), LO(J), LOJ, K, L, IFF, TONHD )
              END IF
             END DO
            END DO
           END DO
          END DO
         END IF
C
C        +++++
C        Set unused synthesizers.
C        +++++
C
C        Now set the unset synthesizers to try to avoid RFI
C        from mixes of harmonics.  Also avoid putting an LO
C        in the RF for 2cm.  Try 15.4 and a couple below before
C        trying 15.6 and up.  The easiest way to orchestrate
C        this is to have a table (DATA) of options.
C
C        Below the loops are over:
C        I:  The index for the list (TLO) of possible LOs to use 
C            for the unset synthesizers
C        J:  The index of the list of previously set synthesizer 
C            frequencies (LO).
C        K:  The harmonic of the TLO(I).
C        L:  The harmonic of the previously set values (LO(J)).
C
C        Don't bother with any of this if all synthesizers set.
C
C        Deal with the tones at half the LO, and harmonics thereof,
C        for frequencies above 8 GHz.
C   
C
C        Loop over preset LOs.  GOTGOOD is just a flag that indicates
C        we don't need to explore harmonics with a newly chosen 
C        unused synthesizer setting.  It will be true if we didn't
C        choose any settings (all harmonics listed above) or if we
C        did not find a good setting with no harmonic issues.
C
         GOTGOOD = NLO .EQ. 3 
         IF( NLO .LT. 3 ) THEN
C
C         Loop over the TLO options list for values for the unset
C         synthesizers.  Note that this may need to change with
C         the new synthesizers which have finer set points and for
C         which power consumption considerations suggest that the
C         sum of SYNTH(1,KS)+SYNTH(2,KS) should be close to 18 GHz
C         ideally.  For now, the existing algorithm should find
C         a usable frequency so don't worry about it yet.
C
C         For future thinking: One way to set a new synthesizer 
C         would be to use a frequency that is very close to a 
C         harmonic of another, so the harmonics never separate by
C         more than 500 MHz.  But with 2 preset LOs, especially if
C         one is the old-style one with coarse tuning, it won't 
C         be that simple.
C
          MLHAR = 0
          I = 1
          DO WHILE( I .LE. NTLO )
C
C          Avoid putting a tone in the 2cm band - jump down below
C          11.5 if this is 2cm.  This protects the whole band.  Is
C          is now needed for other bands too?
C
           IF( TLO(I) .LE. 11.5D0 .OR. ( FE(2,KS) .NE. '2cm' .AND.
     1                 FE(4,KS) .NE. '2cm' ) ) THEN
C
C           Loop over the assigned synthesizer frequencies (set above)
C           looking to see if TLO(I) is usable.  For the TLO frequencies
C           don't bother going above the third harmonic of the primary,
C           but that will be the sixth harmonic of the half tone.
C           If a good frequency is found with no harmonic issues, take
C           it and jump out.  Otherwise prioritize the options and
C           take the one with the highest, lowest harmonic. Really!  
C           For each possible TLO, look at the harmonic numbers for
C           all combinations of LOs and TLO that can cause birdies in
C           the IF(s).  Find the lowest for that TLO.  Then pick the
C           TLO that has the highest value of that lowest harmonic number.
C           Confused yet?
C
C           Try to very strongly discourage anything with a first harmonic
C           value of 1 (the actual signal) participating in a birdy.
C           Those will be much stronger then others.
C
C           Don't worry about tones above 60 GHz (I picked that out of 
C           thin air).
C
            USEIT = .TRUE.
            IF( TLO(I) .LE. 8.D0 ) THEN
              LOT = TLO(I)
              NHT = MIN( 5, INT( FMAX / LOT ) )
            ELSE
              LOT = TLO(I) / 2.D0
              NHT = MIN( 10, INT( FMAX / LOT ) )
            END IF
C
C           Loop over the already chosen synthesizer settings.
C           IHAR will be the minimum harmonic number for cases 
C           where the IF frequency of the harmonic interference 
C           falls in the 512-1024 range.  It will be used in
C           selecting the least bad LO when a good one (no harmonic
C           issues) can be found.
C
            IHAR = 100
            DO J = 1, NLO
             IF( LO(J) .LE. 8.D0 .OR. 
     1           ( MODETEST(KS) .AND. SY(J) .LT. 3 ) ) THEN
               LOJ = LO(J)
               NHJ = MIN( 10, INT( FMAX / LOJ ) )
             ELSE
               LOJ = LO(J) / 2.D0
               NHJ = MIN( 10, INT( FMAX / LOJ ) )
             END IF
C
C            Don't set the unused synthesizer to the same frequency
C            as a used one.  That has potential for trouble.
C
             IF( LOT .EQ. LOJ ) USEIT = .FALSE.
C
C            Now loop over the harmonics. 
C
             IF( USEIT ) THEN
              DO K = 1, NHT
               DO L = 1, NHJ
C
C               Allow a birdy at 500 (out of band), but not 1000 (in band)
C
                IFF = ABS( LOT * K - LOJ * L )
                IF( IFF .GT. 0.52 .AND. IFF .LT. 1.03 ) THEN
                 IHAR = MIN( K, L, IHAR )
                 USEIT = .FALSE.
                END IF
               END DO
              END DO
             END IF
            END DO
C
C           If no birdy possibilities were found, use this TLO.
C           Skip out of the search.
C
            IF( USEIT ) THEN
             DLO = I
             GOTGOOD = .TRUE.
             GO TO 100
C
C           Otherwise, check if this is the best so far (highest IHAR)
C           and, if so, keep track.
C
            ELSE
             IF( IHAR .GT. MLHAR ) THEN
              MLHAR = IHAR
              BESTI = I
             END IF
            END IF
           END IF
           I = I + 1
          END DO 
C
C         If got here, we need to select the best case of those
C         that may have birdies.
C
          DLO = BESTI
         END IF
C
C        Jump here when a usable set is found.  
C
  100    CONTINUE
C
C        Set the unspecified synthesizers to the value selected 
C        above.  Retain the number of the synthesizer set (TSY).
C        Go backwards so that number is the lowest.
C
         TSY = 0 
         DO I = 3, 1, -1
            IF( SYNTH(I,KS) .EQ. 0.D0 ) THEN
               SYNTH(I,KS) = TLO(DLO)
               TSY = I
            END IF
         END DO
C
C        Now, if necessary, warn of frequencies of possible birdies
C        with the synthesizer.  This is just warnings so skip
C        if on a setup file we've done for another VLBA station.
C        Note that the concern about the fundamental tone being
C        at half the output is for the old synthesizers, not the
C        new ones to be deployed in slots 1 and 2 in 2014 (maybe).
C
C        Here we only need to compare the one setting used for the 
C        previously unset synthesizers with the forced settings.
C        We don't need to do this when a good alternative was 
C        found, which would cause GOTGOOD to be set true.  Any
C        problems between previously set synthesizers have already
C        been dealt with in the first HARMWARN call above.
C
C        Note that, if more than one synthesizer needed to be set, 
C        they will be set to the same frequency.  So we only need
C        to test that frequency.
C
C        So start the nested loops with the one new frequency.
C             
         IF( .NOT. GOTGOOD .AND. ISETNUM(KS) .NE. LSETUP ) THEN
C         IF( ISETNUM(KS) .NE. LSETUP ) THEN
          IF( TLO(DLO) .LE. 8.D0 ) THEN
             LOT = TLO(DLO)
             NHT = MIN( 5, INT( FMAX / LOT ) )
          ELSE
             LOT = TLO(DLO) / 2.D0
             NHT = MIN( 10, INT( FMAX / LOT ) )
          END IF
C
C         Loop over the forced settings.  There was an error
C         in the loop lower limit prior to Oct 24, 2013 which would
C         have done something weird.  It was:  DO I = I + 1, NLO.
C         That should have caused trouble, but it seems none of the
C         examples get to this point.  I don't know if anything 
C         else does (all either forced settings or good numbers found
C         by SCHED).
C
          DO I = 1, NLO
           IF( LO(I) .LE. 8.D0 ) THEN
              LOI = LO(I)
              NHI = MIN( 5, INT( FMAX / LOI ) )
           ELSE
              LOI = LO(I) / 2.D0
              NHI = MIN( 10, INT( FMAX / LOI ) )
           END IF
C
C          Double loop over harmonics.
C
           DO K = 1, NHT
            DO L = 1, NHI
             IFF = ABS( LOT * K - LOI * L )
             IF( IFF .GT. 0.52 .AND. IFF .LT. 1.03 ) THEN
              CALL HARMWARN( KS, SY(I), LO(I), LOI, 
     1               TSY, TLO(DLO), LOT, L, K, IFF, TONHD )
             END IF
            END DO
           END DO
C
          END DO
C
         END IF
C
C        Remember this was the last setup.
C
         LSETUP = ISETNUM(KS)
C
       END IF
      END DO
C
      RETURN
      END






