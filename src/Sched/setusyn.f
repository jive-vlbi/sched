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
C     This effect was identified on Oct 17, 2012.  Other higher order
C     cases were also found once the mechanism was identified.
C
C     Previously, the unset synthesizers were put at 15.4 GHz except
C     at 2cm where 10.9 GHz was used.  I vaguely recall that 15.9 was not 
C     used because that would beat against a common 4cm LO (7.6 GHz)
C     second harmonic to produce a birdy.  But even the 15.4 has
C     problems with other harmonics 4 cm.  Here we calculate frequencies
C     that should be ok on a case by case basis.  For an overview of
C     possible interactions between LOs, see the program syn_c.f in
C     the $SCHED/RELATED_CODE/MAKEFREQ area.
C
C     With the new 6cm receiver, LO's between 3.4 and 8.6 GHz can be 
C     used.  No one default setting for the unused LOs will be without
C     problems across the band.
C
C     This routine will procede with two steps.  First, if 2 or 3 
C     synthesizers are already set, the user will be warned if there
C     are problems with mixing of harmonics creating tones in the IFs.
C     We don't claim to understand all the criteria that went into
C     user selected LO so we ask the user to pay attention.
C
C     Next, a setting that is benign for the synthesizers that have
C     been set (maximum of 2 if this is needed) is selected.
C     This is done by starting down a list of possible options
C     until a viable one is found.  The list allows weird preference
C     orders to be selected.  The list is in TLO.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
      INCLUDE   'schfreq.inc'
C
      INTEGER    KS, I, J, K, L, ICH, NLO, NTLO
      LOGICAL    NEEDCAT(MSET), TONHD, USEIT
      PARAMETER  ( NTLO = 32 )
      REAL       LO(3), DLO, TLO(NTLO), IFF
      DATA  TLO  / 15.4, 15.1, 14.9, 14.6, 14.4, 14.1, 15.6, 15.9,
     1             13.9, 13.6, 13.4, 13.1, 12.9, 12.6, 12.4, 12.1,
     2             11.9, 11.6, 11.4, 11.1, 10.9, 10.6, 10.4, 10.1,
     3              9.9,  9.6,  9.4,  9.1,  8.9,  8.4,  8.4,  8.1 /
      DATA  TONHD / .TRUE. /
      SAVE  TONHD
C ------------------------------------------------------------------
C     First detect use of 2 LO's.  If so, check for issues 
C     with harmonics and report them.  Also flag if one needs 
C     to be set because only one is used.
C
C     There are deep nested loops so the indenting is less than
C     usual to keep the lines reasonably long.
C
      DO KS = 1, NSET
       IF( SETSTA(1,KS)(1:4) .EQ. 'VLBA' ) THEN
C
C        Make a short list of the synthesizer settings already
C        established.

         NLO = 0
         DO I = 1, 3
            IF( SYNTH(I,KS) .NE. 0.0 ) THEN
               NLO = NLO + 1
               LO(NLO) = SYNTH(I,KS)
            END IF
         END DO
C
C        Find the possible birdies from pre-set tones.
C        Only do when 2 or 3 are preset.
C        Reduce indention here to prevent too-short lines in
C        the deep nest of do loops and IF statements.
C
         IF( NLO .GE. 2 ) THEN
          DO I = 1, NLO - 1
           DO J = I + 1, NLO
            DO K = 1, 5
             DO L = 1, 5
              IFF = ABS( LO(I) * K - LO(J) * L )
              IF( IFF .GT. 0.45 .AND. IFF .LT. 1.05 ) THEN
               IF( TONHD ) THEN
                TONHD = .FALSE.
                MSGTXT = ' '
                WRITE( MSGTXT, '( 3A )' )
     1              'SETUSYN:  There are possible tones in the ', 
     2              'IF due to mixing of harmonics of the first',
     3              'LOs (harmonic 1 is the tone itself).' 
                CALL WLOG( 1, MSGTXT )
                MSGTXT = ' '
                WRITE( MSGTXT, '( 3A )' )
     1              '  Setup  LO (GHz)  Harmonic  LO(GHz)  ',
     2              'Harmonic IF freq (MHz) '
                CALL WLOG( 1, MSGTXT )
               END IF
               MSGTXT = ' '
               WRITE( MSGTXT, '( I6, F9.1, I9, F9.1, I10, F14.2 )' )
     1              KS, LO(I), K, LO(J), L, IFF*1000.0
               CALL WLOG( 1, MSGTXT )
              END IF
             END DO
            END DO
           END DO
          END DO
         END IF
C
C        Now set the unset synthesizers to try to avoid RFI
C        from mixes of harmonics.  Also avoid putting an LO
C        in the RF for 2cm.  Try 15.4 and a couple below before
C        trying 15.6 and up.  The easiest way to orchestrate
C        this is to have a table (DATA) of option for 2cm and
C        for other.
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
         IF( NLO .LT. 3 ) THEN
          I = 1
          DO WHILE( I .LE. NTLO )
C
           IF( TLO(I) .LE. 11.5 .OR. ( FE(2,KS) .NE. '2cm' .AND.
     1                 FE(4,KS) .NE. '2cm' ) ) THEN
C
C           Loop over the assigned synthesizer frequencies (set above)
C           looking to see if TLO(I) is usable.
C
            USEIT = .TRUE.
            DO J = 1, NLO
             DO K = 1, 3
              DO L = 1, 5

               IFF = ABS( TLO(I) * K - LO(J) * L )
               IF( IFF .GT. 0.45 .AND. IFF .LT. 1.05 ) THEN
                USEIT = .FALSE.
               END IF
              END DO
             END DO
            END DO
            IF( USEIT ) THEN
             DLO = I
             GO TO 100
            END IF
           END IF
           I = I + 1
          END DO 
C
C         If got here, we ran out of possible dummy LO
C         values.  I don't think this should happen with the
C         list provided.  All are checked in syn_c.f.  If this
C         happens, issue a warning, and use 15.4.
C
          MSGTXT = ' '
          WRITE( MSGTXT, '( A, I4, 2A )' ) 
     1       'SETUSYN WARNING: For setup file ', KS,
     2       ' a benign frequency setting the unset front end ',
     3       'synthesizers could not be found.'
          CALL WLOG( 1, MSGTXT )
          MSGTXT = ' '
          WRITE( MSGTXT, '( 2A )' ) 
     1       '                 Beats between harmonics of the ',
     2       'used and unused synthesizers may appear in the IF.'
          CALL WLOG( 1, MSGTXT )
C
          MSGTXT = ' '
          WRITE( MSGTXT, '( 2A )' )
     1       '                 This should not happen.',
     2       '  Please inform the maintainer of SCHED.'
          CALL WLOG( 1, MSGTXT )
          MSGTXT = ' '
          WRITE( MSGTXT, '( 2A )' )
     1       '                 Meanwhile, using 15.4 GHz.'
          CALL WLOG( 1, MSGTXT )
          DLO = 1
         END IF
C
C        Jump here when a usable set is found.  Set the unspecified
C        synthesizers to the value selected above.
C
  100    CONTINUE
         DO I = 1, 3
            IF( SYNTH(I,KS) .EQ. 0.0 ) THEN
               SYNTH(I,KS) = TLO(DLO)
            END IF
         END DO
C
       END IF
      END DO
C
      RETURN
      END
