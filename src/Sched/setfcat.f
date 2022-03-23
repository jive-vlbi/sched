      SUBROUTINE SETFCAT( KS, NEEDCAT )
C
C     Routine for SCHED, called by SETDEFS, to deal with setup 
C     frequency, sideband and other such information.
C
C     First determine which frequency catalog entry to use for a 
C     setup group.  This catalog entry will be used to set 
C     parameters where necessary and will be used to check 
C     parameters that are provided.  Critically, FREQREF and 
C     NETSIDE are known for each channel by the time this is called.
C
C     If an apropriate entry cannot be found, a warning is issued
C     if NEEDCAT is false and an abort is issued if NEEDCAT is true.
C
C     Once done with the frequency catalog (or if it wasn't used),
C     deal with filling out all the sideband and LO information based
C     on what is available.
C
C     Jan 2013 RCW  Adding continuously tunable options.  This is
C     mainly for the VLA, but could possibly be used for other antennas.
C     Sep 2013 RCW  Adjusting for use with the CRD parameters.
C
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
      INCLUDE   'schfreq.inc'
C
      INTEGER        I, KS, KF, ICH, IIF, KIF, NBAD, NBWARN
      INTEGER        USEKF, USEIF(MCHAN), BESTPRIO, LEN1
      LOGICAL        MATCH, GOT, NEEDCAT, OKIF(MCHAN,MFIF), TAKEIT
      CHARACTER      LASTPOL*4
      REAL           OVERLAP, CENTER
      REAL           BESTCENT
      DATA           NBWARN / 0 /
      SAVE           NBWARN
C ---------------------------------------------------------------------
      IF( SDEBUG ) CALL WLOG( 0, 'SETFCAT: Starting' )
C
C     Loop over frequency catalog groups looking for best.
C
      GOT = .FALSE.
      BESTCENT = 1.E5
      BESTOVER(KS) = 0.0
      BESTPRIO = 100
C
      DO KF = 1, NFREQ
C
C        Find if this frequency catalog group is compatible with this
C        setup group (MATCH=.TRUE.).  Also flag which IFs in the 
C        frequency group can be used for each channel (OKIF).
C
         CALL FCOMPARE( KS, KF, MATCH, OKIF, NBAD, .FALSE. )
C
C        Determine which of the frequency groups that are compatible 
C        is best.
C
         IF( MATCH ) THEN
C
C           Determine the amount of bandwidth overlap and the worst
C           offset of a channel from the center of a band.
C
            CALL FMATCH( KS, KF, OVERLAP, CENTER, OKIF, USEIF )
            IF( SDEBUG ) THEN
               SETMSG = ' '
               WRITE( SETMSG, '( A, 2I4, 2F10.2, 32I2)' ) 
     1            'SETFCAT debug: ', KS, KF, OVERLAP, CENTER, 
     2            (USEIF(I),I=1,NCHAN(KS))
               CALL WLOG( 0, SETMSG )
            END IF
C
C           Determine which KF is best.  Go for the best centered
C           of the best priority groups with the most overlap.
C           (Clear as mud, eh!).  Deal separately with the cases
C           where this is clearly the best choice and where we
C           have do decide based on more subtle criteria like 
C           centering.
C

            IF( OVERLAP .GT. 0.0 ) THEN
               IF( SDEBUG ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, 2I4, 2A )' )
     1               'SETFCAT GOT OVERLAP:', KS, KF, ' ', FRNAME(KF)
                  CALL WLOG( 0, MSGTXT )
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, 2F8.2, 2I3, 2F8.2 )' ) 
     1                '                    ', OVERLAP, BESTOVER(KS),
     2                PRIO(KF), BESTPRIO, CENTER, BESTCENT
                  CALL WLOG( 0, MSGTXT )
               END IF
C
C              Clearly best:
C
               TAKEIT = .FALSE.
               IF( OVERLAP .GT. BESTOVER(KS) .OR. 
     1             ( OVERLAP .EQ. BESTOVER(KS) .AND. 
     2             PRIO(KF) .LT. BESTPRIO ) ) THEN
                  TAKEIT = .TRUE.
C
C              Choose by centering:
C
               ELSE IF( OVERLAP .EQ. BESTOVER(KS) .AND. 
     2             PRIO(KF) .EQ. BESTPRIO ) THEN
                  IF( CENTER .LT. BESTCENT ) THEN
                     TAKEIT = .TRUE.
                  END IF
               END IF
C
C              Actually take it and set this one up as the standard.
C              Get the IF limits for later comparison with doppler
C              and schedule set frequencies.  For channels that
C              don't match, set the frequency limits to zero.
C
               IF( TAKEIT ) THEN
                  IF( SDEBUG) WRITE(*,*) 'SETFCAT taking it:', 
     1                 KS, KF, ' ', FRNAME(KF)
                  GOT = .TRUE.
                  USEKF = KF
                  BESTOVER(KS) = OVERLAP
                  BESTPRIO = PRIO(KF)
                  BESTCENT = CENTER
                  DO ICH = 1, NCHAN(KS)
                     IIF = USEIF(ICH)
                     IF( IIF .GT. 0 ) THEN
                        IFREQIF(ICH,KS) = IIF
                        FIFMIN(ICH,KS) = FRF1(IIF,KF)
                        FIFMAX(ICH,KS) = FRF2(IIF,KF)
                     ELSE
                        IFREQIF(ICH,KS) = IIF
                        FIFMIN(ICH,KS) = 0.0D0
                        FIFMAX(ICH,KS) = 0.0D0
                     END IF
                  END DO
               END IF
            END IF
C
         END IF
      END DO
C
C     Some debug printout.
C
      IF( SDEBUG ) THEN
         SETMSG = ' '
         WRITE( SETMSG, '( A, L1, I4, I4, F10.3, I4, 1X, F10.3 )' ) 
     1           'SETFCAT: Match? ', GOT,  KS, USEKF, BESTOVER(KS), 
     2           BESTPRIO, BESTCENT
         CALL WLOG( 0, SETMSG )            
      END IF
C
C     If got a match, process it.
C     For the moment, keep all channels using the same IFREQNUM
C
      IF( GOT ) THEN
         IF( SDEBUG) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, * ) 
     1         'setfcat got one:', ks, usekf, ' ', frname(usekf)
            CALL WLOG( 0, MSGTXT )
         END IF
C
         DO ICH = 1, NCHAN(KS)
            IFREQNUM(ICH,KS) = USEKF
         END DO



C  partially complete coding here.
C ***************  be sure to set something for all channels.
C      This turned into a very bad case of projectus interruptus.
C      I was trying to put in the infrastructure for using more 
C      than 1 freq.dat entry, but now I'm not sure I remember 
C      what I was planning.  At least a lot of the test results
C      are now by channel.  But the second KF has not been 
C      introduced.   Get back to this some day.



C
C        Check if any of the received band is outside the RF bands.
C
         IF( BESTOVER(KS) .LT. TOTBW(KS) ) THEN
            IF( BESTOVER(KS) / TOTBW(KS) .GE. 0.90 ) NBWARN = NBWARN + 1
            IF( NBWARN .LE. 1 .OR. 
     1          BESTOVER(KS) / TOTBW(KS) .LT. 0.90 ) THEN
               CALL WLOG( 1, 'SETFCAT: In setup: ' // SETNAME(KS) )
               CALL WLOG( 1, '         Station '//SETSTA(1,KS) )
               SETMSG = ' '
               WRITE( SETMSG, '( A, F7.2, A, F7.2, A )' )
     1            '         Only ', BESTOVER(KS), ' of ', TOTBW(KS),
     2            ' MHz total bandwidth is within the IFs'
               CALL WLOG( 1, SETMSG )
               CALL WLOG( 1, '         and on the LO side of '//
     1            'any crossover frequencies' )
               CALL WLOG( 1, 
     1            '         in the setup file before any FREQ or ' //
     2            'DOPPLER shifts.' )
               KF = IFREQNUM(1,KS)
               SETMSG = ' '
               WRITE( SETMSG, '( A, A )' ) 
     1            '         IFs defined in frequency group: ', 
     2            FRNAME(KF)(1:LEN1(FRNAME(KF)))
               CALL WLOG( 1, SETMSG )
               IF( BESTOVER(KS) / TOTBW(KS) .GT. 0.98 ) THEN
                  CALL WLOG( 1, '         The loss is small '//
     1               'and may be the result of setting frequencies '//
     2               'for good pulse cal.' )
               END IF
            ELSE IF( NBWARN .EQ. 2 ) THEN
               CALL WLOG( 1, 'SETFCAT: Additional bandwidth '//
     1            'warnings suppressed for small losses.' )
            END IF
         END IF
C
      ELSE
         IF( SDEBUG ) WRITE(*,*) 'setfcat:  No match '
C
C        If there is not a match, write some details about 
C        any near misses.
C
         NBAD = 99
         CALL WLOG( 0, 'SETFCAT: In setup: ' // SETNAME(KS) )
         CALL WLOG( 0, '         Station '//SETSTA(1,KS) )
         CALL WRTMSG( 0, 'SETFCAT', 'freqnomatch' )
         DO KF = 1, NFREQ
            CALL FCOMPARE( KS, KF, MATCH, OKIF, NBAD, .TRUE. )
         END DO
C
C        If needed information from the catalog, complain and die.
C
         IF( NEEDCAT ) THEN
            CALL WLOG( 1, 'SETFCAT:  Inadequate frequency information.'
     1           // '  See the log file for details.' )
            CALL WRTMSG( 0, 'SETFCAT', 'freqcatwarning' )
            CALL ERRSET( KS )
C
C        Otherwise issue a warning.
C
         ELSE
C
C           Finish off the information output.  Make the warning
C           especially strong if it was the VLBA.  Nearly all valid
C           VLBA setups should be covered by freq.dat.
C
            IF( SETSTA(1,KS)(1:4) .EQ. 'VLBA' ) CALL WLOG( 0,
     1              '          Unless this is 3mm, since this is '//
     2              'the VLBA, it is very '//
     3              'likely that there is an error in the setup.' )
            CALL WLOG( 0, '--------------------------------------- ' )
            CALL WLOG( 0, ' ' )
C
C           Don't have a value for IFREQNUM(ICH,KS)
C
            DO ICH = 1, NCHAN(KS)
               IFREQNUM(ICH,KS) = -1
            END DO
         END IF
      END IF
C
C
C
C     Extract all needed information from the frequency catalog.    
C 
C     Initialize the alternative IF spec.
C
      DO ICH = 1, NCHAN(KS)
         ALTIFC(ICH,KS) = '  '
      END DO
C
C ***********************  Allow for multiple IFREQNUMs.
C      write(*,*) 'setfcat: Allow multiple IFREQNUMs.'
C
C       write(*,*) 'setfcat ifreqnum', ifreqnum(1,ks)
      IF( IFREQNUM(1,KS) .GE. 1 ) THEN
         KF = IFREQNUM(1,KS)
         LASTPOL = ' '
         DO ICH = 1, NCHAN(KS)
            KIF = IFREQIF(ICH,KS)
C
C           If the channel didn't have a match, KIF will be zero.
C           That is not a good array index.
C
            IF( KIF .GT. 0 ) THEN
               IF( FIRSTLO(ICH,KS) .EQ. NOTSET )
     1             FIRSTLO(ICH,KS) = FLO1(KIF,KF)
C
C               Set the IFCHAN in SETBBC.  There are issues with 
C               MarkIV that are better handled there.
C
C               IF( IFCHAN(ICH,KS) .EQ. ' ' )
C     1             IFCHAN(ICH,KS) = FIFNAM(KIF,KF)
C
               ALTIFC(ICH,KS) = FALTIF(KIF,KF)
C
C              If setting the polarization, update DUALPOL.
C              It will be false if the polarization was previously
C              unknown.  Jump through a hoop or two because 
C              the first channel might not be getting set yet.
C
               IF( POL(ICH,KS) .EQ. ' ' ) THEN
                  POL(ICH,KS) = FPOL(KIF,KF)
               END IF
               IF( LASTPOL .NE. ' ' .AND. 
     1             POL(ICH,KS) .NE. LASTPOL ) THEN
                  DUALPOL(KS) = .TRUE.
               END IF
               IF( POL(ICH,KS) .NE. ' ' ) LASTPOL = POL(ICH,KS)
C
               IF( SDEBUG ) THEN
                  SETMSG = ' '
                  WRITE( SETMSG, '( A, 4I4, 3F9.2, 1X, 5A )' ) 
     1               'SETFCAT: A ', ICH, KS, KIF, KF, 
     2               FIRSTLO(ICH,KS), FLO1(KIF,KF), FREQREF(ICH,KS),
     3               IFCHAN(ICH,KS), FIFNAM(KIF,KF), ALTIFC(ICH,KS),
     4               POL(ICH,KS), FPOL(KIF,KF)
                  CALL WLOG( 0, SETMSG )
               END IF
            ELSE
C
C              Channel doesn't match.  Leave parameters alone.
C
            END IF
C
         END DO
C
C        The following items are VLBA specific, but involve variables
C        that are not used elsewhere so it doesn't hurt to do for
C        all.
C
C
C        Deal with the DUALX specification.  It will be set .FALSE.
C        if it was not set in the setup file.  This is the mode that
C        allows both "polarizations" at X band to be RCP at 
C        different frequencies.
C
         IF( ( .NOT. DUALX(KS) ) .AND. FDUALX(KF) ) DUALX(KS) = .TRUE.
C
C        Set the 50 cm filter.  The default is 'DEF'.
C
         IF( RCP50CM(KS) .EQ. 'DEF' ) RCP50CM(KS) = FRCP50CM(KF)
         IF( LCP50CM(KS) .EQ. 'DEF' ) LCP50CM(KS) = FLCP50CM(KF)
C
C        Get the FE spec.  This is a bit tricky because it is not
C        indexed by the channel but by the IF (A=1, B=2, C=3, D=4)
C        The ABCD scheme of the VLBA type systems is not changing
C        with the RDBEs.
C
C        It gets tricky for systems that don't follow the VLBA 
C        style convention.  The MarkIV uses 1N, 2N, 1A, 2A
C        but that will all change with the DBBC.  So don't try to
C        deal with it.  There is an ABCD system with the DBBC.
C        I may need to deal with that eventually as it would get
C        through this filter, but might not be right.
C
         DO IIF = 1, FNIF(KF)
            IF( FE(1,KS) .EQ. 'omit' .AND. FIFNAM(IIF,KF) .EQ. 'A' )
     1          FE(1,KS) = FFE(IIF,KF)
            IF( FE(2,KS) .EQ. 'omit' .AND. FIFNAM(IIF,KF) .EQ. 'B' )
     1          FE(2,KS) = FFE(IIF,KF)
            IF( FE(3,KS) .EQ. 'omit' .AND. FIFNAM(IIF,KF) .EQ. 'C' )
     1          FE(3,KS) = FFE(IIF,KF)
            IF( FE(4,KS) .EQ. 'omit' .AND. FIFNAM(IIF,KF) .EQ. 'D' )
     1          FE(4,KS) = FFE(IIF,KF)
         END DO
C
C        Get the synthesizer settings if they were not already set.
C
         DO I = 1, 3
            IF( SYNTH(I,KS) .EQ. 0.D0 ) SYNTH(I,KS) = FSYN(I,KF) 
         END DO
C
C        Setting old VLA parameters removed for SCHED 10.2
C
      END IF
C
C     Set the default 50CM filters if they haven't been set yet.
C
      IF( RCP50CM(KS) .EQ. 'DEF' ) RCP50CM(KS) = 'NARROW'
      IF( LCP50CM(KS) .EQ. 'DEF' ) LCP50CM(KS) = 'NARROW'
C
C     Finally set the BBC and firstlo sidebands and BBC frequencies.
C     The required information should be available by this time.  
C     If anything important was missing, NEEDCAT would have been 
C     set and an error condition would have happened above.
C
      DO ICH = 1, NCHAN(KS)
C
C        First LO sideband.
C
         IF( FREQREF(ICH,KS) .GT. FIRSTLO(ICH,KS) ) THEN
            SIDE1(ICH,KS)   = 'U'
         ELSE
            SIDE1(ICH,KS)   = 'L'
         END IF
C
C        BBC sideband (do consistency check while at it).
C
         IF( NETSIDE(ICH,KS) .EQ. 'U' .AND. SIDE1(ICH,KS) .EQ. 'U' .OR.
     1       NETSIDE(ICH,KS) .EQ. 'L' .AND. SIDE1(ICH,KS) .EQ. 'L' )
     2       THEN
            IF( SIDEBD(ICH,KS) .EQ. 'L' ) THEN
               SETMSG = ' '
               WRITE( SETMSG, '( A, I2, A )' )  
     1             'SETFCAT: Specified BBC sideband for channel ', ICH,
     2             ' not consistent with value deduced from other data'
               CALL WLOG( 1, SETMSG )
               SETMSG = ' '
               WRITE( SETMSG, '( 8X, 6A )' )
     1            '   Netside=', NETSIDE(ICH,KS), 
     2            '   IF sideband=', SIDE1(ICH,KS),
     3            '   BBC sideband=', SIDEBD(ICH,KS)
               CALL WLOG( 1, SETMSG )
               SETMSG = ' '
               WRITE( SETMSG, '( 8X, A, F10.3, A, F10.3 )' )
     1            '   FREQREF=', FREQREF(ICH,KS), 
     2            '   FIRSTLO=', FIRSTLO(ICH,KS)
               CALL WLOG( 1, SETMSG )
               CALL ERRLOG( 'SETFCAT: Setup file: '// SETNAME(KS) )
            END IF
            SIDEBD(ICH,KS) = 'U'
         ELSE IF( NETSIDE(ICH,KS) .EQ. 'U' .AND. SIDE1(ICH,KS) .EQ. 'L'
     1       .OR. NETSIDE(ICH,KS) .EQ. 'L' .AND. SIDE1(ICH,KS) .EQ. 'U'
     2       ) THEN 
            IF( SIDEBD(ICH,KS) .EQ. 'U' ) THEN
               SETMSG = ' '
               WRITE( SETMSG, '( A, I2, A )' )  
     1             'SETFCAT: Specified BBC sideband for channel ', ICH,
     2             ' not consistent with value deduced from other data'
               CALL WLOG( 1, SETMSG )
               WRITE( SETMSG, '( 8X, 6A )' )
     1            '   Netside=', NETSIDE(ICH,KS), 
     2            '   IF sideband=', SIDE1(ICH,KS),
     3            '   BBC sideband=', SIDEBD(ICH,KS)
               CALL WLOG( 1, SETMSG )
               SETMSG = ' '
               WRITE( SETMSG, '( 8X, A, F10.3, A, F10.3 )' )
     1            '   FREQREF=', FREQREF(ICH,KS), 
     2            '   FIRSTLO=', FIRSTLO(ICH,KS)
               CALL WLOG( 1, SETMSG )
               CALL ERRLOG( 'SETFCAT: Setup file: '// SETNAME(KS) )
            END IF
            SIDEBD(ICH,KS) = 'L'
         ELSE
            CALL ERRLOG( 'SETFCAT: Program error: '//NETSIDE(ICH,KS)//
     1        ' '//SIDE1(ICH,KS) )
         END IF
C
C        Set the nominal BBC frequencies if they are needed.
C
         IF( BBSYN(ICH,KS) .EQ. 0.0D0 ) THEN
            BBSYN(ICH,KS) = ABS( FREQREF(ICH,KS) - FIRSTLO(ICH,KS) )
         END IF
C
C        While thinking about sidebands, save the BBC sideband as of this
C        point for use with the VLBA legacy system if reference pointing.
C        The SIDEBD variable can get altered later if the correlator 
C        sideband inversion option is used.  That can make the bookkeeping
C        complicated for the legacy BBC settings.
C
         CRDSIDE(ICH,KS) = SIDEBD(ICH,KS)
C
      END DO
C
      RETURN
      END



