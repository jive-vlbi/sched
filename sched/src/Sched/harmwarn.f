      SUBROUTINE HARMWARN( KS, SY1, LO1, LOI, SY2 ,LO2, LOJ, K, L,
     1                     IFF, TONHD )
C
C     Routine called by SETUSYN to write out warnings about
C     possible cases of harmonic interference.  This was needed
C     in two places and got a bit complicated so it was split
C     out.
C
C     SY1 and SY2 are the synthesizer number being used.  They 
C     will be printed in the table.
C
C     LO1 and LO2 are the synthesizer settings.  LOI and LOJ are
C     the same for LO1/LO2 < 8.0 and half of LO1/LO2 for over 8.0.
C     K and L are the harmonic numbers of LOI and LOJ (the loop
C     over harmonics is in the calling routine).  IFF is the
C     difference frequency in GHz.  The routine is only called
C     if this is within the IF and warnings need to be written.
C     TONHD just determines whether this is the first call for this
C     setup.  If true, the explanations are written and the table
C     headings are written.  If false, only the table line is
C     written.  TONHD will be true once per setup while the internal
C     flag HWARN will only be true for the very first call.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
C
C
      INTEGER     KS, K, L, SY1, SY2
      LOGICAL     TONHD, HWARN
      DOUBLE PRECISION  LO1, LO2, LOI, LOJ, IFF, BDIFF
      REAL        K2, L2
C
      INTEGER     LEN1, NRF, IRF, ISIDE1, ICH
      DOUBLE PRECISION  RF(3), RFF
      CHARACTER   INBBC(3)*3
      DATA        HWARN / .TRUE. /
      SAVE        HWARN
C  -------------------------------------------------------------------
      IF( TONHD ) THEN
         TONHD = .FALSE.
         CALL WLOG( 1, ' ' )
C
C        Give the first line of the warning with the setup file
C        name.
C
         MSGTXT = ' '
         WRITE( MSGTXT, '( 3A )' )
     1       'HARMWARN:  Setup file: ', 
     2       SETNAME(KS)(1:LEN1(SETNAME(KS)))
         CALL WLOG( 1, MSGTXT )
C
C        Give a short description of the problem the first time.
C
         IF( HWARN ) THEN
C
C           Write a detailed explanation from the message file.
C
            CALL WRTMSG( 0, 'HARMWARN', 'harmwarn' )
            HWARN = .FALSE.
C
C           Now write the short information above the table.
C
            MSGTXT = ' '
            WRITE( MSGTXT, '( 2A )' )
     1          '          There are possible tones in the ', 
     2          'IF due to mixing of harmonics of the '
            CALL WLOG( 1, MSGTXT )
            MSGTXT = ' '
            WRITE( MSGTXT, '( 2A )' )
     1          '          VLBA front end synthesizer outputs.  ',
     2          'See sched.runlog for detailed information.'
            CALL WLOG( 1, MSGTXT )
C
         ELSE
C
C        The next time, be more terse.
C
            MSGTXT = ' '
            WRITE( MSGTXT, '( 2A ) ' )
     1          '         This setup also has possible tones ',
     2          'from mixing of LO harmonics.'
            CALL WLOG( 1, MSGTXT )
         END IF
C
C        Write the table header
C
         CALL WLOG( 1, ' ' )
         WRITE( MSGTXT, '( 2A )' )
     1       'Setup Syn  LO(GHz)  Osc Harmonic Syn LO(GHz)  ',
     2       'Osc Harmonic   IF (MHz) RF    In BBC'
         CALL WLOG( 0, MSGTXT )
      END IF
C
C     Before writing the line, get the RF frequencies
C     for the offending harmonic mixes.  This is made 
C     messy by the combinations of LOs used for 22 GHz 
C     and up and the lack of a pointer from the synthesizer 
C     to the channel.  So don't try that way.  It is possible
C     to have 3 distinct IFs for S/X with wide X and 2 with
C     normal S/X and with the wide C band.  Find the different
C     cases by searching through the channels for and looking
C     at the FIRSTLOs.  Write a separate line for each case.
C     note that there will rarely be more than one.
C
C     While at it, determine if the tone is in a BBC.
C
      DO IRF = 1, 3
         RF(IRF) = 0.D0
         INBBC(IRF) = 'NO'
      END DO
      NRF = 0
      DO ICH = 1, NCHAN(KS)
         IF( SIDE1(ICH,KS) .EQ. 'U' ) THEN 
            ISIDE1 = 1
         ELSE
            ISIDE1 = -1
         END IF
C
C        Get the tone RF frequency in this channel.
C
         RFF = FIRSTLO(ICH,KS) + ISIDE1 * IFF * 1000.D0
C
C        See if this is new.  Accumulate distinct cases.
C
         IRF = 1
         DO WHILE( IRF .LE. 3 )
            IF( RFF .EQ. RF(IRF) .OR. RF(IRF) .EQ. 0.D0 ) THEN
               RF(IRF) = RFF
               NRF = MAX( IRF, NRF )
C
C              Check if in BBC.
C
               BDIFF = RF(IRF) - FREQREF(ICH,KS)
               IF( ABS( BDIFF ) .LE. BBFILT(ICH,KS) ) THEN
                  IF( ( NETSIDE(ICH,KS) .EQ. 'U' .AND.
     1                BDIFF .GE. 0.D0 ) .OR.
     2                ( NETSIDE(ICH,KS) .EQ. 'L' .AND.
     3                BDIFF .LE. 0.D0 ) ) INBBC(IRF) = 'YES'
               END IF
C
C              Found the RF case this channel belongs in.  Go
C              to the next channel.
C
               GO TO 100
            END IF
C
C           This channel was not in this IRF.  Go to next IRF.
C
            IRF = IRF + 1
         END DO
C
  100    CONTINUE
      END DO
C
C     Write a line for the data.  If NRF is more than one,
C     write an extra line for each case so we can do the
C     In BBC test.  NRF is never more than 3.
C       
      K2 = K
      IF( LO1 .GT. 8.0D0 ) K2 = K / 2.0
      L2 = L
      IF( LO2 .GT. 8.0D0 ) L2 = L / 2.0
      IF( NRF .GE. 1 ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( I4, I4, F8.1, F7.2, F7.1, I5, F7.1, F7.2,'
     1        //' F7.1, 2F10.2, 3X, A3 )' )  KS, SY1, LO1, LOI, K2, 
     2 	      SY2, LO2, LOJ, L2, IFF*1000.0, RF(1), INBBC(1)
         CALL WLOG( 0, MSGTXT )
      END IF
C
      MSGTXT = ' '
      IF( NRF .GE. 2 ) THEN
         WRITE( MSGTXT, '( 66X, F10.2, 3X, A3 )' ) RF(2), INBBC(2)
         CALL WLOG( 0, MSGTXT )
      END IF
C
      MSGTXT = ' '
      IF( NRF .GE. 3 ) THEN
         WRITE( MSGTXT, '( 66X, F10.2, 3X, A3 )' ) RF(3), INBBC(2)
         CALL WLOG( 0, MSGTXT )
      END IF
C
C     Warn if this will be a strong one because a primary
C     synthesizer output is involved and it is synthesizer
C     1 and 2.
C
      IF( ( LO1 .LT. 8.D0 .AND. K .EQ. 1 ) .OR.
     1    ( LO1 .GT. 8.D0 .AND. K .EQ. 2 ) .OR.
     2    ( LO2 .LT. 8.D0 .AND. L .EQ. 1 ) .OR.
     3    ( LO2 .GT. 8.D0 .AND. L .EQ. 2 ) ) THEN
         IF( ( ABS( LO1 - SYNTH(1,KS) ) .LT. 0.001D0 .OR.
     1         ABS( LO1 - SYNTH(2,KS) ) .LT. 0.001D0 ) .AND.
     2       ( ABS( LO2 - SYNTH(1,KS) ) .LT. 0.001D0 .OR.
     3         ABS( LO2 - SYNTH(2,KS) ) .LT. 0.001D0 ) ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( 2A )' ) 
     1          '      Likely strong tone - From synthesizers 1 and 2',
     2          ' and a primary signal is involved.'
            CALL WLOG( 0, MSGTXT )
         END IF
      END IF
C
      RETURN
      END
