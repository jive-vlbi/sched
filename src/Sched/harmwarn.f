      SUBROUTINE HARMWARN( KS, LO1, LOI, LO2, LOJ, K, L,
     1                     IFF, TONHD )
C
C     Routine called by SETUSYN to write out warnings about
C     possible cases of harmonic interference.  This was needed
C     in two places and got a bit complicated so it was split
C     out.
C
C     LO1 and LO2 are the synthesizer settings.  LOI and LOJ are
C     the same for LO1/LO2 < 8.0 and half of LO1/LO2 for over 8.0.
C
      INCLUDE   'sched.inc'
      INCLUDE   'schset.inc'
C
C
      INTEGER     KS, K, L
      LOGICAL     TONHD
      DOUBLE PRECISION  LO1, LO2, LOI, LOJ, IFF
      REAL        K2, L2
C
      INTEGER     LEN1, NRF, IRF, ISIDE1, ICH
      DOUBLE PRECISION  RF(3), RFF
C  -------------------------------------------------------------------
      IF( TONHD ) THEN
         TONHD = .FALSE.
         CALL WLOG( 1, ' ' )
         MSGTXT = ' '
         WRITE( MSGTXT, '( 3A )' )
     1       'HARMWARN:  Setup file: ', 
     2       SETNAME(KS)(1:LEN1(SETNAME(KS)))
         CALL WLOG( 1, MSGTXT )
         MSGTXT = ' '
         WRITE( MSGTXT, '( 2A )' )
     1       '          There are possible tones in the ', 
     2       'IF due to mixing of harmonics of the '
         CALL WLOG( 1, MSGTXT )
         MSGTXT = ' '
         WRITE( MSGTXT, '( 2A )' )
     1       '          VLBA front end synthesizer outputs.  ',
     2       'See sched.runlog for more information.'
         CALL WLOG( 1, MSGTXT )
C
         MSGTXT = ' '
         WRITE( MSGTXT, '( 2A )' )
     1       '          Above 8 GHz, the main oscillator ',
     2       'runs at half the output frequency, hence '
         CALL WLOG( 1, MSGTXT )
         MSGTXT = ' '
         WRITE( MSGTXT, '( 2A )' )
     1       '          the possibility of half integer ',
     2       'harmonics. '
         CALL WLOG( 1, MSGTXT )
C
C        Write a detailed explanation.
C
         CALL WRTMSG( 0, 'HARMWARN', 'harmwarn' )

         CALL WLOG( 1, ' ' )
         WRITE( MSGTXT, '( 2A )' )
     1       '  Setup   LO (GHz) Osc   Harmonic  LO(GHz)  ',
     2       'Osc   Harmonic   IF  (MHz)  RF '
         CALL WLOG( 1, MSGTXT )
      END IF
C
C     Before writing the line, get the RF frequencies
C     for the offending lines.  This is made messy by the
C     combinations of LOs used for 22 GHz and up and the
C     lack of a pointer from the synthesizer to the 
C     channel.  So don't try that way.  It is possible
C     to have at most 3 distinct FIRSTLO's (for S/X
C     with wide X).  Find them based on FIRSTLO.
C
      DO IRF = 1, 3
         RF(IRF) = 0.D0
      END DO
      NRF = 0
      DO ICH = 1, NCHAN(KS)
         IF( SIDE1(ICH,KS) .EQ. 'U' ) THEN 
            ISIDE1 = 1
         ELSE
            ISIDE1 = -1
         END IF
         RFF = FIRSTLO(ICH,KS) + ISIDE1 * IFF * 1000.D0
         IRF = 1
         DO WHILE( IRF .LE. 3 )
            IF( RFF .EQ. RF(IRF) .OR. RF(IRF) .EQ. 0.D0 ) THEN
               RF(IRF) = RFF
               NRF = MAX( IRF, NRF )
               GO TO 100
            END IF
            IRF = IRF + 1
         END DO
  100    CONTINUE
      END DO
C
C     Write a line for the data.
C       
      K2 = K
      IF( LO1 .GT. 8.0D0 ) K2 = K / 2.0
      L2 = L
      IF( LO2 .GT. 8.0D0 ) L2 = L / 2.0
      MSGTXT = ' '
      WRITE( MSGTXT, '( I6, F9.1, F7.2, F8.1, F10.1, F7.2,'//
     1     ' F9.1, 4F10.2 )' )
     2     KS, LO1, LOI, K2, LO2, LOJ, L2, IFF*1000.0,
     3     (RF(IRF),IRF=1,NRF)
      CALL WLOG( 1, MSGTXT )
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
            CALL WLOG( 1, MSGTXT )
         END IF
      END IF
C
      RETURN
      END
