      SUBROUTINE VXVBRX( KS, ICH, VBCOM )
C
C     Routine for writing comment information to the $IF segments
C     of a VEX file to indicate the receiver, any receiver LO 
C     (when there is a second mix in an IF converter) and any
C     externally selected filter.  This only for VLBA stations
C     but was changed to do all stations on Dec. 13, 2012 (RCW).
C
C     This information is required at the VLBA telescopes and at
C     the VLA because it cannot be deduced in all cases from other 
C     information in the VEX file and there is not normally enough
C     operator interaction to let someone take care of it outside
C     the schedule.  Among other things, it is required to set 
C     the various switches on the VLBA antennas.  
C
C     All of the information needed is in the setup group on a per
C     IF basis.  The calling program has both the setup group KS
C     and ICH, so we only need those to get the info needed.
C
C     Routine by RCW Sep. 2011.
C     Bug fix Nov. 15, 2011.  Was using IF number as channel number.
C
C     Bug found Sept. 10, 2012.  if_def lines are reused if they are 
C     good for more than one station.  But the line is only 
C     constructed based on the first station to use it.  If a 
C     non-VLBA station is first, the comments get omitted.  A user
C     ran into this when he specified EB first.  I am going to 
C     prevent the Vex code (VXCFIF) from using the same if_def for two
C     IFs that are not both VLBA or both non-VLBA.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER        KS, ICH
      INTEGER        LEN1
      INTEGER        SYNPRT
      LOGICAL        GOTIT
      CHARACTER      VBCOM*(*), LOPRT*20, FETXT*6, FILTTXT*6

C ------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'VXVBRX starting.' )
C
      GOTIT = .FALSE.
C
C     Do for all stations.
C                  IF( SETSTA(1,KS)(1:4) .EQ. 'VLBA' ) THEN
C
C
C         Get the first two FE synth settings for the VLBA.
C         This is for once the new synthesizers are installed.
C
          IF( SETSTA(1,KS)(1:4) .EQ. 'VLBA' ) THEN
             WRITE( LOPRT, '( 2F10.2 )' ) SYNTH(1,KS) * 1.D3, 
     1          SYNTH(2,KS) * 1.D3
          ELSE
             LOPRT = '  0.0   0.0 '
          END IF
C
C         Get the receiver.  Recall that, on the VLBA,
C         IFs A, B, C, D correspond to the 4 elements of the
C         FE array.
C
          IF( IFCHAN(ICH,KS) .EQ. 'A' ) THEN
             FETXT = FE(1,KS)
          ELSE IF( IFCHAN(ICH,KS) .EQ. 'B' ) THEN
             FETXT = FE(2,KS)
          ELSE IF( IFCHAN(ICH,KS) .EQ. 'C' ) THEN
             FETXT = FE(3,KS)
          ELSE IF( IFCHAN(ICH,KS) .EQ. 'D' ) THEN
             FETXT = FE(4,KS)
          ELSE
             FETXT = 'NA'
          END IF
C
C         Get the receiver LO frequency, but only for the VLBA
C         because this depends on the synthesizer settings which
C         don't translate well for other stations.
C
C         Whenever the third synthesizer comes under the control
C         of the Executor, we will likely want to put its frequency
C         setting here even when it is not used.  But Matthias and
C         I decided to hold off on that for now (RCW  March 2013).
C
          IF( SETSTA(1,KS)(1:4) .EQ. 'VLBA' ) THEN
             IF( FETXT .EQ. '1cm' ) THEN
                SYNPRT = NINT( 1000.D0 * SYNTH(3,KS) )
             ELSE IF( FETXT .EQ. '7mm' ) THEN
                SYNPRT = NINT( 3000.D0 * SYNTH(3,KS) )
             ELSE IF( FETXT .EQ. '3mm' ) THEN
                SYNPRT = NINT( 6000.D0 * SYNTH(3,KS) )
             ELSE
                SYNPRT = 0
             END IF
          ELSE
             SYNPRT = 0
          END IF
C
C         Get the 50cm filter setting for the VLBA.
C
          IF( SETSTA(1,KS)(1:4) .EQ. 'VLBA' .AND. 
     1        ( FETXT .EQ. '50cm' .OR. FETXT .EQ. '90cm' ) ) THEN
             IF( IFCHAN(ICH,KS) .EQ. 'A' .OR.
     1           IFCHAN(ICH,KS) .EQ. 'C' ) THEN
                FILTTXT = RCP50CM(KS)
             ELSE IF( IFCHAN(ICH,KS) .EQ. 'B' .OR.
     1           IFCHAN(ICH,KS) .EQ. 'D' ) THEN
                FILTTXT = LCP50CM(KS)
             END IF
          ELSE
             FILTTXT = 'NA'
          END IF
C
          GOTIT = .TRUE.
C         END IF              Doing for all stations.
C
C     Fill out VBCOM
C
      VBCOM = ' '
      IF( GOTIT ) THEN
         WRITE( VBCOM, '( 1X, A, 2X, A, I6, 1X, A )' )
     1       LOPRT(1:LEN1(LOPRT)), FETXT(1:LEN1(FETXT)), SYNPRT,
     2       FILTTXT(1:LEN1(FILTTXT))
      ELSE
         VBCOM = ' '
      END IF
C
      RETURN
      END


