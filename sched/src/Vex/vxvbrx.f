      SUBROUTINE VXVBRX( KS, ICH, VBCOM )
C
C     Routine for writing comment information to the $IF segments
C     of a VEX file to indicate the receiver, any receiver LO 
C     (when there is a second mix in an IF converter) and any
C     externally selected filter.  This is only for VLBA stations.
C
C     This information is required at the telescopes because it
C     cannot be deduced in all cases from other information in the
C     VEX file.  It is required to set the various switches on the
C     antenna.  It is station specific to the VLBA
C
C     All of the information needed is in the setup group on a per
C     IF basis.  The calling program has both the setup group KS
C     and ICH, so we only need those to get the info needed.
C
C     Routine by RCW Sep. 2011.
C     Bug fix Nov. 15, 2011.  Was using IF number as channel number.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schset.inc'
C
      INTEGER        KS, ICH
      INTEGER        LEN1
      INTEGER        SYNPRT
      LOGICAL        GOTIT
      CHARACTER      VBCOM*(*), FETXT*6, FILTTXT*6

C ------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'VXVBRX starting.' )
C
      GOTIT = .FALSE.
      IF( SETSTA(1,KS)(1:4) .EQ. 'VLBA' ) THEN
C
C         Get the receiver.  Recall that, on the VLBA,
C         IFs A, B, C, D correspond to the 4 elements of
C         FE.
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
C         Get the receiver LO frequency.
C
          IF( FETXT .EQ. '1cm' ) THEN
             SYNPRT = NINT( 1000.D0 * SYNTH(3,KS) )
          ELSE IF( FETXT .EQ. '7mm' ) THEN
             SYNPRT = NINT( 3000.D0 * SYNTH(3,KS) )
          ELSE IF( FETXT .EQ. '3mm' ) THEN
             SYNPRT = NINT( 6000.D0 * SYNTH(3,KS) )
          ELSE
             SYNPRT = 0
          END IF
C
          IF( FETXT .EQ. '50cm' .OR. FETXT .EQ. '90cm' ) THEN
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
      END IF
C
C     Fill out VBCOM
C
      VBCOM = ' '
      IF( GOTIT ) THEN
         WRITE( VBCOM, '( 1X, A, I6, 1X, A )' )
     1       FETXT(1:LEN1(FETXT)), SYNPRT,
     2       FILTTXT(1:LEN1(FILTTXT))
      ELSE
         VBCOM = ' '
      END IF
C
      RETURN
      END


