      SUBROUTINE CHKCODE( CODE )
C
C     Routine for SCHED that confirms that the experiment code
C     conforms to standards.  
C
C     Check that A, B, and G series observations to have 3 numbers.
C     Otherwise, don't care.  The method used is crude, but clear.
C
      CHARACTER*(*)    CODE, PRCODE*8
      INTEGER          I, J, LEN1, DIG1
      LOGICAL          OK, OKDIG, CHKIT
      CHARACTER*1      NUM(10)
      DATA    NUM  / '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' /
C ---------------------------------------------------------------------
C     Check that a code was given as required.
C
      IF( CODE(1:3) .EQ. 'NUG' ) THEN
         CALL ERRLOG(
     1      'CHKCODE: EXPCODE not specified.  Needed for file names.' )
      END IF
C
C     Check format for VLA, VLBA, EVN, Global, and VSOP projects.  Require
C     that they use 3 digits.  Use PRCODE to avoid case sensitivity.
C
      PRCODE = CODE
      CALL UPCASE( PRCODE )
      CHKIT = .FALSE.
      IF( ( PRCODE(1:1) .EQ. 'A' .OR. PRCODE(1:1) .EQ. 'B' .OR.
     1      PRCODE(1:1) .EQ. 'E' .OR. PRCODE(1:1) .EQ. 'G' ) .AND.
     2      PRCODE(1:2) .NE. 'EG'  ) THEN
         CHKIT = .TRUE.
         DIG1 = 3
      END IF
C
      IF( PRCODE(1:1) .EQ. 'V' .OR. PRCODE(1:1) .EQ. 'W' ) THEN
         CHKIT = .TRUE.
         DIG1 = 2
      END IF
C
      IF( CHKIT ) THEN
         OK = .TRUE.
         IF( LEN1( PRCODE ) .LT. DIG1 + 2 ) THEN
            OK = .FALSE.
         ELSE 
            DO J = DIG1, DIG1 + 2
               OKDIG = .FALSE.
               DO I = 1, 10
                  IF( CODE(J:J) .EQ. NUM(I) ) OKDIG = .TRUE.
               END DO           
               IF( .NOT. OKDIG ) OK = .FALSE.
            END DO
         END IF
C
C        Warn if poor.  PRCODE is needed here because g77 does not like
C        to concatinate with a call argument.  It will not be upcased
C        this time.
C
         IF( .NOT. OK ) THEN
            PRCODE = CODE
            CALL WLOG( 0, 'CHKCODE:  EXPCODE = ' // PRCODE )
            CALL WRTMSG( 0, 'CHKCODE', 'expcode' )
         END IF
C
      END IF
C
      RETURN
      END
