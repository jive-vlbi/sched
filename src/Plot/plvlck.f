      SUBROUTINE PLVLCK( XV1, XV2, YV1, YV2, XE1, XE2, YE1, YE2,
     1                   VAL, VAL1, LMIN, LMAX, EXP, IEXP, SGN,
     2                   OFS, EL, CH, STRVAL )
C
C     Routine for sched that compute or edit an element of
C     a value object
C
      CHARACTER     CH, STRVAL*14
      INTEGER       VAL, VAL1, LMIN, LMAX
      INTEGER       J, EL, OFS, EXP, SGN, IEXP
      REAL          XV1, XV2, YV1, YV2, XE1, XE2, YE1, YE2
C ----------------------------------------------------------------------
C
C     Set the Value of Axis element selected
C
      VAL = ABS( VAL )
C
C     Set the exponent of element selected
C
      IF( CH .EQ. '+' .OR. CH .EQ. '-') THEN
         IF( CH .EQ. '+' ) THEN
            EXP = EXP + 1
            IF( EXP .GT. IEXP ) EXP = 0
         ELSE 
            EXP = EXP - 1
            IF( EXP .LT. 0 ) EXP = IEXP
         END IF
C
C        Update Exponent Button
C
         CALL PLBUTC( XE1, XE2, YE1, YE2, ' ', EXP, 1 )
      END IF
C
C     Set new value
C
      IF(   CH .EQ. 'A' .OR.  CH .EQ. 'D' .OR. CH .EQ. 'X' .OR.
     1    ( CH .GE. '0' .AND. CH .LE. '9' ) ) THEN
C
C        Edit Value
C
         IF( CH .GE. '0' .AND. CH .LE. '9' ) THEN
            CALL PLNEDT( XV1, XV2, YV1, YV2, CH, VAL )
C
C        Set Value
C
         ELSE IF( CH .EQ. 'A' ) THEN
            VAL = VAL + 10**EXP
         ELSE
            VAL = VAL + ( 10**EXP * (-1) )
         END IF
C
C        Check Value Range
C
         IF( EL .EQ. 1 ) THEN
            IF( VAL .GT. LMAX .OR. 
     1        ( VAL .EQ. LMAX .AND. OFS .EQ. 1) ) THEN
               VAL = LMIN
            ELSE IF( VAL .LT. LMIN ) THEN
               VAL = LMAX - OFS
            ENDIF
         ELSE
            IF( ABS( VAL1 ) .EQ. LMAX ) THEN
               VAL = 0
            ELSE
               IF( VAL .GT. 59 ) VAL = 0
               IF( VAL .LT. 0 )  VAL = 59
            END IF
         ENDIF
C
C        Replot a Coordinate element
C
         IF( VAL .GT. (10**8) ) THEN
            CALL PLNUMB( VAL, STRVAL )
         ELSE
            CALL PGNUMB( VAL, 0, 1, STRVAL, J )
         END IF
C
         CALL PLSTXT( XV1, XV2, YV1, YV2, STRVAL, 1, .FALSE. )
C
C        Set the new Value of Axis element selected
C
         IF( EL .EQ. 1 ) VAL = VAL * SGN
C
      END IF
C
      RETURN
      END
