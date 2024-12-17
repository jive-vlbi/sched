      LOGICAL FUNCTION BADLO( PARM, FREQ, FMULT, NADD, 
     1                  FADD1, FADD2, FMIN, FMAX, MSG )
C
C     Function for SCHED, called by CHKVLA and maybe others, that
C     checks an LO setting.  
C
C     Problems with precision preclude using a simple statement such as 
C       IF ( MOD( VLASYNB(KS), 50.0 ) .NE. 10.1 ) THEN
C     Require the match to be good to a part in 2.E10. 
C     All numbers were converted to DOUBLE PRECISION to make this
C     possible.
C
C     The LO (FREQ) must be N*FMULT +- FADD.
C     There can be zero to two (NADD) FADDs: FADD1 and FADD2.  
C     If NADD = -1, just check the range.
C     FREQ must lie between FMIN and FMAX.
C
C     MSG just saves space by using calling routine's variable.
C
      CHARACTER          PARM*(*)
      INTEGER            NADD
      DOUBLE PRECISION   FREQ, FMULT, FADD1, FADD2, AFREQ, BFREQ
      DOUBLE PRECISION   FMIN, FMAX
      DOUBLE PRECISION   TEST, DIFF
      LOGICAL            GOOD
      CHARACTER          MSG*(*)
C --------------------------------------------------------------------
      IF( NADD .LT. -1 .OR. NADD .GT. 2 ) THEN
         CALL ERRLOG( 'BADLO: Bad value for NADD' )
      END IF
C
      BADLO = .FALSE.
C
      AFREQ = ABS( FREQ )
      TEST = AFREQ / 2.D10
      IF( NADD .EQ. 0 ) THEN
C
C        Check for N*FMULT if no offsets.
C
         DIFF = ABS( FMULT * DNINT( AFREQ / FMULT ) - AFREQ )
         IF( DIFF .GT. TEST ) BADLO = .TRUE.
      ELSE IF( NADD .GE. 1 ) THEN
         GOOD = .FALSE.
C
C        Check for N*FMULT + FADD1
C
         BFREQ = AFREQ - FADD1
         DIFF = ABS( FMULT * DNINT( BFREQ / FMULT ) - BFREQ )
         IF( DIFF .LE. TEST ) GOOD = .TRUE.                  
C
C        Check for N*FMULT - FADD1
C
         BFREQ = AFREQ + FADD1
         DIFF = ABS( FMULT * DNINT( BFREQ / FMULT ) - BFREQ )
         IF( DIFF .LE. TEST ) GOOD = .TRUE.                  
C
         IF( NADD .EQ. 2 ) THEN
C
C           Check for N*FMULT + FADD2
C
            BFREQ = AFREQ - FADD2
            DIFF = ABS( FMULT * DNINT( BFREQ / FMULT ) - BFREQ )
            IF( DIFF .LE. TEST ) GOOD = .TRUE.                  
C
C           Check for N*FMULT - FADD2
C
            BFREQ = AFREQ + FADD2
            DIFF = ABS( FMULT * DNINT( BFREQ / FMULT ) - BFREQ )
            IF( DIFF .LE. TEST ) GOOD = .TRUE.                  
C
         END IF
C
         IF( .NOT. GOOD ) BADLO = .TRUE.
C
      END IF
C
C     Check range.
C
      IF( FREQ .LT. FMIN .OR. FREQ .GT. FMAX ) THEN
         BADLO = .TRUE.
      END IF
C
C     Write warnings.
C
      IF( BADLO ) THEN
         MSG = ' '
         WRITE( MSG, '( 3A, F10.3 )' )
     1       'BADLO: Bad LO frequency.  Parameter ', PARM,
     2       ' = ', FREQ
         CALL WLOG( 1, MSG )
         MSG = ' '
         IF( NADD .EQ. 0 ) THEN
            WRITE( MSG, '( A, F10.3 )' )
     1          '       It should be N * ', FMULT
         ELSE IF( NADD .EQ. 1 ) THEN
            WRITE( MSG, '( A, F10.3, A, F8.3 )' )
     1          '       It should be N * ', FMULT, ' +- ', FADD1
         ELSE IF( NADD .EQ. 2 ) THEN
            WRITE( MSG, '( A, F10.3, A, F8.3, A, F8.3 )' )
     1          '       It should be N * ', FMULT, ' +- ', FADD1, 
     2          ' or +- ', FADD2
         END IF
         IF( NADD .NE. -1 ) CALL WLOG( 1, MSG )
C
         MSG = ' '
         WRITE( MSG, '( A, F10.3, A, F10.3 )' )
     1       '       It should be between ', FMIN, ' and ', FMAX
         CALL WLOG( 1, MSG )
C
      END IF
C
      RETURN
      END




