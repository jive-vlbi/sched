      SUBROUTINE CHKVDIFX
C
C     Routine for SCHED called by DEFSET that checks that various
C     parameters are consistent with correlation on the VLBA DiFX
C     software correlator.
C
      INCLUDE     'sched.inc'
      INCLUDE     'schset.inc'
C
      INTEGER     KS, LEN1, ISET
      LOGICAL     WARNBW
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKVDIFX: starting.' )
C
C
      IF( ( CORREL(1:8) .EQ. 'VLBADIFX' .OR.
     1      CORREL(1:4) .EQ. 'VLBA' .OR.
     2      CORREL(1:7) .EQ. 'SOCORRO' ) .AND. .NOT. NOTAPE ) THEN
C
C        Give a short explanation of the style of average time chosen.
C
         IF( CAEXACT ) THEN
            CALL WRTMSG( 0, 'CHKVDIFX', 'CORAVGsetting1' )
         ELSE
            CALL WRTMSG( 0, 'CHKVDIFX', 'CORAVGsetting2' )
         END IF
C
C        Check for known formats.  Allow format NONE among the
C        normal recording formats.
C
         DO KS = 1, NSET
            IF( FORMAT(KS)(1:4) .NE. 'VLBA' .AND.
     1          FORMAT(KS)(1:6) .NE. 'MARK5B' .AND. 
     2          FORMAT(KS)(1:4) .NE. 'MKIV' .AND.
     3          FORMAT(KS)(1:4) .NE. 'VDIF' .AND.
     4          FORMAT(KS)(1:4) .NE. 'NONE' ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A, A )' ) 
     1            'CHKVDIFX:  Socorro DIFX correlator does not ',
     2            'understand format: ', FORMAT(KS)(1:LEN1(FORMAT(KS)))
               CALL ERRLOG( MSGTXT )
            END IF
         END DO
C
C        Check for inappropriate number of channels.
C        We recommend 0.5 MHz per channel or less.  Warn about
C        cases where there are more.  Don't try too hard to get
C        all corners of parameters here - it's just a warning.
C
         WARNBW = .FALSE.
         DO ISET = 1, NSET
            IF( CORCHAN .LT. INT( BBFILT(1,ISET)*2.D0 - 0.1D0 ) ) THEN
               WARNBW = .TRUE.
            END IF
         END DO
         IF( WARNBW ) THEN
            CALL WLOG( 1, 'CHKVDIFX: Your CORCHAN request gives '//
     1         'more than 0.5 MHz per spectral channel for some '//
     2         'setup.' )
            CALL WLOG( 1, '          This may cause excessive '//
     1         'smearing with common a priori clock errors.' )
            CALL WLOG( 1, '          Consider using more channels.' )
         END IF
C
      END IF
C
      RETURN
      END
