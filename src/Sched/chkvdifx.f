      SUBROUTINE CHKVDIFX
C
C     Routine for SCHED called by DEFSET that checks that various
C     parameters are consistent with correlation on the VLBA DiFX
C     software correlator.
C
      INCLUDE     'sched.inc'
      INCLUDE     'schset.inc'
C
      INTEGER     KS, len1
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKVDIFX: starting.' )
C
C
      IF( ( CORREL(1:8) .EQ. 'VLBADIFX' .OR.
     1      CORREL(1:4) .EQ. 'VLBA' .OR.
     2      CORREL(1:4) .EQ. 'SOCORRO' ) .AND. .NOT. NOTAPE ) THEN
C
C        Give a short explanation of the style of average time chosen.
C
         IF( CAEXACT ) THEN
            CALL WRTMSG( 0, 'CHKVDIFX', 'CORAVGsetting1' )
         ELSE
            CALL WRTMSG( 0, 'CHKVDIFX', 'CORAVGsetting2' )
         END IF
C
C        Check for known formats.
C
         DO KS = 1, NSET
            IF( FORMAT(KS)(1:4) .NE. 'VLBA' .AND.
     1          FORMAT(KS)(1:6) .NE. 'MARK5B' .AND. 
     2          FORMAT(KS)(1:4) .NE. 'MKIV' ) THEN
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A, A )' ) 
     1            'CHKVDIFX:  Socorro DIFX correlator does not ',
     2            'understand format: ', FORMAT(KS)(1:LEN1(KS))
               CALL ERRLOG( MSGTXT )
            END IF
         END DO
C
      END IF
C
      RETURN
      END
