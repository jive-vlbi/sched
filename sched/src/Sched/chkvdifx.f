      SUBROUTINE CHKVDIFX
C
C     Routine for SCHED called by DEFSET that checks that various
C     parameters are consistent with correlation on the VLBA DiFX
C     software correlator.
C
      INCLUDE     'sched.inc'
      INCLUDE     'schset.inc'
C
      INTEGER    KS, OVERSAMP, NFFTS, NPFFTS
      REAL       TAVG, RNFFTS
C ----------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'CHKVDIFX: starting.' )
C
C
      IF( ( CORREL(1:8) .EQ. 'VLBADIFX' ) .AND. .NOT. NOTAPE ) THEN
C
C        Give a short explanation of the style of average time chosen.
C
         IF( CAEXACT ) THEN
            CALL WRTMSG( 0, 'CHKVDIFX', 'CORAVGsetting1' )
         ELSE
            CALL WRTMSG( 0, 'CHKVDIFX', 'CORAVGsetting2' )
         END IF
C
      END IF
C
      RETURN
      END
