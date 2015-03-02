      SUBROUTINE PLMARK( ISTA )
C
C     Routine for Sched, called by PLOTMAP, that marks a station
C     position.  Split out because it is used often.
C
      INCLUDE    'sched.inc'
      INCLUDE    'plot.inc'
C
      INTEGER     ISTA
      REAL        XPLT(MAXSTA), YPLT(MAXSTA)
C ---------------------------------------------------------------------
C     Get the station position in the right plot units.
C
      XPLT(ISTA) = LONG(STANUM(ISTA)) / RADDEG
      YPLT(ISTA) = LAT(STANUM(ISTA)) / RADDEG
C
C     Draw a green ring around stations not in their original positions.
C
      IF( MOVEDS(ISTA) ) THEN
         CALL PGSCI( 3 )
         CALL PGSCH( RINGSIZE ) 
         CALL PGPT1( XPLT(ISTA), YPLT(ISTA), -9 )
      END IF
C
C     Now mark the station with the appropriate color.
C
      IF( PSTBCK(ISTA,1) .EQ. 1 .AND. PSTBCK(ISTA,2) .EQ. 0 ) THEN
         CALL PGSCI( 4 )
      ELSE IF( PSTBCK(ISTA,1) .EQ. 1 .AND. PSTBCK(ISTA,2) .EQ. 1 ) THEN
         CALL PGSCI( 2 )
      ELSE IF( PSTBCK(ISTA,1) .EQ. 0 ) THEN
         CALL PGSCI( 7 )
      ELSE
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, 3I6 )' ) 'PLMARK: ',
     1         ISTA, PSTBCK(ISTA,1), PSTBCK(ISTA,2)
         CALL WLOG( 1, MSGTXT )
         CALL ERRLOG( 'PLMARK: Bad PSTBCK.  Programming problem.' )
      END IF
      CALL PGSCH( DOTSIZE )
      CALL PGPT1( XPLT(ISTA), YPLT(ISTA), -9 )
      IF( PSTBCK(ISTA,1) .EQ. 0 .AND. PSTBCK(ISTA,2) .EQ. 1 ) THEN 
         CALL PGSCH( DOTSIZE / 3 ) 
         CALL PGSCI( 2 )
         CALL PGPT1( XPLT(ISTA), YPLT(ISTA), -9 )
      END IF
C
      RETURN
      END
