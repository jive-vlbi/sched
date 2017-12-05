      SUBROUTINE STANO( NAME, KSTA, ISTA, DOIT )
C
C     Routine that finds a station NAME in the station catalog.
C     NAME can match either the catalog station name or the
C     station code.
C
C     KSTA is the station catalog index (STATION and STCODEU).  Zero
C          implies NAME not in list.
C     ISTA is the index in the schedule station list entry (STANAME).
C          Zero implies station not in list.  Note that a station
C          must be in the catalog to be in STANAME.
C     DOIT is true if station name (STATION(ISTA)) matches DOSTA.
C
C     While at it, do not allow NAME to match two stations (this
C     will disallow scheduling with codes where two antennas at the
C     same site have the same code).
C
      INCLUDE   'sched.inc'
C
      CHARACTER   NAME*(*)
      INTEGER     ISTA, JSTA, KSTA, NC, LEN1
Cf2py intent(out) ISTA
Cf2py intent(out) KSTA
      LOGICAL     DOIT
Cf2py intent(out) DOIT
C ---------------------------------------------------------------
C     First find the station in the catalog.  Allow matches with
C     either the station code or the station name.  Don't allow more
C     than one match.  STREAD already prevented multiple entries
C     with the same name, so multiple matches are likely to be code
C     problems.
C
      KSTA = 0
      DO JSTA = 1, MSTA
         IF( NAME .EQ. STATION(JSTA) .OR.
     1       NAME .EQ. STCODEU(JSTA) ) THEN
            IF( KSTA .EQ. 0 ) THEN
               KSTA = JSTA
            ELSE
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A, A )' ) 
     1               'STANO:  Station name ', NAME(1:LEN1(NAME)),
     2               ' matches 2 catalog stations.'
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A, A )' ) 
     1               '        You may be using the station',
     2              ' code for a site with multiple entries in ',
     3              'the stations catalog.'
               CALL WLOG( 1, MSGTXT )
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A )' ) 
     1               '        This can happen if there are multiple',
     2               ' antennas or recording systems at a site.'
               CALL WLOG( 1, MSGTXT )
               WRITE( MSGTXT, '( A, A )' ) 
     1               '        If so, you should use the station ',
     2               'name which will be distinct.'
               CALL WLOG( 1, MSGTXT )
               CALL ERRLOG(  '        Fix the station list.' )
            END IF
         END IF
      END DO
C
C     Check for STANAME index.  Comparing catalog station name against
C     previously found stations, if any.
C
      ISTA = 0
      IF( KSTA .GT. 0 .AND. NSTA .GT. 0 ) THEN
         DO JSTA = 1, NSTA
            IF( STATION(KSTA) .EQ. STANAME(JSTA) ) THEN
               ISTA = JSTA
            END IF
         END DO
      END IF
C
C     Now check against DOSTA.
C
      NC = LEN1( DOSTA )
      IF( DOSTA .EQ. 'ALL' ) THEN
         DOIT = .TRUE.
      ELSE
         IF( KSTA .GT. 0 ) THEN
            DOIT = STATION(KSTA)(1:NC) .EQ. DOSTA(1:NC)
         ELSE
            DOIT = NAME(1:NC) .EQ. DOSTA(1:NC)
         END IF
      END IF
C
      RETURN
      END

