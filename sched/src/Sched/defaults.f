      SUBROUTINE DEFAULTS
C
C     Subroutine for SCHED that takes the user input from both main
C     schedule input and from the catalogs etc and fills out information
C     that was not provided.  
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    ISCN, ISTA, KS
C
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'DEFAULTS starting.' )
C
C     Tie up the loose ends with the source catalogs, adding planets
C     etc if needed.  Be sure that the Julian scan times are available
C     for the ephemeris (deal with scan times first).
C
      CALL SRFINISH
C
C     Now tie up any loose ends related to automatic reference pointing.
C     The stations and source catalogs should be available by here.
C
      CALL PKFINISH
C
C     Set defaults and check the setups.
C
      IF( .NOT. NOSET ) CALL DEFSET
C
C     Clearly lots of stuff still needs to be moved here.
C
C
C     Default the grab stuff.  Put after setups defaulted so we
C     have the bit rate.  The GRABGAP assumes that the required
C     bit rate is the total over all channels.
C
      DO ISCN = SCAN1, SCANL
         IF( GRABTO(ISCN) .NE. 'NONE' .AND. GRABTIME(1,ISCN) .EQ. 0.0
     1       .AND. DATAPATH(ISCN) .EQ. 'IN2DISK' ) THEN
            GRABTIME(1,ISCN) = 30
            IF( GRABTIME(2,ISCN) .EQ. 0.0 ) GRABTIME(2,ISCN) = 10
            IF( GRABGAP(ISCN) .EQ. 0.0 ) THEN
               DO ISTA = 1, NSTA
                  IF( STASCN(ISCN,ISTA) ) THEN
                     KS = NSETUP( ISCN, ISTA )
                     GRABGAP(ISCN) = MAX( GRABGAP(ISCN), 
     1                  5. + GRABTIME(1,ISCN) * TOTBPS(KS) / 110. )
                  END IF
               END DO
            END IF
         END IF
      END DO
C
      RETURN
      END

