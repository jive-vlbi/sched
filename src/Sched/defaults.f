      SUBROUTINE DEFAULTS
C
C     Subroutine for SCHED that takes the user input from both main
C     schedule input and from the catalogs etc and fills out information
C     that was not provided.  
C
C     Jan. 29, 2013  Change the defaults for MINPAUSE and PRESTART.
C     Make them system dependent.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    ISCN, ISTA, KSTA, KS
      LOGICAL    ALLC
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
C     Make sure some items for the VLA are set.
C
      CALL VLASCNS
C
C     Set setup defaults and check the setups.
C
      IF( .NOT. NOSET ) CALL DEFSET
C
C     Set the system dependent defaults for PRESTART and MINPAUSE.
C     Set them both to zero for the VLBA RDBE or VLA WIDAR.
C     Otherwise take the old Mark5A era defaults.  The input 
C     defaulted to the flagged value UNSET to make an explicit
C     input of zero distinguishable.  The values are set
C     for each scan.
C
C     The new MARK5C systems don't need to worry about
C     PRESTART and MINPAUSE.  In fact, by using the data good time
C     in the VEX file, they are ignoring these parameters.  The 
C     MARK5A systems, however, need them.  The defaults should
C     be different.  That is handled here.  Take the non-zero
C     defaults if any of the stations need them.
C
      ALLC = .TRUE.
      DO ISTA = 1, NSTA
         KSTA = STANUM(ISTA)
         IF ( .NOT. USEONSRC(KSTA) ) ALLC = .FALSE.
      END DO
      DO ISCN = SCAN1, SCANL
         IF( ALLC ) THEN
            IF( MINPAUSE(ISCN) .EQ. UNSET * ONESEC ) 
     1          MINPAUSE(ISCN) = 0.D0
            IF( PRESTART(ISCN) .EQ. UNSET * ONESEC ) 
     1          PRESTART(ISCN) = 0.D0
         ELSE
            IF( MINPAUSE(ISCN) .EQ. UNSET * ONESEC ) 
     1            MINPAUSE(ISCN) = 10.D0 * ONESEC
            IF( PRESTART(ISCN) .EQ. UNSET * ONESEC ) 
     1            PRESTART(ISCN) = 5.D0 * ONESEC
         END IF
      END DO
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

