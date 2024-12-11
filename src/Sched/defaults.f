      SUBROUTINE DEFAULTS
C
C     Subroutine for SCHED that takes the user input from both main
C     schedule input and from the catalogs etc and fills out information
C     that was not provided.  It is also a convenient place to check
C     inappropriate catalog choices or attempt to use Mark5A on the 
C     VLBA for other than pointing.
C
C     Jan. 29, 2013  Change the defaults for MINPAUSE and PRESTART.
C     Make them system dependent.
C     Dec 2013  Move MINPAUSE and PRESTART defaulting to RECCTL.
C
      INCLUDE    'sched.inc'
      INCLUDE    'schset.inc'
C
      INTEGER    ISCN, ISTA, KS, KSTA
      LOGICAL    FRDBE, SRDBE, M5AWARN
      DATA       M5AWARN /.TRUE./
      SAVE       M5AWARN
C
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'DEFAULTS starting.' )
C
C     Protect against mixing RDBE and non-RDBE stations and frequency files.
C
      FRDBE = INDEX( FREQFILE, 'RDBE' ) .NE. 0
      SRDBE = INDEX( STAFILE, 'RDBE' ) .NE. 0
      IF( FRDBE .NEQV. SRDBE ) THEN
         MSGTXT = 'DEFAULTS: **** Did you intend to mix RDBE and '//
     1     'non-RDBE station and frequency files? ****'
         CALL WLOG( 1, ' ' )
         CALL WLOG( 1, MSGTXT )
         CALL WLOG( 1, ' ' )
      END IF
C
C     Discourage use of MARK5A on the VLBA except for pointing.
C
      DO ISTA = 1, NSTA
         KSTA = STANUM(ISTA)
         IF( STATION(KSTA)(1:4) .EQ. 'VLBA' .AND. 
     1       DISK(KSTA) .EQ. 'MARK5A' .AND. M5AWARN .AND.
     2       .NOT. NOTAPE ) THEN
            CALL WLOG( 1, 'STREAD: ==== WARNING ==== Mark5A '//
     1          'specified for a VLBA  station.' )
            CALL WLOG( 1, '        Those recorders have been '//
     1          'removed from most VLBA stations.' )
            M5AWARN = .FALSE.
         END IF
      END DO
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
C     Work on setups if there are any.
C
      IF( .NOT. NOSET ) THEN
C
C        Set setup defaults and check the setups.
C
         CALL DEFSET
C
C        Set the default for DOPINCR.  It's a bit complicated with the
C        digital systems, so use a subroutine.
C
         CALL SDOPINCR
C
      END IF
C
C     Set the system dependent defaults for PRESTART and MINPAUSE.
C
      CALL RECCTL
C
C     Default the grab stuff.  Put after setups defaulted so we
C     have the bit rate.  The GRABGAP assumes that the required
C     bit rate is the total over all channels.
C
      DO ISCN = SCAN1, SCANL
         IF( GRABTO(ISCN) .NE. 'NONE' .AND.
     1       DATAPATH(ISCN) .EQ. 'IN2DISK' ) THEN
            IF( GRABTIME(1,ISCN) .LT. 0.0 ) GRABTIME(1,ISCN) = 30
            IF( GRABTIME(2,ISCN) .LT. 0.0 ) GRABTIME(2,ISCN) = 10
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
C     Clearly lots of stuff still needs to be moved here to cleanly
C     separate the inputs from the default setting.  Someday......
C
C
      RETURN
      END

