      SUBROUTINE SRCLOC( ISCN, ISTA, 
     1                   PRA, PDEC, PPMTIME, PDRA, PDDEC )
C
C     Routine for SCHED, called by VLBAST and SCHGEO, to get the
C     source position.  For planets and satellites, it takes into
C     account the changes with time and antenna position.
C
C     ADD PROPER MOTIONS.  But think if that is really right.
C          This is used both for geometry calculations and for
C          output positions.  The answer might be different
C          for the two.
C
      INCLUDE  'sched.inc'
C 
      INTEGER           ISCN, ISTA
      REAL              PDRA, PDDEC
      DOUBLE PRECISION  PRA, PDEC, PPMTIME
C
      INTEGER           ISRC, KSTA, ISAT, IER
      DOUBLE PRECISION  DIST, GEOVEL, DDRA, DDDEC
C ---------------------------------------------------------------------
C
C     Source information:
C
C     Get the position information to use.  For planets, this can
C     be both station and time dependent.  Double precision needed
C     in call for some items that should be single precision later.
C     Also do satellites here as they are essentially the same.
C
      ISRC   = SRCNUM(ISCN)
      KSTA   = STANUM(ISTA)
C
      IF( PLANET(ISRC) .OR. SATEL(ISRC) ) THEN
         IF( PLANET(ISRC) ) THEN
            PPMTIME = STARTJ(ISCN)
            CALL JPLEP2( EPHFILE, SCNSRC(ISCN), PPMTIME, 
     1         (-1.D0)*LONG(KSTA), LAT(KSTA), ELEV(KSTA), 'VLBA',
     2                PRA, PDEC, DDRA, DDDEC, DIST, IER )
            IF( IER .NE. 0 ) THEN
               IF( IER .EQ. 2 ) THEN
                  WRITE( MSGTXT, '( A, I5, 3F15.7 )' )
     1             'VLBAST:  Possible time problem: ', 
     2             ISCN, STARTJ(1), STARTJ(ISCN), PPMTIME
                  CALL WLOG( 1, MSGTXT )
               END IF
               WRITE( MSGTXT, '( A, A, A, I4 )' )
     1            'VLBAST: Problem with ephemeris for ',
     2             SCNSRC(ISCN), '  Error:', IER
               CALL ERRLOG( MSGTXT )
            END IF
         ELSE IF( SATEL(ISRC) ) THEN
            ISAT = SATN(ISRC)
            IF( SATFILE(ISAT) .NE. 'NONE' ) THEN
               CALL SATEP( ISAT, ISCN, ISTA, 'VLBA', PRA, PDEC, 
     1                  DDRA, DDDEC, PPMTIME, DIST, GEOVEL )
            ELSE
               CALL SATTLE( ISAT, ISCN, ISTA, 'VLBA', PRA, PDEC, 
     1                  DDRA, DDDEC, PPMTIME, DIST, GEOVEL )
            END IF
         END IF
C
         PDRA    = DDRA
         PDDEC   = DDDEC
      ELSE
         PRA     = RA2000(ISRC)
         PDEC    = D2000(ISRC)
         PPMTIME = PMTIME(ISRC)
         PDRA    = DRA(ISRC)
         PDDEC   = DDEC(ISRC)
      END IF
C
      RETURN
      END
