      SUBROUTINE PLSAT( ISRC )
C
C     Routine called by plrdpl and plcalb that plots a line 
C     segment on the RD plot for each scan for any source 
C     that is moving.  The calling routine takes care of 
C     determining if the source is moving.
C
C
      INCLUDE       'sched.inc'
C
      INTEGER           ISCN, ISRC, IER, ISAT, KSRC
      REAL              XSCN(2), YSCN(2)
      DOUBLE PRECISION  PRA, PDEC, PPMTIME
      DOUBLE PRECISION  DDRA, DDDEC, DIST
      DOUBLE PRECISION  RASCL, GEOVEL
C ----------------------------------------------------------------
C     IF( DEBUG ) CALL WLOG( 0, 'PLSAT starting' )
C
C     Loop over scans looking for the current source.
C
      DO ISCN = SCAN1, SCANL
         IF( SCNSRC(ISCN) .EQ. SRCNAME(ISRC) ) THEN
C
C           Get the position for this scan.  This will be for
C           the first station.  Hopefully that won't be too
C           inappropriate.
C
            KSRC = SRCATN(ISRC)
            IF( PLANET(KSRC) ) THEN
               PPMTIME = STARTJ(ISCN)
               CALL JPLEP2( EPHFILE, SCNSRC(ISCN), PPMTIME, 
     1            (-1.D0)*LONG(1), LAT(1), ELEV(1), 'VLBA',
     2                   PRA, PDEC, DDRA, DDDEC, DIST, IER )
               IF( IER .NE. 0 ) THEN
                  IF( IER .EQ. 2 ) THEN
                     WRITE( MSGTXT, '( A, I5, 3F15.7 )' )
     1                'PLSAT:  Possible time problem: ', 
     2                ISCN, STARTJ(1), STARTJ(ISCN), PPMTIME
                     CALL WLOG( 1, MSGTXT )
                  END IF
                  WRITE( MSGTXT, '( A, A, A, I4 )' )
     1               'PLSAT: Problem with ephemeris for ',
     2                SCNSRC(ISCN), '  Error:', IER
                  CALL ERRLOG( MSGTXT )
               END IF
            ELSE IF( SATEL(KSRC) ) THEN
               ISAT = SATN(KSRC)
               CALL SATEP( ISAT, ISCN, 0, 'VLBA', PRA, PDEC, 
     1                     DDRA, DDDEC, PPMTIME, DIST, GEOVEL )
            ELSE
               PRA     = RA2000(KSRC)
               PDEC    = D2000(KSRC)
               PPMTIME = PMTIME(KSRC)
               DDRA    = DRA(KSRC)
               DDDEC   = DDEC(KSRC)
            END IF
C
C           Get coordinates adjusted for motion.  Protect against 
C           divide by zero.
C
            RASCL = COS( PDEC )
            RASCL = MAX( RASCL, 1.D-10 )
            XSCN(1) = PRA * 3600.D0 / RADHR + 
     1              ( STARTJ(ISCN) - PPMTIME ) * DDRA / RASCL
            YSCN(1) = PDEC * 3600.D0 / RADDEG + 
     1              ( STARTJ(ISCN) - PPMTIME ) * DDDEC
            XSCN(2) = PRA * 3600.D0 / RADHR + 
     1              ( STOPJ(ISCN) - PPMTIME ) * DDRA / RASCL
            YSCN(2) = PDEC * 3600.D0 / RADDEG +
     1              ( STOPJ(ISCN) - PPMTIME ) * DDDEC
            CALL PGLINE( 2, XSCN, YSCN )
         END IF
      END DO
      RETURN
      END
