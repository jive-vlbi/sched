      SUBROUTINE JPLGOT( GOTALL )
C
C     Routine for SCHED, called by SRREAD, that checks if a source
C     that has not yet been found is a planet in the ephemeris.
C
      INCLUDE   'sched.inc'
C
      LOGICAL           GOTALL
      INTEGER           KSRC, NAM, IV, IER
      DOUBLE PRECISION  PRA, PDEC, PDRA, PDDEC, DIST
C ----------------------------------------------------------------------
      IF( .NOT. GOTALL .AND. EPHFILE .NE. 'NONE' ) THEN
C
         DO KSRC = 1, NSRC
            IF( SRCATN(KSRC) .EQ. 0 ) THEN
C
C              Have a source that is not yet identified.  Call the
C              ephemeris and see if it exists there.  Make the call
C              for station 1 at the start of the experiment.  These
C              will be the coordinates in the "catalog", but the
C              actual coordinates used for each station/scan will
C              be calculated at the time the information is written.
C              This "catalog" information will, however, be used
C              in any optimizations.
C
C              The times haven't been settled yet!  
C
               CALL JPLEP2( EPHFILE, SRCNAME(KSRC), STARTJ(1), 
     1                 (-1.D0)*LONG(1), LAT(1), ELEV(1), 'VLBA', 
     2                  PRA, PDEC, PDRA, PDDEC, DIST, IER )
C
               IF( IER .EQ. 0 ) THEN
C
C                 Add the planet to the source catalog.
C                 Don't know the errors - can they be guessed?
C
                  MSRC = MSRC + 1
C
C                 Don't do SUSED here - it can get it wrong for 
C                 optimized schedules and cause havoc elsewhere.
C                  SUSED(MSRC) = .TRUE.
C
                  SRCATN(KSRC) = MSRC
                  SRLSTN(MSRC) = KSRC
                  PLANET(MSRC) = .TRUE.
                  SOURCE(1,MSRC) = SRCNAME(KSRC)
C
C                 These may not be needed here, but should be harmless.
                  CSUSED(1,MSRC) = '*'
                  DO NAM = 2, MALIAS
                     SOURCE(NAM,MSRC) = ' '
                     CSUSED(NAM,MSRC) = ' '
                  END DO
C
                  C2000(MSRC) = '*'
                  C1950(MSRC) = ' '
                  CDATE(MSRC) = ' '
                  RA2000(MSRC) = PRA
                  D2000(MSRC) = PDEC
                  CALCODE(MSRC) = 'P'
                  REMARK(MSRC) = ' '
                  RAERR(MSRC) = 0.0
                  DECERR(MSRC) = 0.0
                  DO IV = 1, MAXLCH
                     VLSR(IV,MSRC) = 0.0
                  END DO
                  VELREF(MSRC)='G'
                  VELDEF(MSRC)='R'
                  DRA(MSRC) = PDRA
                  DDEC(MSRC) = PDDEC
                  PMTIME(MSRC) = STARTJ(1)
                  WHICHCAT(MSRC) = 'P'
C
C                 Check later if this is for an allowed station.
C                 We don't have enough information yet.
C
               END IF
            END IF
         END DO
C
C        Finally check if we now have all schedule sources.
C
         GOTALL = .TRUE.
         DO KSRC = 1, NSRC
            IF( SRCATN(KSRC) .EQ. 0 ) GOTALL = .FALSE.
         END DO
C
      END IF
C
      RETURN
      END
