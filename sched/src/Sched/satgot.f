      SUBROUTINE SATGOT( GOTALL )
C
C     Routine for SCHED that checks if the source is a satellite
C     for which a spice flie is available.  It is closely
C     modeled on JPLGOT.
C
      INCLUDE       'sched.inc'
C
      LOGICAL           GOTALL
      INTEGER           KSRC, ISAT, NAM, IV, ISSAT, ICH
      DOUBLE PRECISION  SRA, SDEC, SDRA, SDDEC, T0, DIST, GEOVEL
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SATGOT: starting' )
C
      IF( .NOT. GOTALL .AND. NSAT .NE. 0 ) THEN
C
C        Look through any sources not yet found to try to see if it is
C        a satellite that matches one of the SATNAMEs.
C
         DO KSRC = 1, NSRC
            IF( SRCATN(KSRC) .EQ. 0 ) THEN
C
C              Check against the SATNAMEs.
C
               ISSAT = 0
               IF( NSAT .GE. 1 ) THEN
                  DO ISAT = 1, NSAT
                     IF( SRCNAME(KSRC) .EQ. SATNAME(ISAT) ) THEN
                        ISSAT = ISAT
                     END IF
                  END DO
               END IF
C
C              Call the satellite position routine for the first
C              scan for the first source to get a position for
C              the source catalog.  Note that this routine will
C              be called for each scan/station when actually making
C              schedules.  Use presence of SATFILE to determine which
C              routine to call.  SATINI made sure one, and not both
C              of SATFILE and TLEFILE are not 'NONE'.
C
               IF( ISSAT .NE. 0 ) THEN
                  IF( SATFILE(ISSAT)(1:4) .NE. 'NONE' ) THEN
                     CALL SATEP( ISSAT, 1, 0, 'VLBA', 
     1                     SRA, SDEC, SDRA, SDDEC, T0, DIST, GEOVEL )
                  ELSE
                     CALL SATTLE( ISSAT, 1, 0, 'VLBA', 
     1                     SRA, SDEC, SDRA, SDDEC, T0, DIST, GEOVEL )
                  END IF
C
C                 Add the satellite to the source catalog.
C
                  MSRC = MSRC + 1
                  SATEL(MSRC) = .TRUE.
                  SATN(MSRC) = ISSAT
C
C                 Don't do SUSED here - it can get it wrong for 
C                 optimized schedules and cause havoc elsewhere.
C                  SUSED(MSRC) = .TRUE.
C
                  SRCATN(KSRC) = MSRC
                  SRLSTN(MSRC) = KSRC
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
                  RA2000(MSRC) = SRA
                  D2000(MSRC) = SDEC
                  CALCODE(MSRC) = 'S'
                  REMARK(MSRC) = ' '
                  RAERR(MSRC) = 0.0
                  DECERR(MSRC) = 0.0
                  DO IV = 1, MAXLCH
                     VLSR(IV,MSRC) = 0.0
                  END DO
                  DRA(MSRC) = SDRA
                  DDEC(MSRC) = SDDEC
                  PMTIME(MSRC) = STARTJ(1)
                  WHICHCAT(MSRC) = 'S'
                  VELREF(MSRC) = 'G'
                  VELDEF(MSRC) = 'R'
                  DO ICH = 1, MAXLCH
                     VLSR(ICH,MSRC) = GEOVEL
                  END DO
C
C                 Give this source a special calcode to mark it
C                 as a satellite.  This helps run the correlator.
C
                  CALCODE(MSRC) = 'Z'
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
