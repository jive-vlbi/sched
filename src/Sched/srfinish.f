      SUBROUTINE SRFINISH
C
C     Subroutine for SCHED, called at the start of DEFAULTS, that 
C     wraps up the loose ends for source information.  DEFAULTS
C     is called in SCHED right after SCHIN, which reads the source
C     catalogs as one of its last acts.  SRFINISH checks that all 
C     sources are available in the catalog and adds planets, 
C     satellites etc to the main source catalog if needed.
C
C     The scan times in Julian days should be available before the
C     call as they are needed in the ephemeris routines.
C
C     Note that the list of schedule sources (SRCNAME, SRCATN) gets
C     built by SRCFLG here, but gets rebuilt later in SCHOPT after
C     optimization modes, automatic pointing insertions etc have
C     happened, since those processes can add or subtract sources.
C
      INCLUDE     'sched.inc'
      INCLUDE     'schpeak.inc'
C
      INTEGER     KSRC, ISCN, IPSRC, IGRP, ISRC, INAME, JCENT, IGEO
      INTEGER     ICSRC
      LOGICAL     GOTALL, VEXWARN
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SRFINISH starting' )
C
C     Associate sources in SRCNAME with main catalog entries.
C     Note this gets done again later in SCHOPT after any possible
C     schedule adjustments.
C     
      CALL SRCFLG( GOTALL )
C     
C     If don't have all, check for planets for which we have
C     ephemeris data.  If one of the requested sources is a 
C     planet, add that planet to the main source catalog and
C     set the pointers to and from the schedule sources list.
C     
      VEXWARN = .NOT. GOTALL
      CALL JPLGOT( GOTALL )
C      
C     Warn if there are planets with VEX output.  Block later if
C     they are in recording scans.
C     
      IF( DOVEX .AND. GOTALL .AND. VEXWARN ) THEN
         CALL WLOG( 1, 'SRFINISH: VEX requested for a schedule '//
     1       'with planets. ' )
         CALL WLOG( 1, 'SRFINISH: That is only allowed if all ' // 
     1       'planet scans are non-recording (eg pointing) scans.' )
      END IF
C     
C     If still don't have all, check for satellite for which we
C     have orbital elements.  If one is found, add it to the main
C     source catalog and set the pointers to and from the schedule
C     source list.
C     
      VEXWARN = .NOT. GOTALL
      CALL SATGOT( GOTALL )
C      
C     Warn about satellites with VEX output.  Block later if they are
C     recording scans (in VEXOUT).
C     
      IF( DOVEX .AND. GOTALL .AND. VEXWARN ) THEN
         CALL WLOG( 1, 'SRFINISH: VEX requested for a schedule '//
     1       'with satellites. ' )
         CALL WLOG( 1, 'SRFINISH: That is only allowed if all ' // 
     1       'satellite scans are non-recording (eg pointing) scans.' )
      END IF
C     
C     Complain about missing sources if still don't have all.
C     
      IF( .NOT. GOTALL ) THEN
         DO KSRC = 1, NSRC
            IF( SRCATN(KSRC) .EQ. 0 ) THEN
               CALL WLOG( 1, 'SRFINISH: ' // SRCNAME(KSRC) //
     1           ' not in catalogs, ephemeris or satellite file.' )
               IF( EPHFILE .EQ. 'NONE' ) CALL WLOG( 1, 
     1           '                       No planetary ephemeris'//
     2           ' specified.' )
               IF( NSAT .EQ. 0 ) CALL WLOG( 1, '         '//
     1        '              No satellite info specified.' )
               CALL WLOG( 1, '      Note some source names have '//
     1               'changed.  Look for close matches.' )
            END IF
         END DO
         CALL ERRLOG( 'SRFINISH: Missing sources.' )
      END IF
C     
C     Get a short name for each source.  Also check for invalid
C     names.
C     
      CALL SHORTN
C     
C     Loop through scans and get the main source catalog numbers for
C     all specified sources.  Note that we are not picking up
C     the automatic reference pointing sources here since those
C     scans have not been generated yet.
C
C     Note that, while SRCNAME and SRCATN are used in this process
C     the pointer is not saved so it doesn't matter if that list
C     changes later.  The pointers into the main catalog will still
C     be valid.
C
C     Initialize phase center sources pointers.  SRCNUM, IDOPSRC, and
C     IVLAPHS are initialized in SCHIN.
C
      DO JCENT = 1, NCENT
         DO ICSRC = 1, NCSRC(JCENT)
            CTRSRCI(ICSRC,JCENT) = 0
         END DO
      END DO
      DO ISCN = 1, NSCANS
         DO KSRC = 1, NSRC
            IF( SCNSRC(ISCN) .EQ. SRCNAME(KSRC) ) THEN
               SRCNUM(ISCN) = SRCATN(KSRC)
            END IF
            IF( DOPSRC(ISCN) .EQ. SRCNAME(KSRC) ) THEN
               IDOPSRC(ISCN) = SRCATN(KSRC)
            END IF
            IF( VLAPHS(ISCN) .EQ. SRCNAME(KSRC) ) THEN
               IVLAPHS(ISCN) = SRCATN(KSRC)
            END IF
            IF( ICENT(ISCN) .NE. 0 ) THEN
               DO ISRC = 1, NCSRC(ICENT(ISCN))
                  IF( CTRSRCN(ISRC,ICENT(ISCN)) .EQ. SRCNAME(KSRC)) THEN
                     CTRSRCI(ISRC,ICENT(ISCN)) = SRCATN(KSRC)
                  END IF
               END DO
            END IF
         END DO
      END DO
C
C     Get the main catalog source numbers for the possible 
C     reference pointing sources.
C
      DO IGRP = 1, NPKGRP
         DO IPSRC = 1, NPKSRC(IGRP)
            DO ISRC = 1, MSRC
               DO INAME = 1, MALIAS
                  IF( PKSRC(IPSRC,IGRP) .EQ. SOURCE(INAME,ISRC) ) THEN
                     PKSRNUM(IPSRC,IGRP) = ISRC
                  END IF
               END DO
            END DO
         END DO
      END DO
C
C     geo segment sources.
C
      DO IGEO = 1, NGEO
         GEOSRCI(IGEO) = 0
      END DO
      IF( ANYGEO ) THEN
         DO IGEO = 1, NGEO
            DO ISRC = 1, MSRC
               DO INAME = 1, MALIAS
                  IF( GEOSRC(IGEO) .EQ. SOURCE(INAME,ISRC) ) THEN
                     GEOSRCI(IGEO) = ISRC
                  END IF
               END DO
            END DO
         END DO
      END IF
C
      RETURN
      END
