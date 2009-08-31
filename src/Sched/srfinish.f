      SUBROUTINE SRFINISH
C
C     Subroutine for SCHED that wraps up the loose ends for source
C     information.  It checks that all sources are available in
C     the catalog and adds planets, satellites etc if needed.
C
C     The scan times in Julian days should be available before the
C     call as they are needed in the ephemeris routines.
C
      INCLUDE     'sched.inc'
      INCLUDE     'schpeak.inc'
C
      INTEGER     KSRC, ISCN, IPSRC, IGRP, ISRC, INAME
      LOGICAL     GOTALL, VEXWARN
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SRFINISH starting' )
C
C     Associate sources in SRCNAME with catalog entries.
C     
      CALL SRCFLG( GOTALL )
C     
C     If don't have all, check for planets for which we have
C     ephemeris data.
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
C     have orbital elements.
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
C     Loop through scans and get the source catalog numbers for
C     all specified sources.  Note that we are not picking up
C     the automatic reference pointing sources here since those
C     scans have not been generated yet.  
C     
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
         END DO
      END DO
C
C     Get the source numbers for the possible reference pointing
C     sources.
C
      DO IGRP = 1, NPKGRP
         DO IPSRC = 1, NPKSRC(IGRP)
            DO ISRC = 1, MSRC
               DO INAME = 1, 5
                  IF( PKSRC(IPSRC,IGRP) .EQ. SOURCE(INAME,ISRC) ) THEN
                     PKSRNUM(IPSRC,IGRP) = ISRC
                  END IF
               END DO
            END DO
         END DO
      END DO
C
      RETURN
      END
