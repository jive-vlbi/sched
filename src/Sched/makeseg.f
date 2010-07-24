      SUBROUTINE MAKESEG( MKGDEBUG, JSCN, ISCN, LASTISCN, 
     1                    OKGEO, USEGEO, SEGELEV, STARTB, TGEOEND, 
     2                    LSCN, NTSEG, TSRC, IDUM )
C
C     Routine for SCHED called by GEOMAKE that invents a 
C     series of sources to try as a geodetic sequence.
C
C     This particular version goes through the stations in
C     random order and gets sources that are low elevation,
C     also chosen randomly.
C
C     Inputs:
C        MKGDEBUG:  Do debug printouts.
C        JSCN:      Template scan.  Less than ISCN usually.
C        ISCN:      The first scan of the sequence
C        LASTISCN:  The preceeding scan for each station at the start.
C        OKGEO:     Sources in the geodetic set that have a useful
C                   elevation distribution.
C        SEGELEV    2D array (sta,source) of elevations
C        STARTB:    Start time of the sequence.
C        TGEOEND:   End time for a sequence.
C        IDUM:      Seed for the random number generator
C 
C     Outputs:
C        LSCN:      The last scan of the sequence
C        NTSEG:     Number of scans in the geodetic sequence.
C        TSRC:      The sequence of geodetic source numbers for the
C                   geodetic sequence.
C
C     The chosen output scans will be deposited in scans ISCN to LSCN.
C
      INCLUDE 'sched.inc'
C
      INTEGER            MSEG10
      PARAMETER          (MSEG10=10*MSEG)
      INTEGER            JSCN, ISCN, LASTISCN(*)
      INTEGER            TSRC(*), NTSEG
      LOGICAL            MKGDEBUG, OKGEO(*), USEGEO(*)
      REAL               SEGELEV(MAXSTA,MGEO), ELCUT, ELHIGH, ELA
      DOUBLE PRECISION   STARTB, TGEOEND
C
      INTEGER            ISTA, KSTA, JSTA, ISEG, IGEO
      INTEGER            LASTLSCN(MAXSTA)
      INTEGER            NREJECT, IDUM, LSCN, NSLOW
      INTEGER            NGOOD, NCHANCE, CHANCE(MSEG10)
      INTEGER            STASEQ(MAXSTA), GOTLOW(MAXSTA)
      INTEGER            NLATE, NHIGH, SELECTED(MGEO)
      LOGICAL            NOREP, OKSTA(MAXSTA)
      LOGICAL            KEEP, DOWN(MGEO)
      REAL               RAN5, ELLIM
      DOUBLE PRECISION   TAPPROX
      CHARACTER          WHY*60
C --------------------------------------------------------------------
C     Initializations
C
C     NOREP is a flag about whether to allow repeats.  That will only
C     be allowed if we have run out of source that haven't been
C     observed.
C
C     NREJECT is part of protecting against not enough sources
C     available which can put the program in a loop.  This was
C     tested for the middle of the segment in GEOMAKE, but is
C     tested here again with real times.
C
C     NLATE prevents the routine from quitting because of 
C     running out of time until several alternative scans have 
C     been tried.
C
C     NSLOW indicates how many low elevation scans for the chosen
C     station we are trying to get in this pass through the
C     selection loop.
C
C     ELCUT is the elevation cutoff for low elevation scans.
C            Same as LOWLIM in GEOCHK.   Pass some day?
C
C     ELHIGH is the elevation cutoff for high elevation scans.
C            Same as HIGHLIM2 in GEOCHK.  Pass some day?
C
C     DOWN is a flag for an intividual scan slot to help with
C     eliminating sources that were up enough for a positive
C     USEGEO, but are not up at the exact time of this scan.
C
      ISEG = 0
      NTSEG = 0
      NOREP = .TRUE.
      NREJECT = 0
      NLATE = 0
      NSLOW = 1
      ELCUT = 23.5
      ELHIGH = 55.0  
      DO IGEO = 1, NGEO
         SELECTED(IGEO) = 0
         DOWN(IGEO) = .FALSE.
      END DO
C
C     Move LASTISCN to LASTLSCAN where 
C     it can keep getting remade with each call to this routine.
C
      DO ISTA = 1, NSTA
         LASTLSCN(ISTA) = LASTISCN(ISTA)
      END DO
C
C     Set start time of the first new scan.  This is mainly
C     for the case when the segment is at the start of the
C     experiment and all LASTISCN entries are zero.
C
      STARTJ(ISCN) = STARTB
C
C     Make an array of station index numbers that is in 
C     random order.  Add one extra for a scan with mostly
C     high elevations.
C
      DO ISTA = 1, NSTA + 1
         STASEQ(ISTA) = ISTA
         GOTLOW(ISTA) = 0
      END DO
      CALL SCRAMBLE( STASEQ, NSTA + 1, IDUM )
C
C     Start selecting scans.  Jump to 100 when moving to the
C     next station to consider.
C
      JSTA = 0
  100 CONTINUE
C
C        Use the scrambled list to select the next station
C        to consider.
C
         JSTA = JSTA + 1
         IF( JSTA .GT. NSTA + 1 ) THEN
            JSTA = 1
            NSLOW = NSLOW + 1
         END IF
         ISTA = STASEQ(JSTA)
C
C        Jump to next station if we already have enough
C        low data for this one.  This will happen fairly 
C        often with elevations correlated between stations.
C
         IF( GOTLOW(ISTA) .GE. NSLOW ) THEN
C      write(*,*) 'makeseg have enough already  ', jsta, ista,
C     1         gotlow(ista), nslow
            GO TO 100
         END IF
C
C        Tranfer the elevation cutoff to a variable that we can
C        adjust.
C
         ELLIM = ELCUT
C
C        We might jump back here if the first source selected 
C        has some problem and we want to try again for this station.
C
  200    CONTINUE
C
C        Find the sources from which to pick one for the scan.  For
C        most stations, look for low elevations.  For NSTA+1, look
C        for many high elevation stations.
C
         NCHANCE = 0
         DO IGEO = 1, NGEO
            IF( USEGEO(IGEO) .AND. SELECTED(IGEO) .EQ. 0 .AND.
     1            .NOT. DOWN(IGEO) ) THEN
               KEEP = .FALSE.
               IF( ISTA .LE. NSTA .AND. 
     1              SEGELEV(ISTA,IGEO) .LE. ELLIM ) THEN
                  KEEP = .TRUE.
               ELSE IF( ISTA .EQ. NSTA + 1 ) THEN
                  NHIGH = 0
                  DO KSTA = 1, NSTA
                     IF( SEGELEV(KSTA,IGEO) .GT. ELHIGH ) 
     1                    NHIGH = NHIGH + 1
                  END DO
                  IF( NHIGH .GT. NSTA / 2 ) THEN
                     KEEP = .TRUE.
                  END IF
               END IF
C
C              Add the source to the ones to pick from.
C
               IF( KEEP ) THEN
                  NCHANCE = NCHANCE + 1
                  CHANCE(NCHANCE) = IGEO
               END IF
            END IF
         END DO
C
C        If that didn't pick up any sources, go back with a 
C        relaxed elevation limit.  If that doesn't work after a
C        few tries, skip on to the next station as there may be
C        no more of interest for this station.
C
         IF( NCHANCE .EQ. 0 ) THEN
            IF( ISTA .LE. NSTA .AND. ELLIM .LE. 35.0 ) THEN 
               ELLIM = ELLIM + 10.0
C      write(*,*) 'makeseg boosting ELLIM ', JSTA, ISTA, ELLIM
               GO TO 200
            ELSE IF( ISTA .EQ. NSTA + 1 .AND. ELLIM .GT. 35.0 ) THEN
               ELLIM = ELLIM - 10.0
C      write(*,*) 'makeseg dropping ELLIM ', JSTA, ISTA, ELLIM
               GO TO 200
            ELSE
C      write(*,*) 'makeseg nchance 0, abandon station', 
C     1              JSTA, ISTA, ELLIM
               GO TO 100
            END IF
         END IF
C
C        Add the next scan.
C        Initializations.  
C
         ISEG = ISEG + 1
         LSCN = ISEG + ISCN - 1
C
C        Get the approximate start time.
C
         IF( ISEG .EQ. 1 ) THEN
            TAPPROX = STARTB
         ELSE
            TAPPROX = STOPJ(LSCN-1) + 60.D0 * ONESEC
         END IF
C
C        There are NCHANCE sources being considered in this pass.
C        Pick one with the help of a random number generator.
C
         TSRC(ISEG) = CHANCE( 1 + RAN5(IDUM) * NCHANCE )
C
C        Some debug printout.
C
         IF( MKGDEBUG ) THEN
            WRITE(*,*) '=== MAKESEQ NEXT SOURCE ', ISEG, LSCN, NGEO,
     1        '  TRY: ', TSRC(ISEG), '  ', GEOSRC(TSRC(ISEG)), IDUM,
     2        OPMINEL(JSCN), JSCN
         END IF
C
C        Try to insert the source.  Get the geometry.
C
         CALL MAKESCN( LASTLSCN, LSCN, JSCN, GEOSRCI(TSRC(ISEG)),
     1        GEOSRC(TSRC(ISEG)), TAPPROX, OPMINEL(JSCN),
     2        NGOOD, OKSTA )
C         if( ista .le. nsta ) then
C            write(*,*) 'makeseq makescn ', lscn, tsrc(iseg), ista,
C     1        el1(lscn,ista), up1(lscn,ista), up2(lscn,ista),
C     2        oksta(ista)
C         else 
C            write(*,*) 'makeseq makescn ', lscn, tsrc(iseg), ista
C         end if
C
C        Test if the source really is up at the station that we 
C        are paying attention at the moment.  Test against the UP
C        indicators and against STASCN.  Due to different times,
C        sources up when USEGEO was set might be down at the actual
C        scan time.
C
         IF( ( .NOT. STASCN(LSCN,ISTA) .OR. 
     1         UP1(LSCN,ISTA) .NE. ' ' .OR. 
     2         UP2(LSCN,ISTA) .NE. ' ' )
     3            .AND. ISTA .LE. NSTA ) THEN
            DOWN(TSRC(ISEG)) = .TRUE.
            ISEG = ISEG - 1
C       write(*,*) 'makeseg source down ', jsta, ista, tsrc(iseg)
            GO TO 200
         END IF
C
C        Make sure the stop time is before the last allowed.  Reject
C        the scan if so.  Don't stop trying the first time because
C        this scan may have an especially long slew.  But after a 
C        few rejections, give up and assume the geodetic set is 
C        complete.  The maximum trials allowed is drawn from a hat.
C
         IF( STOPJ(LSCN) .GT. TGEOEND ) THEN
            ISEG = ISEG - 1
            NLATE = NLATE + 1
            IF( NLATE .LE. 12 ) THEN
C        write(*,*) 'makeseg late stop, try again ', jsta, ista
               GO TO 100
            ELSE
C        write(*,*) 'makeseg late stop, quit ', jsta, ista
               GO TO 500
            END IF
         END IF
C
C        Keep this source/scan.  Set LASTLSCN as appropriate.
C
         IF( MKGDEBUG ) THEN
            WRITE(*,*) 'MAKESEG: KEEP SOURCE ', TSRC(ISEG), ' SCAN ',
     1         LSCN, STARTJ(LSCN), STOPJ(LSCN), TGEOEND, DUR(LSCN)
         END IF
         NTSEG = ISEG
         SELECTED(TSRC(ISEG)) = LSCN
         DO KSTA = 1, NSTA
            IF( STASCN(LSCN,KSTA) ) THEN
               LASTLSCN(KSTA) = LSCN
               ELA = ( EL1(LSCN,KSTA) + EL2(LSCN,KSTA) ) / 2.0
               IF( ELA .LT. ELCUT ) GOTLOW(KSTA) = GOTLOW(KSTA) + 1
            END IF
         END DO
C
C        Go back to get another scan if there is likely to be time.  
C        Leave some room for slews.  Reset DOWN for the new scan.
C
         WHY = 'Finished last source insertion.'
         IF( STOPJ(LSCN) .LT. TGEOEND - DUR(LSCN) - 60.*ONESEC ) THEN
C      if( ista .ne. nsta + 1 ) then
C         write(*,*) 'makeseg source added to list ', jsta, ista,
C     1       tsrc(iseg), lscn, ' ', up1(lscn,ista), ' ', up2(lscn,ista)
C      else
C         write(*,*) 'makeseg source added to list ', jsta, ista,
C     1       tsrc(iseg), lscn, ' -- -- '
C      end if
            DO IGEO = 1, NGEO
               DOWN(IGEO) = .FALSE.
            END DO
            GO TO 100
         END IF
C      write(*,*) 'makeseg source added, end    ', jsta, ista,
C     1       tsrc(iseg)
C
C        Above is the end of the GOTO loop.  Falling out of it when
C        there is inadequate time left for another scan.
C
C     Get or jump here when a sequence is completed.
C
  500 CONTINUE
C
      RETURN
      END
