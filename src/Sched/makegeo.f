      SUBROUTINE MAKEGEO( LASTISCN, JSCN, ISCN, SEGSRCS, NSEG )
C
C     Routine for SCHED called by ADDGEO that makes a list
C     of sources from the list of geo sources to fit into a
C     geodetic segment at the time of a scan for which such
C     a request was made.
C
C     The basic plan:
C        Loop through trial lists of sources.
C           Choose by random number generator.
C               Don't use previously used source
C               Keep track of sources that are not up and don't use.
C           Calculate a quality measure.
C               Like RMS spanned Sec(Z) of antenna with worst value.
C               Make it  SUM( (SEC(Z) - MEAN(SEC(Z)) )**2 
C                  Don't divide by number of scans so we can favor
C                  groups with more scans.
C        Keep a record of the best sequence (in SEGSRCS).
C            Replace SEGSRCS if a better sequence is found.
C
C        For each set, just stuff the scans into the scans starting
C        with ISCN.  JSCN is the template scan - less than SCAN1.
C
C     Some day, inhibit getting too close to the Sun.
C
      INCLUDE   'sched.inc'
C
      INTEGER           JSCN, ISCN, SEGSRCS(*), LASTISCN(*), NSEG
      INTEGER           MSEG10
      PARAMETER         (MSEG10=10*MSEG)
C
      INTEGER           LASTLSCN(MAXSTA)
      INTEGER           ISEG, ITRIAL, LSCN, I, IDUM, ISTA, NSTSCN
      INTEGER           NTSEG, IGEO, TSRC(MSEG), NGOOD, NREJECT
      INTEGER           YR, DY, CHANCE(MSEG10), NCHANCE, MINKEEP
      REAL              BESTQUAL, TESTQUAL
      REAL              RAN5, DUMMY
      LOGICAL           OKGEO(MGEO), OKSTA(MAXSTA), NOREP, MKGDEBUG
      LOGICAL           RETAIN
      DOUBLE PRECISION  TGEO1, TGEOEND, TAPPROX, STARTB, OKGTIM(MGEO)
      DOUBLE PRECISION  TIMRAD
      CHARACTER         WHY*60, TFORM*8, CTIME*8
C
      DATA              IDUM   / -12345 /
      SAVE              IDUM
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'MAKEGEO starting.' )
      MKGDEBUG = .FALSE.
C
      BESTQUAL = 0
      DO ISEG = 1, MSEG
         SEGSRCS(ISEG) = 0
      END DO
      DO IGEO = 1, MGEO
         OKGEO(IGEO) = .TRUE.
         OKGTIM(IGEO) = 0.D0
      END DO
C
C     Prime the random number generator a bit.  I don't know
C     if this is needed, but without it, the first antenna choices
C     didn't seem too random.
C
      DO I = 1, 100
         DUMMY = RAN5(IDUM)
      END DO
C
C     Save some parameters related to the block of scans.
C
      TGEO1 = 0.D0
      DO ISTA = 1, NSTA
         IF( STASCN(JSCN,ISTA) ) THEN
            IF( LASTISCN(ISTA) .NE. 0 ) THEN
               TGEO1 = MAX( TGEO1, STOPJ(LASTISCN(ISTA)))
            ELSE
               TGEO1 = MAX( TGEO1, TFIRST )
            END IF
         END IF
      END DO
      STARTB = STARTJ(ISCN)
      TGEOEND = TGEO1 + GEOLEN(ISCN)
C
      IF( MKGDEBUG ) THEN
         WRITE(*,*) 'MAKEGEO START TIMES', STARTB, STARTJ(ISCN), TGEOEND
      END IF
C
C     Warn about inappropriate OPMIAN, then set one.
C
      IF( OPMIAN(JSCN) .LT. 2 ) THEN
         MSGTXT = ' '
         WRITE( MSGTXT, '( A, I4, A )' ) 
     1     'MAKEGEO:  OPMINANT of ', OPMIAN(JSCN), 
     2     ' too small for automatic insertion of geodetic sections.'
         CALL WLOG( 1, MSGTXT ) 
         MSGTXT = ' '
         OPMIAN(JSCN) = NSTA / 2
         WRITE( MSGTXT, '( A, I4 )' ) 
     1     '          Resetting to OPMINANT = ', OPMIAN(JSCN)
         CALL WLOG( 1, MSGTXT ) 
         MSGTXT = ' '
      END IF
C
C     Make a pass through all sources looking at the midpoint of
C     the block to see if enough stations are up.  Set OKGEO 
C     accordingly.  Doing this during the loops below can cause sources
C     that are bad only at an extreme part of the block to be kept
C     out of the final result.  Given that we are trying for low 
C     elevation scans, even this scheme won't be perfect, but it is
C     faster than always allowing all sources.
C     Use a dummy ISCN that should be an empty slot for this test.
C     Initialize the LASTLSCN values so that TAPPROX is used.
C
C     Allow elevations to 7 degrees below the specified minimum elevation
C     so sources can be rising or setting during the segment.
C
C     While at it, use the CHANCE array to give more promising sources
C     higher probability of selection - like a weighted lottery.  CHANCE
C     will include a pointer to the input geo sources list.  Every source
C     that is OK will get one CHANCE.  Ones then there will be an 
C     additional chance for each station below 20 degrees.  Note that 
C     the OKGEO scheme could be eliminated because of this, but don't
C     do that yet.
C
      DO ISTA = 1, NSTA
         LASTLSCN(ISTA) = 0
      END DO
      NREJECT = 0
      NCHANCE = 0
      TAPPROX = ( TGEOEND + STARTB ) / 2.D0
      MSGTXT = ' '
      CALL TIMEJ( TAPPROX, YR, DY, TIMRAD )
      CTIME = TFORM( TIMRAD, 'T', 0, 2, 2, '::@' )
      WRITE( MSGTXT, '( A, I4, I4, 1X, A )' ) 
     1    'Building geodetic segment centered at ', YR, DY, CTIME
      CALL WLOG( 1, MSGTXT )
      CALL WLOG( 1, 'Elevations at center for sources considered are: ')
      IF( NSTA .GT. 20 ) CALL WLOG( 1, '(Only first 20 stations)' )
      MSGTXT = ' '
      WRITE( MSGTXT, '( 12X, 20A5 )' ) 
     1        (STCODE(STANUM(ISTA)),ISTA=1,NSTA)
      CALL WLOG( 1, MSGTXT )
      MSGTXT = ' '
      DO IGEO = 1, NGEO
         LSCN = ISCN + 1
         CALL MAKESCN( LASTLSCN, LSCN, JSCN, GEOSRCI(IGEO),
     1        GEOSRC(IGEO), TAPPROX, OPMINEL(JSCN) - 7.0,
     2        NGOOD, OKSTA )
C
C           Make sure enough antennas are good.  Record the time
C           when the source was marked as bad.  
C
         OKGEO(IGEO) = NGOOD .GE. OPMIAN(JSCN)
         IF( .NOT. OKGEO(IGEO) ) THEN
            NREJECT = NREJECT + 1
         ELSE
            NCHANCE = NCHANCE + 1
            IF( NCHANCE .GT. MSEG10 ) CALL ERRLOG( 
     1          'MAKEGEO: Too many chances -- programming error' )
            CHANCE(NCHANCE) = IGEO
            DO ISTA = 1, NSTA
               IF( EL1(LSCN,ISTA) .LT. 20.0 .AND. STASCN(LSCN,ISTA) ) 
     1               THEN
                  NCHANCE = NCHANCE + 1
                  IF( NCHANCE .GT. MSEG10 ) CALL ERRLOG( 
     1                'MAKEGEO: Too many chances - programming error' )
                  CHANCE(NCHANCE) = IGEO
               END IF
            END DO
         END IF
C         IF( MKGDEBUG ) THEN
            IF( OKGEO(IGEO) ) THEN
               WRITE(*,'( 2I5, 2X, A, 20F5.0)' ) IGEO, NCHANCE,
     1             GEOSRC(IGEO), (EL1(LSCN,I),I=1,NSTA)
            END IF
C         END IF
      END DO
C
C     Prevent an opportunity for an infinite loop.  There is still
C     a small chance if there is a source up at the middle of the
C     segment, but there is nothing at the beginning.
C
      IF( NREJECT .EQ. NGEO ) THEN
         CALL ERRLOG( 'MAKEGEO:  None of the sources specified '//
     1        'for a geodetic segment are up at OPMINANT antennas. ' )
      END IF
      IF( MKGDEBUG ) THEN
         WRITE(*,*) 'MAKEGEO OKGEO', NGEO, (OKGEO(I),I=1,NGEO)
         WRITE(*,*) 'MAKEGEO GOING ', TGEO1, TGEOEND, ISCN
      END IF
C
C     Loop over teh trials.  For now, the number is hardwired.  That
C     will likely be pulled out as a user parameter some day.
C
      DO ITRIAL = 1, 4000
         IF( MKGDEBUG ) THEN
            WRITE(*,*) '++++++++++++ MAKEGEO TRIAL ', ITRIAL
         END IF
C
C        Make a source list.  Use a GO TO loop for logical simplicity.
C        Could jump through hoops and use a DO loop, but it's a bit more
C        awkward.
C
C        Move LASTISCN to LASTLSCAN where 
C        it can keep getting remade.         
C
         ISEG = 0
         NOREP = .TRUE.
         DO ISTA = 1, NSTA
            LASTLSCN(ISTA) = LASTISCN(ISTA)
         END DO
C
C        Set start time of the first new scan.  This is mainly
C        for the case when the segment is at the start of the
C        experiment and all LASTISCN entries are zero.
C
         STARTJ(ISCN) = STARTB
C
         NREJECT = 0
  100    CONTINUE
            IF( MKGDEBUG ) THEN
               WRITE(*,*) 'MAKEGEO NEW SOURCE TRY ', ISEG, TSRC(ISEG),
     1              NREJECT, ' ', WHY
            END IF
C
C           Deal with a situation where all sources are being
C           rejected.  This can get the program in an infinite
C           loop.
C
            IF( NREJECT .GT. NGEO * 3 ) THEN
               CALL WLOG( 1, 'MAKEGEO: Rejecting all, or nearly all' //
     1             ' sources while constructing a geodetic segement' )
               CALL WLOG( 1, '         Will stop preventing repeats.' )
               NOREP = .FALSE.
               NREJECT = 0
            END IF
C
C           Work on adding the next scan.
C           Initializations.  
C
            ISEG = ISEG + 1
            LSCN = ISEG + ISCN - 1
C
C           Get the approximate start time.
C
            IF( ISEG .EQ. 1 ) THEN
               TAPPROX = STARTB
            ELSE
               TAPPROX = STOPJ(LSCN-1) + 60.D0 * ONESEC
            END IF
C
C           There are NGEO sources.  Pick one.
C
            TSRC(ISEG) = CHANCE( 1 + RAN5(IDUM) * NCHANCE )
            IF( MKGDEBUG ) THEN
C               write(*,*) '=== makegeo next source ', iseg, lscn, ngeo,
C     1           '  Try: ', tsrc(iseg), '  ', geosrc(tsrc(iseg)), idum,
C     2           opminel(jscn), jscn
            END IF
C
C           If known to be bad, get another.
C
            IF( .NOT. OKGEO(TSRC(ISEG)) ) THEN
               ISEG = ISEG - 1
               WHY = 'Source flagged as not up at enough stations.'
               NREJECT = NREJECT + 1
               GO TO 100
            END IF
C
C           Don't repeat a source.
C
            IF( ISEG .GT. 1 .AND. NOREP ) THEN
               DO I = 1, ISEG - 1
                  IF( TSRC(ISEG) .EQ. TSRC(I) ) THEN
                     ISEG = ISEG - 1
                     NREJECT = NREJECT + 1
                     WHY = 'Source repeated.'
                     GO TO 100
                  END IF
               END DO
            END IF
C
C           Try to insert the source.  Get the geometry.
C
            CALL MAKESCN( LASTLSCN, LSCN, JSCN, GEOSRCI(TSRC(ISEG)),
     1           GEOSRC(TSRC(ISEG)), TAPPROX, OPMINEL(JSCN),
     2           NGOOD, OKSTA )
C
C           Make sure enough antennas are good.  Record the time
C           when the source was marked as bad.  
C
            IF( NGOOD .LT. OPMIAN(JSCN) ) THEN
               ISEG = ISEG - 1
               NREJECT = NREJECT + 1
               WHY = 'Source not up for this scan at enough stations.'
               GO TO 100
            END IF
C
C           Make sure the stop time is before the last allowed
C
            IF( STOPJ(LSCN) .GT. TGEOEND ) THEN
               ISEG = ISEG - 1
               GO TO 200
            END IF
C
C           Keep this one.  Set LASTLSCN as appropriate.
C
            IF( MKGDEBUG ) THEN
C               WRITE(*,*) 'MAKEGEO KEEP SOURCE ', TSRC(ISEG), ' SCAN ',
C     1              LSCN, STARTJ(LSCN), STOPJ(LSCN), TGEOEND, DUR(LSCN)
            END IF
            NTSEG = ISEG
            DO ISTA = 1, NSTA
               IF( STASCN(LSCN,ISTA) ) THEN
                  LASTLSCN(ISTA) = LSCN
               END IF
            END DO
C
C           Get another scan if there is likely to be time.  
C           Leave some room for slews.
C
            WHY = 'Finished last source insertion.'
            IF( STOPJ(LSCN) .LT. TGEOEND - DUR(LSCN) - 60.*ONESEC )
     1         GO TO 100
C
  200    CONTINUE
C
C        Have a sequence
C
C        Make sure each antenna is in at least half the scans.
C        Only test stations that are in JSCN
C
         RETAIN = .TRUE.
         MINKEEP = MSEG10
         DO ISTA = 1, NSTA
            IF( STASCN(JSCN,ISTA) ) THEN
               NSTSCN = 0
               DO I = ISCN, LSCN
                  IF( STASCN(I,ISTA) ) THEN
                     NSTSCN = NSTSCN + 1
                  END IF
               END DO
               MINKEEP = MIN( MINKEEP, NSTSCN )
            END IF
         END DO
         RETAIN = MINKEEP .GE. INT( ( LSCN - ISCN + 1.0 ) / 2.0 + 0.51 )
C         write(*,*) 'makegeo retain:', retain, minkeep, itrial, 
C     1        iscn, lscn, INT( ( LSCN - ISCN + 1.0 ) / 2.0 + 0.51 )
C
C        For kept sequences, test the quality.
C
         IF( RETAIN ) THEN
C
            IF( MKGDEBUG ) THEN
               WRITE(*,*) 'MAKEGEO ---------------------------------- '
               WRITE(*,*) 'MAKEGEO    FINISHED ONE SEQUENCE.', 
     1               ITRIAL, (TSRC(ISEG), ISEG=1,NTSEG)
            END IF
C
C           Get the quality measure of scans ISCN to LSCN.  Isolate this
C           to a subroutine in case someone wants to brew their own measure.
C        
            CALL GEOQUAL( ISCN, LSCN, JSCN, TESTQUAL, MKGDEBUG )
C        
C           See whether to save this one source set.
C        
            IF( MKGDEBUG ) THEN
               WRITE(*,*) 'MAKEGEO - QUALITY PREVIOUS BEST ', BESTQUAL,
     1                 '  CURRENT SET: ', TESTQUAL, ' SCANS: ', NTSEG
            END IF
            IF( TESTQUAL .GT. BESTQUAL ) THEN
C               IF( MKGDEBUG ) THEN
                  MSGTXT = ' '
                  WRITE( MSGTXT, '( A, I5, A, I3, A, F7.3, 20I3 )' )
     1                'MAKEGEO: New Best - Trial: ', ITRIAL, 
     2                ' Number of scans: ', NTSEG, '  Quality: ', 
     3                TESTQUAL, (TSRC(ISEG),ISEG=1,NTSEG)
                  CALL WLOG( 1, MSGTXT )
                  MSGTXT = ' '
C               END IF
               BESTQUAL = TESTQUAL
               DO ISEG = 1, NTSEG
                  SEGSRCS(ISEG) = TSRC(ISEG)
               END DO
               NSEG = NTSEG
            END IF
         END IF
      END DO
C
C     Write result
C
C      IF( MKGDEBUG ) THEN
         WRITE(*,*) 'MAKEGEO: SEGMENT SOURCES', 
     1         (SEGSRCS(ISEG), ISEG=1,NSEG)
C      END IF
C
      RETURN
      END
