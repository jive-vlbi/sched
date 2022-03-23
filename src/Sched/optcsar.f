      SUBROUTINE OPTCSAR( LASTISCN, KSCN, ISCN, SCNSTA, NS1, NS2, NS3, 
     1                    SCN1, SCN2, SCN3, TIME1, TIME2, TIME3 )
C
C     Routine for SCHED, called by OPTCSUB, that trys to determine
C     the optimimal subarrays for good sky coverage over each   
C     antenna.  It examines a grid in AZ and EL.  This routine is
C     related to OPTCELLS except in the use of subarrays.  There will 
C     be two primary subarrays which are optimized, each with
C     at least OPMIAN antennas.  If the best such combination
C     leaves 2 or more antennas free (sources not up), those antennas
C     will go to some other source as a 3ed subarray.
C
C     During operation, the geometry parameters for the input
C     scans will be used to hold geometry for the current test time.
C
C     Note that this routine and the OPTCELLS stuff should perhaps
C     be merged some day.
C    
C     This routine doesn't take into account GAP.  OPTTIM is run 
C     later and does take that into account.
C
      INCLUDE 'sched.inc'
C
      INTEGER     MAXISC
      PARAMETER   (MAXISC=1000)       ! Number of input scans.
C
      INTEGER           LASTISCN(MAXSTA), KSCN, ISCN, JSCN, ISTA
      INTEGER           ISCN1, ISCN2
      INTEGER           SCN1, SCN2, SCN3, NS1, NS2, NS3
      INTEGER           IE, IA, I
      INTEGER           SCNSTA(MAXSTA), NS, LSRC(MAXSTA)
      DOUBLE PRECISION  TIMER
      INTEGER           YEAR, DAY2, MINMIAN
      CHARACTER         TFORM*10, TIMSTR1*10, TIMSTR2*10
      REAL              ARPTS, POINTS(MAXISC,MAXSTA)
      REAL              PTS3, MPTS3
      REAL              PT_ADD, WT_MUL, SL_ADJ
      DOUBLE PRECISION  TAPPROX, TIME1, TIME2, TIME3, TIMEC, DURC
      DOUBLE PRECISION  LASTTIME, LASTSTOP
      SAVE              LASTTIME, LASTSTOP, LSRC
C
C     Cell stuff.
C
      INTEGER           ELI(MAXISC,MAXSTA), AZI(MAXISC,MAXSTA)
      INTEGER           IEL, IAZ
      DOUBLE PRECISION  CELLTIME(3,3,MAXSTA)
      REAL              ELCELL(2),  AZCELL(2), AZTEST, AZCOFF
      SAVE              AZCOFF, ELCELL, AZCELL, CELLTIME
C
C     The cell boundaries.  Make input some day?
C     AZCOFF is added to the azimuth to get the "test" azimuth
C     so that I can have a cell that crosses 0.
C
      DATA      AZCOFF  / 60. /
      DATA      ELCELL  / 15., 70. /
      DATA      AZCELL  / 120., 240. /  
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'OPTCSAR starting ' )
C
      IF( KSCN .EQ. 1 ) THEN
C
C        Make sure that there aren't too many input scans.
C
         IF( NSCANS .GT. MAXISC ) THEN
            WRITE( MSGTXT, '( A, I6 )' ) 
     1        ' OPTCSAR:  Too many input scans for CSUB optimizer'//
     2        ' - max: ', MAXISC
            CALL ERRLOG( MSGTXT )
         END IF
C
C        Initializations.
C        Start the set of new scans beyond NSCANS and use first start
C        time.  TAPPROX is the approximate start time of the next scan.
C        Don't allow this time to be adjusted.
C
         TAPPROX  = STARTJ(1)
         LASTSTOP = STARTJ(1)
C
C        Initialize cell times to well before the start of the 
C        program to try to force quickly getting a complete set
C        of samples.
C
         DO ISTA = 1, NSTA
            DO IEL = 1, 3
               DO IAZ = 1, 3
                  CELLTIME(IEL,IAZ,ISTA) = STARTJ(1) - 20.0 * DUR(1)
               END DO
            END DO
         END DO
C
      ELSE
C      
C        The best guess for the next start time is the end of the
C        last scan.
C
         TAPPROX = STOPJ(ISCN-1)
         LASTSTOP = TAPPROX
C
      END IF
C
C     Initialize the points arrays.  
C     POINTS is for source of input scan JSCN and station ISTA.
C
      ARPTS = 0.0
      DO JSCN = 1, NSCANS
         DO ISTA = 1, NSTA
            POINTS(JSCN,ISTA) = 0.0
         END DO
      END DO
C
C     Get the quality factor for each source/station.
C
C     Loop over sources.
C
      DO JSCN = 1, NSCANS
C
C        Loop over stations.
C
         DO ISTA = 1, NSTA
C
C           Be sure that the input information has not excluded this
C           station from this source (unlikely, but might as well).
C
            IF( STASCN(JSCN,ISTA) ) THEN
C
C              Get geometry and time available.
C
               CALL STAGEO( JSCN, ISTA, TAPPROX, LASTISCN(ISTA),
     1                      LASTTIME, TONSRC(JSCN,ISTA), 'OPTCSAR' )
C
C              For first scan at the station, set TONSRC to the
C              nominal start time.
C
               IF( TONSRC(JSCN,ISTA) .EQ. 0.D0 ) THEN
                  TONSRC(JSCN,ISTA) = TAPPROX
               END IF
C
C              Get cell and time since last sample or that cell.
C              From this get the POINTS.
C              Set POINTS to zero if source is not up at the station.
C
               IF( UP1(JSCN,ISTA) .EQ. ' ' .AND. 
     1             UP2(JSCN,ISTA) .EQ. ' ' .AND.
     2             EL1(JSCN,ISTA) .GT. OPMINEL(JSCN) .AND.
     3             EL2(JSCN,ISTA) .GT. OPMINEL(JSCN) ) THEN
C
C                 Take out effect of cable wrap and add offset.
C
                  AZTEST = MOD( AZ1(JSCN,ISTA) + AZCOFF, 360.0 )
                  IF( AZTEST .LT. 0.0 ) AZTEST = AZTEST + 360.0
C
C                 Calculate which cell the data point is in.
C                 El:
C
                  IF( EL1(JSCN,ISTA) .LT. ELCELL(1) ) THEN
                     ELI(JSCN,ISTA) = 1
                  ELSE IF( EL1(JSCN,ISTA) .LT. ELCELL(2) ) THEN
                     ELI(JSCN,ISTA) = 2
                  ELSE
                     ELI(JSCN,ISTA) = 3
                  END IF
C
C                 Az:
C
                  IF( AZTEST .LT. AZCELL(1) ) THEN
                     AZI(JSCN,ISTA) = 1
                  ELSE IF( AZTEST .LT. AZCELL(2) ) THEN
                     AZI(JSCN,ISTA) = 2
                  ELSE
                     AZI(JSCN,ISTA) = 3
                  END IF
                  IE = ELI(JSCN,ISTA)
                  IA = AZI(JSCN,ISTA)
C
C                 Get the points for this station/scan (units minutes).
C                 First get the minutes since the last sample.
C
                  POINTS(JSCN,ISTA) = 1440.D0 * 
     1                 ( TONSRC(JSCN,ISTA) - CELLTIME(IE,IA,ISTA) )
C
C
C                 Now make an adjustment for slew times.  It might
C                 be better conceptually to do this after the 
C                 final selection of antennas is known and do for
C                 the whole subarray, but that would be difficult.
C
C                 First get the time the source is available relative
C                 to some nominal time - the nominal start of
C                 the scan.  If negative, set to zero (this number
C                 is later squared so a negative value would be seen
C                 as bad too.
C
                  SL_ADJ = ( TONSRC(JSCN,ISTA) - LASTSTOP ) * 1440.0
                  IF( SL_ADJ .LT. 0.0 ) SL_ADJ = 0.0
C
C                 Now develop a multiplier that favors scans that
C                 start before LASTSTOP + OPTSLEW.
C
                  WT_MUL = OPTSLEW**2 / ( SL_ADJ**2 + OPTSLEW**2 )
C
C                 Scale the points for this source/station
C
                  POINTS(JSCN,ISTA) = POINTS(JSCN,ISTA) * WT_MUL
C
C                 Determine any extra weight for samples of low
C                 elevation cells.  This helps stations that never
C                 see low in some directions.
C                 points in the lowest elevation range only.
C                 The function is one that is very small for times
C                 less than OPTLOWT (input parameter) and rises
C                 to near unity for longer times.  Thus a low
C                 elevation scan could get close to 3 times the
C                 weight it might have.
C
                  WT_MUL = 1
                  IF( IE .EQ. 1 ) THEN
                     DO I = 1, 3
                        IF( I .NE. IA ) THEN
                           PT_ADD = 1440.D0 * ( TONSRC(JSCN,ISTA) - 
     1                              CELLTIME(I,IA,ISTA) )
                           WT_MUL = WT_MUL +
     1                       PT_ADD**2 / ( OPTLOWT**2 + PT_ADD**2 )
                        END IF
                     END DO
                  END IF
C
C                 Scale the points for the extra low el weights.
C
                  POINTS(JSCN,ISTA) = POINTS(JSCN,ISTA) * WT_MUL
C
C                 Zero out the points if the source is the same as
C                 the last one observed at this station.
C
                  IF( LASTISCN(ISTA) .GT. 0 ) THEN
                     IF( SRCNUM(JSCN) .EQ. SRCNUM(LASTISCN(ISTA)) ) THEN
                        POINTS(JSCN,ISTA) = 0.0
                     END IF
                  END IF
C
               ELSE
C
C                 Station not up.  Set to zero.
C
                  POINTS(JSCN,ISTA) = 0.0
                  ELI(JSCN,ISTA) = 0
                  AZI(JSCN,ISTA) = 0
                  IE = 0
                  IA = 0
C
               END IF
C
            END IF
         END DO
      END DO
C
C     For each possible pair of sources, find the best subarrays.
C     Skip this if there is no hope of making subarrays based on the
C     minimum OPMIAN.
C
      MINMIAN = 100
      DO JSCN = 1, NSCANS
         MINMIAN = MIN( OPMIAN(JSCN), MINMIAN )
      END DO
C

      IF( MINMIAN .LT. NSTA / 2 .AND. .NOT. OPNOSUB ) THEN
         DO ISCN1 = 1, NSCANS - 1
            DO ISCN2 = ISCN1 + 1, NSCANS
C
C              Derive the optimal subarray configuration for this
C              pair.  Then compare that with the current best.
C              ARPTS will be the sum of POINTS for the best 
C              configuration and SCNSTA will be that configuration.
C              LSRC is in the call just to get its size right.
C
               CALL OPTCSPT( ISCN1, ISCN2, NSTA, POINTS, ARPTS, 
     1                 SCNSTA, LSRC, MAXISC, MAXSTA, OPMIAN(ISCN1), 
     2                 OPMIAN(ISCN2), SCN1, SCN2, NS1, NS2, NS3 )
C
            END DO
         END DO
      ELSE
         NS1 = 0
         NS2 = 0
         NS3 = NSTA
         DO ISTA = 1, NSTA
            SCNSTA(ISTA) = 0
         END DO         
      END IF
C
C     If there are 2 or more unused antennas, find the best source
C     for them.
C
      MPTS3 = 0.0
      SCN3 = 0
      IF( NS3 .LT. 2 ) THEN
C
         NS3 = 0
C
      ELSE
C
C        Loop over input scans.
C
         DO JSCN = 1, NSCANS
            NS = 0
            PTS3 = 0.0
C
C           Loop over the unused stations.
C
            DO ISTA = 1, NSTA
               IF( SCNSTA(ISTA) .EQ. 0 ) THEN
C
C                 Get points if source is up.
C
                  IF( POINTS(JSCN,ISTA) .GT. 0.0 ) THEN
                     NS = NS + 1
                     PTS3 = PTS3 + POINTS(JSCN,ISTA)
                  END IF
C
               END IF
            END DO
C
C           Now see if this source (scan) has all antennas and more 
C           points than previous best.
C
            IF( PTS3 .GT. MPTS3 .AND. NS .GE. 2 ) THEN
               MPTS3 = PTS3
               SCN3 = JSCN
            END IF
         END DO
C
C        If didn't get any sources, set NS3 to 0.
C        Otherwise, install the source.  Add a bit of unnecessary
C        protection against down sources unless OPNOSUB was specified.
C
         NS3 = 0
         IF( MPTS3 .NE. 0.0 ) THEN
            DO ISTA = 1, NSTA
               IF( OPNOSUB .OR. ( SCNSTA(ISTA) .EQ. 0 .AND. 
     1             POINTS(SCN3,ISTA) .NE. 0.0 ) ) THEN
                  SCNSTA(ISTA) = SCN3
                  NS3 = NS3 + 1
               END IF
            END DO
         END IF
C
C
      END IF
C
C     Now NS1, NS2, NS3, SCN1, SCN2, SCN3, and SCNSTA have been
C     set as required.
C
C     Get the start times for each subarray.
C
      TIME1 = 1.D10
      TIME2 = 1.D10
      TIME3 = 1.D10
      DO ISTA = 1, NSTA
         JSCN = SCNSTA(ISTA)
         TIMEC = 0.D0
         IF( JSCN .NE. 0 ) THEN
            IF( SCNSTA(ISTA) .EQ. SCN1 ) THEN
               TIME1 = MIN( TIME1, TONSRC(SCN1,ISTA) )
            ELSE IF( SCNSTA(ISTA) .EQ. SCN2 ) THEN
               TIME2 = MIN( TIME2, TONSRC(SCN2,ISTA) )
            ELSE IF( SCNSTA(ISTA) .EQ. SCN3) THEN
               TIME3 = MIN( TIME3, TONSRC(SCN3,ISTA) )
            END IF
         END IF
      END DO
C
C     Refill the CELLTIME array.  Use scan stop time to discourage
C     doubling up on scans.
C
      DO ISTA = 1, NSTA
         JSCN = SCNSTA(ISTA)
         TIMEC = 0.D0
         IF( JSCN .NE. 0 ) THEN
            IF( JSCN .EQ. SCN1  ) THEN
               TIMEC = TIME1
               DURC  = DUR(JSCN)
            ELSE IF( JSCN .EQ. SCN2 ) THEN
               TIMEC = TIME2
               DURC  = DUR(JSCN)
            ELSE IF( JSCN .EQ. SCN3 ) THEN
               TIMEC = TIME3
               DURC  = DUR(JSCN)
            END IF
            IF( TIMEC .NE. 0.D0 ) THEN
               CELLTIME(ELI(JSCN,ISTA),AZI(JSCN,ISTA),ISTA) = 
     1             TIMEC + DURC
            END IF
         END IF
      END DO
C
C     Print some gory details in debug mode.
C
      IF( DEBUG) THEN
         CALL TIMEJ( TIMEC, YEAR, DAY2, TIMER )
         TIMSTR1 = TFORM( TIMER, 'T', 0, 2, 2, '::@' )
         CALL TIMEJ( TAPPROX, YEAR, DAY2, TIMER )
         TIMSTR2 = TFORM( TIMER, 'T', 0, 2, 2, '::@' )
         WRITE( MSGTXT, '( A, I4, 4A )' ) 
     1        ' NEW SCAN SET - day: ', DAY2, 
     2        ' timec: ', TIMSTR1, ' tapprox: ', TIMSTR2
         CALL WLOG( 0, MSGTXT )
         WRITE( MSGTXT, '( A, 2I5, A, 3I4, A, 3I4 )' )
     1         ' SCAN ', KSCN, ISCN, '    ns: ', NS1, NS2, NS3, 
     2        '    scn: ', SCN1, SCN2, SCN3
         CALL WLOG( 0, MSGTXT )
         DO ISTA = 1, NSTA
            IF( SCNSTA(ISTA) .NE. 0 ) THEN
               JSCN = SCNSTA(ISTA)
               WRITE( MSGTXT, '( I4, A, A8, 2X, A12, F8.2, 1X, A1 )' )
     1            JSCN, ' ', STATION(STANUM(ISTA)), 
     2            SCNSRC(JSCN), EL1(JSCN,ISTA), UP1(JSCN,ISTA)
            ELSE
               WRITE( MSGTXT, '(I4, A, A8, A )' ) 
     1            JSCN, ' ', STATION(STANUM(ISTA)), '  ------ '
            END IF
            CALL WLOG( 0, MSGTXT )
         END DO
      END IF
C
      RETURN
      END
