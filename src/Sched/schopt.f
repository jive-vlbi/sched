      SUBROUTINE SCHOPT
C
C     Routine for the optimization of a schedule.  This will always
C     be called.  In the simplest mode, it just sets SCAN1 and SCANL,
C     calculates the geometric and media parameters, checks time 
C     sequences, avoids bad scan times, and returns.  The next 
C     simplest mode involves dwell time scheduling.  There are also 
C     more complicated modes that use the input scans as essentially 
C     a source list and generate an entirely new schedule that will 
C     be put in scans beyond NSCANS.  After such a use, SCAN1 will
C     be NSCANS + 1 and SCANL will be something higher.
C
C     In addition, SCHOPT can pick it's own sources to either create
C     pointing scans, or to create geodetic (DELZN) sequences.  These
C     scan insertion options are handled with somewhat different logic
C     from the other optimazations.
C
C     The more sophisticated options are used mainly for geodetic or 
C     pointing scheduling, or for scheduling surveys.  This would be 
C     the place to insert the code for other useful optimizations.
C     that would be good to add some day would be:
C
C     An output file that can be used for the scans input of another   
C     sched input can be written if hand editing of the optimized
C     schedule is desired.
C
C     Note that the scan times up to this point are only meaningful 
C     for fixed or duration scheduling.  For dwell scheduling, they
C     are generally well before the actual time after accounting for
C     slews.  For other optimization modes where the input scans are
C     more of a source list, the times are meaningless.
C
C     OPDUR is the experiment total duration.  No scans will be 
C     scheduled after that time.
C
C     Modes (OPTMODE input) are
C      "NONE"   The default.  Don't optimize.  Make adjustments for
C               PRESCAN and get geometry and tape data.  Can include
C               dwell time scheduling.
C      "SCANS"  The original optimizing mode.  It is also used for
C               dwell time scheduling.  The input file has a list 
C               of scans (sometimes a big loop).
C               This routine goes through list taking scans
C               that qualify according to the optimization parameters 
C               and sets the start time to when all antennas are there.
C      "CELLS"  A mode that splits the sky over each antenna
C               into a number of cells and tries to optimize the
C               coverage of those cells.  The input file should contain
C               one scan for each source.  They will not be used in
C               order and each may get used several times.
C      "CSUB"   Like "CELLS" with subarraying.  May not be working.
C      "UPTIME" Makes a series of scans of total length OPDUR for each
C               input scan.  This is meant to help the UPTIME 
C               replacement.
C      "HAS"    Create a schedule using the input scans as a source list
C               with one input scan per output scan.  Try to optimize
C               for hour angles.  This is a one pass operation.  This
C               was used to schedule the VIPS survey project and should
C               be good for other surveys.
C
C
C     In the loop through output scans:
C       KSCN is a count of the passes through the calls to the
C            optimization routines.  For many modes, this keeps
C            track of input scan to consider.  But this is not
C            true for modes that use the input scans as a source list.
C       ISCN is output scan number of the scan to be processed.  This 
C            is the index of the scan in all the arrays that have an 
C            element per scan.  Inserting peaking scans etc can 
C            increment this.  ISCN will in general either equal KSCN
C            or be greater than the NSCANS, the number of input scans.
C            When KSCN > ISCN, the optimization routines need to
C            fill in the scan dependent information (most can be
C            filled with SCNDUP).
C       DONE indicates that there should be no more output scans.
C       KEEP indicates that this scan is to be kept.
C       ADJUST indicates that OPTTIM should adjust the times.
C       LASTISCN(ISTA) is the most recent output scan of any sort,
C            including pointing scans, for station ISTA prior to the 
C            current one.
C       LASTSSCN(ISTA) is the most recent output scan for the station
C            that was not an automatically inserted pointing scan.
C            If pointing scans have been inserted, gap will be honored
C            for LASTSSCN, but not for LASTISCN.
C       NGSCANS is a count of scans that have good data.  Recall that
C            some scans may have no stations up.
C
C     After the optimization loop, the rest of SCHED should use
C     scans SCAN1 through SCANL.
C
      INCLUDE 'sched.inc'
C
      INTEGER           ISCN, KSCN, ISTA, NGSCANS
      INTEGER           LASTISCN(MAXSTA), LASTSSCN(MAXSTA)
      INTEGER           NGOOD, YEAR, DAY1, DAY2, ICSRC
      INTEGER           PEAKOPT, GEOOPT
      LOGICAL           ADJUST, IADJUST, KEEP, DONE, GOTALL
      DOUBLE PRECISION  START, STOP
      CHARACTER         TFORM*8, TIME1*8, TIME2*8
C
C     For test only.
C
C      double precision      SIGMA(20)
C      real                   DUM1
       integer                len1
C
      SAVE              LASTISCN, LASTSSCN
C ---------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 0, 'SCHOPT: Starting.' )
C
C     Initializations.
C
      KSCN = 0
      SCANL = 0
      DONE = .FALSE. 
      NGSCANS = 0
      DO ISTA = 1, NSTA
         LASTISCN(ISTA) = 0
         LASTSSCN(ISTA) = 0
      END DO
C
C     ORIGEN indicates the source of the scan.  It was introduced
C     in Feb. 2014 to help with dealing with using GAP in the presence
C     of automatically inserted pointing scans.  GAP needs to refer
C     back to the last original scan.  Options for ORIGEN are:
C        ORIGEN=1  Scan explicitly specified in the key file.
C        ORIGEN=2  Scan inserted by an optimization mode.
C        ORIGEN=3  Scan inserted for a geodetic segment.
C        ORIGEN=4  Scan inserted for reference pointing.
C     At this point, we only have scans from the key file.
C
      DO ISCN = 1, NSCANS
         ORIGEN(ISCN) = 1
      END DO
C
C     PEAKOPT is related to reference pointing which is triggered by 
C     the user setting AUTOPEAK.  PEAKOPT tells how many more 
C     peaking scans are needed at the moment.  If zero, get the 
C     next target scan.  GEOOPT is a similar variable for insertion
C     of geodetic segments.
C
C     The output scans can be the input scans for OPTMODE 'NONE'
C     and 'SCANS' without automatic insertion of reference pointing.
C     Otherwise, new scans should be created and the output schedule
C     should start at scan SCAN1=NSCANS+1.
C
      PEAKOPT = 0
      GEOOPT = 0
      IF( ( OPTMODE .NE. 'NONE' .AND. OPTMODE .NE. 'SCANS' ) .OR.
     1      AUTOPEAK .OR. ANYGEO ) THEN
         ISCN = NSCANS
         SCAN1 = NSCANS + 1
      ELSE
         ISCN = 0
         SCAN1 = 1
      END IF
      WRITE( MSGTXT, '( A, I5 )' ) 
     1       'SCHOPT:  First output scan will be number ', SCAN1
      CALL WLOG( 0, MSGTXT )
C
C
C     Scan loop.  We are looping over output scans, which may not
C     correspond to input scans.
C
C
      DO WHILE( .NOT. DONE )
         ISCN = ISCN + 1
         IF( ISCN .GT. MAXSCN ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, I5 )' )
     1       'SCHOPT:  Trying to generate too many scans. Max:', MAXSCN
            CALL ERRLOG( MSGTXT )
         END IF
C
         IF( PEAKOPT .NE. 0 .OR. GEOOPT .NE. 0 ) THEN
C
C           The insertion of geodetic sequences and reference pointing
C           scans is initiated by this routine.  The first time that
C           ADDGEO or ADDPEAK is called while processing a scan for
C           which geodetic or pointing scans will be added, all of the
C           required output scans related to the input scan will be
C           generated and added to the output scan sequence.  That
C           pass of this scan loop will then finish by processing the
C           first of the added scans.
C
C           For the following passes through the scan loop, the added
C           scans need to be processed without invoking the optimization
C           routines.   PEAKOPT and GEOOPT keep track of how many more
C           added scans need to be processed so when they are not
C           zero, jump around the optimization routines.  But be 
C           sure to call PEAKOPT or GEOOPT (by setting KEEP and DONE)
C           so that they can decrement PEAKOPT or GEOOPT.
C
            KEEP = .TRUE.
            DONE = .FALSE.
C
         ELSE
C
C           Get a new main scan if no peaking or geodetic scan insertions 
C           are in progress.
C
C           Increment the input pass counter (for many, but not all
C           modes, this is the input scan number).  Note that it does
C           not get incremented while peaking or geoseg scans are
C           being added.
C
            KSCN = KSCN + 1
C
C           Protect against a possible programming error.  Users
C           should not see this one.
C
            IF( KSCN .NE. ISCN .AND. ISCN .LT. NSCANS ) THEN
               WRITE( MSGTXT, '( A, A, 3I7 )' )
     1              'SCHOPT possible programming error. ', 
     2              ' KSCN ne ISCN and ISCN < NSCANS ', 
     3              KSCN, ISCN, NSCANS
               CALL ERRLOG( MSGTXT )
            END IF
C
C           Here is where the optimization subroutines are called.
C           They specify the the next output scan, with source, 
C           stations, and times.  If ADJUST is true, the start time
C           is assumed to be approximate and can be adjusted for 
C           slews etc later.  If false, the times are fixed.  Note
C           that the stop time will also be adjusted if ADJUST is
C           true if only a duration (or dwell) was specified by the user
C           for the scan.  Otherwise the user specified time will be
C           used (if the user specified start, ADJUST will be false).
C   
            IF( OPTMODE .EQ. 'NONE' ) THEN
C   
C              Non-optimizing mode:  Just use next input scan.  If
C              KSCN ne ISCN, then KSCN scan info will be copied to
C              ISCN.  Note that timing from DWELL scheduling will be
C              handled in the OPTTIM call later.  ADJUST will be set
C              appropriately here.  The DWELL case used to be separate
C              but there was no need.  OPTDWELL was deleted.
C   
               CALL OPTNONE( KSCN, ISCN, ADJUST, KEEP, DONE )
C   
            ELSE IF( OPTMODE .EQ. 'SCANS' ) THEN
C   
C              Select scans by number of antennas up and various other 
C              criteria.  The scan start time is set to a good guess. 
C              It will be fine tuned later in OPTTIM.
C   
               CALL OPTSKD( LASTISCN, KSCN, ISCN, ADJUST, KEEP, DONE )
C   
            ELSE IF( OPTMODE .EQ. 'CELLS' ) THEN
C   
C              Cells type optimization:  Here we really try getting
C              fancy.  The input NSCANS scans are used like a source
C              list.  OPTCELLS selects which one to use next and 
C              puts it in scan KSCN + SCAN1 - 1.
C   
               CALL OPTCELLS( LASTISCN, KSCN, ISCN, ADJUST, KEEP, DONE )
               ORIGEN(ISCN) = 2
C   
            ELSE IF( OPTMODE .EQ. 'CSUB' ) THEN
C   
C              Cells type optimization like CELLS, but with subarrays
C              created.
C   
               CALL OPTCSUB( LASTISCN, KSCN, ISCN, ADJUST, KEEP, DONE )
               ORIGEN(ISCN) = 2
C   
            ELSE IF( OPTMODE .EQ. 'UPTIME' ) THEN
C   
C              Creates a string of scans of total length OPDUR for each
C              input scan.  This is for planning.
C   
               CALL OPTUPT( LASTISCN, KSCN, ISCN, ADJUST, KEEP, DONE )
               ORIGEN(ISCN) = 2
C   
            ELSE IF( OPTMODE .EQ. 'HAS' ) THEN
C
C              Create a schedule using the input scans as a source list
C              with one input scan per output scan.  Try to optimize
C              for hour angles.  This is a one pass operation.
C
               CALL OPTHAS( LASTISCN, KSCN, ISCN, ADJUST, KEEP, DONE )
               ORIGEN(ISCN) = 2
C
            ELSE IF( OPTMODE .EQ. 'HIGHEL' ) THEN
C
C              Create a schedule using the input scans as a source list
C              with groups of scans, designated using HIGROUP, from which
C              the one with the highest minimum elevation will be 
C              chosen.  Note this routine can change KSCN.
C
               CALL OPTHIEL( LASTISCN, KSCN, ISCN, ADJUST, KEEP, DONE )
               ORIGEN(ISCN) = 2
C
            ELSE
C   
C              Invalid optimization mode.
C   
               CALL ERRLOG( 'SCHOPT: Invalid OPTMODE: '//OPTMODE )
C   
            END IF
C
C                Some potentially useful debug code.
C                call timej( startj(iscn), year, day1, start )
C                time1 = tform( start, 'T', 0, 2, 2, '::@' )
C                if( iscn .gt. 170 .and. iscn .lt. 190 ) then
C                  write(*,*) ' '
C                  write(*,*) 'schopt 1:        ', kscn, iscn, adjust, ' ', 
C               1                  iadjust, ' ', time1, ' ', scnsrc(iscn)
C                end if
C
C           KSCN not used below this point.
C
C           Some of the optimization routines only give approximate
C           scan times.  Get better, ones now.  Inserted pointing 
C           scans have not been added yet, but they won't 
C           be allowed to adjust the main scan times.  When geodetic
C           scans are being inserted, the times for here are for the
C           dummy scan to which the geodetic segment insertion 
C           parameters are attached.  That scan will not actually
C           be used so what is calculated here is a bit superfluous,
C           but there isn't much overhead and it is simpler not to
C           try to trap that case.  ADDGEO will call OPTTIM and 
C           SCNGEO (in GMKSCN) for the geodetic scans, plus they will
C           get called yet again after the insertions are done.
C
            IF( KEEP .AND. .NOT. DONE ) THEN
C        
C              Get slew times and exact start time of next scan.  The
C              adjustments for DWELL scheduling are done here.  Note
C              that OPTTIM does not calculate geometry for all scans
C              and, even when it does, it may be for the wrong time - 
C              like for a guessed start time.
C        
               CALL OPTTIM( LASTISCN, LASTSSCN, ISCN, ADJUST, 
     1                      .FALSE., .FALSE. )
C        
C              Now be sure that the experiment time boundaries have
C              not been exceeded.  Some optimization modes watch this
C              themselves and miss some closeout stuff (like printing
C              summaries if they aren't the ones that set DONE.  In
C              close cases, they might not think a scan has gone over
C              where the adjustments above cause it to do so.  So
C              avoid this test for those modes.
C        
               IF( OPDUR .NE. 0.D0 .AND. 
     1             STOPJ(ISCN) .GT. STARTJ(SCAN1) + OPDUR .AND.
     2             OPTMODE .NE. 'HAS' ) THEN
                  DONE = .TRUE.
               END IF
C
C              Update the real geometry for the scan.  OPTTIM doesn't
C              really do this.  The pointing insertion system below
C              assumes that this has been done.
C
               CALL SCNGEO( LASTISCN, NGOOD, ISCN )
C        
            END IF
         END IF
C
C        ADJUST was set above for the nominal scan.  The insertion
C        routines below can postpone dealing with that scan and may
C        want other values of ADJUST than was set above.  But then
C        when the main scan is processed in OPTTIM, we may want the
C        original value.  So transfer ADJUST to IADJUST here, then
C        allow the insertion routines to change IADJUST as they
C        see fit.  ADJUST should not be changed below here, or above 
C        while flushing through inserted scans.
C
         IADJUST = ADJUST
C
C        Only keep the scan if told to do so by optimization routine.  
C        Also enter this section if in the middle of adding pointing
C        or geodetic scans.
C
         IF( KEEP .AND. .NOT. DONE ) THEN
C
C           This is the section where additional scans can be inserted 
C           at specific places in the schedule, not as the result of
C           any global optimization.  For now, this means geodetic
C           segments or pointing scans.
C
C           Insert geodetic segment scans if requested.
C
C           Only do it if not in the middle of inserting pointing.
C           Mixing pointing and geo insertion might work, but it
C           sounds both dangerous and unnecessary.
C
C           For any inserted scans, ADDGEO will have run OPTTIM and
C           SCNGEO so use IADJUST to not allow the later runs to 
C           move things around.
C
            IF( PEAKOPT .EQ. 0 .AND. ( GEOLEN(ISCN) .GT. 0.D0 .OR. 
     1            GEOOPT .GE. 1 ) ) THEN
               IADJUST = .FALSE.
               CALL ADDGEO( LASTISCN, ISCN, GEOOPT, KEEP )
            END IF
C
C           Get the geodetic quality measure for some scans.  This
C           was written for testing a premade sequence.  GEOQUAL does the 
C           printing.  Keep the code here in case I want to make it
C           a feature with a switch like GEOTEST = 13 on the last 
C           scan of the group.  The fourth parameter should be a scan
C           that includes all sources (may need to test to find one).
C
C            IF( GEOTEST(ISCN) .GT. 0 ) THEN
C               CALL GEOQUAL( ISCN - GEOTEST + 1, ISCN - GEOTEST + 1, 
C     1              ISCN, ISCN - GEOTEST + 1,
C     2              0.0, DUM1, .TRUE., SIGMA )
C            END IF
C
C           Insert reference pointing scans if requested.
C           It is possible that several scans will be inserted (NADDED 
C           in ADDPEAK).  The primary observing scan will be moved 
C           to scan ISCN+NADDED and the reference pointing scans 
C           will be put in between.  ADDPEAK will firmly establish 
C           the times for the added scans.
C
C           ADDPEAK will also set PEAKOPT so that the optimization
C           routines will be skipped until all the peaking scans and
C           the main scan have been processed.  It will set IADJUST
C           false for all inserted scans.
C      
            CALL ADDPEAK( LASTISCN, ISCN, PEAKOPT, IADJUST )
C      
C           The scans added by ADDPEAK will have the right times and 
C           source but the main scan's setup.  POINT will be set.  
C           MAKEPTG will convert it, or any other scan with POINT 
C           set, into a reference pointing scan.
C           
            CALL MAKEPTG( LASTISCN, ISCN, KEEP )
C      
         END IF
C
C        All possible scan insertions etc are now done.  The details 
C        of the current scan can be finalized.
C
C        Retest KEEP and DONE to allow them (actually only KEEP) be 
C        reset by the above insertion routines.  The main case is 
C        that MAKEPTG may discover no stations that can do reference 
C        pointing are in the scan.
C
         IF( KEEP .AND. .NOT. DONE ) THEN
C
C           Now that all the insertions are done, if they happened,
C           do a final pass of the geometry calculations.  This will
C           not actually be needed in most (any?) cases, but it doesn't 
C           cost much and this ensures everything is in good shape.
C           This can tweak the scan timing (in OPTTIM if IADJUST is 
C           true), gets the geometry at the stations and counts the
C           stations that are considered UP.  Note NGOOD in SCNGEO
C           is not needed here (also determined by AUTODOWN) but 
C           SCNGEO is used elsewhere where it is needed.
C
C           This is the last call to OPTTIM, so apply the PRESCAN 
C           offsets here.  They should only be applied once.
C
            CALL OPTTIM( LASTISCN, LASTSSCN, ISCN, IADJUST, 
     1                   .FALSE., .TRUE. )
            CALL SCNGEO( LASTISCN, NGOOD, ISCN )
C
C           Eliminate stations using disk recorders
C           for which the source is down. Also eliminate antennas
C           that don't reach the source until the scan is over.
C           Don't attempt any scan timing adjustments, but do
C           recalculate NGOOD to reflect new truths.
C
            CALL AUTODOWN( LASTISCN, ISCN, NGOOD )
C
C           Only accept scans with enough antennas up.
C           Make a special case for OPTMODE=CSUB since it will put
C           small numbers of antennas into the 3ed subarray.
C           Also don't complain about small subarrays for reference
C           pointing.
C           Also make an exception for OPTMODE=HAS for which the 
C           optimizing routine makes this selection and small tweaks
C           in scan times might upset those results.
C
            KEEP = KEEP .AND.
     1          ( NGOOD .GE. OPMIAN(ISCN) .OR. OPTMODE .EQ. 'CSUB' .OR.
     2          OPTMODE .EQ. 'HAS' .OR. POINT(ISCN) .GE. 0 )
C
            IF( .NOT. KEEP ) THEN
C
C              Set so that no stations are included.  This is understood
C              in later parts of the program.
C
               DO ISTA = 1, NSTA
                  STASCN(ISCN,ISTA) = .FALSE.
               END DO
C
            ELSE
C
C              Accepted scan.
C
C              Set the start time for data recording.
C              If DOVEX, do not adjust later.
C
               CALL SETTPS( ISCN, LASTISCN )
C
C              If using disk, get the amount of disk used and 
C              add to total.  Also set tape info to default while
C              in a station loop.
C
               DO ISTA = 1, NSTA
                  IF( STASCN(ISCN,ISTA) .AND. 
     1                VLBITP .AND. .NOT. NOSET ) THEN
                     IF( USEDISK(ISTA) ) THEN
                        CALL DISKPOS( ISCN, ISTA, LASTISCN )
                     END IF
                  END IF
               END DO
C
C              Write the new KEYIN input for another run of SCHED
C              in case the user wants to do some edits.  This is
C              most likely to be useful for optimized schedules.
C
               IF( ( OPTMODE .NE. 'NONE' .AND. OPTMODE .NE. 'UPTIME' ) 
     1             .OR. ANYGEO ) THEN
                  CALL OPTSCH( ISCN ) 
               END IF
C
C              Keep a count of the number of scans that will actually
C              have data.
C
               NGSCANS = NGSCANS + 1
C
C              Flag sources that have been used.  Here so that sources
C              inserted for pointing will be included.
C
               SUSED(SRCNUM(ISCN)) = .TRUE.
               IF( .NOT. NOREC(ISCN) ) USEDREC(SRCNUM(ISCN)) = .TRUE.
               IF( IDOPSRC(ISCN) .NE. 0 ) SUSED(IDOPSRC(ISCN)) = .TRUE.
               IF( IVLAPHS(ISCN) .NE. 0 ) SUSED(IVLAPHS(ISCN)) = .TRUE.
               IF( ICENT(ISCN) .NE. 0 ) THEN
                  USEDCENT(SRCNUM(ISCN)) = .TRUE.
                  DO ICSRC = 1, NCSRC(ICENT(ISCN))
                     USEDPHS(CTRSRCI(ICSRC,ICENT(ISCN))) = .TRUE.
                  END DO
               END IF
C
            END IF
C
C           Mark this as most recent scan for each station that is in
C           the scan.  Also mark it as the last non-inserted pointing
C           scan if that is true.
C
            DO ISTA = 1, NSTA
               IF( STASCN(ISCN,ISTA) ) THEN
                  LASTISCN(ISTA) = ISCN
                  IF( ORIGEN(ISCN) .LT. 4 ) THEN
                     LASTSSCN(ISTA) = ISCN
                  END IF
               END IF
            END DO
C
C           Set the last output scan number.
C
            SCANL = ISCN
C
         END IF
C     
C                More potentially useful debug code.
C                call timej( startj(iscn), year, day1, start )
C                time1 = tform( start, 'T', 0, 2, 2, '::@' )
C                if( iscn .gt. 170 .and. iscn .lt. 190 ) then
C                write(*,*) 'schopt 2:        ', kscn, iscn, adjust, ' ', 
C               1                  iadjust, ' ', time1, ' ', scnsrc(iscn)
C                end if
      END DO
C
      IF( DEBUG ) CALL WLOG( 0, 'SCHOPT: 9.' )
      IF( OPTMODE .EQ. 'SCANS' ) THEN
         WRITE( MSGTXT, '( A, I5, A, I5, A )' ) 'SCHOPT optimization: ',
     1       NGSCANS, ' of ', SCANL - SCAN1 + 1, ' scans used. ' 
      END IF
C
C     Check that we have something.
C
      IF( SCANL .EQ. 0 ) THEN
         CALL WLOG( 1, 'SCHOPT: Did not schedule any scans' )
         CALL ERRLOG( ' Abort' )
      END IF
C
C     Rebuild the list of sources actually used in the schedule,
C     the "schedule sources" in the include file.  The only
C     variables affected are NSRC, SRCATN(ISRC), and SRCNAME(ISRC).
C     The rebuild is needed because some of the optimization modes 
C     and pointing modes add scans and sources and some scans may 
C     have been eliminated.  Rebuild the list of schedule sources 
C     with ACCSRC, ignoring potential pointing sources, planets,
C     satellites etc this time because they are already in scans.
C     They weren't when SRFINISH was run earlier.
C     Then rebuild the pointers and flags with SRCFLG.
C
      CALL ACCSRC( .FALSE.)
      CALL SRCFLG( GOTALL )
C
      IF( .NOT. GOTALL ) CALL ERRLOG( 
     1  'SCHOPT: Not all sources found; programming problem.' )
C
C     Take the opportunity to construct a list of pointing center
C     sources paired with lists of offset phase centers.
C
      CALL GETPAIRS
C
C     Write the scan and time range.
C
      MSGTXT = ' '
      CALL TIMEJ( STARTJ(SCAN1), YEAR, DAY1, START )
      CALL TIMEJ( STOPJ(SCANL), YEAR, DAY2, STOP )
      TIME1 = TFORM( START, 'T', 0, 2, 2, '::@' )
      TIME2 = TFORM( STOP, 'T', 0, 2, 2, '::@' )
      WRITE( MSGTXT, '( A, I5, A, I5, A, I5, A, I3, 3A, I3, 2A )' ) 
     1        'SCHOPT:  There will be ',
     2        SCANL - SCAN1 + 1, ' output scans (', SCAN1, ' - ', SCANL,
     3        ') from ', 
     4        DAY1, '/', TIME1, ' to ', 
     5        DAY2, '/', TIME2
      CALL WLOG( 0, MSGTXT )
C
C     Check for a monotonic time sequence at each station.
C     Also, get the extreme times of experiment (used for Mark II tape
C     scheduling, Doppler calculations, and Sun position).  While at
C     it, get statistics for summaries on numbers of scans, hours etc.
C
      CALL  SCHTIM
C
C     Avoid day number confusions for start and stop times 
C     near midnight.
C
      CALL SCH24
C
      IF( DEBUG ) CALL WLOG( 0, 'SCHOPT: Done.' )
C
      RETURN
      END
