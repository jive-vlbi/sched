      SUBROUTINE UVOPT
C
C     Optimize the UV coverage.  Called from PLOTSTA when an 'O'
C     is typed while the cursor is on the stations plot.
C
C     The optimization proceedure is a simple try many combinations
C     of stations.  The quality measure is number of UV cells sampled.
C     A polar, logarithmic grid will be used.
C
C     This routine does the following steps
C
C         Determine how many stations (NOPT ) to try to optimize.  
C         This will be the count of stations with 
C         PSTBCK(ISTA,1) = 1 and PSTBCK(ISTA,2) = 1 (Red on the 
C         stations plot.).
C
C         Set up to try all combinations of NOPT of the stations 
C         that have PSTBCK(ISTA,2) = 1 (Red or yellow with red dot).
C
C         (From the above, you can see how to set up the optimization
C         by toggling all potential sites to red or yellow with red dot,
C         leaving only NOPT of them full red.)
C
C         Set up the grid.  Hardwired at first.  Later user input.
C
C         Cycle through the selected sources, calculating UV, assigning
C         to cells and incrementing cell counts.  Keep a separate
C         count per source, but also keep some weighted sum.
C
C         Keep a record of which combination had the highest count.
C
C         When done, set PSTBCK(ISTA,1) to 1 for the stations 
C         in the best array.  Set the others to 0.  Then on the
C         next command to do a UV plot, the optimized array will
C         be plotted.
C
      INCLUDE    'sched.inc'
      INCLUDE    'plot.inc'
C
      INTEGER    NOPT, NTRY, ISTA, NCOMB
      INTEGER    FLAG(MAXSTA), FLAGBEST(MAXSTA)
      INTEGER    CTR(MAXSTA), I, IC, COUNT, LEN1, ISRC
      REAL       QUALITY, MAXQUAL, TOP5(5,MAXSRC)
      LOGICAL    CARRY(MAXSTA), DONE
      CHARACTER  CHLAT*9, CHLONG*10, TFORM*15
      INTEGER    IOERR, VLBOPE
      LOGICAL    EXISTS
      CHARACTER  OPTFILE*80
      CHARACTER  OPSTAT*4, OPTEXT*256
C
C     Keep information about top arrays.
C
      INTEGER    MKEEP 
      PARAMETER  (MKEEP=200)
      CHARACTER  INFOLINE*128, KEEPLINE(MKEEP)*128
      INTEGER    KEEPNMIN, IK, PRTNMAX, NKEEP
      REAL       KEEPQUAL(MKEEP), KEEPQMIN, PRTQMAX
      LOGICAL    SENT(MKEEP)
C --------------------------------------------------------------------
C     Open the .opt print file.  Always write over 
C     whatever was there before.  Make a new one 
C     for each optimization effort.
C     
      WRITE( OPTFILE, '(A,A)' ) EXPCODE(1:LEN1(EXPCODE)), '.OPT'
      CALL DWCASE( OPTFILE )
      INQUIRE( FILE=OPTFILE, EXIST=EXISTS )
      IF( EXISTS ) THEN
         OPSTAT = 'OLD'
      ELSE
         OPSTAT = 'NEW'
      END IF
      CALL WLOG( 0, 'UVOPT:  Writing new configuration file '//
     1   OPTFILE(1:LEN1(OPTFILE)) )
      IOERR = VLBOPE( IOPT, OPTFILE, 'TEXT', OPSTAT, OPTEXT )
      IF( IOERR .NE. 1 ) CALL ERRLOG( ' Open problem:'//OPTEXT )
C
C     Write some information about the stations.  First the ones
C     to be helf fixed.
C
      WRITE( IOPT,*) 'UV optimization.'
      WRITE( IOPT, '( A, I4, A, I4 )' ) '   GRIDNR = ', GRIDNR, 
     1     '   GRIDNT = ', GRIDNT
      WRITE( IOPT, '( A, F10.2, A, F10.2 )' ) '   GRIDMIN = ', GRIDMIN, 
     1     '   GRIDMAX = ', GRIDMAX
      WRITE( IOPT, '( A, F10.2 )' ) '   GRIDW0 = ', GRIDW0
C
      WRITE( IOPT,*) 'Stations held fixed: '
      DO ISTA = 1, NSTA
         IF( PSTBCK(ISTA,1) .EQ. 1 .AND. PSTBCK(ISTA,2) .EQ. 0 ) THEN
            CHLAT = TFORM( LAT(STANUM(ISTA)), ' ', 1, 2, 2, ':: ' )
            CHLONG = TFORM( LONG(STANUM(ISTA)), ' ', 1, 3, 2, ':: ' )
            WRITE( IOPT, '( 2X, A, A8, 2X, 2A, 2X, 3A, F6.0, A )' )
     1          'station=', STANAME(ISTA), 'lat=', CHLAT, 
     2          'long=', CHLONG, '  elev=', ELEV(STANUM(ISTA)), ' /'
         END IF
      END DO
C
      WRITE( IOPT,*) ' '
      WRITE( IOPT,*) 'Stations to choose from: '
      DO ISTA = 1, NSTA
         IF( PSTBCK(ISTA,2) .EQ. 1 ) THEN
            CHLAT = TFORM( LAT(STANUM(ISTA)), ' ', 1, 2, 2, ':: ' )
            CHLONG = TFORM( LONG(STANUM(ISTA)), ' ', 1, 3, 2, ':: ' )
            WRITE( IOPT, '( 2X, A, A8, 2X, 2A, 2X, 3A, F6.0, A )' )
     1          'station=', STANAME(ISTA), 'lat=', CHLAT, 
     2          'long=', CHLONG, '  elev=', ELEV(STANUM(ISTA)), ' /'
         END IF
      END DO
C
      DO ISRC = 1, NSRC
         DO I = 1, 5
            TOP5(I,ISRC) = 0.0
         END DO
      END DO
C
C     Count the number of stations to optimize and the number of
C     alternatives from which to pick.  FLAG is used in the looping
C     to indicate which stations to try.
C
      NOPT = 0
      NTRY = 0
      DO ISTA = 1, NSTA
         IF( PSTBCK(ISTA,2) .EQ. 1 ) THEN
            NTRY = NTRY + 1
            IF( PSTBCK(ISTA,1) .EQ. 1 ) THEN
               NOPT = NOPT + 1
            END IF
         END IF
      END DO
      IF( NOPT .EQ. 0 ) THEN
         CALL WLOG( 1, 'UVOPT: No stations to optimize' )
         CLOSE( UNIT=IOPT )
         RETURN
      END IF
C
C     Count the number of configurations we will try.  Eventually
C     this can be used to give warnings or give limits.
C     This is  (NTRY!-NOPT!)/NOPT! = 
C          (NTRY/1)*((NTRY-1)/2)*((NTRY-3)/3)*....
C
      NCOMB = 1
      IF( NTRY .GT. NOPT ) THEN
         DO I = 1, NOPT
            NCOMB = NCOMB * ( NTRY + 1 - I ) / I
         END DO
      END IF
C
      MSGTXT = ' '
      WRITE( IOPT,*) ' '
      WRITE( MSGTXT, '( A, I10, A )' ) 
     1   'UVOPT: About to try ', NCOMB, ' different configurations'
      WRITE( IOPT, '(A)' ) MSGTXT(1:LEN1(MSGTXT) )
      CALL WLOG( 0, MSGTXT(1:LEN1(MSGTXT) ) )
      MSGTXT = ' '
      WRITE( MSGTXT, '( A, I3, A, I4 )' ) 
     1   '       picking ', NOPT, ' stations from a list of ', NTRY
      WRITE( IOPT, '(A)' ) MSGTXT(1:LEN1(MSGTXT) )
      CALL WLOG( 0, MSGTXT(1:LEN1(MSGTXT) ) )
      MSGTXT = ' '
C
C     Have NOPT numbers, which are the sequence among the NTRY
C     stations, for the stations to use for a combination.  For
C     example CTR(2)=5 means that the second station of the NOPT
C     being tried will be the fifth that has PSTBCK(ISTA,2)=1.
C     This seems a bit complicated, but I didn't see a cleaner
C     way to deal with it when NOPT is not known and I don't want
C     to put an upper limit on it.
C
      DO I = 1, NOPT
         CTR(I) = I
         CARRY(I) = .FALSE.
      END DO
      CTR(NOPT) = CTR(NOPT) - 1
C
C     Start the loop.  This is some fancy hoop jumping.
C
      DONE = .FALSE.
      MAXQUAL = 0.0
      KEEPNMIN = 1
      KEEPQMIN = 1.E10
      NKEEP = 0
      DO IK = 1, MKEEP
         KEEPQUAL(IK) = 0.0
      END DO
      DO IC = 1, NCOMB
         CARRY(NOPT) = .TRUE.
         DO I = NOPT, 2, -1
            IF( CARRY(I) ) CTR(I) = CTR(I) + 1
            CARRY(I-1) = CTR(I) .GT. NTRY - ( NOPT - I )
         END DO
         IF( CARRY(1) ) CTR(1) = CTR(1) + 1
         IF( CTR(1) .GT. NTRY + 1 - NOPT ) THEN
            DONE = .TRUE.
         ELSE
            DO I = 2, NOPT
               IF( CARRY(I-1) ) CTR(I) = CTR(I-1) + 1
            END DO
         END IF
C
C        Reset the flags.  0 means not involved. 1 is an 
C        unchanging station.  2 is a changing station, set
C        below.  Distinguishing allows the UV counter to only
C        recalculate the changing part.
C
         DO ISTA = 1, NSTA
            IF( PSTBCK(ISTA,1) .EQ. 1 .AND. PSTBCK(ISTA,2) .EQ. 0 ) THEN
               FLAG(ISTA) = 1
            ELSE
               FLAG(ISTA) = 0
            END IF
         END DO
C
C        Go through the FLAGS setting the right ones based
C        on CTR.
C
         DO I = 1, NOPT
            COUNT = 0
            DO ISTA = 1, NSTA
               IF( PSTBCK(ISTA,2) .EQ. 1 ) COUNT = COUNT + 1
               IF( CTR(I) .EQ. COUNT ) THEN
                  FLAG(ISTA) = 2
                  GO TO 100
               END IF
            END DO
  100       CONTINUE
         END DO
C
C        
         IF( MOD( IC, 10 ) .EQ. 1 ) THEN
            MSGTXT = ' ' 
            WRITE( MSGTXT, '( A, I7, A, I8, A, 10I3 )' ) 
     1         'UVOPT:  Sample configuration ', IC, ' of ', 
     2            NCOMB, ' Stations: ',
     3            ( CTR(I),I=1,NOPT )
            CALL WLOG( 0, MSGTXT(1:LEN1(MSGTXT)) )
            MSGTXT = ' ' 
         END IF
C
C        Get the quality measure for this array.
C
         CALL UVQUAL( IC, FLAG, QUALITY, TOP5, INFOLINE )
C
C        Keep track of best.  Do on best to initialize FLAGBEST.
C
         IF( QUALITY .GT. MAXQUAL .OR. IC .EQ. 1 ) THEN
            MAXQUAL = QUALITY
            DO ISTA = 1, NSTA
               FLAGBEST(ISTA) = FLAG(ISTA)
            END DO
         END IF
C
C        Keep the INFOLINE for the top MKEEP arrays for later printing
C          
         IF( NKEEP .LT. MKEEP .AND. QUALITY .GT. 0.0 ) THEN
            NKEEP = NKEEP + 1
            KEEPQUAL(IC) = QUALITY
            KEEPLINE(IC) = INFOLINE
            IF( KEEPQUAL(IK) .LT. KEEPQMIN ) THEN
               KEEPNMIN = IC
               KEEPQMIN = KEEPQUAL(IC)
            END IF
         ELSE IF( QUALITY .GT. KEEPQMIN ) THEN
            NKEEP = MKEEP
C
C           Replace the old minimum with this one.
C
            KEEPQUAL(KEEPNMIN) = QUALITY
            KEEPLINE(KEEPNMIN) = INFOLINE
C
C           Now find the new minimum for use next time.
C           
            KEEPQMIN = 1.E10
            DO IK = 1, MKEEP
               IF( KEEPQUAL(IK) .LT. KEEPQMIN ) THEN
                  KEEPNMIN = IK
                  KEEPQMIN = KEEPQUAL(IK)
               END IF
            END DO
         END IF
C         
      END DO
C
C     Set PSTBCK(ISTA,1)=1 for the best stations and =0 for
C     the rest.
C
      WRITE( IOPT, * ) ' '
      WRITE( IOPT, * ) 'The best array of this bunch: '
      DO ISTA = 1, NSTA
         IF( FLAGBEST(ISTA) .EQ. 0 ) THEN
            PSTBCK(ISTA,1) = 0
         ELSE IF( FLAGBEST(ISTA) .EQ. 1 ) THEN
            PSTBCK(ISTA,1) = 1
         ELSE IF( FLAGBEST(ISTA) .EQ. 2 ) THEN
            PSTBCK(ISTA,1) = 1
         END IF
         IF( FLAGBEST(ISTA) .GE. 1 ) THEN
            CHLAT = TFORM( LAT(STANUM(ISTA)), ' ', 1, 2, 2, ':: ' )
            CHLONG = TFORM( LONG(STANUM(ISTA)), ' ', 1, 3, 2, ':: ' )
            WRITE( IOPT, '( 2X, A, A8, 2X, 2A, 2X, 3A, F6.0, A, I2 )' )
     1          'station=', STANAME(ISTA), 'lat=', CHLAT, 
     2          'long=', CHLONG, '  elev=', ELEV(STANUM(ISTA)), 
     3          ' / ! ', FLAGBEST(ISTA)
         END IF
      END DO
C
      WRITE( IOPT, * ) ' '
      WRITE( IOPT, * ) 'The top 5 quality factors for each source:'
      DO ISRC = 1, NSRC
         IF( PSOBCK(ISRC) .EQ. 1 ) THEN
            WRITE( IOPT, '( A, 5F7.4 )' )
     1         SRCNAME(ISRC), ( TOP5(I,ISRC), I=1,5 )
         END IF
      END DO
      WRITE( IOPT, * ) ' '
      WRITE( IOPT, * ) 'The top 5 overall quality factors:'
      WRITE( IOPT, '( A, 5F7.4 )' )
     1   'Sum over sources ', ( TOP5(I,MAXSRC), I=1,5 )
C
      DO ISRC = 1, NSRC
         WRITE( IOPT, '( A, A, 2I6 )' )
     1     'Source: ', SRCNAME(ISRC), ISRC, 
     2     PSOBCK(ISRC)
      END DO
C
C     Write the information lines from the top arrays.
C     Sort by a very crude mechanism, but this will be very fast
C     compared to the rest of this routine's work.
C
      WRITE( IOPT, '( A )' ) ' '
      WRITE( IOPT, '( A, I6, A )' ) 'Top ', NKEEP, ' arrays. '
      WRITE( IOPT, '( A )' ) '  Numbers are a sequence number, '//
     1   'the overall quality, and the individual source qualities.'
C
      DO I = 1, NKEEP
         SENT(I) = .FALSE.
      END DO
      IF( NKEEP .EQ. 0 ) THEN
         WRITE( IOPT, '( A )' )
     1      '  There were no arrays with data on the grid. '
      ELSE
         DO I = 1, NKEEP
            PRTQMAX = 0.0
            PRTNMAX = 0
            DO IC = 1, NKEEP
               IF( KEEPQUAL(IC) .GT. PRTQMAX .AND. .NOT. SENT(IC) ) THEN
                  PRTNMAX = IC
                  PRTQMAX = KEEPQUAL(IC)
               END IF
            END DO
            IF( PRTNMAX .GE. 0 ) THEN
               WRITE( IOPT, '(I4, A, A)' ) I, ':', 
     1              KEEPLINE(PRTNMAX)(1:LEN1(KEEPLINE(PRTNMAX)) )
            END IF
            SENT(PRTNMAX) = .TRUE.
         END DO
      END IF
C
C     Close the print file so that the buffers will flush.
C
      CLOSE( UNIT=IOPT )
C
      RETURN
      END



