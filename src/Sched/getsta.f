      SUBROUTINE GETSTA( ISCN, VALUE, KC, KI, GOTVEX )
C
C     Subroutine for SCHED, called by SCHIN, that deals with station
C     information.
C
C     It accumulates the list of stations in the schedule and 
C     associates them with catalog entries.  It also
C     sets the logical, STASCN(ISCN,ISTA), that says that station 
C     ISTA in the internal stations arrays is used in scan ISCN.
C
C     It filters stations based on the DOSTA request.
C
C     It also gets the station dependent requests for tape 
C     motions such as changes, rewinds etc.
C
      INCLUDE 'sched.inc'
C
      INTEGER          ISCN, ISTA, KSTA, INSTA, KI(*), I1, LEN1
      INTEGER          KEYPTR
      DOUBLE PRECISION VALUE(*)
      LOGICAL          ERRS, FLAG(MAXSTA), DOIT, GOTVEX
      CHARACTER        NAMEST*8, KC(*)*(*)
      CHARACTER        KCHAR*256, FILEUP*80
      CHARACTER        LSTAFIL*80
      SAVE             LSTAFIL
C -------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. 3 ) CALL WLOG( 1, 'GETSTA: starting.' )
      IF( ISCN .EQ. 1 ) LSTAFIL = ' '
C
C     Read the station catalog if haven't done it before.  Note
C     that the LOCAFILE (LOCFILE in inputs and rdcat.inc) is a
C     file that can contain the station location information.  It may
C     not be needed because the stations file can also contain that
C     information.  RDSTA will only complain about problems with the
C     LOCFILE if it can't find the locations in the stations file.
C
      STAFILE = KCHAR( 'STAFILE', 80, .FALSE., VALUE, KC, KI )
      LOCAFILE = KCHAR( 'LOCFILE', 80, .FALSE., VALUE, KC, KI )
      CALL ENVIR( STAFILE )
      CALL ENVIR( LOCAFILE )
      FILEUP = STAFILE
      CALL UPCASE( FILEUP )
      IF( ( FILEUP .NE. 'NONE' .OR. LEN1(STAFILE) .NE. 4 ) .AND. 
     1      STAFILE .NE. LSTAFIL ) THEN
C
         CALL STREAD( IUSTA, .TRUE. )
C
      END IF
      LSTAFIL = STAFILE
C
C     Get station request for this scan.
C
      I1 = KEYPTR( 'STATions', KC, KI )
      IF( ISCN .EQ. 1 .AND. VALUE(I1) .EQ. 0.D0 ) THEN
C
C        No stations are specified for first scan.  Abort. 
C
         CALL ERRLOG( 'GETSTA: No stations specified!' )
C
      ELSE IF( VALUE(I1) .EQ. 0.D0 ) THEN      
C
C        No new stations are specified, default to last scan.
C
         DO ISTA = 1, MAXSTA
            STASCN(ISCN,ISTA) = STASCN(ISCN-1,ISTA)
         END DO
C
      ELSE  
C 
C        New stations given.  Note that, if any new stations are
C        listed, the whole list must be given.
C
         DO ISTA = 1, MAXSTA
            STASCN(ISCN,ISTA) = .FALSE.
         END DO
         DO INSTA =1, MAXSTA
            IF( VALUE(I1-1+INSTA) .NE. 0.D0 ) THEN  
C
C              Decode station.
C
               WRITE( NAMEST, '(A8)' ) VALUE(I1-1+INSTA)
               CALL UPCASE( NAMEST )
C
C              Find in catalog and station list.
C              Note - KSTA is catalog index, ISTA is STANAME index.
C
               CALL STANO( NAMEST, KSTA, ISTA, DOIT )
C
C              Only think about stations that pass the DOSTA test.
C
               IF( DOIT ) THEN
C
C                 Treat station not found.
C
                  IF( KSTA .EQ. 0 ) THEN
                     CALL WLOG( 1, 'GETSTA: Station '//NAMEST//' 
     1                   not found in catalogs' )
                     CALL ERRLOG( 'GETSTA: Note -- specify station '//
     1                   'catalog before end of first scan input.' )
                  END IF
C
C                 Treat new station.
C
                  IF( ISTA .EQ. 0 ) THEN
                     NSTA = NSTA + 1
                     IF( NSTA .GT. MAXSTA ) 
     1                  CALL ERRLOG('SCHIN: Too many stations!')
C
                     ISTA = NSTA
                     STANAME(ISTA) = STATION(KSTA)
C
C                    Get the pointers.  
C                    STANUM points from schedule station ISTA to 
C                      catalog station KSTA
C                    ISCHSTA points from catalog station KSTA to
C                      schedule station ISTA.
C                    
                     STANUM(ISTA) = KSTA
                     ISCHSTA(KSTA) = ISTA
C
C                    Record if a VEX file will be needed.
C
                     IF( CONTROL(STANUM(ISTA)) .EQ. 'VEX' ) 
     1                    GOTVEX = .TRUE.
C
C                    Record if a DRUDG file is needed for VSOP.
C
                     IF( CONTROL(STANUM(ISTA)) .EQ. 'VSOP' ) 
     1                    DOVSOP = .TRUE.
C
C                    Record if all stations are VLBA stations.
C                    If so, we can assume some things are done
C                    that might not be done elsewhere.
C
                     IF( STANAME(ISTA)(1:4) .NE. 'VLBA' )
     1                  ALLVLBA = .FALSE.
C
                  END IF
C
C                 Add the station to the scan.
C
                  STASCN(ISCN,ISTA) = .TRUE. 
C
               END IF
C
            END IF
         END DO
      END IF
C
C
C     Now deal with the station dependent tape requests.
C     Note cannot put TAPE in the call because of subscript order.
C
C     First: Tape change.
C
      ERRS = .FALSE.
C
      I1 = KEYPTR( 'TAPE', KC, KI )
      CALL TAPEFLAG( ISCN, 'Tape change', FLAG, VALUE(I1), ERRS )
      DO ISTA = 1, NSTA
         TAPE(ISCN,ISTA) = FLAG(ISTA)
      END DO
C
C     Rewind:
C
      I1 = KEYPTR( 'REWIND', KC, KI )
      CALL TAPEFLAG( ISCN, 'Rewind', FLAG, VALUE(I1), ERRS )
      DO ISTA = 1, NSTA
         REWIND(ISCN,ISTA) = FLAG(ISTA)
      END DO
C
C     Fast forward
C
      I1 = KEYPTR( 'FASTFOR', KC, KI )
      CALL TAPEFLAG( ISCN, 'Fast forward', FLAG, VALUE(I1), ERRS )
      DO ISTA = 1, NSTA
         FASTF(ISCN,ISTA) = FLAG(ISTA)
      END DO
C
C     Reverse
C
      I1 = KEYPTR( 'REVERSE', KC, KI )
      CALL TAPEFLAG( ISCN, 'Reverse', FLAG, VALUE(I1), ERRS )
      DO ISTA = 1, NSTA
         REVERSE(ISCN,ISTA) = FLAG(ISTA)
      END DO
C
      IF( ERRS ) CALL ERRLOG( 'GETSTA: Error in tape motion requests ' )
C
      RETURN
      END
