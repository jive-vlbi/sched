      SUBROUTINE VXSCH( )
C
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 020596 
C     This routine generates a VEX format description
C     of the experiment $SCHED section only
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      INTEGER     ISCN, ISTA, LEN1
      INTEGER     DAY1, DAY2, INTSTOP, LPOS, INPAGE
      INTEGER     YEAR, DAY, DOY, JD, MONTH, FTMIN, DATLAT
      INTEGER     TRANSTAR, TRANEND, TRANLEN, GRABSTOP
      INTEGER     I, LASTSCN, ISET, VXGTST, NTSYS(MAXSTA)
      INTEGER     NTSYSON(MAXSTA), TSYSGAP(MAXSTA)
      REAL        STASPD(MANT) 
      CHARACTER   FULTIM*18, TPSUBP*1, TMPSRC*32
      CHARACTER   DNAME*3, MNAME*3, DIRECT*1 
      CHARACTER   LINE*132, TFORM*9, TRANTYPE*10, SCANNAME*10
      CHARACTER   DISKFILE*30, STNLC*2
      DOUBLE PRECISION  STARTT, STOPT, TAPOFF
      LOGICAL     SKIPPED, WARNFS, WARNTS(MAXSTA), DATATRAN, WARNGP
      LOGICAL     GAPERR, FMTNONE, TSYSMESS, WARNTSOF(MAXSTA), WARNBANK
      REAL        STGB, SCNGAP, MINGAP
C
C     Tape information from TPDAT.
C
      INTEGER          TPPASS, TPDIR, TPINDX, TPHEAD, TPDRIV
      LOGICAL          DOTAPE, DOFAST, DOREW
C -------------------------------------------------------------
      IF (DEBUG) CALL WLOG( 1,'VXSCH: Start VEX SCHED section ')
      LINE = ' '      
      WARNFS = .FALSE.
      WARNGP = .FALSE.
      WARNBANK = .FALSE.
      DO ISTA = 1,NSTA
        WARNTSOF(ISTA) = .FALSE.
        NTSYSON(ISTA) = 0
        WARNTS(ISTA) = .FALSE.
        NTSYS(ISTA) = 0
        TSYSGAP(ISTA) = 0
      END DO
C
C     initalise STASPD, speed for stations
C
      DO ISTA=1,MANT
         STASPD(ISTA) = -1.0
      END DO
C
C     Set up a date line.
C
      CALL TIMEJ( STOPJ(1), YEAR, DOY, STOPT )
      MONTH = 1
      DAY = DOY
      CALL TDATECW( YEAR, MONTH, DAY, JD, MNAME, DNAME )
C
C     Write SCHED section add some comments in case this gets pasted
C     into VEX schedule
C
      WRITE( IVEX, '( A, A1, /, A1, 1X, A, A, /, A1, 1X, A )')  
     1     '$SCHED', SEP,
     2     COM,'schedule section for experiment ', EXPCODE, COM, EXPT
C
C     Now loop through scans. A few shortcuts are kept
C
      INPAGE = -100
      INTSTOP = 0
      STARTT = 0
      STOPT = 0
      DO ISCN = SCAN1, SCANL
C
C        If the scan was skipped, just note that fact.
C
         SKIPPED = .TRUE.
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) SKIPPED = .FALSE.
         END DO
C
C        If this Mode uses FORMAT=NONE, then skip it. This can be
C         changed (with a corresponding change in vxscns.f) whenever the FS
C         gets an automated pointing procedure.
C
         FMTNONE = .FALSE.
         ISET = VXGTST( MODSCN(ISCN) )
         IF( FORMAT(ISET)(1:4) .EQ. 'NONE' ) THEN
           FMTNONE = .TRUE.
         END IF
C
C
C        Check various tape issues, return a common tape offset
C
         CALL VXSCHK( ISCN, TAPOFF, WARNFS, WARNTS, WARNTSOF, NTSYS,
     1                  NTSYSON, TSYSGAP, WARNBANK)

C
C
         IF( SKIPPED ) THEN
            TMPSRC = SCNSRC(ISCN)
            CALL VXSTNM( TMPSRC, .FALSE.)
            WRITE( IVEX, '(A1, 4X, A, A)' ) COM, 'Skipping scan on:',
     1          TMPSRC(1:LEN1(TMPSRC))
            INPAGE = INPAGE + 1
         ELSE IF( FMTNONE ) THEN
            TMPSRC = SCNSRC(ISCN)
            CALL VXSTNM( TMPSRC, .FALSE.)
            WRITE( IVEX, '(A1, 4X, A, A, A)' ) COM, 'Skipping scan ',
     1          'with FORMAT=NONE on:',
     2          TMPSRC(1:LEN1(TMPSRC))
            INPAGE = INPAGE + 1
         ELSE
C  
C           Now process a good scan.
C           First write scanname, probably easiest and useful as number
C
            WRITE( SCANNAME, '( A2, I4.4 )' ) 'No', ISCN
            WRITE( IVEX, '( A, 1X, A, A, A1 )' )
     1           'scan', SCANNAME(1:LEN1(SCANNAME)), SEP
            INPAGE = INPAGE + 1
C
C           Maybe there's a comment
C
            IF (ANNOT(ISCN) .NE. ' ' ) THEN
               WRITE( IVEX, '( A1, 5X, A )' ) COM, 
     1             'Note a COMMENT was inserted during scheduling: '
               WRITE( IVEX, '( A1, 7X, A )' ) COM, ANNOT(ISCN)
            END IF
C
C           May move scan time fwd for tapestart: then write comment
C
            IF ( ABS(TAPOFF*86400d0) .GT. 1 ) THEN 
               CALL TIMEJ( STARTJ(ISCN), YEAR, DAY1, STARTT )
               WRITE( FULTIM(1:5), '( I4, A1 )' ) YEAR,'y'
               WRITE( FULTIM(6:9), '( I3.3, A1 )' ) DAY1,'d'
               FULTIM(10:18) = TFORM( STARTT, 'T', 0, 2, 2, 'hms' )

               WRITE( LINE, '( A1, 4X, A, A, A, A )' ) 
     1             COM, 'start=', FULTIM, ' <= original start, ',
     2             'modified for tape start.'

               WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
               INPAGE = INPAGE + 1
            END IF
C                  
C           Get scan times
C
            CALL TIMEJ( (STARTJ(ISCN)-TAPOFF), YEAR, DAY1, STARTT )
            CALL TIMEJ( STOPJ(ISCN), YEAR, DAY2, STOPT )
            WRITE( FULTIM(1:5), '( I4, A1 )' ) YEAR,'y'
            WRITE( FULTIM(6:9), '( I3.3, A1 )' ) DAY1,'d'
            FULTIM(10:18) = TFORM( STARTT, 'T', 0, 2, 2, 'hms' )
C        
            WRITE( LINE, '( 5X, A, A, A1 )' ) 
     1          'start=', FULTIM, SEP
C            
C           write mode, why not use 1 line for compactness
C
            LPOS = LEN1(LINE)+1
            WRITE( LINE(LPOS:) , '( 1X, A, A, A1 )' ) 'mode=',
     1          MDLINK(MODSCN(ISCN))(1:LEN1(MDLINK(MODSCN(ISCN)))), 
     2          SEP
C  
C           Write source
C
            LPOS = LEN1(LINE)+1
            TMPSRC = SCNSRC(ISCN)
            CALL VXSTNM( TMPSRC, .FALSE. )
            WRITE( LINE(LPOS:) , '( 1X, A, A, A1 )' ) 'source=',
     1          TMPSRC(1:LEN1(TMPSRC)), SEP
C
C           Flush line
C
            WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
            INPAGE = INPAGE + 1
C
C           Write the data_transfer section, if appropriate
C
            DATATRAN = .FALSE.
C
C           first, a sanity check
C
            IF( GRABTO(ISCN) .EQ. 'FILE' .AND. DATAPATH(ISCN) .EQ.
     1                  'IN2NET' ) THEN
              CALL PRTSCN( ISCN )
              WRITE ( MSGTXT, '(A, A, A)' ) 
     1          'VXSCH: You have requested a GRABTO (ftp) scan, but ',
     2          'you are  not recording to disk. You must set ',
     3          'DATAPATH=IN2DISK'
              CALL ERRLOG ( MSGTXT )
            END IF
            IF( GRABTO(ISCN) .EQ. 'NET' ) THEN
              CALL PRTSCN( ISCN )
              WRITE ( MSGTXT, '(A, A)' ) 
     1          'VXSCH: You have requested GRABTO=NET, but ',
     2          'that is not supported in VEX and will be ignored. '
              CALL WLOG ( 1, MSGTXT )
            END IF
C
C           set things up for an ftp scan
C
            IF( GRABTO(ISCN) .EQ. 'FILE' ) THEN
C              ftp scans
               WRITE( IVEX, '( A, A )' ) COM, ' ftp scan'
               DATATRAN = .TRUE.
               TRANTYPE = 'disk2file'
               INTSTOP = NINT( ( STOPJ(ISCN) - 
     .             (STARTJ(ISCN)-TAPOFF) ) 
     1             * 86400d0)
C              default TRANLEN should be 30 seconds               
               TRANLEN = NINT(GRABTIME(1,ISCN) )
               IF ( TRANLEN .LE. 0) TRANLEN = 30
C              default TRANEND should be 10 seconds from end of scan
               GRABSTOP = NINT (GRABTIME(2,ISCN))
               IF (GRABSTOP .LE. 0) GRABSTOP = 10
               TRANSTAR = INTSTOP - (TRANLEN + GRABSTOP)
               TRANEND = INTSTOP - GRABSTOP
C              check the transfer time is consistent with the scan length
               IF (TRANSTAR .LT. 0 .OR. TRANEND .LT. 0 .OR. 
     1                  TRANSTAR .GT. TRANEND) THEN
                 CALL WRTMSG ('VXSCH', 'vexgrabtime')
                 CALL PRTSCN( ISCN )
                 WRITE( MSGTXT, '( A, A, A, A )' )
     1              'VXSCH:   WARNING: You have scheduled a GRABTO ',
     2              'scan, but the GRABTIME is not consistent with ',
     3              'the scan length. Please increase the scan length ',
     4              'or change the GRABTIME parameters.'
                 CALL ERRLOG( MSGTXT )
               END IF
            END IF
C
C           set things up for an evlbi scan
C
            IF( DATAPATH(ISCN) .EQ. 'IN2NET' ) THEN
C              evlbi scans
               WRITE( IVEX, '( A, A )' ) COM, ' eVLBI scan'
               DATATRAN = .TRUE.
               TRANTYPE = 'in2net'
               INTSTOP = NINT( ( STOPJ(ISCN) - 
     .             (STARTJ(ISCN)-TAPOFF) ) 
     1             * 86400d0)
C              for evlbi always transfer whole scan, so don't fill in the
C               transfer times
C               TRANLEN = NINT(GRABTIME(1,ISCN) )
C               TRANLEN = INTSTOP
C               TRANSTAR = INTSTOP - TRANLEN 
C               TRANEND = INTSTOP 
            END IF
C           Write the data_transfer statements
            IF ( DATATRAN ) THEN
C
C              Make sure that we are not continuous recording from
C              the previous scan. Insist on a 10 second gap (this can
C              be shorter when we get rid of adaptive tape motion).
C
               SCNGAP = REAL( ( STARTJ(ISCN) - TAPOFF 
     1                       - STOPJ (ISCN-1) ) * 86400.d0)
               IF ( GRABTO(ISCN) .EQ. 'FILE' ) THEN
                 MINGAP = 11.0
               END IF
               IF ( DATAPATH(ISCN) .EQ. 'IN2NET' ) THEN
                 IF ( ISCN .GT. SCAN1 .AND. DATAPATH(ISCN) .NE.
     1                          DATAPATH(ISCN-1) ) THEN
                   MINGAP = 11.0
                 ELSE
                   MINGAP = 0.0
                 END IF
               END IF
C                   
               IF ( ISCN .GT. SCAN1 .AND. SCNGAP .LT. MINGAP ) THEN
                 CALL PRTSCN ( ISCN )
                 WRITE ( MSGTXT, '(A, A, A, I4, A)' )
     1           'VXSCH: You have scheduled an ftp or eVLBI ',
     2           'scan as part of continuous recording - you must ',
     3           'leave a ', NINT(MINGAP), 
     4           ' second gap before the scan.'
                 CALL ERRLOG ( MSGTXT )
               END IF
C
C              write the data transfer statements for each station
C
               DO ISTA = 1, NSTA
C
C                 check if enough time is available at the end of the scan
C                 to do the ftp data transfer - print warning if not
C
                  IF( GRABTO(ISCN) .EQ. 'FILE' .AND. ISCN .LT. SCANL) 
     1                                                          THEN
                     CALL VXTRAN ( TRANLEN, ISCN, ISTA, GAPERR )
                     IF (GAPERR) WARNGP = .TRUE.
                  END IF
                  LINE = ' '
                  LPOS = 6
                  IF( STASCN(ISCN,ISTA) ) THEN
C                   Form the file name
                    IF( GRABTO(ISCN) .EQ. 'FILE' ) THEN
                      WRITE( STNLC, '(A)') STCODE(STANUM(ISTA))(1:2)
                      CALL DWCASE( STNLC )
                      WRITE( DISKFILE, '( A, A1, A2, A1, A, A1, A3 )' )
     1                   EXPCODE(1:LEN1(EXPCODE)), '_', 
     2                   STNLC(1:2), '_', 
     3                   SCANNAME(1:LEN1(SCANNAME)), '.', 'm5a'
                     ELSE
                       WRITE( DISKFILE, '( 1X )' )
                     END IF
C                       
                     WRITE( LINE(LPOS:LPOS+14), '( A )' ) 
     1                      'data_transfer='
                     LPOS = LEN1(LINE) + 1
                     WRITE( LINE(LPOS:LPOS+3), '( A2, A1 )' ) 
     1                    STCODE(STANUM(ISTA))(1:2), COL
                     LPOS = LEN1(LINE) + 1
                     WRITE( LINE(LPOS:LPOS+12), '( 1X, A, A1 )' ) 
     1                    TRANTYPE(1:LEN1(TRANTYPE)), COL
                     LPOS = LEN1(LINE) + 1
                     WRITE( LINE(LPOS:), '( 1X, A, A1 )' ) 
     1                    DISKFILE(1:LEN1(DISKFILE)), COL
                     LPOS = LEN1(LINE) + 1
                     IF( GRABTO(ISCN) .EQ. 'FILE' ) THEN
                       WRITE( LINE(LPOS:), '( I5, 1X, A, A1 )' ) 
     1                      TRANSTAR, 'sec', COL
                     ELSE IF( DATAPATH(ISCN) .EQ. 'IN2NET' ) THEN
                       WRITE( LINE(LPOS:), '( 1X, A1 )' ) COL
                     END IF
                     LPOS = LEN1(LINE) + 1
                     IF( GRABTO(ISCN) .EQ. 'FILE' ) THEN
                       WRITE( LINE(LPOS:), '( I5, 1X, A, A1 )' ) 
     1                      TRANEND, 'sec', COL
                     ELSE IF( DATAPATH(ISCN) .EQ. 'IN2NET' ) THEN
                       WRITE( LINE(LPOS:), '( 1X, A1 )' ) COL
                     END IF
                     LPOS = LEN1(LINE) + 1
C                    for now, the 'options' field is always blank. 
                     WRITE( LINE(LPOS:), '( 1X, A1 )' ) 
     1                    SEP
C                     LPOS = LEN1(LINE) + 1
                  END IF
                  WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
               END DO
            END IF
C     
C           Write the antenna based data, maybe start with header
C     
            IF( INPAGE .GT. 78 ) INPAGE = 0
            IF( INPAGE .LE. 0 ) THEN
               WRITE(IVEX, '( A1, 14X, A1, A9, A1, A9, A1, A9, A1, 
     1             A5, A1, A7, A1, A4, A1, A7 )' )
     2             COM, COL, 'data_good', COL, 'data_stop', COL, 
     3             'goto_foot', COL,
     4             ' pass', COL, '  wrap ', COL, 'driv',COL, 'tape at'
               INPAGE = 1
            END IF
            DO ISTA = 1, NSTA
C
               IF( STASCN(ISCN,ISTA) ) THEN
C     
C                 Write this in fixed format use LINE
C
                  LINE = ' '
                  LPOS = 6
                  WRITE( LINE(LPOS:LPOS+7), '( A )' ) 'station='
C
C                 Station code
C                     
                  LPOS = LEN1(LINE) + 1
                  WRITE( LINE(LPOS:LPOS+2), '( A2, A1 )' )  
     1                 STCODE(STANUM(ISTA))(1:2), COL
C
C                 next is start time in job with good data
C
                  LPOS = LEN1(LINE) + 1
C
C                 estimate good data start based on slewing
C
                  DATLAT = IDNINT( ( TONSRC(ISCN,ISTA) - 
     .                (STARTJ(ISCN) - TAPOFF) ) 
     1                * 86400D0 )
                  IF( DATLAT .LE. 0 ) DATLAT = 0
C
C                 Write INTSTOP, last bit af good data 
C
                  INTSTOP = NINT( ( STOPJ(ISCN) - 
     .                (STARTJ(ISCN)-TAPOFF) ) 
     1                * 86400d0)
                  DATLAT = MIN( DATLAT, INTSTOP )
                  WRITE( LINE(LPOS:LPOS+9), '( I5, A4, A1 )' ) DATLAT,
     1                ' sec', COL
                  LPOS = LEN1(LINE) + 1
                  WRITE( LINE(LPOS:LPOS+9), '( I5, A4, A1 )' ) INTSTOP,
     1                ' sec', COL
C
C                 Extract the tape commands from PTDAT.
C
                  CALL TPPACK( 'UNPACK', TPDAT(1,ISCN,ISTA), DOTAPE, 
     1                DOFAST, DOREW, TPPASS, TPDRIV, TPDIR, 
     2                TPINDX, TPHEAD )
C
                  LPOS = LEN1(LINE) + 1
C
C                 Assumes tape speed does not change for S2!
C
                  IF ( RECORDER(STANUM(ISTA)) .EQ. 'S2' ) THEN 
C
C                    use STASPD to get usage, is it set? 
C
                     IF( STASPD(ISTA) .LT. 0. ) THEN
C
C                    determine density and then speed
C
                        IF( DENSITY(ISTA) .EQ. 'H' ) THEN
                           STASPD(ISTA) = SPEEDH(NSETUP(ISCN,ISTA))
                        ELSE
                           STASPD(ISTA) = SPEEDL(NSETUP(ISCN,ISTA))
                        END IF
                     END IF
C
C                    multiple with 1 feet/min in units of ips
C
                     FTMIN = NINT( TPFOOT1(ISCN,ISTA)/
     1                   (5.0 * STASPD(ISTA)) )
C
C                    Communicate tape change to FS: 
C
                     IF ( DOTAPE ) FTMIN = 0
C
                     WRITE( LINE(LPOS:LPOS+9), '( I5, A4, A1 )' )
     1                   FTMIN,' min', COL 
                  ELSE IF( USETAPE(ISTA) ) THEN
                     WRITE( LINE(LPOS:LPOS+9), '( I6, A3, A1 )' ) 
     1                      NINT(TPFOOT1(ISCN,ISTA)),' ft', COL
                  ELSE IF( USEDISK(ISTA) ) THEN
C                    Find the last scan that this station participated
C                    in and print the GB at the end of that scan.
C                    Note, STASCN(ISCN,ISTA) is a flag that indicates
C                    that station ISTA is in scan ISCN.  
                     IF( ISCN .GT. 1 ) THEN
                        LASTSCN = 0
                        DO I = SCAN1, ISCN-1
                          IF( STASCN(I, ISTA) ) LASTSCN = I
                        END DO
                        IF( LASTSCN .GT. 0 ) THEN
                          STGB = GBYTES(LASTSCN,ISTA)
                        ELSE
                          STGB = 0.0
                        END IF
                     ELSE 
                        STGB = 0.0
                     END IF
                     WRITE( LINE(LPOS:LPOS+11), '( F8.3, A3, A1 )' ) 
     1                      STGB,' GB', COL
                     
                  END IF
C
C                 next is pass and subpass
C
                  LPOS = LEN1(LINE) + 1
                  IF ( RECORDER(STANUM(ISTA)) .EQ. 'S2' ) THEN
                     WRITE( LINE(LPOS:LPOS+5), '( 3X, I1, 1X, A1 )' ) 
     1                   (TPINDX - 1),  COL
                  ELSE
                     TPSUBP = 'X'
                     IF( TPHEAD .EQ. 1) TPSUBP = 'A'
                     IF( TPHEAD .EQ. 2) TPSUBP = 'B'
                     IF( TPHEAD .EQ. 3) TPSUBP = 'C'
                     IF( TPHEAD .EQ. 4) TPSUBP = 'D'
                     IF( TPHEAD .EQ. 5) TPSUBP = 'E'
                     IF( TPHEAD .EQ. 6) TPSUBP = 'F'
                     IF( TPHEAD .EQ. 7) TPSUBP = 'G'
                     IF( TPHEAD .EQ. 8) TPSUBP = 'H'
C
                     IF( USETAPE(ISTA) ) THEN
                       WRITE( LINE(LPOS:LPOS+5), 
     1                     '( 1X, I2, A1, 1X, A1 )' ) TPINDX, TPSUBP,  
     2                     COL
                     ELSE IF( USEDISK(ISTA) ) THEN
                       WRITE( LINE(LPOS:LPOS+3), 
     1                     '( 3X, A1 )' ) COL
                     END IF
                  END IF
C
C                 pointscr not implemented leave blank
C
                  LPOS = LEN1(LINE) + 1
                  WRITE( LINE(LPOS:LPOS+7), '( A6, 1X, A1 )' ) ' ',
     1                    COL
C
C                 Write tapedrive in last column, should be head 1 day
C                 S2 is a bit special...
C                 Now set the drive correctly:
C
                  IF (RECORDER(STANUM(ISTA)) .EQ. 'S2' .AND. 
     1                TPDRIV .GT. STNDRIV(STANUM(ISTA))/8  ) THEN
C
C                    Following expr yields drive 1 for most cases
C
                     TPDRIV = ( STNDRIV(STANUM(ISTA)) + 7 ) /8 
C
C                    But let's check anyway
C
                     IF( TPDRIV .EQ. 0 .OR. TPDRIV .GT. 1 ) THEN
                        CALL PRTSCN( ISCN )
                        WRITE( MSGTXT, '( A, I2, A, A )' )
     2                      'VXSCH:   WARNING: Requesting drive No',
     3                      TPDRIV, ' for station ',
     4                      STATION(STANUM(ISTA))
                        CALL WLOG( 1, MSGTXT )
                     END IF
                  END IF 
C
C                 agreed with NRV that norec can be drive 0
C
                  IF( NOREC(ISCN) ) TPDRIV = 0
                  LPOS = LEN1(LINE) + 1
                  WRITE( LINE(LPOS:LPOS+2), '( 1X, I1, A1 )' ) TPDRIV,
     1                 SEP
C
C                 write direction and footage
C
                  IF( USETAPE(ISTA) ) THEN
                     DIRECT = 'R'
                     IF( TPDIR .EQ. 1) DIRECT = 'F'
                     LPOS = LEN1(LINE) + 1
                     WRITE( LINE(LPOS:LPOS+10), 
     1                   '( 1X, A1, 1X, A1, A1, I5.5 )' )
     2                   COM, DIRECT, '@', 
     3                   NINT(TPFOOT1(ISCN,ISTA))
                  END IF
C
C                 flush line
C
                  WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
                  INPAGE = INPAGE + 1
C     
               END IF
            END DO
            WRITE( IVEX, '( A, A1 )' ) 'endscan', SEP
            INPAGE = INPAGE + 1
C
         END IF
      END DO
C
C     Print warning about frequency of Tsys. 
      TSYSMESS = .FALSE.
      DO ISTA = 1, NSTA
         IF( WARNTS(ISTA) ) THEN
            WRITE( MSGTXT, '( A, A, I5, A , I4, A)' ) 
     2         STATION(STANUM(ISTA)), ' has ',
     3         NTSYS(ISTA), 
     4         ' Tsys measurements. Maximum interval = ',
     5         TSYSGAP(ISTA), ' minutes.'
            CALL WLOG( 1, MSGTXT )
            TSYSMESS = .TRUE.
         END IF
      END DO
      IF( TSYSMESS ) THEN
         CALL WLOG( 1,'VXSCH: Tsys calibration at MkIV stations is '//
     1       'taken during every gap in recording, but these '//
     2       'appear over 15 min apart for the stations listed above!')
         CALL WLOG( 1,'       This can be improved by inserting'//
     1       ' gaps at regular intervals. ')
         CALL WRTMSG( 'VXSCH', 'tsysgap' )
      END IF
C
      TSYSMESS = .FALSE.
      DO ISTA = 1, NSTA
         IF( WARNTSOF(ISTA) ) THEN
            WRITE( MSGTXT, '( A, A, I5, A, I5, A )' ) 
     1         STATION(STANUM(ISTA)), ': only ',
     2         NTSYSON(ISTA), ' out of ', NTSYS(ISTA),
     3         ' Tsys measurements are on-source'
            CALL WLOG( 1, MSGTXT )
            TSYSMESS = .TRUE.
         ENDIF
      END DO
      IF( TSYSMESS ) THEN
         CALL WLOG( 1, 'VXSCH: Stations listed above are affected ' //
     1        'by slewing during Tsys calibration')
         CALL WRTMSG( 'VXSCH', 'tsysoffsrc' )
      END IF
C
      IF( WARNFS ) 
     1    CALL WLOG( 1,'VXSCH: WARNING: Scan timing problem '//
     2    'for PCFS, this VEX will NOT run!!!!')
      IF( WARNGP ) THEN
         CALL WRTMSG( 'VXSCH', 'ftpgap' )
      END IF
C
      IF( WARNBANK ) THEN
          CALL WLOG( 1, ' ')
          CALL WLOG( 1, '!!!!!!!!!!!!!!!!!!!!!')
          CALL WLOG( 1, 'VXSCH: WARNING: one or more of  ' //
     1          'your stations has continuous recording ' //
     2          'for more than 90 minutes. Disk packs can ' //
     3          'only be changed during gaps in recording which ' //
     4          'should be much more frequent than this. Please add ' //
     5          'some gaps to your schedule!' )
          CALL WLOG( 1, '!!!!!!!!!!!!!!!!!!!!!')
      END IF
C
      RETURN
      END
