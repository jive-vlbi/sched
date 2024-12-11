      SUBROUTINE VXSCH( )
C
C     Routine specific for the VEX extension of SCHED. 
C     By H.J. van Langevelde, JIVE, 020596 
C     This routine generates a VEX format description
C     of the experiment $SCHED section only
C
C     Deleting tape stuff, including S2.  July 20, 2010  RCW.
C     Copy of original kept in sched_ARCHIVE_nonSVN/obsolete_routines/
C     and also in SVN versions 294 and earlier.
C
C     When USEDISK is false since tape was taken out, some elements of 
C     the lines were not written, causing parsing errors on read.  
C     fixed  Nov. 15, 2011  RCW.
C
C     Be selective about skipping FORMAT=NONE scans.  For now, do not
C     do so for VLBA stations.  Perhaps add more exceptions later,
C     or make it a stations.dat parameter, or make the Field System
C     happy with such scans.  RCW  Dec 8, 2011
C
C     Switch to use of TSCAL for warnings about calibration time.
C     Nov. 4, 2013  RCW.
C
C     Add the wrap zone specification, but block it temporarily
C     for most stations.  Jan 2014.  RCW.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
      INCLUDE 'vxlink.inc'
C
      INTEGER     ISCN, ISTA, LEN1, NCHR, INT, ILEN
      INTEGER     MCH, ICH, JCH1, JCH2
      INTEGER     DAY1, INTSTOP, LPOS, INPAGE
      INTEGER     YEAR, DATLAT
      INTEGER     TRANSTAR, TRANEND, TRANLEN, GRABSTOP
      INTEGER     I, LASTSCN, KS, NTSYS(MAXSTA)
      INTEGER     NTSYSON(MAXSTA), TSYSGAP(MAXSTA), TPDRIV
      REAL        STASPD(MANT) 
      CHARACTER   FULTIM*18, TMPSRC*32
      CHARACTER   LINE*132, TFORM*9, TRANTYPE*10, SCANNAME*10
      CHARACTER   DISKFILE*30, STNLC*2, ZONE*5
      DOUBLE PRECISION  STARTT, TAPOFF, IDAY
      LOGICAL     SKIPPED, WARNFS, WARNTS(MAXSTA), DATATRAN, WARNGP
      LOGICAL     GAPERR, SKIPNONE, TSYSMESS, WARNTSOF(MAXSTA)
      LOGICAL     ANYNONE, WARNBANK, OLDWARNB
      REAL        STGB, SCNGAP, MINGAP
C -------------------------------------------------------------
      IF (DEBUG) CALL WLOG( 1,'VXSCH: Start VEX SCHED section ')
      LINE = ' '      
      WARNFS = .FALSE.
      WARNGP = .FALSE.
      WARNBANK = .FALSE.
      OLDWARNB = .FALSE.
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
C     Old code called TIMEJ and TDATECW to set up a date line.  
C     But then the results were never used.  Removed Jan. 2, 2013
C
C     Write SCHED section add some comments in case this gets pasted
C     into VEX schedule
C
      WRITE( IVEX, '( A, A1, /, A1, 1X, A, A, /, A1, 1X, A )')  
     1     '$SCHED', SEP,
     2     COM,'schedule section for experiment ', EXPCODE, COM, EXPT
C
C     Now loop through scans. A few shortcuts are kept
C     RCW Jan. 2, 2013: Removed unneeded STARTT and STOPT initializations.
C     INTSTOP initialization probably not needed.  
C
      INPAGE = -100
      INTSTOP = 0
      DO ISCN = SCAN1, SCANL
C
C        If the scan was skipped, just note that fact.
C        This is likely to be because of some SCHED optimization mode.
C
         SKIPPED = .TRUE.
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) SKIPPED = .FALSE.
         END DO
C
C        Skip the scan if FORMAT=NONE and no stations need to keep
C        such scans (set SKIPNONE=.TRUE.).  SKIPNONE should end up 
C        false for any scan with a different format or for a scan that
C        includes stations, such as the VLBA, that needs such scans.
C        Such scans include reference pointing scans.
C        ANYNONE records whether the FORMAT=NONE regardless of whether
C        the scan will be retained.  It triggers a comment.
C
         SKIPNONE = .TRUE.
         ANYNONE = .FALSE.
         DO ISTA = 1, NSTA
            IF( STASCN(ISCN,ISTA) ) THEN  
               KS = NSETUP(ISCN,ISTA)
               IF( FORMAT(KS)(1:4) .NE. 'NONE' .OR. 
     1             STANAME(ISTA)(1:4) .EQ. 'VLBA'   ) THEN
                  SKIPNONE = .FALSE.
               END IF
               IF( FORMAT(KS)(1:4) .EQ. 'NONE' ) 
     1             ANYNONE = .TRUE.
            END IF
         END DO
C
C
C        Check various tape issues, return a common tape offset
C
         CALL VXSCHK( ISCN, TAPOFF, WARNFS, WARNTS, WARNTSOF, NTSYS,
     1                  NTSYSON, TSYSGAP, WARNBANK)
C        Let the user know which was the first scan to raise warnbank
         IF (.NOT. OLDWARNB .AND. WARNBANK) THEN
            CALL WLOG(0, ' ' )
            WRITE( MSGTXT, '(A)' )
     1         'VXSCH: The scan detailed below has exceeded the ' //
     2         'limit for continuous recording. Insert a gap ' //
     3         'before this scan, or reduce its length if necessary:'
            CALL WLOG(0, MSGTXT )
            CALL PRTSCN( ISCN, 'VXSCH' )
            CALL WLOG(0, ' ' )
         END IF
         OLDWARNB = WARNBANK
C
C
         IF( SKIPPED ) THEN
            TMPSRC = SCNSRC(ISCN)
            CALL VXSTNM( TMPSRC, .FALSE.)
            WRITE( IVEX, '(A1, 4X, A, A)' ) COM, 'Skipping scan on:',
     1          TMPSRC(1:LEN1(TMPSRC))
            INPAGE = INPAGE + 1
         ELSE IF( SKIPNONE ) THEN
C
C           Skip a scan with FORMAT=NONE if there are no stations that 
C           need to keep such scans, such as the VLBA.
C
            TMPSRC = SCNSRC(ISCN)
            CALL VXSTNM( TMPSRC, .FALSE.)
            MSGTXT = ' '
            WRITE( MSGTXT, '(A1, 4X, A, A, A)' ) COM, 'Skipping scan ',
     1          'with FORMAT=NONE on:',
     2          TMPSRC(1:LEN1(TMPSRC))
            WRITE( IVEX, '(A)' ) MSGTXT(1:LEN1(MSGTXT))
            INPAGE = INPAGE + 1
         ELSE
C  
C           Now process a good scan.
C           First write scanname, probably easiest and useful as number
C           Allow for more than 10000 - highly unlikely but just possible
C           with a 24 hr schedule of 15 second scans and using WRAP24
C           which turns it into a 48 hour schedule.
C
            IF( ISCN .LE. 9999 ) THEN
               WRITE( SCANNAME, '( A2, I4.4 )' ) 'No', ISCN
            ELSE
               WRITE( SCANNAME, '( A2, I5.5 )' ) 'No', ISCN
            END IF
            WRITE( IVEX, '( A, 1X, A, A, A1 )' )
     1           'scan', SCANNAME(1:LEN1(SCANNAME)), SEP
            INPAGE = INPAGE + 1
C
C           Maybe there's a comment.  DRUDG has a maximum line length
C           of 128 including the comment character.  ANNOT is 128
C           characters long and it is printed in the vex file with
C           8 other characters in front.  If needed, break the line.
C           Try to break at a word.
C
            IF (ANNOT(ISCN) .NE. ' ' ) THEN
               JCH1 = -1
               NCHR = LEN1( ANNOT(ISCN) )
               IF( NCHR .LE. 120 ) THEN
                  MCH = NCHR
                  JCH1 = 0
                  JCH2 = 0
               ELSE
                  DO ICH = 120, NCHR - 120 + 1, -1
                     IF( ANNOT(ISCN)(ICH:ICH) .EQ. ' ' ) THEN
                        MCH = ICH
                        JCH1 = ICH + 1
                        JCH2 = NCHR
                        GO TO 200
                     END IF
                  END DO
  200             CONTINUE
                  IF( JCH1 .EQ. -1 ) THEN
                     MCH = 120
                     JCH1 = 121
                     JCH2 = NCHR
                  END IF
               END IF
               IF( ANNOT(ISCN) .NE. ' ' ) THEN
                  WRITE( IVEX, '( A1, 5X, A )' ) COM, 
     1               'Note a COMMENT was inserted during scheduling: '
                  WRITE( IVEX, '( A1, 7X, A )' ) COM,
     1                      ANNOT(ISCN)(1:MCH)
                  INPAGE = INPAGE + 1
                  IF( JCH1 .GT. 1 ) THEN
                     WRITE( IVEX, '( A1, 10X, A )' ) COM, 
     1                      ANNOT(ISCN)(JCH1:JCH2)
                     INPAGE = INPAGE + 1
                  END IF
               END IF
            END IF
C
C           Add a comment for FORMAT=NONE scans.
C
            IF( ANYNONE ) THEN
               WRITE( IVEX, '( A1, 5X, A )' ) COM,
     1            'This is a FORMAT=NONE, non-recording scan.'
               INPAGE = INPAGE + 1
            END IF
C
C           Add any intents.  This is as comments for now.
C
            IF( NSCINT(ISCN) .GE. 1 ) THEN
               DO INT = 1, NSCINT(ISCN)
                  LINE = ' '
                  ILEN = LEN1( INTENT(ISCINT(INT,ISCN)) )
                  WRITE( LINE, '( 5A )' ) COM,
     1                ' intent = ', QOT, 
     2                INTENT(ISCINT(INT,ISCN))(1:ILEN), QOT
                  WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
                  INPAGE = INPAGE + 1
               END DO
C
C              For VLA phasing scans, add the phasing subscan length.
C              This is here because there is a scan dependent argument
C              in the INTENT.
C
               IF( PHASING(ISCN) ) THEN
                  WRITE( LINE, '( 4A, I3.3, A )' ) COM,
     1                ' intent = ', QOT, 
     2                'VLA:PHASE_SUBSCAN=', VLAPTIME(ISCN), QOT
                  WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
                  INPAGE = INPAGE + 1
               END IF
            END IF
C
C           May move scan time fwd for tapestart: then write comment
C
C            print*, 'tapoff=',ABS(TAPOFF*86400d0) 
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
C           Get scan times.  Be sure to use the start time year, day
C           etc.  RCW Jan. 2, 2013 after problem at year boundary.
C           There was a call to TIMEJ for the STOPJ too, but the output
C           wasn't used for anything so it was removed.
C           
C
            CALL TIMEJ( (STARTJ(ISCN)-TAPOFF), YEAR, DAY1, STARTT )
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
              CALL PRTSCN( ISCN, 'VXSCH' )
              WRITE ( MSGTXT, '(A, A, A)' ) 
     1          'VXSCH: You have requested a GRABTO (ftp) scan, but ',
     2          'you are  not recording to disk. You must set ',
     3          'DATAPATH=IN2DISK'
              CALL ERRLOG ( MSGTXT )
            END IF
            IF( GRABTO(ISCN) .EQ. 'NET' ) THEN
              CALL PRTSCN( ISCN, 'VXSCH' )
              WRITE ( MSGTXT, '(A, A)' ) 
     1          'VXSCH: You have requested GRABTO=NET, but ',
     2          'that is not supported in VEX and will be ignored. '
              CALL WLOG ( 1, MSGTXT )
            END IF
C
C           set things up for an ftp scan
C           Note default on read (from inmain.f) for GRABTIMEs 1 and 2
C           is UNSET = -9999.D0.  Here test against LT zero. This was 
C           to allow GRABTIME(2) to be zero which didn't work before.  
C           Feb. 20, 2013  RCW.
C           
C           In defaults.f, the GRABTIME=30,10 defaults were set when 
C           GRABTO is not "NONE" and DATAPATH is IN2DISK so there
C           may be some duplication of effort going on.
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
               IF (GRABSTOP .LT. 0) GRABSTOP = 10
               TRANSTAR = INTSTOP - (TRANLEN + GRABSTOP)
               TRANEND = INTSTOP - GRABSTOP
C              check the transfer time is consistent with the scan length
               IF (TRANSTAR .LT. 0 .OR. TRANEND .LT. 0 .OR. 
     1                  TRANSTAR .GT. TRANEND) THEN
                 CALL WRTMSG( 0, 'VXSCH', 'vexgrabtime')
                 CALL PRTSCN( ISCN, 'VXSCH' )
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
               IF( ISCN .GT. SCAN1 ) THEN
                  SCNGAP = REAL( ( STARTJ(ISCN) - TAPOFF 
     1                          - STOPJ (ISCN-1) ) * 86400.d0)
                  IF ( GRABTO(ISCN) .EQ. 'FILE' ) THEN
                    MINGAP = 11.0
                  END IF
                  IF ( DATAPATH(ISCN) .EQ. 'IN2NET' ) THEN
                    IF ( ISCN .GT. SCAN1 .AND. DATAPATH(ISCN) .NE.
     1                             DATAPATH(ISCN-1) ) THEN
                      MINGAP = 11.0
                    ELSE
                      MINGAP = 0.0
                    END IF
                  END IF
C                      
                  IF ( ISCN .GT. SCAN1 .AND. SCNGAP .LT. MINGAP ) THEN
                    CALL PRTSCN ( ISCN, 'VXSCH' )
                    WRITE ( MSGTXT, '(A, A, A, I4, A)' )
     1              'VXSCH: You have scheduled an ftp or eVLBI ',
     2              'scan as part of continuous recording - you must ',
     3              'leave a ', NINT(MINGAP), 
     4              ' second gap before the scan.'
                    CALL ERRLOG ( MSGTXT )
                  END IF
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
C              Do not write non-VLBA stations for which FORMAT=NONE
C              Note use of NSETUP rather than MODSET.  I think this
C              should be equivalent.  This is a step toward using 
C              the main SCHED bookkeeping which I think will be 
C              simpler.
C
               IF( STASCN(ISCN,ISTA) .AND. (
     1              STANAME(ISTA)(1:4) .EQ. 'VLBA' .OR.
     2              FORMAT(NSETUP(ISCN,ISTA)) .NE. 'NONE' ) ) THEN
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
C                 Use a difference of integers to avoid round off 
C                 situations giving a duration one second too large.
C                 This gets a bit tricky because the numbers for
C                 the Julian days are too big to be held in a normal
C                 integer, so the high order digits need to be 
C                 subtracted first.
C
                  IDAY = DINT( STARTJ(ISCN) )
                  INTSTOP = NINT( ( STOPJ(ISCN) - IDAY ) * 86400.D0 ) - 
     1              NINT( ( STARTJ(ISCN) - IDAY - TAPOFF ) * 86400.D0 )
                  DATLAT = MIN( DATLAT, INTSTOP )
                  WRITE( LINE(LPOS:LPOS+9), '( I5, A4, A1 )' ) DATLAT,
     1                ' sec', COL
                  LPOS = LEN1(LINE) + 1
                  WRITE( LINE(LPOS:LPOS+9), '( I5, A4, A1 )' ) INTSTOP,
     1                ' sec', COL
C
                  LPOS = LEN1(LINE) + 1
C
                  IF( USEDISK(ISTA) ) THEN
C
C                    Find the last scan that this station participated
C                    in and print the GB at the end of that scan.
C                    Note, STASCN(ISCN,ISTA) is a flag that indicates
C                    that station ISTA is in scan ISCN.  
C
                     IF( ISCN .GT. SCAN1 ) THEN
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
                     WRITE( LINE(LPOS:LPOS+12), '( F9.3, A3, A1 )' ) 
     1                      STGB,' GB', COL
C
                  ELSE
C                     
C                    If not using disk (eg pointing), just put in blanks.
C
                     WRITE( LINE(LPOS:LPOS+2), '( 2X, A1 )' ) COL 
C
                  END IF
C
C                 Next is pass and subpass
C
                  LPOS = LEN1(LINE) + 1
                  IF ( DISK(STANUM(ISTA)) .EQ. 'LBADR' ) THEN
                     WRITE( LINE(LPOS:LPOS+5), '( 3X, I1, 1X, A1 )' ) 
     1                   0,  COL
                  ELSE
C
C                     In all other cases, put in the space and separator.
C
                      WRITE( LINE(LPOS:LPOS+3), 
     1                     '( 3X, A1 )' ) COL
                  END IF
C
C                 Pointscr not implemented leave blank
C                 Get the pointing sector from WRAPZONE.
C                 But only do this for the VLBA for now until the EVN
C                 etc are comfortable with having it here.  There is
C                 a fear that some stations will pay attention to it,
C                 but not have it properly implemented.
C
                  IF( STANAME(ISTA)(1:4) .EQ. 'VLBA' ) THEN
                     CALL WRAPZONE( IVEX, ISCN, ISTA, ZONE )
                  ELSE 
                     ZONE = ' '
                  END IF
C
                  LPOS = LEN1(LINE) + 1
                  WRITE( LINE(LPOS:LPOS+7), '( 1X, A5, 1X, A1 )' ) 
     1                    ZONE, COL
C
C                 Write tapedrive in last column, should be head 1 day
C                 agreed with NRV that norec can be drive 0
C                 With removal of tape stuff, this is 1 if recording, 
C                 0 if not.
C
                  TPDRIV = 1
                  IF( NOREC(ISCN) .OR. .NOT. USEDISK(ISTA) ) TPDRIV = 0
                  LPOS = LEN1(LINE) + 1
                  WRITE( LINE(LPOS:LPOS+2), '( 1X, I1, A1 )' ) TPDRIV,
     1                 SEP
C
C                 flush line
C
                  WRITE( IVEX, '( A )' ) LINE(1:LEN1(LINE))
                  INPAGE = INPAGE + 1
C     
               ELSE IF( STASCN(ISCN,ISTA) ) THEN
C     
C                 Write a comment line indicating the station is
C                 being skipped because of FORMAT=NONE
C
                  LINE = ' '
                  LPOS = 6
                  WRITE( LINE, '( A1, 4X, A, A1, 4X, A, A, A )' ) 
     1               COM, 'station=', STCODE(STANUM(ISTA))(1:2), COL,
     2              ' Skipping scan with FORMAT=NONE on:',
     3               TMPSRC(1:LEN1(TMPSRC))
C
               END IF
            END DO
            WRITE( IVEX, '( A, A1 )' ) 'endscan', SEP
            INPAGE = INPAGE + 1
C
         END IF
      END DO
C
C     Print warning about frequency of Tsys.   Switch trigger for this
C     from CONTROL=VEX and not VLA to TSCAL='GAP'  Nov. 4, 2013 RCW.
C
      TSYSMESS = .FALSE.
      DO ISTA = 1, NSTA
C         IF( WARNTS(ISTA) .AND. ( CONTROL(STANUM(ISTA)) .EQ. 'VEX' 
C     1      .AND. STATION(STANUM(ISTA))(1:3) .NE. 'VLA' ) ) THEN
         IF( WARNTS(ISTA) .AND. ( TSCAL(STANUM(ISTA)) .EQ. 'GAP' ) ) 
     1        THEN 
C
            WRITE( MSGTXT, '( A, A, A, I5, A , I4, A)' ) 
     2         'VXSCH: ', STATION(STANUM(ISTA)), ' has ',
     3         NTSYS(ISTA), 
     4         ' Tsys measurements. Maximum interval = ',
     5         TSYSGAP(ISTA), ' minutes.'
            CALL WLOG( 1, MSGTXT )
            TSYSMESS = .TRUE.
         END IF
      END DO
      IF( TSYSMESS ) THEN
         CALL WLOG( 1,'VXSCH: Tsys calibration at most MkIV stations '//
     1       'is taken during every gap in recording, ')
         CALL WLOG( 1,'       but these ' //
     1       'appear over 15 min apart for the stations listed above!')
         CALL WLOG( 1,'       This can be improved by inserting'//
     1       ' gaps at regular intervals. ')
         CALL WLOG( 1,'       Note this is not an issue for '//
     1       ' Westerbork or Arecibo' )
         CALL WRTMSG( 0, 'VXSCH', 'tsysgap' )
      END IF
C
C     Only give the warning about Tsys measurements off source if 
C     TSCAL is 'GAP'.
C
      TSYSMESS = .FALSE.
      DO ISTA = 1, NSTA
         IF( WARNTSOF(ISTA) .AND. TSCAL(STANUM(ISTA)) .EQ. 'GAP' ) THEN
            WRITE( MSGTXT, '( 2A, A, I5, A, I5, A )' ) 
     1         'VXSCH: ', STATION(STANUM(ISTA)), ': only ',
     2         NTSYSON(ISTA), ' out of ', NTSYS(ISTA),
     3         ' Tsys measurements are on-source'
            CALL WLOG( 1, MSGTXT )
            TSYSMESS = .TRUE.
         ENDIF
      END DO
      IF( TSYSMESS ) THEN
         CALL WLOG( 1, 'VXSCH: Stations listed above are affected ' //
     1        'by slewing during Tsys calibration')
         CALL WRTMSG( 0, 'VXSCH', 'tsysoffsrc' )
      END IF
C
      IF( WARNFS ) 
     1    CALL WLOG( 1,'VXSCH: WARNING: Scan timing problem '//
     2    'for PCFS, this VEX will NOT run!!!!')
      IF( WARNGP ) THEN
         CALL WRTMSG( 0, 'VXSCH', 'ftpgap' )
      END IF
C
      IF( WARNBANK ) THEN
C
C        Use the WRTMSG call to give the full warning about insufficient
C        gaps for media changes.
C        
         CALL WRTMSG( 1, 'VXSCH', 'warnbank' )
      END IF
C
      RETURN
      END
