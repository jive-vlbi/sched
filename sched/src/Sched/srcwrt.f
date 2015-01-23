      SUBROUTINE SRCWRT( IOUT, IP )
C
C     Subroutine for SCHED that writes the actual source lines for
C     SRCLST.  See first few lines of code for meaning of IP.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schpeak.inc'
C
      INTEGER       MSEP, ML
      PARAMETER     ( MSEP = 7 )
      PARAMETER     ( ML = 10 + 2 * MAXSET )
C
      INTEGER       IP, ISRC, LEN1, IOUT, INAME
      INTEGER       ICHN
      INTEGER       MLINE, CLINE, NMLINE, IL, NPCH
      INTEGER       ISET, NFC, NCHSET, CH1SET, IPAIR, ICSRC
      DOUBLE PRECISION  TSCAN, TBASE, ESCAN, EBASE
      CHARACTER     FF*1
      CHARACTER*16  TFORM, TRA50, TRA20, TRAP, TDEC50, TDEC20, TDECP
      CHARACTER*16  TRAPM, TDECPM
      CHARACTER     CVELREF*12, CVELDEF*18, HEADLINE*80, KFSETS*78
      CHARACTER*132 LINE(ML), PFSETS*30, PRSRC*12
      LOGICAL       PMUSED, CALL1
      DATA          CALL1 / .TRUE. /
      SAVE          CALL1
C ------------------------------------------------------------------
      IF( DEBUG ) CALL WLOG( 1, 'SRCWRT starting.' )
      FF = CHAR( 12 )
C
      IF( IP .EQ. 1 ) THEN
        HEADLINE = ' POSITIONS OF SOURCES USED IN RECORDING SCANS'
      ELSE IF( IP .EQ. 2 ) THEN
        HEADLINE = ' POSITIONS OF ADDITIONAL SOURCES USED ONLY '//
     1     'IN NON-RECORDING SCANS'
      ELSE IF( IP .EQ. 3 ) THEN
        HEADLINE = ' POSITIONS OFADDITIONAL SOURCES USED ONLY FOR '//
     1     'PHASE CENTERS'
      END IF
C
      IF( CALL1 ) THEN
         WRITE( IOUT, '( A1, A, /, A, /, A, A, /, A, /, ' //
     1    'A, A, F10.3, /, A )' )
     2    FF, HEADLINE(1:LEN1(HEADLINE)),
     3    '     Catalog positions marked with *. ',
     4    '     Precession of date coordinates is based on stop ',
     5    'time of first scan.',
     6    '     Names used in schedule marked with *. ',
     7    '     Observation date used in B1950/J2000 coordinate ',
     8    'conversion (PRECDATE):', PRECDATE,
     9    '     No adjustments are made for rates (DRA, DDEC).'
      ELSE
         WRITE( IOUT, '( 1X, /, A )' ) HEADLINE(1:LEN1(HEADLINE))
      END IF
      CALL1 = .FALSE.
C
      IF( IP .EQ. 2 ) THEN
         WRITE( IOUT, '( 1X, /, A, A)' )
     1    '     An unused dummy source of a scan that becomes ',
     2    'a geodetic segment will show up here.'
      END IF
C
      WRITE( IOUT, '( 2X, /, A, 24X, A, 23X, A /, 22X, A, 6X, A, 
     1        /, 2X )' )
     2    '   Source', ' Source position (RA/Dec) ', 'Error',
     3    '  (B1950)             (J2000)             (Date) ',
     4    '     (mas)'
C
C     Loop over sources.
C
      DO ISRC = 1, MSRC
         IF( ( IP .EQ. 1 .AND. USEDREC(ISRC) ) .OR.
     1       ( IP .EQ. 2 .AND. SUSED(ISRC) .AND. .NOT. USEDREC(ISRC) )
     2       .OR. ( IP .EQ. 3 .AND. .NOT. SUSED(ISRC) .AND. 
     3        USEDPHS(ISRC) ) ) THEN
C
            DO IL = 1, ML
               LINE(IL) = ' '
            END DO
            NMLINE = 0
C
C           Write the names into the first columns for up to 
C           MALIAS lines if there are multiple names.
C
            DO INAME = 1, MALIAS
               IF( SOURCE(INAME,ISRC) .NE. ' ' ) THEN
                  NMLINE = NMLINE + 1
                  WRITE( LINE(NMLINE), '( T2, A1, 1X, A12 )' ) 
     1                CSUSED(INAME,ISRC), SOURCE(INAME,ISRC)
               END IF
            END DO
C
C           Write the source positions on the first 2 lines.
C
            TRA50 = TFORM( RA1950(ISRC), 'T', 0, 2, 9, '   ' )
            TRA20 = TFORM( RA2000(ISRC), 'T', 0, 2, 9, '   ' )
            TRAP  = TFORM( RAP(ISRC),    'T', 0, 2, 9, '   ' )
            TDEC50 = TFORM( D1950(ISRC),  ' ', 1, 2, 8, '   ' )
            TDEC20 = TFORM( D2000(ISRC),  ' ', 1, 2, 8, '   ' )
            TDECP  = TFORM( DECP(ISRC),   ' ', 1, 2, 8, '   ' )

            WRITE( LINE(1)(20:132), '( 3( A1, 1X, A16, 2X ), F8.2 )' )
     1          C1950(ISRC), TRA50, 
     2          C2000(ISRC), TRA20, 
     3          CDATE(ISRC), TRAP, RAERR(ISRC)
            WRITE( LINE(2)(20:132), '( 3( A1, A16, 3X ), F8.2 )' )  
     1          C1950(ISRC), TDEC50, 
     2          C2000(ISRC), TDEC20, 
     3          CDATE(ISRC), TDECP, DECERR(ISRC)
C
C           Make a line giving where SCHED got the position.
C
            IF( WHICHCAT(ISRC) .EQ. 'i' ) THEN
               LINE(3) = LINE(3)(1:21) //
     1              'From catalog imbedded in main SCHED input file.'
            ELSE IF( WHICHCAT(ISRC) .EQ. '1' ) THEN
               LINE(3) = LINE(3)(1:21) //
     1              SRCFILE(1:LEN1(SRCFILE))
            ELSE IF( WHICHCAT(ISRC) .EQ. '3' ) THEN
               LINE(3) = LINE(3)(1:21) //
     1              SRCFILE2(1:LEN1(SRCFILE2))
            ELSE IF( WHICHCAT(ISRC) .EQ. 'P' ) THEN
               LINE(3) = LINE(3)(1:21) //
     1              'Planet.  Position is for scan 1, center Earth.'
            ELSE IF( WHICHCAT(ISRC) .EQ. 'S' ) THEN
               LINE(3) = LINE(3)(1:21) //
     1              'Satellite. Position is for scan 1, center Earth.'
            ELSE IF( WHICHCAT(ISRC) .EQ. '2' ) THEN
               LINE(3) = LINE(3)(1:21) //
     1              PSRCFILE(1:LEN1(PSRCFILE))
            ELSE
               CALL ERRLOG( 'SRCLST: Programming problem.  WHICHCAT '//
     1              'not set.' )
            END IF
C
C           Start keeping track of the current line because some of
C           the following are not done in all cicumstances.
C
            CLINE = 3
C
C           Write any remark included in catalog for this source.
C
            IF( REMARK(ISRC) .NE. ' ') THEN
               CLINE = CLINE + 1
               LINE(CLINE) = LINE(CLINE)(1:21) //
     1              REMARK(ISRC)(1:LEN1(REMARK(ISRC)))
            END IF
C
C           Write any velocities used.
C
            IF( DIDNDOP(ISRC) .GT. 0 ) THEN
               IF( VELDEF(ISRC) .EQ. 'R' ) THEN
                  CVELDEF = 'radio definition'
               ELSE IF( VELDEF(ISRC) .EQ. 'O' ) THEN
                  CVELDEF = 'optical definition'
               ELSE IF( VELDEF(ISRC) .EQ. 'Z' ) THEN
                  CVELDEF = 'redshift'
               ELSE
                  CALL ERRLOG( 'SRCLST: Bad VELDEF - program problem.' )
               END IF
               IF( VELREF(ISRC) .EQ. 'L' ) THEN
                  CVELREF = 'LSR'
               ELSE IF( VELREF(ISRC) .EQ. 'H' ) THEN
                  CVELREF = 'heliocentric'
               ELSE IF( VELREF(ISRC) .EQ. 'G' ) THEN
                  CVELREF = 'geocentric'
               ELSE
                  CALL ERRLOG( 'SRCLST: Bad VELREF - program problem.' )
               END IF
C
               CLINE = CLINE + 1
               WRITE( LINE(CLINE)(22:132), '( 5A )' ) 
     1            'Doppler based on ',
     2            CVELREF(1:LEN1(CVELREF)), ' frame and ',
     3            CVELDEF(1:LEN1(CVELDEF)), '.  Velocities:'
C
               CLINE = CLINE + 1
               NPCH = MIN( DIDNDOP(ISRC), 8 )
               WRITE( LINE(CLINE)(22:132), '( 8( F9.2,:) )' )
     1            ( VLSR(ICHN,ISRC), ICHN=1,NPCH )
C
               IF( DIDNDOP(ISRC) .GE. 9 ) THEN
                  CLINE = CLINE + 1
                  NPCH = MIN( DIDNDOP(ISRC), 16 )
                  WRITE( LINE(CLINE)(22:132), '( 8( F9.2,:) )' )
     1               ( VLSR(ICHN,ISRC), ICHN=9,NPCH )
               END IF

            ELSE IF(  DOPPED(ISRC) ) THEN
               CLINE = CLINE + 1
               WRITE( LINE(CLINE)(22:132), '( A )' ) 
     1            'Doppler based on other sources.'
            END IF
C
C           Tell about proper motions.
C
            PMUSED = PMRA(ISRC) .NE. 0.D0 .OR.
     1               PMDEC(ISRC) .NE. 0.D0 .OR. 
     2               PARALAX(ISRC) .NE. 0.D0
            IF( PMUSED ) THEN
               TRAPM  = TFORM( RACAT(ISRC),    'T', 0, 2, 9, '   ' )
               TDECPM  = TFORM( DECCAT(ISRC),   ' ', 1, 2, 8, '   ' )
C
               CLINE = CLINE + 1
               WRITE( LINE(CLINE)(22:132), '( A, F10.5 )' )
     1             'Proper motion used.  Reference epoch: ', 
     2             EPOCHT(ISRC)
C
               CLINE = CLINE + 1
               WRITE( LINE(CLINE)(22:132), '( 4A )' )
     1             '   At epoch: RA = ', TRAPM, 
     2             '   Dec = ', TDECPM
C
               CLINE = CLINE + 1
               WRITE( LINE(CLINE)(22:132), '( A, F8.2, A, F8.2, A )' )
     1             '   Rates: RA = ', PMRA(ISRC) * 1000.0, 
     2             ' mas/yr   Dec = ', PMDEC(ISRC) * 1000.0, ' mas/yr'
C
               CLINE = CLINE + 1
               WRITE( LINE(CLINE)(22:132), '( A, F9.4, A )' )
     1             '   Paralax: ', PARALAX(ISRC) * 1000.0, ' mas.'
C
            END IF
C
C           Planetary motion used.
C
            IF( DRA(ISRC) .NE. 0.0 .OR.
     1          DDEC(ISRC) .NE. 0.0 ) THEN
               MSGTXT = ' '
               IF( PMUSED ) THEN
                  MSGTXT = 
     1               'Planetary motion (includes proper motion).'
               ELSE
                  MSGTXT = 'Planetary motion used.'
               END IF
C
               CLINE = CLINE + 1
               WRITE( LINE(CLINE)(22:132), '( 2A, F12.4 )' )
     1            MSGTXT(1:LEN1(MSGTXT)), '  Ref. MJD: ', 
     2            PMTIME(ISRC)
C
               CLINE = CLINE + 1
               WRITE( LINE(CLINE)(22:132), '( A, E12.5, 
     1                A, A, E12.5, A  )' )
     2            '   Rates: RA = ', DRA(ISRC), ' s/day ',
     3            '  Dec = ', DDEC(ISRC), ' arcsec/day '
            END IF
C
C           Write ephemeris file names if appropriate.
C
            IF( WHICHCAT(ISRC) .EQ. 'P' ) THEN
               CLINE = CLINE + 1
               WRITE( LINE(CLINE)(22:132), '( A, A )' )
     1               'EPHFILE: ', EPHFILE(1:LEN1(EPHFILE))
            END IF
            IF( WHICHCAT(ISRC) .EQ. 'S' ) THEN
               CLINE = CLINE + 1
               WRITE( LINE(CLINE)(22:132), '( A, A )' ) 'KERFILE: ',
     1                 KERFILE(SATN(ISRC))(1:LEN1(KERFILE(SATN(ISRC))))
               CLINE = CLINE + 1
               IF( SATFILE(SATN(ISRC))(1:4) .NE. 'NONE' ) THEN
                  WRITE( LINE(CLINE)(22:132), '( A, A )' ) 'SATFILE: ',
     1                 SATFILE(SATN(ISRC))(1:LEN1(SATFILE(SATN(ISRC))))
               END IF
               IF( TLEFILE(SATN(ISRC))(1:4) .NE. 'NONE' ) THEN
                  WRITE( LINE(CLINE)(22:132), '( A, A )' ) 'TLEFILE: ',
     1                 TLEFILE(SATN(ISRC))(1:LEN1(TLEFILE(SATN(ISRC))))
               END IF
           END IF
C
C           Tell if the source is a pointing center for a multi-field
C           processing.
C
            IF( NPAIR .GT. 0 ) THEN
               DO IPAIR = 1, NPAIR
                  IF( PAIRSRC(IPAIR) .EQ. ISRC ) THEN
                     CLINE = CLINE + 1
                     WRITE( LINE(CLINE)(22:132), '( A, A )' )
     1                 ' Pointing center for phase center group: ',
     2                 CTRNAME(PAIRCENT(IPAIR))
                  END IF
               END DO
C
C              Tell if the source is in a phase center group for a 
C              multi-field processing.
C
               DO IPAIR = 1, NPAIR
                  DO ICSRC = 1, NCSRC(PAIRCENT(IPAIR))
                     IF( CTRSRCI(ICSRC,PAIRCENT(IPAIR)) .EQ. 
     1                  ISRC ) THEN
                        CLINE = CLINE + 1
                        WRITE( LINE(CLINE)(22:132), '( A, A )' )
     1                    ' Member of phase center group: ',
     2                    CTRNAME(PAIRCENT(IPAIR))
                     END IF
                  END DO
               END DO
            END IF
C
C           Now write out the lines.
C
            MLINE = MAX( NMLINE, CLINE )
            DO IL = 1, MLINE
               WRITE( IOUT, '( A )' ) LINE(IL)(1:LEN1(LINE(IL)))
            END DO
            WRITE( IOUT, '(1X)' )
         END IF
      END DO
C
C     Now write the observing hours summary as a separate section.  
C
      WRITE( IOUT, '( 1X, /, 1X, /, A )' ) 
     1      'SOURCE SCAN SUMMARY FOR SOURCES LISTED ABOVE'


      WRITE( IOUT, '( 1X, /, A, /, A, A )' )
     1    '     Scan hours are for recording scans only. ',
     2    '     Baseline hours are only counted for scans above ',
     3    'horizon at both ends.'
      IF( FUZZY) WRITE( IOUT, '( A, A )' )
     1    '     ''Core'' scans are those for which PREEMPT is ',
     2    'not ''EXTRA''.'
      IF( DOSCANS(1) .NE. 0 ) WRITE( IOUT, '( A, A, /, A )' )
     1    '     The ''DOSCANS'' columns are for scans in the range ',
     2    'selected by DOSCANS',
     3    '     Those are the scans sent to the Vex and other files'
C
      LINE(1) = ' '
      LINE(2) = ' '
      LINE(1) = '  Source       Setup file             Frequency sets'
      LINE(2)(36:78) = '(duplicates not shown)'
C
      IF( FUZZY .AND. DOSCANS(1) .EQ. 0 ) THEN
         LINE(1)(71:101) = ' Core scans  (Hours)  All scans'
         LINE(2)(71:102) = 'Scan  Baseline    Scan  Baseline'
      ELSE IF( FUZZY .AND. DOSCANS(1) .NE. 0 ) THEN
         LINE(1)(71:101) = ' Core scans  (Hours) DOSCANS'
         LINE(2)(71:102) = 'Scan  Baseline    Scan  Baseline'
      ELSE IF( .NOT. FUZZY .AND. DOSCANS(1) .EQ. 0 ) THEN
         LINE(1)(71:86) = 'Observing hours'
         LINE(2)(71:86) = ' Scan  Baseline'
      ELSE IF( .NOT. FUZZY .AND. DOSCANS(1) .NE. 0 ) THEN
         LINE(1)(71:86) = ' DOSCANS hours'
         LINE(2)(71:86) = ' Scan  Baseline'
      ELSE
         CALL ERRLOG( 'SRCWRT: Programming error with FUZZY and '//
     1        'DOSCANS' )
      END IF
C
      WRITE( IOUT, '( A )' ) LINE(1)(1:LEN1(LINE(1)))
      WRITE( IOUT, '( A )' ) LINE(2)(1:LEN1(LINE(2)))
C
C     Get scan and baseline hours for source.  Planning schedules
C     can have TONSRC way past STARTJ so protect against that.
C     Break down by setup file.
C
      DO ISRC = 1, MSRC
         IF( ( IP .EQ. 1 .AND. USEDREC(ISRC) ) .OR.
     1       ( IP .EQ. 2 .AND. SUSED(ISRC) .AND. .NOT. USEDREC(ISRC) )
     2       .OR. ( IP .EQ. 3 .AND. .NOT. SUSED(ISRC) .AND. 
     3        USEDPHS(ISRC) ) ) THEN
C
            DO IL = 1, ML
               LINE(IL) = ' '
            END DO
C
C           Write the name.  There should only be one.
C
            MLINE = 0
            DO INAME = 1, MALIAS
               IF( CSUSED(INAME,ISRC) .EQ. '*' ) THEN
                  PRSRC = SOURCE(INAME,ISRC)
               END IF
            END DO
C
            CLINE = 0
            DO ISET = 1, NSETF
               CALL SBHOURS( ISRC, TSCAN, TBASE, ESCAN, EBASE, 
     1                       ISET, KFSETS )
               IF( ESCAN .GT. 0.D0 ) THEN
C
C                 Characters for setup - get end of file path.
C
                  NCHSET = LEN1(SETFILE(ISET))
                  CH1SET = MAX( NCHSET-20, 1 )
C
C                 Number of characters of list of frequency sets.
C                 This will end up being incomplete in some extreme cases.
C                 Put a + on the end when it is.
C
                  NFC = LEN1( KFSETS )
                  PFSETS = KFSETS
                  IF( NFC .GE. 29 ) THEN
                     PFSETS(29:30) = ' +'
                     NFC = 30
                  END IF
C
                  IF( FUZZY ) then
                     WRITE( IOUT, '( 2X, A, T14 , A, T35, A, T67, '//
     1                 '2( F8.3, F10.3) )' )
     2                 PRSRC, SETFILE(ISET)(CH1SET:NCHSET), 
     3                 PFSETS(1:NFC),
     4                 TSCAN * 24.D0, TBASE * 24.D0, 
     5                 ESCAN * 24.D0, EBASE * 24.D0
                  ELSE
                     WRITE( IOUT, '( 2X, A, T14 , A, T35, A, T67, '//
     1                 '2( F8.3, F10.3) )' )
     2                 PRSRC, SETFILE(ISET)(CH1SET:NCHSET), 
     3                 PFSETS(1:NFC),
     4                 ESCAN * 24.D0, EBASE * 24.D0
                  END IF
                  PRSRC = '  '
C
               END IF
            END DO
         END IF
      END DO
C
      RETURN
      END






