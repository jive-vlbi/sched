      SUBROUTINE SRCLST( IOUT, MODE )
C
C     Subroutine for SCHED that writes source list to print file.
C     MODE=2 - write a second list in format for transfer of 
C              coordinates to the correlator data base.
C     MODE=1 - No such second list.
C
      INCLUDE  'sched.inc'
      INCLUDE  'schpeak.inc'
C
      INTEGER       ISRC, LEN1, IOUT, J, MODE, LENS, LENR, LEND
      INTEGER       ICHN, NPAIRS
      INTEGER       KSRC, JN, KN, MLINE, CLINE, ML, IL, NPCH
      PARAMETER     ( ML = 20 )
      REAL          MAXSEP, SRCSEP
      DOUBLE PRECISION  SLA_DSEP, TSCAN, TBASE
      CHARACTER     FF*1
      CHARACTER*16  TFORM, TRA50, TRA20, TRAP, TDEC50, TDEC20, TDECP
      CHARACTER*16  TRAPM, TDECPM
      CHARACTER     CVELREF*12, CVELDEF*18
      CHARACTER*132 LINE(ML)
      LOGICAL       PMUSED
C ------------------------------------------------------------------
C
      FF = CHAR( 12 )
C
      WRITE( IOUT, '( A1, A, A, /, A, /, A, A, /, 2( A, / ), ' //
     1    'A, A, F10.3, /, A, /, A, /, A, A )' )
     2    FF, ' SOURCE LIST -- ', EXPT(1:LEN1(EXPT)),
     3    '     Catalog positions marked with *. ',
     4    '     Precession of date coordinates is based on stop ',
     5    'time of first scan.',
     6    '     Names used in schedule marked with *. ',
     7    '     Short names used in VLA and SNAP files marked with +. ',
     8    '     Observation date used in B1950/J2000 coordinate ',
     9    'conversion (PRECDATE):', PRECDATE,
     a    '     No adjustments are made for rates (DRA, DDEC).',
     b    '     Scan hours are for recording scans only. ',
     c    '     Baseline hours are only counted for scans above ',
     d    'horizon at both ends.'
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
         IF( SUSED(ISRC) ) THEN
            DO IL = 1, ML
               LINE(IL) = ' '
            END DO
            MLINE = 3
C
C           Write the names into the first columns for up to 
C           5 lines if there are multiple names.
C
            WRITE( LINE(1), '( T2, A1, 1X, A12 )' ) 
     1          CSUSED(1,ISRC), SOURCE(1,ISRC)
C
            IF( SOURCE(2,ISRC) .NE. ' ' ) THEN
               WRITE( LINE(2), '( T2, A1, 1X, A12 )' ) 
     1             CSUSED(2,ISRC), SOURCE(2,ISRC)
            END IF
C
            IF( SOURCE(3,ISRC) .NE. ' ' ) THEN
               WRITE( LINE(3), '( T2, A1, 1X, A12 )' ) 
     1             CSUSED(3,ISRC), SOURCE(3,ISRC)
            END IF
C
            IF( SOURCE(4,ISRC) .NE. ' ' ) THEN
               WRITE( LINE(4), '( T2, A1, 1X, A12 )' ) 
     1             CSUSED(4,ISRC), SOURCE(4,ISRC)
               MLINE = 4
            END IF
C
            IF( SOURCE(5,ISRC) .NE. ' ' ) THEN
               WRITE( LINE(5), '( T2, A1, 1X, A12 )' ) 
     1             CSUSED(5,ISRC), SOURCE(5,ISRC)
               MLINE = 5
            END IF
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
            ELSE IF( WHICHCAT(ISRC) .EQ. 'P' ) THEN
               LINE(3) = LINE(3)(1:21) //
     1              'Planet.  Position is for scan 1, station 1.'
            ELSE IF( WHICHCAT(ISRC) .EQ. 'S' ) THEN
               LINE(3) = LINE(3)(1:21) //
     1              'Satellite. Position is for scan 1, station 1.'
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
               MLINE = MAX( MLINE, CLINE )
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
               MLINE = MAX( MLINE, CLINE )
               NPCH = MIN( DIDNDOP(ISRC), 8 )
               WRITE( LINE(CLINE)(22:132), '( 8( F9.2,:) )' )
     1            ( VLSR(ICHN,ISRC), ICHN=1,NPCH )
C
               IF( DIDNDOP(ISRC) .GE. 9 ) THEN
                  CLINE = CLINE + 1
                  MLINE = MAX( MLINE, CLINE )
                  NPCH = MIN( DIDNDOP(ISRC), 16 )
                  WRITE( LINE(CLINE)(22:132), '( 8( F9.2,:) )' )
     1               ( VLSR(ICHN,ISRC), ICHN=9,NPCH )
               END IF

            ELSE IF(  DOPPED(ISRC) ) THEN
               CLINE = CLINE + 1
               MLINE = MAX( MLINE, CLINE )
               WRITE( LINE(CLINE)(22:132), '( A )' ) 
     1            'Doppler based on other sources.'
            END IF
C
C           Now write the observing hours summary.  May or may
C           not be on 5th source name line depending on doppler.
C
C           Get scan and baseline hours for source.  Planning schedules
C           can have TONSRC way past STARTJ so protect against that.
C
            CALL SBHOURS( ISRC, TSCAN, TBASE )
            CLINE = CLINE + 1
            MLINE = MAX( MLINE, CLINE )
            WRITE( LINE(CLINE)(22:132), '( F8.3, A, F10.3, A )' )
     1          TSCAN * 24.D0, ' scan hours, ', 
     2          TBASE * 24.D0, ' baseline hours above horizon.'

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
               MLINE = MAX( MLINE, CLINE )
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
               MLINE = MAX( MLINE, CLINE )
            END IF
C
C           Write ephemeris file names if appropriate.
C
            IF( WHICHCAT(ISRC) .EQ. 'P' ) THEN
               CLINE = CLINE + 1
               WRITE( LINE(CLINE)(22:132), '( A, A )' )
     1               'EPHFILE: ', EPHFILE(1:LEN1(EPHFILE))
               MLINE = MAX( MLINE, CLINE )
            END IF
            IF( WHICHCAT(ISRC) .EQ. 'S' ) THEN
               CLINE = CLINE + 1
               WRITE( LINE(CLINE)(22:132), '( A, A )' ) 'KERFILE: ',
     1                 KERFILE(SATN(ISRC))(1:LEN1(KERFILE(SATN(ISRC))))
               CLINE = CLINE + 1
               WRITE( LINE(CLINE)(22:132), '( A, A )' ) 'SATFILE: ',
     1                 SATFILE(SATN(ISRC))(1:LEN1(SATFILE(SATN(ISRC))))
               MLINE = MAX( MLINE, CLINE )
            END IF
C
C           Now write out the lines.
C
            DO IL = 1, MLINE
               WRITE( IOUT, '( A )' ) LINE(IL)(1:LEN1(LINE(IL)))
            END DO
C
C           Write a space before the next source.
C
            WRITE( IOUT, '(1X)' )
         END IF
      END DO
C
C     Give sun distance summary.
C
      WRITE( IOUT, '( 2( 1X, / ), A, A, /, A, A )' )
     1  '  The solar corona can cause unstable phases for sources',
     2  ' too close to the Sun.',
     3  '  SCHED provides warnings at individual scans for distances',
     4  ' less than 10 degrees.'
C
      WRITE( IOUT, '( A, A, /, A )' )
     1    '  The distance from the Sun to each source in this schedule',
     2    ' is:',
     3    '    Source         Sun distance (deg) '
      DO ISRC = 1, MSRC
         IF( SUSED(ISRC) ) THEN
            DO J = 1, 5
               IF( CSUSED(J,ISRC) .NE. ' ' ) THEN
                  WRITE( IOUT, '( 3X, A12, 4X, F8.1 )' )
     1                 SOURCE(J,ISRC), SUNDIS(ISRC)
               END IF
            END DO
         END IF
      END DO
C
      WRITE( IOUT, '( X, /, A, A, /, A, A, /, A, /, A )' )
     1  '  Barry Clark estimates from predictions by Ketan Desai of',
     2  ' IPM scattering sizes ', 
     3  '  that the Sun will cause amplitude reductions on the ',
     4  ' longest VLBA baselines ',
     5  '  at a solar distance of 60deg F^(-0.6) where F is in GHz. ',
     6  '  For common VLBI bands, this is: '
C
      WRITE( IOUT, '( 9(A, F8.0, A, /) )' )
     1  '       327 MHz    ', 60.0*(0.327**(-0.6)), ' deg ',
     1  '       610 MHz    ', 60.0*(0.610**(-0.6)), ' deg ',
     1  '       1.6 GHz    ', 60.0*(1.6**(-0.6)), ' deg ',
     1  '       2.3 GHz    ', 60.0*(2.3**(-0.6)), ' deg ',
     1  '       5.0 GHz    ', 60.0*(5.0**(-0.6)), ' deg ',
     1  '       8.4 GHz    ', 60.0*(8.4**(-0.6)), ' deg ',
     1  '      15.0 GHz    ', 60.0*(15.**(-0.6)), ' deg ',
     1  '      22.0 GHz    ', 60.0*(22.**(-0.6)), ' deg ',
     1  '      43.0 GHz    ', 60.0*(43.**(-0.6)), ' deg '
C
C     Make a list of source separations for pairs closer than some
C     amount.  Set that limit to 30 degrees for now.  Only do in 
C     in summary.  Limit the total number of pairs by dropping
C     MAXSEP after some number.
C
      IF( IOUT .EQ. ISUM .AND. MSRC .GE. 2 ) THEN
         MAXSEP = 30.0
         WRITE( IOUT, '( 1X, /, A, F6.2, A, /, 1X )' )
     1     ' Source separations in degrees for pairs closer than ',
     2      MAXSEP,' degrees.'
C
         NPAIRS = 0
         DO ISRC = 1, MSRC - 1
            IF( SUSED(ISRC) ) THEN
               JN = 1
               DO J = 1, 5
                  IF( CSUSED(J,ISRC) .NE. ' ' ) THEN
                     JN = J
                  END IF
               END DO
               DO KSRC = ISRC + 1, MSRC
                  IF( SUSED(KSRC) ) THEN
                     KN = 1
                     DO J = 1, 5
                        IF( CSUSED(J,KSRC) .NE. ' ' ) THEN
                           KN = J
                        END IF
                     END DO
                     SRCSEP = SLA_DSEP( RAP(ISRC), DECP(ISRC),
     1                    RAP(KSRC), DECP(KSRC) ) / RADDEG
                     IF( SRCSEP .LE. MAXSEP ) THEN
                        WRITE( IOUT, '( 5X, A12, 2X, A12, 2X, F8.4 )' )
     1                   SOURCE(JN,ISRC), SOURCE(KN, KSRC), SRCSEP
                     END IF
                     NPAIRS = NPAIRS + 1
C
C                    Deal with too many.  Set NPAIRS so low this will
C                    only happen once.
C
                     IF( NPAIRS .GT. 50 ) THEN
                        MAXSEP = 5.0
                        NPAIRS = -10000000
                        WRITE( IOUT, '( 5X, A, F6.2, A )' ) 
     1                    'Too many.  Drop maximum separation to ',
     2                    MAXSEP, ' deg.'
                     END IF
                  END IF
               END DO
            END IF
         END DO
      END IF
C
C     Now the third list in the format for the transfer of coordinates
C     to the correlator data base.
C
      IF( MODE .EQ. 2 .AND. .NOT. NOTAPE ) THEN
         WRITE( IOUT, '( 5( 1X, / ), A, A )' )
     1      ' J2000 coordinates formatted for possible transfer to ',
     2      'the correlator data base:' 
         DO ISRC = 1, MSRC
            IF( SUSED(ISRC) ) THEN
               TRA20 = TFORM( RA2000(ISRC), 'T', 0, 2, 9, 'hms' )
               LENR = LEN1( TRA20 )
               IF( D2000(ISRC) .GE. 0.D0 ) THEN
                 TDEC20 = TFORM( D2000(ISRC),  ' ', 0, 2, 8, 'd''"' )
               ELSE
                 TDEC20 = TFORM( D2000(ISRC),  ' ', 1, 2, 8, 'd''"' )
               END IF
               LEND = LEN1( TDEC20 )
C
               DO J = 1, 5
                  IF( CSUSED(J,ISRC) .NE. ' ' ) THEN
                     LENS = LEN1( SOURCE(J,ISRC) )
                     
                     WRITE( IOUT, '( 6A )' )
     1                 'name = ''', SOURCE(J,ISRC)(1:LENS),
     2                 '''  ra = ', TRA20(1:LENR),
     3                 '  dec = ', TDEC20(1:LEND)
                  END IF
               END DO
            END IF
         END DO
      END IF
C
      RETURN
      END






