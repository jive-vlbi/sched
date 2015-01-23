      SUBROUTINE RDGEO
C
C     Routine for NEWLOC that reads the geodetic results.
C
      INCLUDE 'newloc.inc'
C
      INTEGER   LEN1, IC, NC, IS, JS
      INTEGER   EPSY, EPSM, EPSD, ERR, IER
      DOUBLE PRECISION   READX, READY, READZ
      DOUBLE PRECISION   EPMJD
      REAL      READEX, READEY, READEZ, READOFF
      REAL      READRX, READRY, READRZ
      CHARACTER INSTA*8, MODSTA*8
C
C     Sept. 27, 2011.  Switch to a formatted write, then deal with
C     the blanks and episodic times in some station names.  Also
C     I'm no longer starting with an old locations.dat, so create
C     new stations for each input station.
C
C -------------------------------------------------------------------
C
C     Initialize the coordinates.
C
      DO IS = 1, MS
         VLBIX(IS)  = 0.D0
         VLBIEX(IS) = 0.D0
         VLBIY(IS)  = 0.D0
         VLBIEY(IS) = 0.D0
         VLBIZ(IS)  = 0.D0
         VLBIEZ(IS) = 0.D0
         VLBIRX(IS) = 0.D0
         VLBIRY(IS) = 0.D0
         VLBIRZ(IS) = 0.D0
         VLBIJDAY(IS) = 0.D0
         VLBIBEG(IS) = 0.D0
         VLBIEND(IS) = 100000.D0
         FRAME(IS) = ' '
         GOTPOS(IS) = ' '
         GOTRAT(IS) = ' '
         GOTOFF(IS) = ' '
      END DO
C
C     Read the geodetic solutions from the file from Goddard.
C
100   CONTINUE
      WRITE(*,*)
      WRITE(*,*) 'Name of new geodetic frame:'
      READ(*,'(A)') NEWFRM
      WRITE(*,*) 'Geodetic VLBI station positions file:'
      READ(*,'(A)') POSFILE
      WRITE(*,'(2A)') ' Positions file: ', POSFILE(1:LEN1(POSFILE))
      WRITE(*,'(2A)') ' New frame name: ', NEWFRM
      OPEN( UNIT=9, FILE=POSFILE, STATUS='OLD', ACCESS='SEQUENTIAL',
     1      FORM='FORMATTED', IOSTAT=IER )
      IF( IER .NE. 0 ) GO TO 100
C
C     Get dates for reference frame and "current" comparisons.
C
      WRITE(*,*) 'Reference epoch for rates yr, mo, dy.'
      WRITE(*,*) 'Also epoch (yr, mo, dy) ' //
     1     'for print of current positions'
      WRITE(*,*) ' (eg 1997 1 1 2006 3 10):'
      READ(*,*) YEAR, MONTH, DAY, CYEAR, CMONTH, CDAY
      WRITE(*,*) 'Reference day: ', YEAR, MONTH, DAY
      WRITE(*,*) 'Current epoch: ', CYEAR, CMONTH, CDAY
C
      CALL JULDA( YEAR, MONTH, DAY, JDATE, GMST )
      JDAY = JDATE - 2400000.5
      CALL JULDA( CYEAR, CMONTH, CDAY, CJDATE, GMST )
      CJDAY = CJDATE - 2400000.5
C
C     Read a line of station information.  My attempts to do this
C     as a free format read were pretty badly frustrated by the
C     possibility of station names containing blanks and episodic
C     dates.  So use fixed format (Sept 2011 mod).  Then eliminate
C     blanks and actually use the episodic dates.  There should
C     no longer be a need to run a set script to fix the names.
C
      WRITE(*,*) ' '
      WRITE(*,*) 'Reading positions file. '
      WRITE(*,*) ' St.#  In Name  Fixed Name  Start YMD MJD  ERR'
C
C     Jump here for next line of the Positions file.
C
150   CONTINUE
         NWORDS = MWD
         CALL GETLINE( WORD, WLEN, NWORDS, INLINE, 9, 6 )
C
C        Only pay attention to the one type of position line.
C
         IF( NWORDS .GE. 0 .AND. WORD(1) .EQ. 'STA_GCX:' ) THEN
C
            NS = NS +1
            IS = NS
            IF( NS .GT. MS ) THEN
               WRITE(*,*) 'Too many stations - increase ',
     1            'array sizes.'
               STOP
            END IF
C
C           Read the name and epsodic date in fixed format.
C           Figure out how many blanks there are in the name
C           to see what to add to the word count to get the
C           positions below.  I can't just read the positions
C           as is because they have too many digits and no "D0"
C           to force parsing as a double precision.
C
            READ( INLINE, '( 10X, A8, 1X, 3I2, '//
     1             ' 3( 5X, F15.3, 4X, F10.3 ) )' )
     2         INSTA, EPSY, EPSM, EPSD, READX, READEX,
     3         READY, READEY, READZ, READEZ
C
C           Process the station name to replace embedded blanks
C           with underscores and get rid of trailing underscores.
C
            NC = LEN1( INSTA )
            STA(IS) = INSTA
            DO IC = 2,NC
               IF( INSTA(IC:IC) .EQ. '_' ) STA(IS)(IC:IC) = ' '
               IF( INSTA(IC:IC) .EQ. ' ' ) STA(IS)(IC:IC) = '_'
            END DO
C
C           Get the start time for the positions file.
C           Don't do this if one was not given.
C           There is a Y2K issue lurking here with a 2 digit year.
C           Test year, month, and day since at least the year can be
C           zero (2000)
C
            IF( EPSY .NE. 0 .OR. EPSM .NE. 0 .OR. EPSD .NE. 0) THEN
               EPSY = EPSY + 1900
               IF( EPSY .LE. 1950 ) EPSY = EPSY + 100
               CALL SLA_CLDJ( EPSY, EPSM, EPSD, EPMJD, ERR )
               IF( ERR .NE. 0 .AND. ERR .NE. 3 ) THEN
                  WRITE(*,*) 'Problem converting to MJD for position.',
     2                EPSY, EPSM, EPSD
                  STOP
               END IF     
               VLBIBEG(NS) = EPMJD
               WRITE(*, '( I5, 4A, I6, 2I3, F8.0, I3 )' )
     1             IS, '  ', INSTA, '  ', STA(IS),
     2             EPSY, EPSM, EPSD, EPMJD, ERR
            ELSE
               VLBIBEG(NS) = 0.D0
               WRITE(*, '( I5, 4A )' )  is, '  ', insta, '  ', sta(is)
            END IF
C
C           Transfer data to the output arrays
C
            VLBIX(NS) = READX / 1.D3
            VLBIY(NS) = READY / 1.D3
            VLBIZ(NS) = READZ / 1.D3
            VLBIEX(NS) = READEX / 1.D3
            VLBIEY(NS) = READEY / 1.D3
            VLBIEZ(NS) = READEZ / 1.D3
C
C           Add the frame name:
C
            FRAME(IS) = NEWFRM
C
C           Go back for another station.
C
            GO TO 150
C
         ELSE IF( NWORDS .GE. 0 ) THEN
C
C           Go back for another line
C
            GO TO 150
C
         END IF
C
C     This is the end of the reading positions loop.
C
C     Fill in the stop times as they are not provided on input.
C     They must be determined to avoid overlaps.
C
C     Loop over stations to set.
C
      DO IS = 1, NS
C 
C        Loop over comparison stations.  All entries have their
C        final start times from the input file.  Set the stop 
C        time to the next start time on the same station that 
C        is after the current entry's start time.
C
         DO JS = 1, NS
            IF( IS .NE. JS .AND. STA(IS) .EQ. STA(JS) .AND.
     1          VLBIBEG(JS) .GT. VLBIBEG(IS) .AND.
     2          VLBIBEG(JS) .LT. VLBIEND(IS) ) THEN
               VLBIEND(IS) = VLBIBEG(JS)
            END IF
         END DO
      END DO
C
C     Open and read the rates file.
C
      WRITE(*,*)
      WRITE(*,*)
  200 WRITE(*,*) 'Geodetic VLBI station rates file:'
      READ(*,'(A)') VELFILE
      WRITE(*,'(2A)') ' Rates file: ', VELFILE(1:LEN1(VELFILE))
      OPEN( UNIT=10, FILE=VELFILE, STATUS='OLD', ACCESS='SEQUENTIAL',
     1      FORM='FORMATTED', IOSTAT=IER )
      IF( IER .NE. 0 ) GO TO 200
250   CONTINUE
C
C        Read a line of station velocity information.
C        There is only one per station - no episodic effect so the
C        entry may need to apply to more than one station in the
C        output file.
C
         NWORDS = MWD
         CALL GETLINE( WORD, WLEN, NWORDS, INLINE, 10, 6 )
         IF( NWORDS .GE. 0 .AND. WORD(1) .EQ. 'STA_GVX:' ) THEN
C
C           Decode the station and start time.  Do a fixed format
C           read of the velocity data while at it.
C           
            READ( INLINE, '( 10X, A8, 5X, F9.2, '//
     1             '17X, F9.2, 17Xx, F9.2 )' )
     2         INSTA, READRX, READRY, READRZ
C
C           Process the station name to replace embedded blanks
C           with underscores and get rid of trailing underscores.
C
            NC = LEN1( INSTA )
            MODSTA = INSTA
            DO IC = 2,NC
               IF( INSTA(IC:IC) .EQ. '_' ) MODSTA(IC:IC) = ' '
               IF( INSTA(IC:IC) .EQ. ' ' ) MODSTA(IC:IC) = '_'
            END DO
        write(*,*) 'rates station: ', insta, '  ', modsta
C
C           Determine which station this is and transfer the data.
C           This may catch more than one entry if there is episodic
C           motion.
C
            DO IS = 1, NS
               IF( STA(IS) .EQ. MODSTA ) THEN
                  GOTRAT(IS) = 'GEO'
                  VLBIRX(IS) = READRX / 1.D3
                  VLBIRY(IS) = READRY / 1.D3
                  VLBIRZ(IS) = READRZ / 1.D3
C
C                 Add the externally supplied epoch.
C
                  VLBIJDAY(IS) = JDAY
C
C                 Write that the station has been added.
C
                  write(*,*) 'Rates added ', modsta, IS,
     1                VLBIRX(IS), VLBIRY(IS), VLBIRZ(IS)
               END IF
            END DO
C
C           Go back for another station.
C
            GO TO 250
C
         ELSE IF( NWORDS .GE. 0 ) THEN
C
C           Go back for another line.
C
            GO TO 250
C
         END IF
C
C     This is the end of the reading velocities loop.
C
C     Open and read the axis offsets file:
C
      WRITE(*,*) ' '
      WRITE(*,*) ' '
270   WRITE(*,*) 'Axis offsets file:'
      READ(*,'(A)') OFFFILE
      WRITE(*,*) 'Axis offsets file: ', OFFFILE(1:LEN1(OFFFILE))
      OPEN( UNIT=11, FILE=OFFFILE, STATUS='OLD', ACCESS='SEQUENTIAL',
     1      FORM='FORMATTED', IOSTAT=IER )
      IF( IER .NE. 0 ) GO TO 270
C
C     Read the first line and write it out, but don't use it.
C
      TEXT = ' ' 
      READ( 11, '(A)' ) TEXT
      WRITE(*, '(A)' ) TEXT
      WRITE(*,*)
C
  275 CONTINUE
C
C        Read a line of station axis offset information.
C
         NWORDS = MWD
         CALL GETLINE( WORD, WLEN, NWORDS, INLINE, 11, 6 )
         IF( NWORDS .GE. 0 .AND. WORD(1)(1:1) .NE. '#' ) THEN
C
C           Skip first and last lines which are not commented
C           but are not data lines.
C
            IF( WORD(1) .EQ. 'VLBI_AXIS_OFFSET' ) GO TO 275
C
C           Decode the station and start time.  Do a fixed format
C           read of the velocity data while at it.
C           
            READ( INLINE, '( A8, 12X  F7.4 )' )
     1         INSTA, READOFF
C
C           Process the station name to replace embedded blanks
C           with underscores and get rid of trailing underscores.
C
            NC = LEN1( INSTA )
            MODSTA = INSTA
            DO IC = 2,NC
               IF( INSTA(IC:IC) .EQ. '_' ) MODSTA(IC:IC) = ' '
               IF( INSTA(IC:IC) .EQ. ' ' ) MODSTA(IC:IC) = '_'
            END DO
        write(*,*) 'axis offset station: ', insta, '  ', modsta
C
C           Determine which station this is and transfer the offset data.
C
            DO IS = 1, NS
               IF( STA(IS) .EQ. MODSTA ) THEN
                  GOTOFF(IS) = 'GEO'
                  VLBIOF(IS) = READOFF
                  WRITE(*,*) 'Offset added ', IS, STA(IS), VLBIOF(IS)
               END IF
            END DO
C
C           Return for another station.
C
            GO TO 275
C
         ELSE IF( NWORDS .GE. 0 ) THEN
C
C           Go for another line.
C
            GO TO 275
C
         END IF
C
C     End of axis offset section.
C
      RETURN
      END
