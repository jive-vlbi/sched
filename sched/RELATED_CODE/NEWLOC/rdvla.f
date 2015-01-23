      SUBROUTINE RDVLA
C
C     Routine for NEWLOC to read the VLA baseline file.
C     Sept. 28, 2011  Make read files cut and pasted from the
C     VLA parminator.  Here are the instructions to create the file:
C
C     In Firefox, go to https://mcmonitor.evla.nrao.edu/parminator 
C     Search on X, Y, Z separately and cut and paste to this file.
C        Can use click at start and shift click at end to highlight all lines, 
C        then copy in Firefox and paste in emacs.
C     Remove non-existant stations like AAB and pad 13.  Positions are zero.
C     Seek replace tabs to spaces
C     Sort on field 3 (C-u 3 M-x sort-fields).  
C        Will do in region from mark to cursor.
C     Save.
C
      INCLUDE 'newloc.inc'
C
      INTEGER           LEN1, IV, GETNV, PADN(24), IER
      DOUBLE PRECISION  GETNUM
      DATA  PADN / 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 14, 16, 18,
     1              20, 24, 28, 32, 36, 40, 48, 56, 64, 72 /
C     -----------------------------------------------------------------
C
C     Open and read the VLA stations file.
C     The file I have is much different from the 2006 file so I
C     need another format.
C
C     And yet another format shows up.
C
C     First ask for the new frame name and file name.
C
300   CONTINUE
      WRITE(*,*) ' '
      WRITE(*,*) ' '
C
      WRITE(*,*) 'New VLA Frame Text:'
      READ(*,'(A)') VLAFRM
C
      WRITE(*,*) 'VLA station file:'
      READ(*,'(A)') VLAFILE
C
      WRITE(*,'(2A)') 'VLA station file: ', VLAFILE(1:LEN1(VLAFILE))
      WRITE(*,'(2A)') 'VLA frame text: ', VLAFRM
      OPEN( UNIT=12, FILE=VLAFILE, STATUS='OLD', ACCESS='SEQUENTIAL',
     1      FORM='FORMATTED', IOSTAT=IER )
      IF( IER .NE. 0 ) THEN
         WRITE(*,*) 'Problem opening VLA file. ', IER
         STOP
      END IF
      READV = .FALSE.
C
C     First insert the phased array position at the origin of the
C     VLA coordinate system and generate the names of the VLA pads
C     in a reasonable order.
C
      VNAME(1) = 'VLA'
      VLAX(1) = 0.D0
      VLAY(1) = 0.D0
      VLAZ(1) = 0.D0
C
C     VLA file format changed again and I need the leading zero for
C     all pads.
C
C      DO IV = 1, 9
C         WRITE( VNAME(IV+1),  '( A5, I1 )' ) 'VLA_W', PADN(IV)
C         WRITE( VNAME(IV+25), '( A5, I1 )' ) 'VLA_E', PADN(IV)
C         WRITE( VNAME(IV+49), '( A5, I1 )' ) 'VLA_N', PADN(IV)
C         WRITE(*,*) IV, VNAME(IV+1), VNAME(IV+25), VNAME(IV+49)
C      END DO
C      DO IV = 10, 24
C
      WRITE(*,*) ' '
      WRITE(*,*) ' Recognized VLA station (pad) names: '
      DO IV = 1, 24
         WRITE( VNAME(IV+1),  '( A5, I2.2 )' ) 'VLA_W', PADN(IV)
         WRITE( VNAME(IV+25), '( A5, I2.2 )' ) 'VLA_E', PADN(IV)
         WRITE( VNAME(IV+49), '( A5, I2.2 )' ) 'VLA_N', PADN(IV)
         WRITE(*, '( I3, A, 3( 2X, A8 ) )' ) 
     1       IV, ' ', VNAME(IV+1), VNAME(IV+25), VNAME(IV+49)
      END DO
      NV = 73
C
C     Now get the rest.
C     Set the axis offset to zero when data found.
C     The file I got from Vivek in 2010 will have much redundant data.  
C     Use the last, simply by writing over anything already found.
C     The 2011 file from the paraminator only has one per station.
C     Don't require lines of 3 words.  Some catalogs have extra
C     stuff on the end.  But I have added ! at the start of all comment
C     lines.  The 2011 file did not have this.
C
      NWORDS=0
      WRITE(*,*) ' '
      DO WHILE ( NWORDS .GT. -1 )
        NWORDS = MWD
        CALL GETLINE( WORD, WLEN, NWORDS, INLINE, 12, 6 )
        IF( NWORDS .GT. 0 .AND. WORD(1)(1:1) .NE. '#' .AND.
     1       WORD(1)(1:1) .NE. '!' ) THEN
           IF( WORD(4) .EQ. 'X' ) THEN
              IV = GETNV( WORD(3), VNAME, NV, MP )
              VLAX(IV) = GETNUM( WORD(5), 1, WLEN(5) )
              VLAAXOF(IV) = 0.D0
           ELSE IF( WORD(4) .EQ. 'Y' ) THEN
              IV = GETNV( WORD(3), VNAME, NV, MP )
              VLAY(IV) = GETNUM( WORD(5), 1, WLEN(5) )
           ELSE IF( WORD(4) .EQ. 'Z' ) THEN
              IV = GETNV( WORD(3), VNAME, NV, MP )
              VLAZ(IV) = GETNUM( WORD(5), 1, WLEN(5) )
           ELSE
              WRITE(*,*) 'Bad line? ', INLINE(1:LEN1(INLINE))
           END IF
        END IF
      END DO
C
C     Get rid of the leading zeros to match the traditional SCHED
C     station files.  They were needed for the new EVLA data files.
C
      DO IV = 1, NV
         IF( VNAME(IV)(6:6) .EQ. '0' ) THEN
            VNAME(IV)(6:7) = VNAME(IV)(7:7) // ' '
         END IF
C
C        Basically a debug print:
C
         write(*, '( A, A, 3I5, 3F12.4 )' ) ' VLA Station found: ', 
     1           VNAME(IV), iv, nv, mp, vlax(iv),
     2           vlay(iv), vlaz(iv)
      END DO
C
C     Write the list.
C
      WRITE(*,*) ' '
      WRITE(*, '( A )' ) 'VLA pad locations in VLA frame.'
      DO IV = 1, NV
         WRITE(*, '( A8, 3F14.4 )' ) 
     1       VNAME(IV), VLAX(IV), VLAY(IV), VLAZ(IV)
      END DO
C
C
      RETURN
      END
