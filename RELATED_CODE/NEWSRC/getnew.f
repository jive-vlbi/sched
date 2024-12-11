      SUBROUTINE GETNEW
C
C     Read the new source catalog.  Usually this will be a GSFC or
C     Petrov file.  It can be in a variety of formats, which causes
C     the complications here.  Odd characters should have been 
C     removed ahead of time in FIXFILE.  This routine reads the
C     temporary file created by FIXFILE.
C
      INCLUDE  'rdcat.inc'
      INCLUDE  'newsrc.inc'
C
      INTEGER   I, ICAT, IR, IH, IN, JN, IFL, NOBS, INLEN
      INTEGER   RAH, DECD, RAM, DECM, RDSRC, ISTAT, LEN1
      DOUBLE PRECISION  RAS, DECS
      LOGICAL   HEADDONE
      CHARACTER INLINE*256, NEWSIGN*1
C
C ----------------------------------------------------------------------
C
      WRITE(*,*) ' '
      WRITE(*,'(A,A)') 
     1   ' Reading new catalog: ', GEOFILE(1:LEN1(GEOFILE))
      WRITE(*,*) ' '
C
C     Initialize counters etc.
C
      NCAT = 0
      HEADDONE = .FALSE.
C
C
C     Now read the new catalog for actual content.
C
C     Return here for the next line from the new input file.
C
  200 CONTINUE
C
C        Count the input sources.  This will be the index for the
C        arrays during the read.  It will go one too high for the
C        last read.  Fix that at the end.
C
         NCAT = NCAT + 1
         IF( NCAT .GT. MSRC ) THEN
            WRITE(*,*) 'Too many sources in new file.  Increase MCAT.'
            STOP
         END IF
C
C        Initialize the source variables that might not get set.
C
         DO I = 1, MAL
            NEWNAME(I,NCAT) = ' '
         END DO
         IFLXREF(NCAT) = ' '
         INEQUIN(NCAT) = ' '
         DO IFL = 1, MFLX
            IFLX(IFL,NCAT) = 0.0
         END DO
C
C        Read a line of the input file.  Set up to skip header lines.
C        Do not read lines of SCHED format files.
C
         IF( SOUFMT .NE. 'SCHED' ) THEN
            READ( 10, '(A)', END=300 ) INLINE
            INLEN = LEN1(INLINE)
C           write(*,*) INLINE(1:LEN1(INLINE))
         END IF
C
C        First read a Goddard type source line.
C        Now there are two Goddard formats.  The Astro data was
C        presented to me in a different format from what we are
C        used to.  It also comes with 3 names, which is new.
C        This was some time before 2012.
C
         IF( SOUFMT .EQ. 'GSFC' .AND. LEN1(SOUFMT) .EQ. 4 ) THEN
            IF( INLINE(1:8) .EQ. 'SOU_GCO:' ) THEN
C
C              Read the line with fixed format.
C
               READ( INLINE, '( 10X, A8, 6X, I2, 1X, I2, 1X, F11.8,'//
     1              '3X, F11.4, 6X, A1, I2, 1X, I2, 1X, F10.7, 3X,'//
     2              ' F11.4, 22X, I8 )' )
     3             NEWNAME(1,NCAT), RAH, RAM, RAS, NEWRAE(NCAT), 
     4             NEWSIGN, DECD, DECM, DECS, NEWDECE(NCAT), NOBS
C
C
C              Set the REMARK based on the user provided strings since
C              it is not in the file.  There are no fluxes so those
C              may come from the old file.  Don't set IFLXREF
C
               INREMARK(NCAT) = USRREMRK
               INEQUIN(NCAT) = 'J2000'
C
C              The RA error appears to be mas of coordinate error, so
C              no change needed.
C
            ELSE
C
               NCAT = NCAT -1
C
C              Skip the line.
C
               GO TO 200
            END IF
C
C        The GSFC format of the astro solutions, which is different:
C
         ELSE IF( SOUFMT .EQ. 'GSFCAST' ) THEN
C
C           Read the 2010a_astro and later astro catalogs.
C           For this format, there is nothing distinctive about the
C           actual source lines rather than the header lines.  But
C           if there is a string "--------" at the start of the line,
C           all lines before are header and lines after are data except
C           one blank line.  Look for that string of dashes.
C
            IF( HEADDONE .AND. INLEN .GT. 0 ) THEN
C
C              Read the line with fixed format.
C
               READ( INLINE, '( A8, 1X, A8, 1X, A10,'//
     1              '2X, I2, 1X, I2, 1X, F9.6, 2X, F6.3, '//
     2              '2X, A1, I2, 1X, I2, 1X, F8.5, 2X, F6.3,'//
     3              '10X, I6 )' )
     4             NEWNAME(1,NCAT), NEWNAME(2,NCAT), NEWNAME(3,NCAT), 
     5             RAH, RAM, RAS, NEWRAE(NCAT), 
     6             NEWSIGN, DECD, DECM, DECS, NEWDECE(NCAT), NOBS
C
C
C              Set the REMARK based on the user provided strings since
C              it is not in the file.  There are no fluxes so those
C              may come from the old file.  Don't set IFLXREF
C
               WRITE( INREMARK(NCAT), '( A, I5, A )' )
     1            USRREMRK(1:LEN1(USRREMRK)), NOBS, ' obs.'
               INEQUIN(NCAT) = 'J2000'
C
C              The RA error appears to be mas of coordinate error, so
C              no change needed.
C
            ELSE 
C
               NCAT = NCAT - 1
C
               IF( INLINE(1:8) .EQ. '--------' ) HEADDONE = .TRUE.
C
C              Skip the line
C              Recent versions have a blank after the line with the dashes.
C              Deal with that by testing INLEN above.
C              More recent versions have no dashes.  Aarg!  Add the 
C              version with the verticle bars.
C
               GO TO 200
            END IF
C
C
         ELSE IF( SOUFMT .EQ. 'GSFCAS2' ) THEN
C
C           In Dec. 2014 with a preliminary astro catalog from GSFC, I 
C           encountered yet another format.  This will be called 
C           GSFCAS2.
C
C           For this format, there is nothing distinctive about the
C           actual source lines rather than the header lines.  But
C           there is a line of column delimiters just before the actual
C           data.
C
C           There is only one source name.  There is a column for IAU
C           names, but it is not filled in.  Leave reading it as an
C           option, but blank it if it's set to 0, as all are in the
C           file I have.  The unused column is the first, so treat that
C           as NEWNAME(2,NCAT)
C
C           The errors are in arcseconds.
C
            IF( HEADDONE .AND. INLEN .GT. 0 ) THEN
C
C              Read the line with fixed format.
C
               READ( INLINE, '( A8, 1X, A8, '//
     1              '1X, I2, 1X, I2, 1X, F11.8, 1X, '//
     2              'A1, I2, 1X, I2, 1X, F10.7, 1X, F10.8, 1X, F9.7, '//
     3              '38X, I6 )' )
     4             NEWNAME(2,NCAT), NEWNAME(1,NCAT), RAH, RAM, RAS, 
     6             NEWSIGN, DECD, DECM, DECS, 
     7             NEWRAE(NCAT), NEWDECE(NCAT), NOBS
               WRITE( *, '( A8, 1X, A8, '//
     1              '1X, I2, 1X, I2, 1X, F11.8, 1X, '//
     2              'A1, I2, 1X, I2, 1X, F10.7, 1X, F10.8, 1X, F9.7, '//
     3              '38X, I6 )' )
     4             NEWNAME(2,NCAT), NEWNAME(1,NCAT), RAH, RAM, RAS, 
     6             NEWSIGN, DECD, DECM, DECS, 
     7             NEWRAE(NCAT), NEWDECE(NCAT), NOBS
C
               IF( NEWNAME(2,NCAT) .EQ. '0' ) NEWNAME(2,NCAT) = ' '
               NEWNAME(3,NCAT) = ' '
C
C              The RA error appears to be the actual coordinate error
C              in arcseconds.  Convert to mas.  No Dec correction needed.
               NEWRAE(NCAT) = NEWRAE(NCAT) * 1000.0
               NEWDECE(NCAT) = NEWDECE(NCAT) * 1000.0
C
C              Set the REMARK based on the user provided strings since
C              it is not in the file.  There are no fluxes so those
C              may come from the old file.  Don't set IFLXREF
C
               WRITE( INREMARK(NCAT), '( A, I5, A )' )
     1            USRREMRK(1:LEN1(USRREMRK)), NOBS, ' obs.'
               INEQUIN(NCAT) = 'J2000'
C
            ELSE 
C
               NCAT = NCAT - 1
C
               IF( INLINE(1:24) .EQ. '        |        |  |  |' ) 
     1              HEADDONE = .TRUE.
C
C              Skip the line
C
               GO TO 200
            END IF
C
C
C        Alternatively, read a line from Petrov's *.txt file.  That was
C        the only file that has the flux densities.  Note added Mar 2014.
C        Petrov's keyin files now have the flux densities as of at least
C        2012, so the .txt files will likely not be used in the future.
C
         ELSE IF( SOUFMT .EQ. 'PETROV' ) THEN
C    
            IF( INLINE(1:1) .NE. '#' ) THEN
               READ( INLINE, '( 3X, A8, 1X, A10, 2X, I2, 1X, I2, '//
     1              '1X, F9.6, 1X, A1, I2, 1X, I2, 1X, F8.5, F8.2, '//
     2              ' F7.2, 8X, I7, 5( F8.3,F7.3 ), 2X, A15 )' )
     3             NEWNAME(2,NCAT), NEWNAME(1,NCAT), RAH, RAM, RAS, 
     4             NEWSIGN, DECD, DECM, DECS, 
     5             NEWRAE(NCAT), NEWDECE(NCAT), NOBS,
     6             IFLX(2,NCAT), IFLX(3,NCAT), IFLX(5,NCAT), 
     7             IFLX(6,NCAT), IFLX(8,NCAT), IFLX(9,NCAT),
     8             IFLX(11,NCAT), IFLX(12,NCAT), IFLX(14,NCAT), 
     9             IFLX(15,NCAT), IFLXREF(NCAT)
C
C              Set the assumed frequencies.
C
               IFLX(1,NCAT) = 2.3
               IFLX(4,NCAT) = 4.9
               IFLX(7,NCAT) = 8.6
               IFLX(10,NCAT) = 15.2
               IFLX(13,NCAT) = 22.2
C
C              Set the remarks based on the user provided strings since
C              they are not in the file.
C
               WRITE( INREMARK(NCAT), '( A, I5, A )' )
     1            USRREMRK(1:LEN1(USRREMRK)), NOBS, ' obs.'
               IFLXREF(NCAT) = USRFLXRF
               INEQUIN(NCAT) = 'J2000'
C
C              The RA error appears to be mas of coordinate error, so
C              no change needed.
C
            ELSE
               NCAT = NCAT - 1
               GO TO 200
            END IF
C
C        The input file is SCHED format.  Petrov is trying to provide
C        these, but they still have oddities - he seems to have no
C        compunctions about adding characters where numbers are expected.
C        That is why the file was filtered earlier.
C
         ELSE IF( SOUFMT .EQ. 'SCHED' ) THEN
C
C           Read an entry from the new catalog.
C          
            ISTAT = RDSRC( 10, .FALSE., 'TEMP.DELETE' )
            IF( ISTAT .NE. 0 ) THEN 
               GO TO 300
            END IF
C          
C           Collect the data.  For SCHED input, there is a REMARK in
C           the file.  But there is no flux reference in the 2011 
C           petrov file.  For such cases, leave IFLXREF blank.
C          
            DO I = 1, MAL
               NEWNAME(I,NCAT) = SRCNAM(I)
            END DO
            INEQUIN(NCAT)         = SRCEQ
            NEWRA(NCAT)          = SRCRA
            NEWDEC(NCAT)         = SRCDEC
            NEWRAE(NCAT)          = SRCRAE
            NEWDECE(NCAT)         = SRCDECE
            INREMARK(NCAT)        = SRCRMK
            IFLXREF(NCAT)         = SRCFREF
            DO I = 1, MFLX
               IFLX(I,NCAT)   = SRCFLUX(I)
            END DO
            IF(IFLX(1,NCAT) .NE. 0.0 .AND. IFLXREF(NCAT) .EQ. ' ') THEN
               IFLXREF(NCAT) = USRFLXRF
            END IF
C
C           The RA error appears to be mas of coordinate error, so
C           no change needed.
C
         ELSE
            WRITE(*,*) 'Unrecognized format'
            STOP
         END IF
C
C        Extract the radian forms of RA and DEC for all of the 
C        non-SCHED formats.
C
         IF( SOUFMT .NE. 'SCHED' ) THEN
            NEWRA(NCAT) = 3600.D0 * RAH + 60.D0 * RAM + RAS
            NEWRA(NCAT) = NEWRA(NCAT) * 15.D0 * RADSEC
            NEWDEC(NCAT) = 3600.D0 * DECD + 60.D0 * DECM + DECS
            NEWDEC(NCAT) = NEWDEC(NCAT) * RADSEC
            IF( NEWSIGN .EQ. '-' ) NEWDEC(NCAT) = -1.D0 * NEWDEC(NCAT)
         END IF
         GO TO 200
C
C     Jump here at end.
C
  300 CONTINUE
      NCAT = NCAT - 1
C
C     Check for duplicate names.  I think I've seen at least one.
C     Just search a reasonable range, assuming the files are sorted
C     by RA so duplicate names should be close.
C
      IR = 50
C
      DO ICAT = 1, NCAT - 1
         IH = MIN( ICAT + IR, NCAT )
         DO I = ICAT + 1, IH
            IN = 1
            DO WHILE ( NEWNAME(IN,ICAT) .NE. ' ' .AND. IN .LE. MAL )
               JN = 1
               DO WHILE ( NEWNAME(JN,I) .NE. ' ' .AND. JN .LE. MAL )
                  IF( NEWNAME(IN,ICAT) .EQ. NEWNAME(JN,I) ) THEN
                     WRITE(*, '( 2A )' )
     1                  'Sources with duplicate names in ', 
     2                  GEOFILE(1:LEN1(GEOFILE))
                     WRITE(*, '( A, I6, 4A )' )
     1                  '     Source ', ICAT, 
     2                  ' First and duplicate names: ',
     3                  NEWNAME(1,ICAT), ' ', NEWNAME(IN,ICAT)
                     WRITE(*, '( A, I6, 4A )' )
     1                  '     Source ', I,
     2                  ' First and duplicate names: ',
     3                  NEWNAME(1,I), ' ', NEWNAME(JN,I)
                  END IF
                  JN = JN + 1
               END DO
               IN = IN + 1
            END DO
         END DO
      END DO
C
C     Check for NONAME here too (see comments in GETOLD)
C
      DO ICAT = 1, NCAT
         DO I = 1, MAL
            IF( NAME(I,ICAT) .EQ. 'NONAME' ) THEN
               WRITE(*,*) 'Found default source NONAME in new catalog:',
     1              ICAT, I, ' ', NEWNAME(1,ICAT), NEWNAME(I,ICAT)
            END IF
         END DO
      END DO
C
      RETURN
      END
