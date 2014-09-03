      PROGRAM SEARCHSRC
C
C     Quick and dirty source search tool.
C
C     Read a SCHED catalog.
C     Filter by position, flux density and resolution
C     Write a new SCHED catalog.
C     Adapted from newsrc.f  Jun. 12, 2013
C     Latest mods (add resolution) Aug. 5, 2014
C
C
      INCLUDE 'rdcat.inc'
      INCLUDE 'searchsrc.inc'
C
      INTEGER          LEN1, I, ISRC, J, K
      INTEGER          IER, IO
      INTEGER          ICH
      INTEGER          ISTAT, RDSRC
      INTEGER          NIN, NOUT
      INTEGER          IRAH, IRAM, IRAS, IDECD, IDECM, IDECS
      DOUBLE PRECISION PI, RADSEC
      DOUBLE PRECISION REFRA, REFDEC, SLA_DSEP
      REAL             MAXOFF, MINFLUX, MINRES
      LOGICAL          GOTFLUX, ISNEW(MSRC)
      CHARACTER        TEXT*80, LASTEQ*5
      CHARACTER        RAOUT*16, DECOUT*17, TFORM*17
      CHARACTER        OUTLINE*132
C  -------------------------------------------------------------------
      write(*,*) '--------------------------------------------------'
      write(*,*) '-------------   NEWSRC starting.------------------'
      write(*,*) '--------------------------------------------------'
      write(*,*) 'Requests for user input: '
C
C     Get the constant PI (as in 3.14...) the tricky way.
C
      PI = 4.0D0 * DATAN( 1.D0 )
      RADSEC = PI / ( 3600.D0 * 180.D0 )
C
C     Get the user input.
C
C     First get the catalog:
C
      WRITE(*,*) '  SCHED source file to search:'
      READ(*,'(A)') SRCFILE
C
C     Get user position, offset, and minimum flux.
C
      WRITE(*,*) 
     1    '  Reference positions RA, Dec, 3 integers each H M S D M S:'
      READ(*,*) IRAH, IRAM, IRAS, IDECD, IDECM, IDECS
      REFRA = RADSEC * 15.D0 * ( IRAH * 3600.D0 + IRAM * 60.D0 + IRAS )
      REFDEC = RADSEC * ( IDECD * 3600.D0 + IDECM * 60.D0 + IDECS )
C
      WRITE(*,*) '  Maximum offset (deg), minimum flux, and ',
     1      'minimum resolution (fract). '
      READ(*,*) MAXOFF, MINFLUX, MINRES
C
      WRITE(*,*) '  Output SCHED catalog:'
      READ(*,'(A)') NEWFILE
C
C     Echo the user input:
C
      WRITE(*,*) ' '
      WRITE(*,*) '--------Summary of user input.------------'
      WRITE(*,*) 'Input SCHED catalog:    ', 
     1            SRCFILE(1:LEN1(SRCFILE))
      WRITE(*,'( A, 3I4, A, 3I4 )' ) ' Reference position, RA: ', 
     1       IRAH, IRAM, IRAS, '  Dec: ', IDECD, IDECM, IDECS
C
      WRITE(*,'( A, F8.3, A, F8.3, A )' )
     1     '  Write sources above ', MINFLUX, ' Jy within ', 
     2     MAXOFF, ' deg of ref position.'
C
      WRITE(*,*) 'Output in SCHED catalog format file:     ', 
     1           NEWFILE(1:LEN1(NEWFILE))
      WRITE(*,*) '---------End of user input.---------'
C
C     Get the old catalog.
C
C
      NSRC = 0
50    CONTINUE
C
C        Read the catalog.
C
         ISTAT = RDSRC( 8, .TRUE., SRCFILE )
         IF( ISTAT .NE. 0 ) THEN 
            GO TO 100
         END IF
C
C        Collect the data.
C
         NSRC            = NSRC + 1
         IF( NSRC .GT. MSRC ) THEN
            WRITE(*,*) 'Need to allow more sources. '
            STOP
         END IF
         VER             = SRCVER
         DO I = 1, MAL
            NAME(I,NSRC) = SRCNAM(I)
         END DO
         CALCODE(NSRC)   = SRCCAL
         EQUINOX(NSRC)   = SRCEQ
         RA(NSRC)        = SRCRA
         DEC(NSRC)       = SRCDEC
         RAERR(NSRC)     = SRCRAE
         DECERR(NSRC)    = SRCDECE
         REMARK(NSRC)    = SRCRMK
         FLUXREF(NSRC)   = SRCFREF
         DO I = 1, MFLX
            FLUX(I,NSRC)   = SRCFLUX(I)
         END DO
         ISNEW(NSRC)     = .FALSE.
         GO TO 50
  100 CONTINUE
C
C     Tell the user something.
C
      NIN = NSRC
      WRITE(*,*) ' '
      WRITE(*,'( I6, A , A )' )
     1   NIN, ' Sources read from old SCHED catalog:  ', SRCFILE
C
C     Get the separations (deg) and maximum flux densities for each
C     source.  Keep a resolution number corresponding to the 
C     peak flux.  Set a keep flag.
C
      DO ISRC = 1, NSRC
         SEP(ISRC) = SLA_DSEP( RA(ISRC), DEC(ISRC), REFRA, REFDEC )
         SEP(ISRC) = SEP(ISRC) * 180.D0 / PI
C       write(*,*) isrc, '  ', name(1,isrc), sep(isrc), 
C     1                 ra(isrc), dec(isrc)
         PEAKFLX(ISRC) = 0.D0
         DO I = 1, MFLX
            IF( MOD( I, 3 ) .EQ. 2 .AND. 
     1            FLUX(I,ISRC) .GT. PEAKFLX(ISRC) ) THEN
               PEAKFLX(ISRC) = MAX( FLUX(I,ISRC), PEAKFLX(ISRC) )
               IF( I+1 .LE. MFLX ) THEN
                  RESOLV(ISRC) = FLUX(I+1,ISRC) / FLUX(I,ISRC)
               ELSE
                  RESOLV(ISRC) = 0.0
               END IF
            END IF
         END DO
         KEEP(ISRC) = SEP(ISRC) .LE. MAXOFF .AND.
     1                PEAKFLX(ISRC) .GT. MINFLUX .AND.
     2                RESOLV(ISRC) .GT. MINRES
      END DO      
C
C     Open the output file so that header lines can be transferred.
C
C     Write the new file.
C
      OPEN( UNIT=12, FILE=NEWFILE, STATUS='NEW', ACCESS='SEQUENTIAL',
     1      FORM='FORMATTED', IOSTAT=IER )
      IF( IER .NE. 0 ) THEN
         WRITE(*,*) 'Error opening ', NEWFILE(1:LEN1(NEWFILE))
         STOP
      END IF
C
      WRITE( 12, '( A )' ) ' '
      WRITE( 12, '( 2A )' ) '!  Flux numbers are in triplets of ',
     1     'frequency (GHz), total and unresolved flux (Jy)'
      WRITE( 12, '( 2A )' ) '!  Flux of -9.99 means no data.  ',
     1     '-1 is unknown limit.'
      WRITE( 12, '( 2A )' ) '!  Other negative is -1*(upper limit).'
      WRITE( 12, '( A )' ) ' '
      WRITE( 12, '( A )' ) ' '
C
C -----------------------
C     Sort the resulting file.
C     Keep the template, but don't sort for now.  Maybe later
C     sort on distance.
C
C      DO ISRC = 1, NSRC
C         RASORT(ISRC) = RA(ISRC)
C         IND(ISRC) = ISRC
C      END DO
C      CALL SORTCW( NSRC, RASORT, IND )
C      WRITE(*,*) 'Finished sorting new data.'
C      LASTEQ = '-----'
C ---------------------------------- 
      NOUT = 0
C
C
      DO ISRC = 1, NSRC
         TEXT = ' '
C         IO = IND(ISRC)    use if sorting
         IO = ISRC
         IF( KEEP(ISRC) ) THEN
            NOUT = NOUT + 1
            IF( EQUINOX(IO) .NE. LASTEQ ) THEN
               WRITE( 12, '( A, A )' ) 'EQUINOX = ', EQUINOX(IO)
               LASTEQ = EQUINOX(IO)
            END IF
            WRITE( TEXT, '( A )' ) 'SOURCE='
            DO I = 1, MAL
               IF( NAME(I,IO) .NE. ' ' ) THEN
                  J = LEN1( TEXT ) + 1
                  K = J + LEN1(NAME(I,IO) ) + 2
                  TEXT(J:K) = ''''//NAME(I,IO)(1:LEN1(NAME(I,IO)))//
     1                 ''','
               END IF
            END DO
            K = LEN1(TEXT) - 1
            WRITE( 12, '(A)' ) TEXT(1:K)
            RAOUT = TFORM( RA(IO), 'T', 0, 2, 10, ':: ' )
            DECOUT = TFORM( DEC(IO), 'D', 1, 2, 9, ':: ' )
            WRITE( 12, '( 5A, F8.3, A, F8.3, 3A )' )
     1          '     RA=', RAOUT(1:LEN1(RAOUT)), 
     2          ' DEC= ', DECOUT(1:LEN1(DECOUT)),
     3          ' RAERR=', RAERR(IO), ' DECERR=', DECERR(IO),
     4          ' CALCODE=''', CALCODE(IO), ''' '
            WRITE( 12, '( A, A, A )' )
     1          '     REMARKS=''', REMARK(IO)(1:LEN1(REMARK(IO))), ''''
            GOTFLUX = .FALSE.
            DO I = 1, MFLX-2, 3
               IF( FLUX(I+1,IO) .GT. -9.0 .AND. 
     1             FLUX(I+1,IO) .NE. 0.0 .AND.
     2             FLUX(I+2,IO) .GT. -9.0 .AND.
     3             FLUX(I+2,IO) .NE. 0.0 ) GOTFLUX = .TRUE.
            END DO
            IF( GOTFLUX ) THEN
               WRITE( OUTLINE, '( A )' ) '     FLUX = '
               DO I = 1, MFLX-2, 3
                  IF( ( FLUX(I+1,IO) .GT. -9. .AND. 
     1                  FLUX(I+1,IO) .NE. 0.0 ) .OR.
     2                ( FLUX(I+2,IO) .GT. -9. .AND.
     3                  FLUX(I+2,IO) .NE. 0.0 ) ) THEN
                     ICH = LEN1( OUTLINE )
                     IF( OUTLINE(ICH:ICH) .NE. '=' ) THEN
                        ICH = ICH + 1
                        OUTLINE(ICH:ICH) = ','
                     END IF
                     IF( ICH .GT. 85 ) THEN
                        WRITE( 12, '( A )' ) OUTLINE(1:ICH+1)
                        OUTLINE = '      '
                        ICH = 6
                     END IF
                     WRITE( OUTLINE(ICH+1:ICH+27), 
     1                   '( F7.2, A, F6.2, A, F6.2 )' )
     2                   FLUX(I,IO), ',', FLUX(I+1,IO), 
     3                   ',', FLUX(I+2,IO)
                  END IF         
               END DO
               ICH = LEN1( OUTLINE )
               IF( ICH .LT. 70 ) THEN
                  OUTLINE = OUTLINE(1:ICH) // 
     1               '    FLUXREF = ''' // 
     2               FLUXREF(IO)(1:LEN1(FLUXREF(IO))) // ''''
                  WRITE( 12, '( A )' ) OUTLINE(1:LEN1(OUTLINE))
                  OUTLINE = ' '
               ELSE
                  WRITE( 12, '( A )' ) OUTLINE(1:ICH)
                  OUTLINE = '      '
                  WRITE( 12, '( A, A, A )' ) '     FLUXREF = ''', 
     1                  FLUXREF(IO)(1:LEN1(FLUXREF(IO))) // ''''
               END IF
            END IF
            WRITE( 12, '( A, F10.3 )' ) '     ! Separation = ', 
     1          SEP(IO)
            WRITE( 12, '( A )' ) ' /'
         END IF

      END DO
      WRITE(*,'( I6, A )') NSRC, ' distinct sources processed.'
      WRITE(*,'( I6, A, A )') NOUT, ' sources written to output file: ', 
     1      NEWFILE(1:LEN1(NEWFILE))
C
      STOP
      END
