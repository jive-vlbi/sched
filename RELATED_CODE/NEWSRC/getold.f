      SUBROUTINE GETOLD( NIN )
C
C     Routine for NEWSRC to read the old SCHED catalog that will
C     serve as the basis for the new one as the new catalog replaces
C     or auguments the old one.  Even when the new one is replacing
C     the old one, the old one will provide aliases and flux densities
C     where appropriate.
C
C     NIN is the output number of sources read.  It matches NSRC after
C     this routine, but later NSRC will change.
C
      INCLUDE   'rdcat.inc'
      INCLUDE   'newsrc.inc'
C
      INTEGER    I, NIN, ISTAT, LEN1, RDSRC
      INTEGER    ISRC, IR, IH, IN, JN
C ----------------------------------------------------------------
C     The continue line is where we loop back to for the next source.
C
      NSRC = 0
50    CONTINUE
C
C        Read the old catalog using the SCHED routine RDSRC.  The 
C        results are in the variables in rdcat.inc.
C
         ISTAT = RDSRC( 8, .TRUE., SRCFILE )
         IF( ISTAT .NE. 0 ) THEN 
            GO TO 100
         END IF
C
C        Collect the data from the rdcat.inc parameters.
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
         ISNEW(NSRC)     = 0
         GO TO 50
  100 CONTINUE
C
C     Tell the user something about the old SCHED catalog.
C
      NIN = NSRC
      WRITE(*,*) ' '
      WRITE(*,'( A, A )' )
     1  ' Finished reading old SCHED catalog: ', 
     2   SRCFILE(1:LEN1(SRCFILE))
      WRITE(*,'( A, I5, A )' ) '   Found ', NSRC, ' sources.'
      WRITE(*,*) ' '
C
C     Check for duplicate names.
C
      IR = 50
C
      DO ISRC = 1, NSRC - 1
         IH = MIN( ISRC + IR, NSRC )
         DO I = ISRC + 1, IH
            IN = 1
            DO WHILE ( NAME(IN,ISRC) .NE. ' ' .AND. IN .LE. MAL )
               JN = 1
               DO WHILE ( NAME(JN,I) .NE. ' ' .AND. JN .LE. MAL )
                  IF( NAME(IN,ISRC) .EQ. NAME(JN,I) ) THEN
                     WRITE(*, '( 2A )' )
     1                  'Sources with duplicate names in ', 
     2                  SRCFILE(1:LEN1(SRCFILE))
                     WRITE(*, '( A, I6, 4A )' )
     1                  '     Source ', ISRC, 
     2                  ' First and duplicate names: ',
     3                  NAME(1,ISRC), ' ', NAME(IN,ISRC)
                     WRITE(*, '( A, I6, 4A )' )
     1                  '     Source ', I,
     2                  ' First and duplicate names: ',
     3                  NAME(1,I), ' ', NAME(JN,I)
                  END IF
                  JN = JN + 1
               END DO
               IN = IN + 1
            END DO
         END DO
      END DO
C
C     Check for the default name from SCHED catalogs.  It's presence
C     would suggest a problem.  For a while, I got many.  It was fixed
C     by being sure INEQUIN was set.  I really don't understand how
C     they were related, so leave this debug code here as part of the
C     program.
C
      DO ISRC = 1, NSRC
         DO I = 1, MAL
            IF( NAME(I,ISRC) .EQ. 'NONAME' ) THEN
               WRITE(*,*) 'Found default source NONAME in old catalog:',
     1              ISRC, I, ' ', NAME(1,ISRC), NAME(I,ISRC)
            END IF
         END DO
      END DO
C
C
      RETURN
      END
