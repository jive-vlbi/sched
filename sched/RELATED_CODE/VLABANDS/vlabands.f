      PROGRAM VLABANDS
C
C     Program to read VLA file that contains the band specifications
C     and create a data statement for SCHED subroutine CHKVLA and
C     create a table for the SCHED Manual.
C
      INTEGER           MBANDS, NB, IB, NIF, NROT, LEN1
      PARAMETER         (MBANDS=25)
      DOUBLE PRECISION  FEAB(MBANDS), FECD(MBANDS)
      INTEGER           SYNA(MBANDS), SYNB(MBANDS) 
      DOUBLE PRECISION  FLUKEA(MBANDS), FLUKEB(MBANDS)
      DOUBLE PRECISION  SYNPA(MBANDS), SYNPB(MBANDS)
      DOUBLE PRECISION  LO1A(MBANDS), LO1D(MBANDS)
      DOUBLE PRECISION  FYIFA, FYIFB, FEBWA, FEBWD
      DOUBLE PRECISION  FYA1(MBANDS), FYA2(MBANDS) 
      DOUBLE PRECISION  FYD1(MBANDS), FYD2(MBANDS)
      CHARACTER         BAND(MBANDS)*2, IF(MBANDS)*10, ROT(MBANDS)*10
      CHARACTER         INLINE*80, NAME*80, VLABW(MBANDS)*4
      CHARACTER         FORM1*40, FORM2*40, FORMA*40, FORMD*40
      CHARACTER         FEFILTER(MBANDS)*4
C --------------------------------------------------------------------
C     Open vla file as delivered by Ken Sowinski.
C
      WRITE(*,*) 'Input file:'
      READ(*,'(A)') NAME
      OPEN( UNIT=8, FILE=NAME, FORM='FORMATTED', STATUS='OLD' )
C
C     Loop through inputs filling those found.
C
      NB = 0
  100 CONTINUE
         READ( 8, '(A)', END=990 ) INLINE
C
C        Look for LO card.
C
         IF( INLINE(3:4) .EQ. 'LO' ) THEN
            NB = NB + 1
            BAND(NB) = INLINE(1:2)
            READ( INLINE(7:20), '( 2F7.2 )' ) FEAB(NB), FECD(NB)
            READ( INLINE(26:30), '( I5 )' ) SYNA(NB)
            READ( INLINE(36:40), '( I5 )' ) SYNB(NB)
            VLABW(NB) = INLINE(55:58)
            IF( VLABW(NB) .EQ. '    ' ) VLABW(NB) = '0000'
            FEFILTER(NB) = INLINE(55:58)
            IF( FEFILTER(NB) .EQ. '    ' ) FEFILTER(NB) = '0000'
            IF(NB) = INLINE(61:70)
            ROT(NB) = INLINE(71:80)
            IF( MOD( ABS( SYNA(NB) ), 50 ) .EQ. 10 ) THEN
               SYNPA(NB) = SIGN( ABS( SYNA(NB) ) + 0.1D0, SYNA(NB) )
            ELSE IF( MOD( ABS( SYNA(NB) ), 50 ) .EQ. 40 ) THEN
               SYNPA(NB) = SIGN( ABS( SYNA(NB) ) - 0.1D0, SYNA(NB) )
            ELSE
               WRITE(*,*) ' Odd SYNA ', SYNA(NB)
            END IF
            IF( MOD( ABS( SYNB(NB) ), 50 ) .EQ. 10 ) THEN
               SYNPB(NB) = SIGN( ABS( SYNB(NB) ) + 0.1D0, SYNB(NB) )
            ELSE IF( MOD( ABS( SYNB(NB) ), 50 ) .EQ. 40 ) THEN
               SYNPB(NB) = SIGN( ABS( SYNB(NB) ) - 0.1D0, SYNB(NB) )
            ELSE
               WRITE(*,*) ' Odd SYNB ', SYNB(NB)
            END IF
            WRITE(*,*) NB, ' ', BAND(NB), 
     1          SYNA(NB), SYNB(NB), SYNPA(NB), SYNPB(NB)
         END IF
C
C        Now the FI card.  Assume all bands read by now.
C
         IF( INLINE(3:4) .EQ. 'FI' ) THEN
            DO IB = 1, NB
               IF( INLINE(1:2) .EQ. BAND(IB) ) THEN
                  READ( INLINE(17:30), '( F14.6 )' ) FLUKEA(IB)
                  READ( INLINE(37:50), '( F14.6 )' ) FLUKEB(IB)
               END IF
            END DO  
         END IF
C
C        Return for next line
C
         GO TO 100
C
  990 CONTINUE
C
C     Get the FIRSTLO's and bands.
C
      DO IB = 1, NB
C
C        Get the effect of synthesizers other than the first LO.
C
         FYIFA = SYNPA(IB) + 900. + FLUKEA(IB) 
         FYIFB = SYNPB(IB) + 800. + FLUKEB(IB)
C
C        Get the front end bandwidth.
C
         FEBWA = 50.D0
         FEBWD = 50.D0
         IF( FEFILTER(IB)(1:1) .EQ. '1' ) FEBWA = 25.D0
         IF( FEFILTER(IB)(4:4) .EQ. '1' ) FEBWD = 25.D0
         IF( FEFILTER(IB)(1:1) .EQ. '2' ) FEBWA = 12.5D0
         IF( FEFILTER(IB)(4:4) .EQ. '2' ) FEBWD = 12.5D0
C
C        Get FIRSTLO (=LO1A) and the VLA band edges.
C   
         IF( BAND(IB)(2:2) .EQ. 'X' .OR. BAND(IB)(2:2) .EQ. 'U' ) THEN
            FYA1(IB) = 1000.D0 * FEAB(IB) - FYIFA
            FYD1(IB) = 1000.D0 * FECD(IB) - FYIFB
            FYA2(IB) = FYA1(IB) - FEBWA
            FYD2(IB) = FYD1(IB) - FEBWD
            LO1A(IB) = FYA1(IB) + 600.D0
            LO1D(IB) = FYD1(IB) + 600.D0
         ELSE IF( BAND(IB)(2:2) .EQ. 'Q' ) THEN
            FYA1(IB) = 1000.D0 * ( FEAB(IB) - FECD(IB) ) + FYIFA
            FYD1(IB) = 1000.D0 * ( FEAB(IB) - FECD(IB) ) + FYIFB
            FYA2(IB) = FYA1(IB) + FEBWA
            FYD2(IB) = FYD1(IB) + FEBWD
            LO1A(IB) = FYA1(IB) - 600.D0
            LO1D(IB) = FYD1(IB) - 600.D0
         ELSE
            FYA1(IB) = 1000.D0 * FEAB(IB) + FYIFA
            FYD1(IB) = 1000.D0 * FECD(IB) + FYIFB
            FYA2(IB) = FYA1(IB) + FEBWA
            FYD2(IB) = FYD1(IB) + FEBWD
            LO1A(IB) = FYA1(IB) - 600.D0
            LO1D(IB) = FYD1(IB) - 600.D0
         END IF
      END DO
C
C     Now write the desired stuff - old form.
C
      DO IB = 1, NB
         WRITE( *, '( 5X, I1, 13X, 3A, F10.2, A, F8.1, A, F8.1, A )' )
     1      MOD(IB,10), '''', BAND(IB), ''',', LO1A(IB), 'D0,', 
     2      FEAB(IB), 'D0,', FECD(IB), 'D0,'
      END DO
C
C     Write the LO card stuff in a form for a data statement.
C
      WRITE(*, '( 1X, /, 10X, A, /, A, A )' )
     1  'VLA STANDARD BANDS - VLA SYNTHESIZER SETTINGS ',
     2  '       BAND   FEAB   FECD  SYNA    SYNB   ',
     3   '  FLUKEA     FLUKEB  FEFILT'
      DO IB = 1, NB
         WRITE( *, '( 5X, I1, 1X, 3A, 2(F4.1,A),  2(F6.1,A), 
     1         2(F9.5,A), A )' )
     2      MOD(IB,10), '''', BAND(IB), ''',',  FEAB(IB), 'D0,', 
     3      FECD(IB), 'D0,', SYNPA(IB), 'D0,', SYNPB(IB), 'D0,',
     4      FLUKEA(IB),  'D0,',FLUKEB(IB),  'D0,'
      END DO
C
      WRITE(*, '( 1X, /, 10X, A, /, A )' )
     1  'VLA FILES ETC. ',
     2  '       BAND     BW  FEFILTER     IF     ROT '
      DO IB = 1, NB
         NIF = LEN1( IF(IB) )
         NROT = LEN1( ROT(IB) )
         WRITE( *, '( 5X, I1, 1X, 11A )' )
     1      MOD(IB,10), '''', BAND(IB), ''', ''', VLABW(IB), ''', ''',
     2      FEFILTER(IB), ''', ''', IF(IB)(1:NIF), ''', ''', 
     3      ROT(IB)(1:NROT), ''','
      END DO
C
C     For CHKVLA - list of valid bands.
C
      WRITE(*,*) ('''', BAND(IB), ''',', IB=1, NB )
C
C     Write a table for the Manual with SCHED type VLA parms.
C
      WRITE(*,'( 1X, /, 1X, /, 1X, / A, /, 1X )' ) '  MANUAL TABLES: '
      WRITE(*, '( 1X, /, 10X, A, /, A, A )' )
     1  'VLA STANDARD BANDS - VLA SYNTHESIZER SETTINGS ',
     2  'VLABAND  VLAFEAB  VLAFECD  VLASYNA   VLASYNB   ',
     3   'FLUKEA     FLUKEB  FEFILT'
      DO IB = 1, NB
         WRITE( *, '( 2X, A, 2F10.2, 2F10.1, 2F11.5, 2X, A )' )
     1      BAND(IB), FEAB(IB), FECD(IB), SYNPA(IB), SYNPB(IB),
     2      FLUKEA(IB), FLUKEB(IB), FEFILTER(IB)
      END DO
C
C     Write second table with FIRSTLO's and VLA passbands.  Adjust
C     the format depending on how many digits are needed.
C
      WRITE(*, '( 1X, /, 10X, A, /, A, A )' )
     1  'VLA STANDARD BANDS - VLBI FIRSTLO and VLA BANDPASSES',
     2  'VLABAND FIRSTLO A  FIRSTLO D         BANDPASS A   ',
     3  '        BANDPASS D'
      DO IB = 1, NB
C
C        First LO format adjustment.
C
         IF( MOD( ABS( LO1A(IB) ) + 0.0001D0, 0.1D0 ) .GT. 
     1            0.001D0 ) THEN
            FORMA = 'F12.5,'
         ELSE
            FORMA = 'F12.2,'
         END IF
         IF( MOD( ABS( LO1D(IB) ) + 0.0001D0, 0.1D0) .GT. 
     1            0.001D0 ) THEN
            FORMD = 'F12.5, 2X, '
         ELSE
            FORMD = 'F12.2, 2X, '
         END IF
C
C        Bandpasses format adjustment.
C
         IF( MOD( FYA1(IB)+0.0001D0, 0.1D0 ) .GT. 0.001D0 ) THEN
            FORM1 = 'F10.5, A, F9.5, 1X, '
         ELSE
            FORM1 = 'F10.2, A, F9.2, 1X, '
         END IF
         IF( MOD( FYD1(IB)+0.0001D0, 0.1D0) .GT. 0.001D0 ) THEN
            FORM2 = 'F10.5, A, F9.5 )'
         ELSE
            FORM2 = 'F10.2, A, F9.2 )'
         END IF
C
C        Write the table.
C
         WRITE( *, '( 2X, A, ' // FORMA // FORMD // FORM1 // FORM2 )
     1      BAND(IB), LO1A(IB), LO1D(IB), FYA1(IB), '-', FYA2(IB),
     1      FYD1(IB), '-', FYD2(IB)
      END DO
C
C     Add note at the bottom.
C
      WRITE(*,'( A,A )' )
     1   ' Note that IF A and C are usually at the same ',
     2   'frequency as are B and D.'
C
      STOP
      END


