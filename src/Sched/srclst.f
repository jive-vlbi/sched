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
      INTEGER       MSEP, ML
      PARAMETER     ( MSEP = 7 )
      PARAMETER     ( ML = 20 )
C
      INTEGER       ISRC, LEN1, IOUT, J, K, MODE, LENS, LENR, LEND
      INTEGER       NPAIRS, ISEP, IP
      INTEGER       KSRC, JN, KN
      INTEGER       HISTSEP(MSEP)
      REAL          HISTLEV(MSEP)
      REAL          MAXSEP, SRCSEP
      DOUBLE PRECISION  SLA_DSEP
      CHARACTER*16  TFORM, TRA20, TDEC20
      LOGICAL       ANYS(3)
      DATA          HISTLEV / 3.0, 5.0, 7.5, 10.0, 15., 20., 30. /
C ------------------------------------------------------------------
C     Detect if there are sources in any of the 3 categories.  Those
C     are 1) used in recording, 2) used in non-recoreding scans, and
C     3) used in multiple phase center lists.
C
      DO IP = 1, 3
         ANYS(IP) = .FALSE.      
      END DO
      DO ISRC = 1, MSRC
         IF( USEDREC(ISRC) ) ANYS(1) = .TRUE.
         IF( SUSED(ISRC) .AND. .NOT. USEDREC(ISRC) ) ANYS(2) = .TRUE.
         IF( USEDPHS(ISRC) .AND. .NOT. SUSED(ISRC) ) ANYS(3) = .TRUE.
      END DO
C
C     Actually write the main source list.
C
      DO IP = 1, 3
         IF( ANYS(IP) ) THEN
            CALL SRCWRT( IOUT, IP )
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
C     amount.  Make two passes through this part of the code.  For
C     the first, make a cumulative histogram of the number of pairs
C     of each separation.  Then determine the maximum separation to
C     use and still give a finite list.  Then go through writing the
C     list.  Never try going above 30 deg.
C
      IF( IOUT .EQ. ISUM .AND. MSRC .GE. 2 ) THEN
         DO ISEP = 1, MSEP
            HISTSEP(ISEP) = 0
         END DO
         MAXSEP = 1.0
         DO K = 1, 2
            IF( K .EQ. 2 ) THEN
               WRITE( IOUT, '( 1X, /, A, F6.2, A, /, 1X )' )
     1          ' Source separations in degrees for pairs closer than ',
     2            MAXSEP,' degrees.'
            END IF
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
     1                       RAP(KSRC), DECP(KSRC) ) / RADDEG
C
C                       On first pass, fill out the histogram.
C                       On the second, write the pair.
C
                        IF( K .EQ. 1 ) THEN
                           DO ISEP = 1, MSEP
                              IF( SRCSEP .LE. HISTLEV(ISEP) ) THEN
                                 HISTSEP(ISEP) = HISTSEP(ISEP) + 1
                              END IF
                           END DO
                        ELSE IF( SRCSEP .LE. MAXSEP ) THEN
                           WRITE( IOUT, 
     1                        '( 5X, A12, 2X, A12, 2X, F8.4 )' )
     2                        SOURCE(JN,ISRC), SOURCE(KN, KSRC), SRCSEP
                           NPAIRS = NPAIRS + 1
                        END IF
                     END IF
                  END DO
               END IF
            END DO
C
C           On the first pass, determine MAXSEP
C
            IF( K .EQ. 1 ) THEN
               DO ISEP = 1, MSEP
                  IF( HISTSEP(ISEP) .LE. 50 ) THEN
                     MAXSEP = HISTLEV(ISEP)
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






