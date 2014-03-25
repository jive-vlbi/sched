      SUBROUTINE WRTDAT
C
C     Subroutine for NEWLOC that writes various information and 
C     tables to standard output.
C
C     No longer compare with old locations.dat as I no longer have it.
C
      INCLUDE 'newloc.inc'
C
      INTEGER       LEN1
      DOUBLE PRECISION  CONST, CONST2
C ------------------------------------------------------------------
C
C     Some summary information.
C
      WRITE(*,*)
      WRITE(*,*)
      WRITE( *, '( A, I4, A, A )' )
     1   ' Read ', NS, ' stations from ', POSFILE(1:LEN1(POSFILE))
C
      WRITE( *, '( A, I4, A, A )' )
     1   ' Read ', NR, ' station velocities from ', 
     2   VELFILE(1:LEN1(VELFILE))
      WRITE( *, '( A, I4, A, A )' )
     1   ' Read ', NO, ' axis offsets from ', 
     2   OFFFILE(1:LEN1(VLAFILE))
C
      WRITE( *, '( A, I4, A, A )' )
     1   ' Read ', NV, ' VLA pad positions from ', 
     2   VLAFILE(1:LEN1(VLAFILE))
C
      WRITE(*,*) 'Reference epoch Julian date for revised stations ', 
     1          JDATE, '  MJD:', JDAY
      WRITE(*,*)
C
C     Write tables of results
C
      WRITE(*,*) ' Axis offset of -99.99 means it was not found.'
      WRITE(*,*) 
C
C     Write a table header.
C
      WRITE(*,*) 
     1  'Station          ITRF X          ITRF Y          ITRF Z '//
     1  '    ERR X   ERR Y   ERR Z    Rate X  Rate Y  Rate Z  Ax. Off.'
      WRITE(*,*) 
     1  '                   (m)             (m)             (m)  '//
     1   '     (m)     (m)     (m)     (m/yr)  (m/yr)  (m/yr)    (m)'
C
C     Write the IRTF positions:
C     For historical reasons, there are two entries for PT.
C
      WRITE(*,*) ' '
      DO I = 1, NS
         IF( VLBIX(I) .NE. 0.D0 ) THEN
            PSTA(1) = STA(I)
            IF( STA(I) .EQ. 'PIETOWN' ) THEN
               PSTA(2) = 'PT-VLBA'
               NPRT = 2
            ELSE
               NPRT = 1
            END IF
            DO IPRT = 1, NPRT
               WRITE(*,'( A8, 2X, 3(F16.4), 3(F8.4), 2X, ' //
     1             '3(F8.4), F10.4 )' ) 
     2            PSTA(IPRT), VLBIX(I), VLBIY(I), VLBIZ(I),
     3            VLBIEX(I), VLBIEY(I), VLBIEZ(I),
     4            VLBIRX(I), VLBIRY(I), VLBIRZ(I), VLBIOF(I)
            END DO
         END IF
      END DO
C
C     Write the VLA positions.  Use the VLBI rates for N8.
C     They were written above because they are already in the final output
C     data set.  Don't rewrite.
C
C      WRITE(*,*)
C      DO I = 1, NV
C         WRITE(*,'( A, 2X, 3(F16.4), 3(F8.4), 2X, 3(F8.4) )' ) VNAME(I),
C     1         IRVLAX(I), IRVLAY(I), IRVLAZ(I),
C     2         IRVLAEX(I), IRVLAEY(I), IRVLAEZ(I),
C     3         VLBIRX(ISN8), VLBIRY(ISN8), VLBIRZ(ISN8)
C      END DO
C
C
C     Write adjusted positions and differences from input data set.
C     Remove the comparison with the old as I don't have those 
C     since I'm not reading the old locations.dat any more.
C
      WRITE(*,*) ' '
      WRITE(*,*) ' Positions adjusted to ', CYEAR, CMONTH, CDAY
C       WRITE(*,*) ' This helps see the effect of rate differences'
C       WRITE(*,*) ' The difference is between the new data and old ', 
C      1             'locations file.'
      CONST = ( CJDAY - JDAY ) / 365.25D0
      WRITE(*,*) ' Years from reference. ', 
     1    CONST, CJDAY, JDAY
      WRITE(*,'(A,A)') 
     1  'Station       Adjusted X      Adjusted Y      Adjusted Z '
C     2          '           Diff X          Diff Y          Diff Z'
      WRITE(*,'(A,A)') 
     1  '                   (m)             (m)             (m)  '
C      2          '             (m)               (m)            (m)  '
      DO I = 1, NS
C
C        Antennas not in the new solution, or the VLA, have not yet
C        had their coordinates filled in from the input locations file.
C        so they won't be printed.  They are filled in in WRTLOC.
C
         IF( VLBIX(I) .NE. 0.D0 ) THEN
            PSTA(1) = STA(I)
            IF( STA(I) .EQ. 'PIETOWN' ) THEN
               PSTA(2) = 'PT-VLBA'
               NPRT = 2
            ELSE
               NPRT = 1
            END IF
C
C            Not doing the comparison with the old data because we 
C            don't have it.
C
C            IF( DBEPO(1) .NE. 0 ) THEN
C                CONST2 = ( CJDAY - DBEPO(I) ) / 365.25D0
C            ELSE
                CONST2 = 0.D0
C            END IF
C
            DO IPRT = 1, NPRT
               CONST = ( CJDAY - VLBIJDAY(I) ) / 365.25D0
C               WRITE(*,'( A8, 2X, 6(F16.4) )' )
               WRITE(*,'( A8, 2X, 3(F16.4) )' )
     1            PSTA(IPRT), 
     2            VLBIX(I) + CONST * VLBIRX(I),
     3            VLBIY(I) + CONST * VLBIRY(I),
     4            VLBIZ(I) + CONST * VLBIRZ(I)
C     5            VLBIX(I) + CONST*VLBIRX(I) - DBX(I) - CONST2*DBDX(I),
C     6            VLBIY(I) + CONST*VLBIRY(I) - DBY(I) - CONST2*DBDY(I),
C     7	          VLBIZ(I) + CONST*VLBIRZ(I) - DBZ(I) - CONST2*DBDZ(I)
            END DO
         END IF
      END DO
C
      RETURN
      END
