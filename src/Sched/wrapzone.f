      SUBROUTINE WRAPZONE( IUNIT, ISCN, ISTA, ZONE )
C
C     Subroutine for SCHED that determines the WRAP segment that 
C     antenna ISTA is expected to be in for scan ISCN. 
C
C     If ISCN=0, the routine determines the names and ranges for each
C     valid zone, and writes them to the Vex file for the $ANTENNA 
C     section. (erroneously put in STATION section at first).  Fixed
C     on Jan 23, 2014.  RCW.
C
C     When ISCN is not zero, the zone name is returned.  This is
C     used with writing the $SCHED section of the Vex file.
C
C     The wrap zone names are taken from the Vex documentation.
C
C     The zone specification is for the start of the scan, but 
C     shouldn't change.
C
C     Written Jan. 8, 2014  RCW.
C
      INCLUDE  'sched.inc'
C
      INTEGER       ISCN, ISTA, NZONES, IUNIT, LEN1
      REAL          ZLOWAZ(6), ZHIGHAZ(6), ZLOWEL(6), ZHIGHEL(6)
      CHARACTER     ZONE*(*), ZNAMES(6)*5
C
      INTEGER       KSTA, I
C -------------------------------------------------------------------
      KSTA = STANUM(ISTA)
C
C     Deal with setting up the zones.  These are not saved, but 
C     rather recalculated for each case.  Only do this for ALTAZ
C     antennas.  Note AR has a range of 720 deg and we don't want
C     to exclude it.
C
      IF( MOUNT(KSTA) .EQ. 'ALTAZ' .AND. 
     1   AX1LIM(2,KSTA) - AX1LIM(1,KSTA) .LE. 720.0 ) THEN
         IF( ISCN .EQ. 0 ) THEN
C
C           Get the zones, including any over-the-top zones.
C
C           First, the unlikely case that there is no overlap region
C           so there is only one zone, or 2 with over-the-top:
C
            IF( AX1LIM(2,KSTA) - AX1LIM(1,KSTA) .LE. 360.0 ) THEN
               ZNAMES(1) = '&n'
               ZLOWAZ(1) = AX1LIM(1,KSTA)
               ZHIGHAZ(1) = AX1LIM(2,KSTA)
               IF( AX2LIM(2,KSTA) .LE. 90.0001 ) THEN
                  NZONES = 1
                  ZLOWEL(1) = AX2LIM(1,KSTA)
                  ZHIGHEL(1) = AX2LIM(2,KSTA)
               ELSE
                  NZONES = 2
                  ZLOWEL(1) = AX2LIM(1,KSTA)
                  ZHIGHEL(1) = 90.0
C
                  ZNAMES(2) = '&np'
                  ZLOWEL(2) = 90.0
                  ZHIGHEL(2) = AX2LIM(2,KSTA)
C
C                 Write a warning about support level.
C 
                  CALL WLOG( 1, 'WRAPZONE: ' //
     1               STATION(KSTA) // ' has over-the-top pointing.' )
                  CALL WLOG( 1, '          This is not well supported '
     1               // 'in SCHED.  Az and El may be wrong ' )
                  CALL WLOG( 1, '          and slew calculations may '
     1               // 'be way off.' )
               END IF
            ELSE 
C
C              Now the more normal case with overlap (usually
C              the overlap is 180 deg), but less than 2 turns.
C
               IF( AX2LIM(2,KSTA) .LE. 90.0001 ) THEN
                  NZONES = 3
                  DO I = 1, 3
                     ZLOWEL(I) = AX2LIM(1,KSTA)
                     ZHIGHEL(I) = AX2LIM(2,KSTA)
                  END DO
               ELSE
                  NZONES = 6
                  DO I = 1, 3
                     ZLOWEL(I) = AX2LIM(1,KSTA)
                     ZHIGHEL(I) = 90.0
                     ZLOWEL(I+3) = 90.0
                     ZHIGHEL(I+3) = AX2LIM(2,KSTA)
                  END DO
               END IF
               ZNAMES(1) = '&ccw'
               ZLOWAZ(1) = AX1LIM(1,KSTA)
               ZHIGHAZ(1) = AX1LIM(2,KSTA) - 360.0
C      
               ZNAMES(2) = '&n'
               ZLOWAZ(2) = AX1LIM(2,KSTA) - 360.0
               ZHIGHAZ(2) = AX1LIM(1,KSTA) + 360.0
C      
               ZNAMES(3) = '&cw'
               ZLOWAZ(3) = AX1LIM(1,KSTA) + 360.0
               ZHIGHAZ(3) = AX1LIM(2,KSTA)
C
               IF( NZONES .EQ. 6 ) THEN
                  ZNAMES(4) = '&ccwp'         
                  ZLOWAZ(4) = ZLOWAZ(1) 
                  ZHIGHAZ(4) = ZHIGHAZ(1)
C
                  ZNAMES(5) = '&ccwp'         
                  ZLOWAZ(5) = ZLOWAZ(2) 
                  ZHIGHAZ(5) = ZHIGHAZ(2)
C
                  ZNAMES(6) = '&ccwp'         
                  ZLOWAZ(6) = ZLOWAZ(3) 
                  ZHIGHAZ(6) = ZHIGHAZ(3)
               END IF
            END IF
C
C           Now write out the pointing sector lines for the Vex file. 
C           Note we are only doing this for ALTAZ antennas with less 
C           than 720 deg range.
C
            DO I = 1, NZONES
               MSGTXT = ' '
               WRITE( MSGTXT, '( A, A, A, F7.2, A, F7.2, A, F6.2, A, '
     1            // 'F6.2, A, I2 )' ) '     pointing_sector = ', 
     2            ZNAMES(I),
     3            ' : az :', ZLOWAZ(I), ' deg:', ZHIGHAZ(I), 
     4            ' deg: el :', ZLOWEL(I), ' deg:', ZHIGHEL(I), 
     5            ' deg; * cable wrap zone', I
               WRITE( IUNIT, '(A)' ) MSGTXT(1:LEN1(MSGTXT))
            END DO
C
         ELSE
C
C           Process a scan/antenna if it is in the scan.
C           The routine probably won't be called if the combination
C           is not in the scan, but be careful.
C
            IF( STASCN(ISCN,ISTA) ) THEN
               IF( AZ1(ISCN,ISTA) .GE. AX1LIM(1,KSTA) .AND. 
     1             AZ1(ISCN,ISTA) .LT. AX1LIM(2,KSTA) - 360.0 ) THEN
                  ZONE = '&ccw'
               ELSE IF( AZ1(ISCN,ISTA) .GT. AX1LIM(1,KSTA) +360.0 .AND. 
     1                  AZ1(ISCN,ISTA) .LE. AX1LIM(2,KSTA) ) THEN
                  ZONE = '&cw'
               ELSE
                  ZONE = '&n'
               END IF
C
C              Deal with over-the-top.
C
               IF( AX2LIM(2,KSTA) .GT. 90.0 .AND. 
     1             EL1(ISCN,ISTA) .GT. 90.0 ) THEN
                  ZONE = ZONE // 'p'
               END IF
            ELSE
               ZONE = ' '
            END IF
         END IF
C            
      ELSE IF( MOUNT(KSTA) .EQ. 'ALTAZ' .AND. 
     1    AX1LIM(2,KSTA) - AX1LIM(1,KSTA) .GT. 720.0 ) THEN
C
C        There are more than 2 turns.  Refuse this for now.
C        Only warn on ISCN=0 call
C
         IF( ISCN .EQ. 0 ) THEN
            MSGTXT = ' '
            WRITE( MSGTXT, '( A, A )' )
     1         'WRAPZONE: Station ', STATION(KSTA), 
     2         ' has an azimuth wrap range over 180 degrees.'
            CALL WLOG( 1, MSGTXT )
            CALL WLOG( 1,
     1         '          SCHED is not set up for that.' )
            CALL WLOG( 1,
     1         '          Will not give wrap zones in VEX file.' )
            NZONES = 0
            ZONE = ' '
         END IF
      ELSE
C
C        Other mount 
C
         NZONES = 0
         ZONE = ' '
      END IF
C
      RETURN
      END
