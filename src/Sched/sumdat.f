      CHARACTER*6 FUNCTION SUMDAT( ITEM, ISCN, ISTA )
C
C     A function to generate the character string for an individual
C     item in a the summary file.
C
      INCLUDE 'sched.inc'
C
      INTEGER     ISCN, ISTA, TEARLY, TDWELL, ITSLEW, ISYNC
      CHARACTER   ITEM*(*)
      INTEGER          IGB
      CHARACTER        UPA*1, UPC*2
C -------------------------------------------------------------------
C     Get the average rise state.  Not needed in most, but 
C     easier to just get it without fussing about whether
C     it's needed.  Also, keep out of the STASCN IF because
C     it may be wanted for sources that have been removed
C     because of being down.
C
      UPA = ' '
      UPC = UP1(ISCN,ISTA)//UP2(ISCN,ISTA)
      IF( UPC .EQ. 'HH' ) UPA = 'H'
      IF( UPC .EQ. 'DD' ) UPA = 'D'
      IF( UPC .EQ. ' D' .OR. UPC .EQ. ' H' ) UPA = 'S'
      IF( UPC .EQ. 'D ' .OR. UPC .EQ. 'H ' ) UPA = 'R'
      IF( UPC .EQ. 'HD' .OR. UPC .EQ. 'DH' ) UPA = 'H'
      IF( UPC .EQ. 'WW' ) UPA = 'W'
C
C     Detect if the station is to be used.
C
      IF( STASCN(ISCN,ISTA) ) THEN 
C
C        Get the desired item.
C
         IF( ITEM .EQ. 'EL1' ) THEN
            WRITE( SUMDAT, '( I5, A1 )' ) 
     1           NINT( EL1(ISCN,ISTA) ), UP1(ISCN,ISTA)
C
         ELSE IF( ITEM .EQ. 'EL2' ) THEN
            WRITE( SUMDAT, '( I5, A1 )' ) 
     1           NINT( EL2(ISCN,ISTA) ), UP2(ISCN,ISTA)
C
         ELSE IF( ITEM .EQ. 'ELA' ) THEN
            WRITE( SUMDAT, '( I5, A1 )' ) 
     1           NINT( ( EL1(ISCN,ISTA) + EL2(ISCN,ISTA) ) / 2 ), 
     2           UPA
C
         ELSE IF( ITEM .EQ. 'AZ1' ) THEN
            WRITE( SUMDAT, '( I5, A1 )' ) 
     1           NINT( AZ1(ISCN,ISTA) ), UP1(ISCN,ISTA)
C
         ELSE IF( ITEM .EQ. 'AZ2' ) THEN
            WRITE( SUMDAT, '( I5, A1 )' ) 
     1           NINT( AZ2(ISCN,ISTA) ), UP2(ISCN,ISTA)
C
         ELSE IF( ITEM .EQ. 'AZA' ) THEN
            WRITE( SUMDAT, '( I5, A1 )' ) 
     1           NINT( ( AZ1(ISCN,ISTA) + AZ2(ISCN,ISTA) ) / 2 ), 
     2           UPA
C
         ELSE IF( ITEM .EQ. 'PA1' ) THEN
            WRITE( SUMDAT, '( I5, 1X )' ) NINT( PA1(ISCN,ISTA) )
C
         ELSE IF( ITEM .EQ. 'PA2' ) THEN
            WRITE( SUMDAT, '( I5, 1X )' ) NINT( PA2(ISCN,ISTA) )
C
         ELSE IF( ITEM .EQ. 'HA1' ) THEN
            WRITE( SUMDAT, '( F5.1 )' ) HA1(ISCN,ISTA) 
C
         ELSE IF( ITEM .EQ. 'HA2' ) THEN
            WRITE( SUMDAT, '( F5.1 )' ) HA2(ISCN,ISTA)
C
C        The following section is tape items.  Remove some day.
C
         ELSE IF( ITEM(1:4) .EQ. 'TAPE' ) THEN
C
C           No longer supporting tape.  Put the disk bytes in 
C           for TAPE1 and nothing for TAPE2
C
            IF( ITEM .EQ. 'TAPE1' ) THEN
               IF( NOREC(ISCN) ) THEN 
                  SUMDAT = 'Stop '
               ELSE IF( USEDISK(ISTA)  ) THEN
                  IGB = NINT( GBYTES(ISCN,ISTA) )
                  WRITE( SUMDAT, '( I5 )' ) IGB
               END IF
C
            ELSE IF( ITEM .EQ. 'TAPE2' ) THEN
               SUMDAT = '---- '
            END IF
C
         ELSE IF( ITEM .EQ. 'DISK' .OR. ITEM .EQ. 'DISC' ) THEN
            IF( NOREC(ISCN) ) THEN
                  SUMDAT = ' Stop '
            ELSE IF( USEDISK(ISTA) ) THEN
               IGB = NINT( GBYTES(ISCN,ISTA) )
               WRITE( SUMDAT, '( I5 )' ) IGB
            ELSE
               SUMDAT = '----'
            END IF
C
         ELSE IF( ITEM .EQ. 'TPSTART' ) THEN
            WRITE( SUMDAT, '( I5, 1X )' ) IDNINT( TPSTART(ISCN,ISTA) * 
     1            86400.D0 )
C
         ELSE IF( ITEM .EQ. 'EARLY' ) THEN
            TEARLY = IDNINT( ( STARTJ(ISCN) - TONSRC(ISCN,ISTA) ) * 
     1               86400.D0 )
            WRITE( SUMDAT, '( I5, A1 )' ) TEARLY, UPA
C
         ELSE IF( ITEM .EQ. 'DWELL' ) THEN
            TEARLY = IDNINT( ( STARTJ(ISCN) - TONSRC(ISCN,ISTA) ) * 
     1               86400.D0 )
            IF( UP1(ISCN,ISTA) .EQ. ' ' .AND. UP2(ISCN,ISTA) .EQ. ' ' ) 
     1          THEN
               IF( TEARLY .GE. 0 ) THEN
                  TDWELL = IDNINT( ( STOPJ(ISCN) - STARTJ(ISCN) ) * 
     1                  86400.D0 )
               ELSE
                  TDWELL = IDNINT( ( STOPJ(ISCN) - TONSRC(ISCN,ISTA) ) *
     1                  86400.D0 )
               END IF
            ELSE
               TDWELL = 0
            END IF
            WRITE( SUMDAT, '( I5, A1 )' ) TDWELL, UPA
C
         ELSE IF( ITEM .EQ. 'SLEW' ) THEN
            ITSLEW = IDNINT( TSLEW(ISCN,ISTA) * 86400.D0 )
            WRITE( SUMDAT, '( I5, 1X )' ) ITSLEW
C
         ELSE IF( ITEM .EQ. 'SYNC' ) THEN
            ISYNC = IDNINT( ( STOPJ(ISCN) - TCORR(ISCN,ISTA) ) *
     1              86400.D0 )
            WRITE( SUMDAT, '( I5, 1X )' ) ISYNC
C
         ELSE IF( ITEM .EQ. ' ' ) THEN
            SUMDAT = '     '
C
C        Test for understood items is in SUMDESC.
C
         END IF
C
      ELSE
C
C        Station not in scan.  But still give the UP indicators
C        in case the station was removed by SCHED to show why.
C
         IF( ITEM .EQ. 'EL1' .OR. ITEM .EQ. 'AZ1' ) THEN
            WRITE( SUMDAT, '( A5, A1 )' ) '  ---', UP1(ISCN,ISTA)
         ELSE IF( ITEM .EQ. 'EL2' .OR. ITEM .EQ. 'AZ2' ) THEN
            WRITE( SUMDAT, '( A5, A1 )' ) '  ---', UP2(ISCN,ISTA)
         ELSE IF( ITEM .EQ. 'ELA' .OR. ITEM .EQ. 'AZA' .OR.
     1            ITEM .EQ. 'DWELL' .OR. ITEM .EQ. 'EARLY' ) THEN
            WRITE( SUMDAT, '( A5, A1 )' ) '  ---', UPA
         ELSE
            SUMDAT = '  --- '
         END IF
C
      END IF
C
      RETURN
      END
