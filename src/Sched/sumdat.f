      CHARACTER*5 FUNCTION SUMDAT( ITEM, ISCN, ISTA )
C
C     A function to generate the character string for an individual
C     item in a the summary file.
C
      INCLUDE 'sched.inc'
C
      INTEGER     ISCN, ISTA, TEARLY, TDWELL, ITSLEW
      CHARACTER   ITEM*(*), DIRECT*1
C
C     Tape information from TPDAT.
C
      INTEGER          TPPASS, TPDIR, TPINDX, TPHEAD, TPDRIV, IGB
      LOGICAL          DOTAPE, DOFAST, DOREW
C -------------------------------------------------------------------
C     First detect if the station is to be used.
C
      IF( STASCN(ISCN,ISTA) ) THEN 
C
C        Get the desired item.
C
         IF( ITEM .EQ. 'EL1' ) THEN
            WRITE( SUMDAT, '( I4, A1 )' ) 
     1           NINT( EL1(ISCN,ISTA) ), UP1(ISCN,ISTA)
C
         ELSE IF( ITEM .EQ. 'EL2' ) THEN
            WRITE( SUMDAT, '( I4, A1 )' ) 
     1           NINT( EL2(ISCN,ISTA) ), UP2(ISCN,ISTA)
C
         ELSE IF( ITEM .EQ. 'AZ1' ) THEN
            WRITE( SUMDAT, '( I4, A1 )' ) 
     1           NINT( AZ1(ISCN,ISTA) ), UP1(ISCN,ISTA)
C
         ELSE IF( ITEM .EQ. 'AZ2' ) THEN
            WRITE( SUMDAT, '( I4, A1 )' ) 
     1           NINT( AZ2(ISCN,ISTA) ), UP2(ISCN,ISTA)
C
         ELSE IF( ITEM .EQ. 'PA1' ) THEN
            WRITE( SUMDAT, '( I4, 1X )' ) NINT( PA1(ISCN,ISTA) )
C
         ELSE IF( ITEM .EQ. 'PA2' ) THEN
            WRITE( SUMDAT, '( I4, 1X )' ) NINT( PA2(ISCN,ISTA) )
C
         ELSE IF( ITEM .EQ. 'HA1' ) THEN
            WRITE( SUMDAT, '( F5.1 )' ) HA1(ISCN,ISTA) 
C
         ELSE IF( ITEM .EQ. 'HA2' ) THEN
            WRITE( SUMDAT, '( F5.1 )' ) HA2(ISCN,ISTA)
C
         ELSE IF( ITEM(1:4) .EQ. 'TAPE' ) THEN
C
C           Extract the tape commands from PTDAT.
C
            CALL TPPACK( 'UNPACK', TPDAT(1,ISCN,ISTA), DOTAPE, DOFAST,
     1                   DOREW, TPPASS, TPDRIV, TPDIR, TPINDX, TPHEAD )
C
C           Now deal with first or second line.
C
            IF( ITEM .EQ. 'TAPE1' ) THEN
               IF( NOREC(ISCN) ) THEN 
                  SUMDAT = 'Stop '
               ELSE IF( USETAPE(ISTA) ) THEN
                  IF( TPDIR .EQ. 1 ) THEN
                     DIRECT = 'F'
                  ELSE
                     DIRECT = 'R'
                  END IF
                  WRITE( SUMDAT, '( I1, A1, I2.2 )' )
     1                    TPDRIV, DIRECT, TPINDX
               ELSE IF( USEDISK(ISTA) .AND. .NOT. USETAPE(ISTA) ) THEN
                  IGB = GBYTES(ISCN,ISTA)
                  WRITE( SUMDAT, '( I4 )' ) IGB
               END IF
C
            ELSE IF( ITEM .EQ. 'TAPE2' ) THEN
               IF( NOREC(ISCN) ) THEN 
                  SUMDAT = '---- '
               ELSE
                  IF( TPHEAD .LT. 10 ) THEN
                     WRITE( SUMDAT, '( I1, A1, I2.2, A1 )' ) TPHEAD, 
     1                  '/', NINT( TPFOOT1(ISCN,ISTA) / 1000. ), ' '
                  ELSE
                     WRITE( SUMDAT, '( I2, A1, I2.2 )' ) TPHEAD, 
     1                  '/', NINT( TPFOOT1(ISCN,ISTA) / 1000. )
                  END IF
               END IF
            END IF
C
         ELSE IF( ITEM .EQ. 'DISK' .OR. ITEM .EQ. 'DISC' ) THEN
            IF( NOREC(ISCN) ) THEN
                  SUMDAT = 'Stop '
            ELSE IF( USEDISK(ISTA) ) THEN
               IGB = GBYTES(ISCN,ISTA)
               WRITE( SUMDAT, '( I4 )' ) IGB
            ELSE IF( USETAPE(ISTA) ) THEN
               SUMDAT = 'Tape'
            ELSE
               SUMDAT = '----'
            END IF
C
         ELSE IF( ITEM .EQ. 'TPSTART' ) THEN
            WRITE( SUMDAT, '( I4, 1X )' ) IDNINT( TPSTART(ISCN,ISTA) * 
     1            86400.D0 )
C
         ELSE IF( ITEM .EQ. 'EARLY' ) THEN
            TEARLY = IDNINT( ( STARTJ(ISCN) - TONSRC(ISCN,ISTA) ) * 
     1               86400.D0 )
            WRITE( SUMDAT, '( I4, 1X )' ) TEARLY
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
            WRITE( SUMDAT, '( I4, 1X )' ) TDWELL
C
         ELSE IF( ITEM .EQ. 'SLEW' ) THEN
            ITSLEW = IDNINT( TSLEW(ISCN,ISTA) * 86400.D0 )
            WRITE( SUMDAT, '( I4, 1X )' ) ITSLEW
C
         ELSE IF( ITEM .EQ. ' ' ) THEN
            SUMDAT = '     '
C
C        Test for understood items is in SUMDESC.
C
         END IF
C
C        Mark tape changes.  Don't cover UP unless needed.
C
         DOTAPE = MOD( TPDAT(1,ISCN,ISTA), 10 ) .GT. 0
         IF( DOTAPE .AND. ITEM(1:4) .NE. 'TAPE' ) THEN
            SUMDAT(5:5) = 't'
         END IF

      ELSE
C
C        Station not in scan.
C
         IF( ITEM .EQ. 'EL1' .OR. ITEM .EQ. 'AZ1' ) THEN
            WRITE( SUMDAT, '( A4, A1 )' ) ' ---', UP1(ISCN,ISTA)
         ELSE IF( ITEM .EQ. 'EL2' .OR. ITEM .EQ. 'AZ2' ) THEN
            WRITE( SUMDAT, '( A4, A1 )' ) ' ---', UP2(ISCN,ISTA)
         ELSE
            SUMDAT = ' --- '
         END IF
C
      END IF
C
      RETURN
      END
