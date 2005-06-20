      SUBROUTINE STALST
C
C     Subroutine for SCHED that writes a table of station information 
C     to summary file.  Include the tape initialization stuff.
C
      INCLUDE  'sched.inc'
C
      INTEGER    ISTA, ISCAT, IC1, IC2, BLENKM, LEN1, JSTA, JSCAT
      INTEGER    NNSTA
      REAL       BLENSQ
      CHARACTER  CDEN*4, CAA*3, CAR*3
      LOGICAL    ANYTAPE, ANYDISK, ANYELSE
C ----------------------------------------------------------------------
C
      WRITE( ISUM, '( 1X, /, 1X, /, 1X, /, A, /, 1X, /, A,A, /, 1X )' )
     1      'STATIONS USED IN SCHEDULE:',
     2      '   Station  Code   Latitude Longitude Elevation ',
     3      '       X           Y           Z '
      DO ISTA = 1, NSTA
          ISCAT = STANUM(ISTA)
          WRITE( ISUM, '( 3X, A8, 2X, A3, 2F10.5, F10.0, 2X, ' //
     1           ' 3F12.2 )' ) 
     2        STATION(ISCAT), STCODE(ISCAT),
     3        LAT(ISCAT)/RADDEG,
     4        LONG(ISCAT)/RADDEG,
     5        ELEV(ISCAT),
     6        XPOS(ISCAT), YPOS(ISCAT), ZPOS(ISCAT)
      END DO
C
C     Make a matrix of baseline lengths, if there are any
C     baselines (for VLA schedules, there often won't be).
C     Only do first 30 stations across if there are more.
C
      NNSTA = NSTA
      IF( NNSTA .GT. 30 ) NNSTA = 30
      IF( NNSTA .GT. 1 ) THEN
         WRITE( ISUM, '( 1X, /, 1X, /, 1X, /, A, /, 1X )' )
     1         'BASELINE LENGTHS (km)'
         MSGTXT = ' '
C
C        Write the station codes across the top.
C
         DO ISTA = 1, NNSTA
            ISCAT = STANUM(ISTA)
            IC1 = 11 + ( ISTA - 1 ) * 6
            IC2 = IC1 + 1
            MSGTXT(IC1:IC2) = STCODE(ISCAT)
         END DO
C
C        Now write a line for each station.
C
         WRITE( ISUM, '( A, /, 1X )' ) MSGTXT(1:LEN1(MSGTXT))
         DO JSTA = 1, NSTA
            JSCAT = STANUM(JSTA)
            MSGTXT = '   ' // STCODE(JSCAT)
            DO ISTA = 1, NNSTA
               ISCAT = STANUM(ISTA)
               BLENSQ = ( ( XPOS(JSCAT) - XPOS(ISCAT) ) / 1000.0 )**2 +
     1                  ( ( YPOS(JSCAT) - YPOS(ISCAT) ) / 1000.0 )**2 +
     2                  ( ( ZPOS(JSCAT) - ZPOS(ISCAT) ) / 1000.0 )**2
               BLENKM = SQRT( BLENSQ )
               IC1 = 8 + ( ISTA - 1 ) * 6
               IC2 = IC1 + 5
               WRITE( MSGTXT(IC1:IC2), '( I5 )' ) BLENKM
            END DO
            WRITE( ISUM, '( A )' ) MSGTXT(1:LEN1(MSGTXT))
         END DO
      END IF
C
C     Recording systems.
C
      IF( .NOT. ( VLAONLY .OR. NOTAPE ) ) THEN
C
C        First find out what we have.
C
         ANYTAPE = .FALSE.
         ANYDISK = .FALSE.
         ANYELSE = .FALSE.
         DO ISTA = 1, NSTA      
            ANYTAPE = ANYTAPE .OR. USETAPE(ISTA)
            ANYDISK = ANYDISK .OR. USEDISK(ISTA)
            ANYELSE = ANYELSE .OR. .NOT. ( USETAPE(ISTA) .OR.
     1                   USEDISK(ISTA) )
         END DO
         WRITE( ISUM, '( 1X, /, 1X, /, A )' )
     1      'RECORDING SYSTEM INFORMATION:'
         IF( ANYTAPE .AND. ANYDISK ) WRITE( ISUM, '( 5X, A )' )
     1      'Some stations may be scheduled for both tape and disk.'
C
C        Tape initialization stuff.  Throw in a check of the density
C        since it's easy.
C
         IF( ANYTAPE ) THEN
            WRITE( ISUM, '( 1X, /, A, /, A, /, 2A, /,'//
     1             '2A )' )
     2         '  TAPES - Stations potentially recording on tape.',
     3         '    (See setup details later for times per pass.)',
     4         '   Station Drives Head pos  Tape len  Density',
     5         '  Start position  Auto   Auto   Recorder  DAR',
     6         '                                (ft)           ',
     7         'Driv Head indx  Aloc  Reverse'
C	   
C           Loop over stations.
C	   
            DO ISTA = 1, NSTA
               IF( USETAPE(ISTA) ) THEN
                  ISCAT = STANUM(ISTA)
                  IF( DENSITY(ISTA) .EQ. 'H' ) THEN
                     CDEN = 'High'
                  ELSE IF( DENSITY(ISTA) .EQ. 'L' ) THEN
                     CDEN = 'Low '
                  ELSE
                     CALL ERRLOG( 'STALST: Bad density (' //
     1                 DENSITY(ISTA)// ') at '//STATION(ISCAT) )
                  END IF
C	        
                  IF( AUTOALOC(ISTA) ) THEN
                     CAA = 'Yes'
                  ELSE
                     CAA = ' No'
                  END IF
C	        
                  IF( AUTOREV(ISTA) ) THEN
                     CAR = 'Yes'
                  ELSE
                     CAR = ' No'
                  END IF
C	        
C                 Write the line.
C	        
                  WRITE( ISUM, '( 3X, A8, I5, I8, I11, 4X, A4, '//
     1                   'I7, I7, 7X, A, 4X, A, 4X, A6, 2X, A5 )' )
     2                 STATION(ISCAT), STNDRIV(ISCAT), NHDPOS(ISTA),
     3                 TPLENG(ISTA), CDEN, TPSDRIV(ISTA), TPSINDX(ISTA),
     4                 CAA, CAR, RECORDER(ISCAT), DAR(ISCAT)
               END IF
            END DO
         END IF
C
C        Deal with disk stations.
C
         IF( ANYDISK ) THEN
            WRITE( ISUM, '( 1X, /, A, /, A )' )
     1          '  DISKS - Stations potentially recording on disks.',
     2          '   Station    Drive type '
            DO ISTA = 1, NSTA
               IF( USEDISK(ISTA) ) THEN
                  ISCAT = STANUM(ISTA)
                  WRITE( ISUM, '( 3X, A8, 5X, A6 )' )
     1               STATION(ISCAT), DISK(ISCAT)
               END IF
            END DO              
         END IF
C
C        Others.
C
         IF( ANYELSE ) THEN
            WRITE( ISUM, '( 1X, /, A, /, A, /, 1X )' )
     1          '  OTHER - Stations with other recording systems.',
     2          '   Station    Drive type '
            DO ISTA = 1, NSTA
               IF( .NOT. ( USEDISK(ISTA) .OR. USETAPE(ISTA) ) ) THEN
                  ISCAT = STANUM(ISTA)
                  WRITE( ISUM, '( 3X, A8, 5X, A6 )' )
     1               STATION(ISCAT),  RECORDER(ISCAT)
               END IF
            END DO              
         END IF
C
      END IF
C
      RETURN
      END




