      SUBROUTINE OMSSET
C
C     Routine for SCHED that writes setup information for OMS
C     at the VLBA correlator.
C
      INCLUDE       'sched.inc'
      INCLUDE       'schset.inc'
C
      INTEGER            I, KS, ISCN, NCHR, NCHR1, ICHAN, LEN1
      INTEGER            ISTA, KF, JS
      INTEGER            SETSCANS, NRSCANS, USE4VLBA(MAXSET), VLBA1
      LOGICAL            VLBASAME, SAMESET, ISVLBA
      DOUBLE PRECISION   SETHOURS, NRHOURS
C ---------------------------------------------------------------------
C     For each setup file, find out if we can use a generic VLBA output
C     group.
C     I figured out later that this will mess up the scan count statistics
C     when not all VLBA stations are in every scans.  This can be a 
C     factor of 2 effect for pointing setups.  And since SC is often
C     the first VLBA station and it often does not see the final scans,
C     it can be a problem there too.
C
      DO KF = 1, NSETF
         USE4VLBA(KF) = 0
         VLBA1 = 0
         VLBASAME = .TRUE.
         DO KS = 1, NSET
            IF( ISETNUM(KS) .EQ. KF .AND. 
     1          SETSTA(1,KS)(1:4) .EQ. 'VLBA' ) THEN
               IF( VLBA1 .EQ. 0 ) THEN
                  VLBA1 = KS
               ELSE
                  IF( .NOT. SAMESET(KS,VLBA1) ) THEN
                     VLBASAME = .FALSE.
                  END IF
               END IF
            END IF
         END DO
         IF( VLBASAME .AND. VLBA1 .NE. 0 ) THEN
            USE4VLBA(KF) = VLBA1
         ELSE
            USE4VLBA(KF) = 0
         END IF
      END DO

C
C     Loop over the setups and write their parameters.  
C     
      DO KS = 1, NSET
C
C        Get some statistics of the usage of this setup.
C
         SETSCANS = 0
         SETHOURS = 0.0
         NRSCANS = 0
         NRHOURS =  0.0
         DO ISCN = SCAN1, SCANL
            DO ISTA = 1, NSTA
               IF( STASCN(ISCN,ISTA) ) THEN 
                  JS = NSETUP(ISCN,ISTA)
                  KF = ISETNUM(JS)
                  ISVLBA = STANAME(ISTA)(1:4) .EQ. 'VLBA'
                  IF( JS .EQ. KS .OR.
     1                ( ISVLBA .AND. USE4VLBA(KF) .EQ. KS ) ) THEN
                     IF( NOREC(ISCN) ) THEN
                        NRSCANS = NRSCANS + 1
                        NRHOURS = NRHOURS + 
     1                      ( STOPJ(ISCN) - STARTJ(ISCN) ) * 24.D0
                     ELSE
                        SETSCANS = SETSCANS + 1
                        SETHOURS = SETHOURS + 
     1                       ( STOPJ(ISCN) - STARTJ(ISCN) ) * 24.D0
                     END IF
C
C                    Only count each scan once.
C
                     GO TO 100
                  END IF
               END IF
            END DO
  100       CONTINUE
         END DO
C
C        If it got used at all, write out info.
C        Don't write any but the first VLBA station if all
C        VLBA setups for this setup file are the same.
C
         KF = ISETNUM(KS)
         IF( ( SETSCANS .GE. 1 .OR. NRSCANS .GE. 1 ) .AND.
     1       ( USE4VLBA(KF) .EQ. KS .OR. USE4VLBA(KF) .EQ. 0 .OR. 
     2         SETSTA(1,KS)(1:4) .NE. 'VLBA' ) ) THEN
C
            WRITE( IOMS, '( 1X, /, A, /, 1X )' )
     1          'BEGIN = STATION_SETUP'
C
            IF( USE4VLBA(KF) .EQ. KS ) THEN
               WRITE( IOMS, '( A, A )' )
     1          '    STATION            = VLBA    '
            ELSE
               WRITE( IOMS, '( A, A )' )
     1          '    STATION            = ', STCODE(ISETSTA(KS))
            END IF
C
            IF( SETSCANS .GE. 1 ) THEN
               WRITE( IOMS, '( A, I4, A, F6.2, A )' )
     1          '    RECORD_SCANS       = ', SETSCANS
               WRITE( IOMS, '( A, F6.2 )' )
     1          '    RECORD_HOURS       = ', SETHOURS
            END IF
C
            IF( NRSCANS .GE. 1 ) THEN
               WRITE( IOMS, '( A, I4, A, F6.2, A )' )
     1          '    NORECORD_SCANS     = ', NRSCANS
               WRITE( IOMS, '( A, F6.2 )' )
     1          '    NORECORD_HOURS     = ', NRHOURS
            END IF
C
            WRITE( IOMS, '( 5( A, 1X ) )' )    
     1          '    FE                 = ', 
     2          (FE(I,KS)(1:LEN1(FE(I,KS))),I=1,4)
            WRITE( IOMS, '( A, I4 )' ) 
     1          '    NUM_CHANNELS       = ', NCHAN(KS)
            WRITE( IOMS, '( A, A )' )  
     1          '    FORMAT             = ', FORMAT(KS)
            WRITE( IOMS, '( A, F5.2 )' ) 
     1          '    SPEEDUP            = ', SPEEDUP(KS)
C     
C           Channel information.
C     
            SETMSG = ' '
            MSGTXT = ' '
            WRITE( SETMSG, '( A )' ) 
     1          '    IF                 = '
            WRITE( MSGTXT, '( A )' ) 
     1          '    BBC_SB             = '
            NCHR = 27
            DO ICHAN = 1, NCHAN(KS)
               WRITE( SETMSG(NCHR:NCHR+1), '( A )' ) 
     1              IFCHAN(ICHAN,KS)
               WRITE( MSGTXT(NCHR:NCHR), '( A )' )
     1              SIDEBD(ICHAN,KS)
               NCHR = NCHR + 3
            END DO
            WRITE( IOMS, '( A )' ) SETMSG(1:NCHR)
            WRITE( IOMS, '( A )' ) MSGTXT(1:NCHR)
C
C           Extra channel information not requested for OMS but
C           maybe useful for other users.
C           Keeping separate track of the length of the POL line
C           because formally POL is CHARACTER*4, but we only use
C           the first.
C
            SETMSG = ' '
            MSGTXT = ' '
            WRITE( SETMSG, '( A )' ) 
     1          '    NET_SB             = '
            WRITE( MSGTXT, '( A )' ) 
     1          '    POL                = '
            NCHR = 27
            NCHR1 = 27
            DO ICHAN = 1, NCHAN(KS)
               WRITE( SETMSG(NCHR:NCHR), '( A1 )' ) 
     1              NETSIDE(ICHAN,KS)
               WRITE( MSGTXT(NCHR1:NCHR1), '( A1 )' )
     1              POL(ICHAN,KS)
               NCHR = NCHR + 3
               NCHR1 = NCHR1 + 3
            END DO
            WRITE( IOMS, '( A )' ) SETMSG(1:NCHR)
            WRITE( IOMS, '( A )' ) MSGTXT(1:NCHR1)
C
C           Write the frequency sets used with this setup.
C
            CALL OMSFREQ( KS, IOMS )
C
C           Close out the station setup section.
C
            WRITE( IOMS, '( 1X, /, A )' )
     1          'END = STATION_SETUP'
         END IF
      END DO
C
      RETURN
      END
