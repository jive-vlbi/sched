      SUBROUTINE VLBATP( ISCN, ISTA, REWSP, TPCDRIV, TPCINDX, TPCDIR,
     1                   SETSC )
C
C     Routine for SCHED, called by VLBA, that writes the tape motion
C     commands for a VLBA tape drive.
C
C     The tape speed is SPEEDH or SPEEDL (from the SETUP file).  
C     The online system automatically calculates the tape speed 
C     from the mode (fan out etc) and the sample rate.  All we 
C     need to tell it is +-RUN.  Elsewhere in SCHED, the actual 
C     speed is needed to keep track of tape footage.
C     If NOREC, obey any rewind etc. commands as set in REWSP.
C     The online system only wants STOP, +-RUN, or +-REWIND
C     If AUTOALLOC requested, give only tape command.
C
      INCLUDE 'sched.inc'
      INCLUDE 'schset.inc'
C
      INTEGER           ISCN, ISTA, TPCDRIV, TPCINDX, HEADX, TPCDIR
      INTEGER           LEN1, HEADPOS
      CHARACTER         REWSP*7, SSPEED*4
      LOGICAL           SETSC
C ----------------------------------------------------------------------
      IF( DEBUG .AND. ISCN .LE. 3 ) CALL WLOG( 0, 'VLBATP: Starting' )
C
C     Don't do any of this if not using tape.
C
      IF( .NOT. USETAPE(ISTA) ) RETURN
C
C     Deal with no-record and setup scans.
C
      IF( NOREC(ISCN) .OR. SETSC ) THEN
C
C        Deal with a no-record scan.
C
         IF( .NOT. AUTOALOC(ISTA) ) THEN 
            HEADX = HEADPOS( TPCINDX, HEADMODE(ISTA), 1 )
            WRITE( IUVBA, '(A, I2, A, A, A, I2, A, I2, A, I4, A )' )
     1         'tape=(', TPCDRIV, ',', REWSP(1:LEN1(REWSP)), 
     2         ') write=(', TPCDRIV, 
     3         ',off) head=(', TPCDRIV, ',', HEADX, ') '
            IF( TWOHEAD .AND. 
     1          TPCDRIV+1 .LE. STNDRIV(STANUM(ISTA)) ) THEN
               HEADX = HEADPOS( TPCINDX, HEADMODE(ISTA), 2 )
               WRITE( IUVBA, '(A, I2, A, A, A, I2, A, I2, A, I4, A )' )
     1            'tape=(', TPCDRIV+1, ',', REWSP(1:LEN1(REWSP)), 
     2            ') write=(', TPCDRIV+1, 
     3            ',off) head=(', TPCDRIV+1, ',', HEADX, ') '
            END IF
         ELSE
            WRITE( IUVBA, '( A, A, A )' )
     1         'tape=(1,', REWSP(1:LEN1(REWSP)), ') write=(1,off)'
            IF( TWOHEAD .AND. 
     1          2 .LE. STNDRIV(STANUM(ISTA)) ) THEN
               WRITE( IUVBA, '( A, A, A )' )
     1           'tape=(2,', REWSP(1:LEN1(REWSP)), ') write=(2,off)'
            END IF
         END IF
C
      ELSE
C
C        Now a recording scan.  Get the direction.
C
         IF( TPCDIR .GT. 0 .OR. AUTOALOC(ISTA) ) THEN
            SSPEED = '+RUN'
         ELSE
            SSPEED = '-RUN'
         END IF
C
C        Request motion.
C
         IF( .NOT. AUTOALOC(ISTA) ) THEN
            HEADX = HEADPOS( TPCINDX, HEADMODE(ISTA), 1 )
            WRITE( IUVBA, '(A, I2, A, A, A, I2, A, I2, A, I4, A )' )
     1         'tape=(', TPCDRIV, ',', SSPEED, 
     2         ')   write=(', TPCDRIV,
     3         ',on)  head=(', TPCDRIV, ',', HEADX, ') '
            IF( TWOHEAD .AND. 
     1          TPCDRIV+1 .LE. STNDRIV(STANUM(ISTA)) ) THEN
               HEADX = HEADPOS( TPCINDX, HEADMODE(ISTA), 2 )
               WRITE( IUVBA, '(A, I2, A, A, A, I2, A, I2, A, I4, A )' )
     1            'tape=(', TPCDRIV+1, ',', SSPEED, 
     2            ')   write=(', TPCDRIV+1,
     3            ',on)  head=(', TPCDRIV+1, ',', HEADX, ') '
            END IF
         ELSE
            WRITE( IUVBA, '(A, A, A)' )
     1         'tape=(1,', SSPEED, ')  write=(1,on)'
            IF( TWOHEAD .AND. 
     1          2 .LE. STNDRIV(STANUM(ISTA)) ) THEN
               WRITE( IUVBA, '(A, A, A)' )
     1            'tape=(2,', SSPEED, ')  write=(2,on)'
            END IF
         END IF
C
      END IF
C
      RETURN
      END




