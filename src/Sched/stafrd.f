      SUBROUTINE STAFRD( ISCN, ISTA, IE, DFRDX, DFRDY, DDELDX, DDELDY )
C
C     Subroutine for to give the numbers for plotting the residual
C     fringe rates and delays from 1 arcsecond offsets in X and Y.
C
C     The results here might be thought of as for a baseline from the
C     center of the Earth to the station.  Actual baseline parameters
C     can be calculated from differences.
C
C     IE specifies which end of the scan to process.
C
      INCLUDE 'sched.inc'
C
      INTEGER     ISCN, ISTA, CATSTA, CATSRC, KF, IE
      REAL        U
C use commented out     REAL        V
      REAL        HAR, CH, SH, CD, SD, UXY
      REAL        BBCFREQ(MAXCHN), BBCBW(MAXCHN), FRQ
      REAL        DFRDX, DFRDY, DDELDX, DDELDY
      LOGICAL     WARNFQ
      DOUBLE PRECISION  LOSUM(MAXCHN)
C
C     OMEGA is the rotation rate of the Earth in radians per second.
C
      REAL        OMEGA, C, CONST
      PARAMETER   ( OMEGA = 7.2722052E-5 )
      PARAMETER   ( C = 2.99792458E8 )
      DATA        WARNFQ / .TRUE. /
C -------------------------------------------------------------------
C     Get the frequency.  Use the first channel.
C
      KF = FSETI( ISCN, ISTA )
      IF( KF .EQ. 0 .OR. NOSET ) THEN
         FRQ = 22200.0
         IF( WARNFQ ) THEN
            CALL WLOG( 1, 'STAFRD:  No setup, using 22.2 '//
     1                'GHz for fringe rate plot.' )
         END IF
         WARNFQ = .FALSE.
      ELSE
         CALL FSFREQ( KF, LOSUM, BBCFREQ, BBCBW )
         FRQ = LOSUM(1)
      END IF
C
C     Get station number in catalog.
C
      CATSTA = STANUM(ISTA)
      CATSRC = SRCNUM(ISCN)
C
C     Get the trig terms.
C
      IF( IE .EQ. 1 ) THEN
         HAR = HA1(ISCN,ISTA) * RADHR
      ELSE IF( IE .EQ. 2 ) THEN
         HAR = HA2(ISCN,ISTA) * RADHR
      ELSE
         CALL ERRLOG( 'Bad end parameter in STAFRD.  Need 1 or 2.' )
      END IF
      CD = COS( DECP(CATSRC) )
      SD = SIN( DECP(CATSRC) )
      UXY = SQRT( XPOS(CATSTA)**2 + YPOS(CATSTA)**2 )
      CH = COS( HAR )
      SH = SIN( HAR )
      U =  (-UXY) * SH / 1000.0 
C      V = ( ZPOS(CATSTA) * CD - UXY * SD * CH ) / 1000.0
C
C     Now get the fringe rate derivatives.
C     Note DFRDX = CONST * CD * CH / CD where one CD deals with
C     projection and the other deals with the difference between
C     an offset on the sky and the corresponding change in RA.
C     FR = -1.0 * UXY * SH * CD * FRQ * 1.E6 / C = -1.0 * CONST * SH * CD
C     DEL = -1.0 * ZPOS * SD + UXY * CD * CH
C     The sign convention is to be determined.
C
      CONST = 1000.0 * (RADDEG/3600.0) * UXY * OMEGA * FRQ * 1.E6 / C
      DFRDX = -1.0 * CONST * CH
      DFRDY = CONST * SD * SH
C
C     Next delay.  Note that there is are CDs that cancel in DELDX
C     as in DFRDX.
C
      CONST = 1.0E9 * RADDEG / ( 3600.0 * C )
      DDELDX = CONST * ( -1.0 * UXY * SH )
      DDELDY = CONST * ( -1.0 * ZPOS(CATSTA) * CD + UXY * SD * CD )
C
C     debug printout.
C
      write(*,'(A,3I6,E10.3,4F10.3)') ' stafrd ', ista, iscn, kf, uxy,
     1     dfrdx, dfrdy, ddeldx, ddeldy
C
      RETURN
      END
