      SUBROUTINE PLFLOD ( PFLNAM, STAT )
C
C     Routine for sched that load the information saved in the
C     parameters file
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      CHARACTER        PFLNAM*(*)
      CHARACTER*16     STRING, CLASS
      CHARACTER*80     STRIN1, ERRSTR*80
      INTEGER          STAT, I, J, K, W, N, N1, N2, IER, LUN, NT
C ----------------------------------------------------------------------
C
      LUN  = 92
      STAT = 0
      NT   = 1
C
 100  FORMAT(A, I4)
 110  FORMAT(A, I4, I4)
 120  FORMAT(A, I4)
 130  FORMAT(A)
 140  FORMAT(I4)
 150  FORMAT(I8, 4I4)
C
C     Try to open the specified file to read
C     
      IER = 1
      OPEN( UNIT=LUN, FILE=PFLNAM, FORM='FORMATTED',
     1      STATUS='OLD', ERR=500 )
C
C     Read Inputs Class
C
 1000 READ( LUN, 100, END=600, ERR=500 ) CLASS, N     
C
C     Read and set Class Objects
C
      IF( CLASS .EQ. 'EXPERIMENT' ) THEN
         READ( LUN, 130, END=600, ERR=500 ) STRING
         IER = 2
         IF( STRING .NE. EXPCODE ) GO TO 500
C
      ELSE IF( CLASS .EQ. 'STATIONS' ) THEN
         DO 10 I=1,NSTA
           PSTBCK(I,1) = 0
           PSTBCK(I,2) = 0
 10      CONTINUE
         PSTNUM = 0
         DO 14 J=1,N
            READ( LUN, 110, END=600, ERR=500 ) STRING, N1, N2
            DO 12 I=1,NSTA
               IF( STRING .EQ. STANAME(I) ) THEN
                  PSTBCK(I,1) = N1
                  PSTBCK(I,2) = N2
                  IF( N1 .EQ. 1) PSTNUM = PSTNUM + 1
               END IF
 12         CONTINUE
 14      CONTINUE
C
      ELSE IF( CLASS .EQ. 'STASWIN' ) THEN
         PSTCNT = N
C
      ELSE IF( CLASS .EQ. 'BASELINE' ) THEN
         IF( N .EQ. 0 ) THEN
             PSTBAS = .FALSE.
         ELSE
             PSTBAS = .TRUE.
         END IF
C
      ELSE IF( CLASS .EQ. 'SOURCES' ) THEN
         DO 20 I=1,NSRC
           PSOBCK(I) = 0
 20      CONTINUE
         PSONUM = 0
         DO 22 I=1,N
            READ( LUN, 120, END=600, ERR=500 ) STRING, N1
            DO J = 1, NSRC
               IF( STRING .EQ. SRCNAME(J) ) THEN
                  PSOBCK(J) = N1
                  IF( N1 .EQ. 1 ) PSONUM = PSONUM + 1
               END IF
            END DO
 22      CONTINUE
C
      ELSE IF( CLASS .EQ. 'SOURWIN' ) THEN
         PSOCNT = N
C
      ELSE IF( CLASS .EQ. 'SETUPS' ) THEN
         K = 0
         W = 0 
         DO 30 I=1,N
            READ( LUN, 120, END=600, ERR=500 ) STRIN1, N1
            IF( STRIN1 .EQ. SETFILE(I) ) THEN
               PSFPOI(I) = N1
               IF( N1 .EQ. 1 ) K = K + 1
               IF( W  .EQ. 0 ) W = I
            END IF
 30      CONTINUE
C
         PSFCNT = W / 5
         IF( MOD( W, 5 ) .GT. 0 ) PSFCNT = PSFCNT + 1
         PSFBCK = 0
         IF( K .EQ. NSETF ) PSFBCK = 1
C
      ELSE IF( CLASS .EQ. 'PLOTTYPE' ) THEN
         POPBCK = N
C
      ELSE IF( CLASS .EQ. 'BIGAREA' ) THEN
         IF( N .EQ. 0 ) THEN
             PXYBIG = .FALSE.
         ELSE
             PXYBIG = .TRUE.
         END IF
C
      ELSE IF( CLASS .EQ. 'XAXIS' ) THEN
         PXYBCK(1) = N
C
      ELSE IF( CLASS .EQ. 'TIMEZONE' ) THEN
         READ( LUN, 120, END=600, ERR=500 ) STRING, N1
C
         POFVAL(1) = N1
C
         POFVAL(2) = 0
         DO 40 I=1,NSTA
            IF( STRING .EQ. STANAME(I) ) POFVAL(2) = I
 40      CONTINUE
C
      ELSE IF( CLASS .EQ. 'XMIN' ) THEN
         READ( LUN, 140, END=600, ERR=500 ) PXSEXP(1)
C
      ELSE IF( CLASS .EQ. 'XMAX' ) THEN
         READ( LUN, 140, END=600, ERR=500 ) PXSEXP(2)
C
      ELSE IF( CLASS .EQ. 'YAXIS' ) THEN
         PXYBCK(2) = N
C
      ELSE IF( CLASS .EQ. 'YMIN' ) THEN
         READ( LUN, 140, END=600, ERR=500 ) PXSEXP(3)
C
      ELSE IF( CLASS .EQ. 'YMAX' ) THEN
         READ( LUN, 140, END=600, ERR=500 ) PXSEXP(4)
C
C     Line widths (RCW addition)
C
      ELSE IF( CLASS .EQ. 'LINEWIDSC' ) THEN
         PLYLW(1) = N
      ELSE IF( CLASS .EQ. 'LINEWIDHD' ) THEN
         PLYLW(2) = N
      ELSE IF( CLASS .EQ. 'LABWIDSC' ) THEN
         PLYAW(1) = N
      ELSE IF( CLASS .EQ. 'LABWIDHD' ) THEN
         PLYAW(2) = N
C
      END IF 
C
C     Check the Type value while NT less equal max type
C
      IF( NT .LE. PXYMAX ) THEN
         IF( CLASS .EQ. PXYTYP(NT) ) THEN
            DO 50 J = 1, N
               READ( LUN, 150, END=600, ERR=500 ) PXSVAL(NT,J,1),
     1               PXSVAL(NT,J,2), PXSVAL(NT,J,3), PXSSGN(NT,J)
 50         CONTINUE
            NT = NT + 1
         END IF
      END IF
C
C     Read another Class
C
      GO TO 1000
C
C     Report errors
C
 500  ERRSTR = 'ERROR Loading Inputs File: '
      IF( IER .EQ. 1 ) THEN
         ERRSTR(28:) = 'Unable to open file.'
      ELSE IF( IER .EQ. 2 ) THEN
         ERRSTR(28:) = 'Different experiment.'
      ELSE
         ERRSTR(28:) = 'Generic read error.'
      END IF
C
      CALL PUTOUT( ERRSTR )
      STAT = -1
C
C     Close file and returns
C
 600  CONTINUE
      CLOSE( LUN )
C
      RETURN
      END
