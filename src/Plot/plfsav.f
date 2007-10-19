      SUBROUTINE PLFSAV( PFLNAM )
C
C     Routine for sched that save the current configuration 
C     in a parameters file
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      CHARACTER        PFLNAM*(*)
      CHARACTER        STRING*16, STRIN1*80
      INTEGER          I, N, K, J, LUNSAV
C ----------------------------------------------------------------------
C
      LUNSAV = 92
C
 100  FORMAT(A, I4)
 110  FORMAT(A, I4, I4)
 120  FORMAT(A, I4)
 130  FORMAT(A)
 140  FORMAT(I4)
 150  FORMAT(I8, 4I4)
C
C     Try to open the specified file to write or read
C     
      OPEN( UNIT=LUNSAV, FILE=PFLNAM, FORM='FORMATTED',
     1      STATUS='UNKNOWN', ERR=500 )
C
C     Save Experiment Name
C
      N = 1
      STRING = 'EXPERIMENT'
      WRITE( LUNSAV, 100) STRING, N     
      STRING = EXPCODE
      WRITE( LUNSAV, 130) STRING     
C
C     Save Station Informations
C
      STRING = 'STATIONS'
      WRITE( LUNSAV, 100) STRING, NSTA     
      DO 10 I=1,NSTA
         STRING = STANAME(I)
         WRITE( LUNSAV, 110) STRING, PSTBCK(I,1), PSTBCK(I,2)
 10   CONTINUE
C
C     Save Stations Position in Scroll Window
C
      STRING = 'STASWIN'
      WRITE( LUNSAV, 100) STRING, PSTCNT     
C
C     Save Single Baseline Informations
C
      STRING = 'BASELINE'
      N = 0
      IF( PSTBAS ) N = 1
      WRITE( LUNSAV, 100) STRING, N     
C
C     Save Sources Informations
C
      STRING = 'SOURCES'
      WRITE( LUNSAV, 100) STRING, NSRC     
      DO 20 I=1,NSRC
         STRING = SRCNAME(I)
         WRITE( LUNSAV, 120) STRING, PSOBCK(I)
 20   CONTINUE
C
C     Save Sources Position in Scroll Window
C
      STRING = 'SOURWIN'
      WRITE( LUNSAV, 100) STRING, PSOCNT     
C
C     Save Setups File Informations
C
      STRING = 'SETUPS'
      WRITE( LUNSAV, 100) STRING, NSETF     
      DO 30 I=1,NSETF
         N = PSFPOI(I)
         STRIN1 = SETFILE(I)
         WRITE( LUNSAV, 120) STRIN1, N
 30   CONTINUE
C
C     Save Plot Type Informations
C
      STRING = 'PLOTTYPE'
      WRITE( LUNSAV, 100) STRING, POPBCK     
C
C     Save Big area input Informations
C
      STRING = 'BIGAREA'
      N = 0
      IF( PXYBIG ) N = 1
      WRITE( LUNSAV, 100) STRING, N     
C
C     Save Xaxis Type Informations
C
      STRING = 'XAXIS'
      WRITE( LUNSAV, 100) STRING, PXYBCK(1)     
C
C     Save Time Offset Informations
C
      STRING = 'TIMEZONE'
      N = 1
      WRITE( LUNSAV, 100) STRING, N
      IF( POFVAL(2) .GT. 0 ) THEN
         STRING = STANAME(POFVAL(2))
      ELSE
         STRING = 'Greenwich'
      END IF
      WRITE( LUNSAV, 120) STRING, POFVAL(1)
C
C     Save Xmin Scale Informations
C
      N = 1 
      STRING = 'XMIN'
      WRITE( LUNSAV, 100) STRING, N
      WRITE( LUNSAV, 140) PXSEXP(1)
C
C     Save Xmax Scale Informations
C
      N = 1 
      STRING = 'XMAX'
      WRITE( LUNSAV, 100) STRING, N
      WRITE( LUNSAV, 140) PXSEXP(2)
C
C     Save Yaxis Type Informations
C
      STRING = 'YAXIS'
      WRITE( LUNSAV, 100) STRING, PXYBCK(2)     
C
C     Save Ymin Scale Informations
C
      N = 1 
      STRING = 'YMIN'
      WRITE( LUNSAV, 100) STRING, N
      WRITE( LUNSAV, 140) PXSEXP(3)
C
C     Save Ymax Scale Informations
C
      N = 1 
      STRING = 'YMAX'
      WRITE( LUNSAV, 100) STRING, N
      WRITE( LUNSAV, 140) PXSEXP(4)
C
C     Save line width information  (RCW addition)
C
      STRING = 'LINEWIDSC'
      WRITE( LUNSAV, 100 ) STRING, PLYLW(1)
      STRING = 'LINEWIDHD'
      WRITE( LUNSAV, 100 ) STRING, PLYLW(2)
      STRING = 'LABWIDSC'
      WRITE( LUNSAV, 100 ) STRING, PLYAW(1)
      STRING = 'LABWIDHD'
      WRITE( LUNSAV, 100 ) STRING, PLYAW(2)
C
C     Save Axis Type Selected Value
C
      DO 50 K = 1, PXYMAX
         N = 4
         STRING = PXYTYP(K)
         WRITE( LUNSAV, 100) STRING, N
         DO 40 J = 1, N
            WRITE( LUNSAV, 150) PXSVAL(K,J,1), PXSVAL(K,J,2),
     1                          PXSVAL(K,J,3), PXSSGN(K,J)
 40      CONTINUE
 50   CONTINUE
C
      CLOSE( LUNSAV )
C
      RETURN
C
 500  CONTINUE
      CALL PUTOUT( 'ERROR opening file to save/load inputs' )
      RETURN
      END
