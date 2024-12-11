      SUBROUTINE PLSTSL( MODE )
C
C     Scroll Left or Right the STATIONS area
C     If MODE equal 1 Clear the Area
C
C
      INCLUDE 'sched.inc'
      INCLUDE 'plot.inc'
C
      CHARACTER     STRING*12, ANUM*2
      INTEGER       MODE, I, J, K, N, REV 
      REAL          XC1, XC2, YC1, YC2, XL, CSIZ
C ----------------------------------------------------------------------
C
C     Set Font dimension, Select Buttons Policy
C     and Label offset.
C
      CSIZ  = PPNDIM(3)
      XL    = 0.02
C
C     Save PGPLOT calling attribute
C
      CALL PGSAVE
C
C     Clear Stations Area
C
      N = PSTPLT / 2
      IF( MODE .EQ. 1 ) THEN
         XC1 = PPSDIM(1)    - 0.01
         XC2 = PPSDIM(2)    + 0.01
         YC1 = PSTBXP(3,N) - 0.01
         YC2 = PSTBXP(4,1)  + 0.01
         CALL PGSCI( 0 )
         CALL PGRECT( XC1, XC2, YC1, YC2 )
      END IF
C
C     Draw the stations box
C     If stations greater than 22 set the max station button to 22
C     else to NSTA. The name index is set relative to the scroll index
C
      J = PSTCNT * N
      K = NSTA - J
      IF( K .GT. PSTPLT ) K = PSTPLT
C
      DO 20 I=1,K
         J = J + 1
         REV = 0
         IF ( PSTBCK(J,1) .GT. 0 ) REV = 1
         CALL PLBUTS( .FALSE., PSTBXP(1,I), PSTBXP(2,I), PSTBXP(3,I),
     1                PSTBXP(4,I), XL, CSIZ, ' ', 0, REV)
         REV = 0
         IF ( PSTBCK(J,2) .GT. 0 ) REV = 1
         CALL PLBUTS( .FALSE., PSTBXH(1,I), PSTBXH(2,I), PSTBXH(3,I),
     1                PSTBXH(4,I), XL, CSIZ, STANAME(J), 0, REV)
 20   CONTINUE
C
C     Make and plot a Scroll Bar with Label
C
      IF( PSTSCR ) THEN
         J = PSTCNT * ( PSTPLT / 2 )
         K = NSTA - J
         IF( K .GT. PSTPLT ) THEN
            K = J + PSTPLT
         ELSE
            K = NSTA
         END IF
C
         J = J + 1
         N = 1
         CALL PGNUMB( J, 0, 1, ANUM, I )
         STRING = ANUM(1:I)//'-'
         N = N + I + 1
         CALL PGNUMB( K, 0, 1, ANUM, I )
         STRING(N:) = ANUM(1:I)//' of '
         N = N + I + 4
         CALL PGNUMB( NSTA, 0, 1, ANUM, I )
         STRING(N:) = ANUM(1:I)
C
         CALL PLSBAR( PSTSBR(1), PSTSBR(2), PSTSBR(3),
     1                PSTSBR(4), 2, 1, PSTBXM )
C
         CALL PGSLW( 5 )
         CALL PGSCF( 1 )
         CALL PGSCH( 1.0 )
         CALL PGSCI( 15 )
         YC1 = PSTSBR(3) + 0.01
         XC1 = PSTSBR(1) + 0.05
         CALL PGPTXT(XC1, YC1, 0.0, 0.0, STRING )
      END IF
C      
C     Restore PGPLOT calling attributes
C
      CALL PGUNSA
C
      RETURN
      END
